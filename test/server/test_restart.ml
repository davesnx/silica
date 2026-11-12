open Silica
module Json = Silica_json.Json
module Session = Silica_server.Session

let pending : (unit -> unit) Queue.t = Queue.create ()
let clock = ref 1000.

module Io = struct
  type 'a t = 'a

  let return x = x
  let bind x f = f x
  let sleep _ = ()
  let async f = Queue.add f pending
  let now () = !clock

  type 'a promise = 'a option ref

  let promise () = ref None
  let resolve p v = if !p = None then p := Some v

  let await p ~timeout:_ =
    while not (Queue.is_empty pending) do
      (Queue.take pending) ()
    done;
    !p
end

module Server = Silica_server.Make (Io)

let flush () =
  while not (Queue.is_empty pending) do
    (Queue.take pending) ()
  done

let contains text part =
  let length = String.length part in
  let rec from index =
    index + length <= String.length text
    && (String.sub text index length = part || from (index + 1))
  in
  from 0

type tally = Counting of int | Waiting of int [@@deriving state_shape]
type tally_event = Add | Wait | Tick

let tally_codec : tally Codec.t =
  Codec.make
    ~encode:(function
      | Counting count ->
          Codec.constructor "Counting" [ Codec.int.encode count ]
      | Waiting count ->
          Codec.constructor "Waiting" [ Codec.int.encode count ]
      )
    ~decode:(fun json ->
      match Codec.constructor_of json with
      | Ok ("Counting", [ count ]) ->
          Result.map (fun count -> Counting count) (Codec.int.decode count)
      | Ok ("Waiting", [ count ]) ->
          Result.map (fun count -> Waiting count) (Codec.int.decode count)
      | Ok _ | Error _ ->
          Error "not a tally"
    )

let event_codec : tally_event Codec.t =
  Codec.make
    ~encode:(fun event ->
      Codec.string.encode
        (match event with Add -> "add" | Wait -> "wait" | Tick -> "tick")
    )
    ~decode:(fun json ->
      match Codec.string.decode json with
      | Ok "add" ->
          Ok Add
      | Ok "wait" ->
          Ok Wait
      | Ok "tick" ->
          Ok Tick
      | Ok other ->
          Error ("bad event " ^ other)
      | Error message ->
          Error message
    )

let field = Ref.make "field"

let tally =
  Machine.make ~id:"tally" ~init:(Counting 0) ~shape:tally_shape
    ~owns:(function Waiting _ -> [ Source.after 50 Tick ] | Counting _ -> [])
    ~entry:(function Counting _ -> [ Action.Focus field ] | Waiting _ -> [])
    (fun state event ->
      match (state, event) with
      | Counting count, Add ->
          Machine.Go (Counting (count + 1))
      | Counting count, Wait ->
          Machine.Go (Waiting count)
      | Waiting count, Tick ->
          Machine.Go (Counting (count + 10))
      | (Counting _ | Waiting _), _ ->
          Machine.Ignore
    )

let () = Machine.register_codecs tally ~state:tally_codec ()

let tally_live =
  Live.declare ~channel:"tally" ~state:tally_codec ~event:event_codec
    (Component.make tally (fun _ -> Node.text "tally"))

let plain =
  Machine.make ~id:"plain" ~init:0
    ~shape:(Machine.shape ~name:(fun _ -> "on") ())
    (fun count event ->
      match event with Add -> Machine.Go (count + 1) | Wait | Tick -> Ignore
    )

let plain_live =
  Live.declare ~channel:"plain" ~state:Codec.int ~event:event_codec
    (Component.make plain (fun _ -> Node.text "plain"))

let who =
  Remote.declare ~path:"/api/who" ~input:Codec.unit ~output:Codec.string
    ~rejected:Codec.string

let secret = "0123456789abcdef0123"
let origin = "http://app.test"
let logs : string list ref = ref []

let observer =
  {
    Silica_server.stderr_observer with
    log = (fun ~source line -> logs := (source ^ ": " ^ line) :: !logs);
  }

let logged part = List.exists (fun line -> contains line part) !logs
let records : Transition.t list ref = ref []

let directory () =
  let directory = Filename.temp_dir "silica-restart" "" in
  Filename.concat directory "state.json"

let make ?(dev = true) ?keep state_file =
  let config =
    {
      (Silica_server.default_config ~origins:(Silica_server.Origins [ origin ])
         ~secret
      )
      with
      dev =
        ( if dev then
            Some { Silica_server.state_file; inspect = None }
          else
            None
        );
      now = (fun () -> !clock);
      observer;
    }
  in
  let server =
    Server.create config
      ~observe_live:(fun _ ~channel:_ ->
        Transition.observer (fun pending ->
            records := Transition.resolve pending :: !records
        )
      )
      ~read:(fun _ -> None)
      ~handlers:
        [
          Server.implement who (fun (context : Server.context) () ->
              Ok context.session.id
          );
        ]
      ~islands:[ Server.Island tally_live; Server.Island plain_live ]
      ~topics:[]
      ~page:(fun (page : Server.page) ->
        {
          Silica_server.status = 200;
          headers = [ ("content-type", "text/plain") ];
          body = "csrf=" ^ page.csrf;
        }
      )
      ()
  in
  Option.iter
    (fun (save, load) -> Server.keep server ~name:"data" ~save ~load)
    keep;
  server

let request ?(meth = "GET") ?(headers = []) ?(body = "") path =
  { Silica_server.meth; path; query = []; headers; body }

let header name (response : Silica_server.response) =
  List.assoc_opt name response.headers

let after marker text =
  let start = String.length marker in
  String.sub text start (String.length text - start)

type client = { cookie : string; csrf : string }

let visit server =
  let response = Server.handle server (request "/") in
  match header "set-cookie" response with
  | None ->
      Alcotest.fail "no cookie"
  | Some cookie ->
      {
        cookie = List.hd (String.split_on_char ';' cookie);
        csrf = after "csrf=" response.body;
      }

let page server client =
  Server.handle server (request ~headers:[ ("cookie", client.cookie) ] "/")

let call server client =
  Server.handle server
    (request ~meth:"POST"
       ~headers:
         [
           ("cookie", client.cookie);
           ("origin", origin);
           ("content-type", "application/json");
           ("x-silica-csrf", client.csrf);
         ]
       ~body:"null" "/api/who"
    )

type socket = { sent : string list ref; connection : Server.connection }

let connect server client ~channel ~instance =
  match
    Server.upgrade server
      (request
         ~headers:[ ("origin", origin); ("cookie", client.cookie) ]
         Silica_server.live_path
      )
  with
  | Server.Refused response ->
      Alcotest.failf "refused with %d" response.status
  | Server.Accepted attach ->
      let sent = ref [] in
      let socket =
        attach
          {
            Server.send = (fun text -> sent := text :: !sent);
            close = (fun _ -> ());
          }
      in
      let fake = { sent; connection = socket } in
      socket.receive
        (Protocol.encode_client
           (Protocol.Hello
              {
                v = Protocol.version;
                csrf = client.csrf;
                sub = Protocol.Island { channel; instance };
              }
           )
        );
      fake

let send fake id event =
  fake.connection.receive (Protocol.encode_event event_codec ~id event)

let states codec fake =
  List.filter_map
    (fun text ->
      match Protocol.decode_server text with
      | Ok (Protocol.State { ack; state; actions; _ }) -> (
          match Protocol.decode_state codec state with
          | Ok value ->
              Some (ack, value, List.length actions)
          | Error message ->
              Alcotest.fail message
        )
      | Ok _ ->
          None
      | Error message ->
          Alcotest.fail message
    )
    (List.rev !(fake.sent))

let last codec fake =
  match List.rev (states codec fake) with
  | state :: _ ->
      state
  | [] ->
      Alcotest.fail "no state"

let tally_t =
  Alcotest.testable
    (fun format -> function
      | Counting count ->
          Format.fprintf format "Counting %d" count
      | Waiting count ->
          Format.fprintf format "Waiting %d" count
      )
    ( = )

let read_file path = In_channel.with_open_bin path In_channel.input_all

let write_file path text =
  Out_channel.with_open_gen [ Open_wronly; Open_creat; Open_trunc ] 0o600 path
    (fun channel -> Out_channel.output_string channel text
  )

let mode path = (Unix.stat path).st_perm
let reset () =
  logs := [];
  records := [];
  Queue.clear pending

let restart_tests =
  let open Alcotest in
  [
    test_case "the cookie, the token and the island survive a restart" `Quick
      (fun () ->
        reset ();
        let state_file = directory () in
        let first = make state_file in
        let client = visit first in
        let socket = connect first client ~channel:"tally" ~instance:"i1" in
        send socket 1 Add;
        send socket 2 Add;
        send socket 3 Add;
        check (triple int tally_t int) "three adds" (3, Counting 3, 0)
          (last tally_codec socket);
        Server.shutdown first;
        check int "owner only" 0o600 (mode state_file);
        records := [];
        let second = make state_file in
        check bool "the file is removed after the read" false
          (Sys.file_exists state_file);
        check int "the island waits for its socket" 1 (Server.islands second);
        let response = page second client in
        check (option string) "the cookie names a session" None
          (header "set-cookie" response);
        check string "the same token" ("csrf=" ^ client.csrf) response.body;
        check int "a call with the token passes" 200 (call second client).status;
        let socket = connect second client ~channel:"tally" ~instance:"i1" in
        check
          (list (triple int tally_t int))
          "resumed at the saved state, with no entry actions"
          [ (3, Counting 3, 0) ]
          (states tally_codec socket);
        send socket 3 Add;
        send socket 4 Add;
        check (triple int tally_t int) "an old event id is dropped"
          (4, Counting 4, 0) (last tally_codec socket);
        match List.rev !records with
        | first :: _ ->
            check string "the log says resumed" "resumed"
              ( match first.change with
              | Transition.Resumed ->
                  "resumed"
              | Started | Unhandled | Kept | Entered | Ended ->
                  "other"
              );
            check string "at the saved state" "counting" first.target;
            check (list string) "no actions" [] first.actions
        | [] ->
            fail "no record"
    );
    test_case "a resumed island starts the timer its state owns" `Quick
      (fun () ->
        reset ();
        let state_file = directory () in
        let first = make state_file in
        let client = visit first in
        let socket = connect first client ~channel:"tally" ~instance:"i1" in
        send socket 1 Add;
        send socket 2 Wait;
        Queue.clear pending;
        check (triple int tally_t int) "waiting" (2, Waiting 1, 0)
          (last tally_codec socket);
        Server.shutdown first;
        records := [];
        let second = make state_file in
        let socket = connect second client ~channel:"tally" ~instance:"i1" in
        ( match !records with
        | [ resumed ] ->
            check (list string) "only the start of the timer"
              [ "start timer tally/waiting/after 50ms 50ms" ]
              resumed.actions
        | _ ->
            fail "one record"
        );
        flush ();
        check (triple int tally_t int) "the timer fired" (2, Counting 11, 1)
          (last tally_codec socket)
    );
    test_case "a broken island state drops that island only" `Quick (fun () ->
        reset ();
        let state_file = directory () in
        let first = make state_file in
        let client = visit first in
        let one = connect first client ~channel:"tally" ~instance:"i1" in
        let two = connect first client ~channel:"tally" ~instance:"i2" in
        send one 1 Add;
        send two 1 Add;
        send two 2 Add;
        Server.shutdown first;
        let text = read_file state_file in
        let broken =
          match Json.of_string text with
          | Ok json -> (
              match Json.classify json with
              | `Assoc fields ->
                  Json.declassify
                    (`Assoc
                       (List.map
                          (fun (name, value) ->
                            match (name, Json.classify value) with
                            | "islands", `List islands ->
                                ( name,
                                  Json.declassify
                                    (`List
                                       (List.map
                                          (fun island ->
                                            match Json.classify island with
                                            | `Assoc island
                                              when List.assoc_opt "instance"
                                                     island
                                                   = Some
                                                       (Json.declassify
                                                          (`String "i2")
                                                       ) ->
                                                Json.declassify
                                                  (`Assoc
                                                     (List.map
                                                        (fun (key, value) ->
                                                          if key = "snapshot"
                                                          then
                                                            ( key,
                                                              Json.declassify
                                                                (`String "bad")
                                                            )
                                                          else
                                                            (key, value)
                                                        )
                                                        island
                                                     )
                                                  )
                                            | _ ->
                                                island
                                          )
                                          islands
                                       )
                                    )
                                )
                            | _ ->
                                (name, value)
                          )
                          fields
                       )
                    )
              | _ ->
                  fail "not an object"
            )
          | Error message ->
              fail message
        in
        write_file state_file (Json.to_string broken);
        let second = make state_file in
        check int "one island restored" 1 (Server.islands second);
        check bool "the log names the island and the reason" true
          (logged
             "island \"tally\" (instance \"i2\"): its snapshot does not decode"
          );
        let one = connect second client ~channel:"tally" ~instance:"i1" in
        let two = connect second client ~channel:"tally" ~instance:"i2" in
        check tally_t "i1 kept" (Counting 1)
          (let _, state, _ = last tally_codec one in
           state
          );
        check tally_t "i2 starts again" (Counting 0)
          (let _, state, _ = last tally_codec two in
           state
          )
    );
    test_case
      "an island with no state codec starts again, and the log says how to \
       register it"
      `Quick (fun () ->
        reset ();
        let state_file = directory () in
        let first = make state_file in
        let client = visit first in
        let socket = connect first client ~channel:"plain" ~instance:"p1" in
        send socket 1 Add;
        check (triple int int int) "one" (1, 1, 0) (last Codec.int socket);
        Server.shutdown first;
        check bool "the log names the machine" true
          (logged "island \"plain\" (instance \"p1\") not saved");
        check bool "and how to register the codec" true
          (logged
             "\"plain\" has no registered codecs: call Machine.register_codecs"
          );
        let second = make state_file in
        let socket = connect second client ~channel:"plain" ~instance:"p1" in
        check (triple int int int) "the initial state" (0, 0, 0)
          (last Codec.int socket)
    );
    test_case "a second shutdown and create reach the same state" `Quick
      (fun () ->
        reset ();
        let state_file = directory () in
        let first = make state_file in
        let client = visit first in
        let socket = connect first client ~channel:"tally" ~instance:"i1" in
        send socket 1 Add;
        send socket 2 Add;
        Server.shutdown first;
        let saved = read_file state_file in
        let second = make state_file in
        Server.shutdown second;
        check string "the same file" saved (read_file state_file);
        Sys.remove state_file;
        Server.shutdown second;
        check bool "a second shutdown of one server does nothing" false
          (Sys.file_exists state_file);
        write_file state_file saved;
        let third = make state_file in
        check string "the same token" ("csrf=" ^ client.csrf)
          (page third client).body;
        let socket = connect third client ~channel:"tally" ~instance:"i1" in
        check (triple int tally_t int) "the same state" (2, Counting 2, 0)
          (last tally_codec socket)
    );
    test_case
      "kept values come back, also through a server that did not claim them"
      `Quick (fun () ->
        reset ();
        let state_file = directory () in
        let loaded = ref [] in
        let keep value =
          ( (fun () -> value),
            fun text ->
              loaded := text :: !loaded;
              Ok ()
          )
        in
        let first = make ~keep:(keep "v1") state_file in
        check (list string) "nothing to load at first" [] !loaded;
        Server.shutdown first;
        let second = make state_file in
        Server.shutdown second;
        let _third = make ~keep:(keep "v2") state_file in
        check (list string) "the value of the first server" [ "v1" ] !loaded
    );
    test_case "a failing load of a kept value is logged" `Quick (fun () ->
        reset ();
        let state_file = directory () in
        Server.shutdown
          (make ~keep:((fun () -> "v1"), fun _ -> Ok ()) state_file);
        let _second =
          make ~keep:((fun () -> "v1"), fun _ -> Error "bad data") state_file
        in
        check bool "logged" true (logged "kept value data not loaded: bad data")
    );
  ]

let security_tests =
  let open Alcotest in
  [
    test_case "with no dev, nothing is written or read" `Quick (fun () ->
        reset ();
        let state_file = directory () in
        let loaded = ref false in
        let first =
          make ~dev:false
            ~keep:
              ( (fun () -> "v"),
                fun _ ->
                  loaded := true;
                  Ok ()
              )
            state_file
        in
        let client = visit first in
        Server.shutdown first;
        check bool "no file" false (Sys.file_exists state_file);
        let other = make state_file in
        ignore (visit other);
        Server.shutdown other;
        let second =
          make ~dev:false
            ~keep:
              ( (fun () -> "v"),
                fun _ ->
                  loaded := true;
                  Ok ()
              )
            state_file
        in
        check bool "the file stays" true (Sys.file_exists state_file);
        check bool "no load" false !loaded;
        check bool "the session is unknown" true
          (header "set-cookie" (page second client) <> None)
    );
    test_case "a file that others can read or write is ignored" `Quick
      (fun () ->
        reset ();
        let state_file = directory () in
        let first = make state_file in
        let client = visit first in
        Server.shutdown first;
        Unix.chmod state_file 0o644;
        let second = make state_file in
        check bool "logged" true
          (logged "it must belong to this user with mode 600");
        check bool "not removed" true (Sys.file_exists state_file);
        check bool "the session is unknown" true
          (header "set-cookie" (page second client) <> None)
    );
    test_case "a file that is not JSON is ignored" `Quick (fun () ->
        reset ();
        let state_file = directory () in
        write_file state_file "{ not json";
        let server = make state_file in
        check bool "logged" true (logged "ignored: not JSON");
        check int "no island" 0 (Server.islands server)
    );
    test_case "a forged island with no saved session is dropped" `Quick
      (fun () ->
        reset ();
        let state_file = directory () in
        write_file state_file
          {|{"sessions":[],"islands":[{"session":"abcd","channel":"tally","instance":"i1","machine":"tally","snapshot":["Counting",5],"ack":0,"entry":0}],"kept":{}}|};
        let server = make state_file in
        check int "no island" 0 (Server.islands server);
        check bool "logged" true (logged "its session is gone")
    );
    test_case "an island of another machine is dropped" `Quick (fun () ->
        reset ();
        let state_file = directory () in
        let first = make state_file in
        let client = visit first in
        ignore client;
        Server.shutdown first;
        let text = read_file state_file in
        let session =
          match Json.of_string text with
          | Ok json -> (
              match Json.classify json with
              | `Assoc fields -> (
                  match
                    Option.map Json.classify (List.assoc_opt "sessions" fields)
                  with
                  | Some (`List [ session ]) -> (
                      match Json.classify session with
                      | `Assoc session -> (
                          match
                            Option.map Json.classify
                              (List.assoc_opt "id" session)
                          with
                          | Some (`String id) ->
                              (Json.to_string (List.assoc "sessions" fields), id)
                          | _ ->
                              fail "no id"
                        )
                      | _ ->
                          fail "not an object"
                    )
                  | _ ->
                      fail "one session"
                )
              | _ ->
                  fail "not an object"
            )
          | Error message ->
              fail message
        in
        let sessions, id = session in
        write_file state_file
          (Printf.sprintf
             {|{"sessions":%s,"islands":[{"session":"%s","channel":"tally","instance":"i1","machine":"plain","snapshot":["Counting",5],"ack":0,"entry":0}],"kept":{}}|}
             sessions id
          );
        let server = make state_file in
        check int "no island" 0 (Server.islands server);
        check bool "logged" true
          (logged "it ran the machine \"plain\", the channel now has \"tally\"")
    );
  ]

let session_tests =
  let open Alcotest in
  let session ?(id = "ab12") ?(created = 100.) ?(last_seen = 200.)
      ?(data = [ ("user", "ada") ]) () =
    { Session.id; created; last_seen; data }
  in
  [
    test_case "save and load keep every session" `Quick (fun () ->
        let store = Session.create () in
        Session.set store (session ());
        Session.set store (session ~id:"cd34" ~data:[] ());
        let copy = Session.create () in
        check (result int string) "two" (Ok 2)
          (Session.load copy ~now:300. (Session.save store));
        check
          (option (list (pair string string)))
          "data"
          (Some [ ("user", "ada") ])
          (Option.map
             (fun (s : Session.session) -> s.data)
             (Session.get copy ~now:300. "ab12")
          )
    );
    test_case "load skips what a cookie could not hold" `Quick (fun () ->
        let store = Session.create ~idle:1000. () in
        Session.set store (session ~id:"BAD ID" ());
        Session.set store (session ~id:"ff" ~last_seen:2000. ());
        Session.set store (session ~id:"aa" ~last_seen:50. ());
        Session.set store (session ~id:"bb" ~created:0. ~last_seen:10. ());
        Session.set store (session ~id:"cc" ());
        let copy = Session.create ~idle:1000. () in
        check (result int string) "only the valid one" (Ok 1)
          (Session.load copy ~now:1100. (Session.save store));
        check bool "cc" true (Session.get copy ~now:1100. "cc" <> None)
    );
    test_case "load does not replace a session of the store" `Quick (fun () ->
        let store = Session.create () in
        Session.set store (session ~data:[ ("user", "eve") ] ());
        let copy = Session.create () in
        Session.set copy (session ());
        check (result int string) "none added" (Ok 0)
          (Session.load copy ~now:300. (Session.save store));
        check
          (option (list (pair string string)))
          "kept"
          (Some [ ("user", "ada") ])
          (Option.map
             (fun (s : Session.session) -> s.data)
             (Session.get copy ~now:300. "ab12")
          )
    );
    test_case "text that is not an array is an error" `Quick (fun () ->
        let store = Session.create () in
        check bool "not JSON" true
          (Result.is_error (Session.load store ~now:0. "{"));
        check bool "an object" true
          (Result.is_error (Session.load store ~now:0. "{}"))
    );
  ]

let resume_tests =
  let open Alcotest in
  [
    test_case
      "resume gives the starts of what the snapshot owns, and nothing else"
      `Quick (fun () ->
        let describe actions = List.map Transition.describe actions in
        check (list string) "the timer of waiting"
          [ "start timer tally/waiting/after 50ms 50ms" ]
          (describe (Machine.resume tally () (Waiting 3)));
        check (list string) "no entry focus" []
          (describe (Machine.resume tally () (Counting 3)));
        let _, started = Machine.start tally () in
        check (list string) "start runs the entry" [ "focus field" ]
          (describe started)
    );
  ]

let environment_tests =
  let open Alcotest in
  [
    test_case "the secret comes from the environment" `Quick (fun () ->
        let localhost = Silica_server.Localhost
        and public = Silica_server.Origins [ origin ]
        and dev =
          Some { Silica_server.state_file = "state.json"; inspect = None }
        in
        if Sys.getenv_opt "SILICA_SECRET" = None then (
          check bool "public origins need it" true
            (Result.is_error
               (Silica_server.secret_of_env ~origins:public ~dev:None)
            );
          check bool "dev needs it" true
            (Result.is_error
               (Silica_server.secret_of_env ~origins:localhost ~dev)
            );
          check bool "a local server gets a random one" true
            ( match Silica_server.secret_of_env ~origins:localhost ~dev:None with
            | Ok secret ->
                String.length secret >= 16
            | Error _ ->
                false
            )
        );
        Unix.putenv "SILICA_SECRET" "short";
        check bool "a short one is an error" true
          (Result.is_error
             (Silica_server.secret_of_env ~origins:localhost ~dev:None)
          );
        Unix.putenv "SILICA_SECRET" secret;
        check (result string string) "the value" (Ok secret)
          (Silica_server.secret_of_env ~origins:public ~dev)
    );
    test_case "dev mode comes from SILICA_DEV_STATE" `Quick (fun () ->
        Unix.putenv "SILICA_DEV_STATE" "";
        check bool "empty" true (Silica_server.dev_of_env () = None);
        Unix.putenv "SILICA_DEV_STATE" "_build/state.json";
        check (option string) "set" (Some "_build/state.json")
          (Option.map
             (fun (dev : Silica_server.dev) -> dev.state_file)
             (Silica_server.dev_of_env ())
          )
    );
  ]

let html_server ~dev =
  Server.create
    {
      (Silica_server.default_config ~origins:(Silica_server.Origins [ origin ])
         ~secret
      )
      with
      dev =
        ( if dev then
            Some { Silica_server.state_file = directory (); inspect = None }
          else
            None
        );
    }
    ~read:(fun _ -> None)
    ~handlers:[] ~islands:[] ~topics:[]
    ~page:(fun _ ->
      {
        Silica_server.status = 200;
        headers = [ ("content-type", "text/html; charset=utf-8") ];
        body = "<html><body><p>hi</p></body></html>";
      }
    )
    ()

let dev_client_tests =
  let open Alcotest in
  [
    test_case "with dev, a page loads the dev client before </body>" `Quick
      (fun () ->
        check string "body"
          "<html><body><p>hi</p><script type=\"module\" \
           src=\"/_silica/dev/client.js\"></script></body></html>"
          (Server.handle (html_server ~dev:true) (request "/")).body
    );
    test_case "with no dev, a page has no dev client" `Quick (fun () ->
        check string "body" "<html><body><p>hi</p></body></html>"
          (Server.handle (html_server ~dev:false) (request "/")).body
    );
  ]

let () =
  Alcotest.run "restart"
    [
      ("restart", restart_tests);
      ("security", security_tests);
      ("sessions", session_tests);
      ("resume", resume_tests);
      ("environment", environment_tests);
      ("dev client", dev_client_tests);
    ]
