open Silica

module Io = struct
  type 'a t = 'a

  let return value = value
  let bind value next = next value
  let sleep _ = ()
  let async run = run ()
  let now () = 1000.

  type 'a promise = 'a option ref

  let promise () = ref None
  let resolve promise value = if !promise = None then promise := Some value
  let await promise ~timeout:_ = !promise
end

module Server = Silica_server.Make (Io)

type tally = Counting of int [@@deriving state_shape]
type tally_event = Add

let tally_codec : tally Codec.t =
  Codec.make
    ~encode:(fun (Counting count) -> Codec.int.encode count)
    ~decode:(fun json ->
      Result.map (fun count -> Counting count) (Codec.int.decode json)
    )

let event_codec : tally_event Codec.t =
  Codec.make
    ~encode:(fun Add -> Codec.string.encode "add")
    ~decode:(fun json ->
      match Codec.string.decode json with
      | Ok "add" ->
          Ok Add
      | _ ->
          Error "not an event"
    )

let tally =
  Machine.make ~id:"tally" ~init:(Counting 0) ~shape:tally_shape
    ~events:(Machine.shape ~name:(fun Add -> "add") ())
    (fun (Counting count) Add -> Machine.Go (Counting (count + 1)))

let () = Machine.register_codecs tally ~state:tally_codec ~event:event_codec ()

let tally_live =
  Live.declare ~channel:"tally" ~state:tally_codec ~event:event_codec
    (Component.make tally (fun _ -> Node.text "tally"))

let origin = "http://app.test"

let short_directory () =
  let name =
    Filename.concat
      (Filename.get_temp_dir_name ())
      ("ss-" ^ Silica_server_kit.Random_source.hex_id ~bytes:4)
  in
  Unix.mkdir name 0o700;
  name

type hub = {
  path : string;
  lines : string list ref;
  connections : int ref;
  lock : Mutex.t;
}

let fake_hub () =
  let path = Filename.concat (short_directory ()) "inspect.sock" in
  let listener = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind listener (Unix.ADDR_UNIX path);
  Unix.listen listener 4;
  let hub =
    { path; lines = ref []; connections = ref 0; lock = Mutex.create () }
  in
  ignore
    (Thread.create
       (fun () ->
         try
           while true do
             let socket, _ = Unix.accept ~cloexec:true listener in
             Mutex.protect hub.lock (fun () -> incr hub.connections);
             ignore
               (Thread.create
                  (fun () ->
                    let input = Unix.in_channel_of_descr socket in
                    try
                      while true do
                        match In_channel.input_line input with
                        | Some line ->
                            Mutex.protect hub.lock (fun () ->
                                hub.lines := line :: !(hub.lines)
                            )
                        | None ->
                            raise Exit
                      done
                    with Exit | Sys_error _ -> ()
                  )
                  ()
               )
           done
         with Unix.Unix_error _ -> ()
       )
       ()
    );
  hub

let lines hub = Mutex.protect hub.lock (fun () -> List.rev !(hub.lines))
let connections hub = Mutex.protect hub.lock (fun () -> !(hub.connections))

let wait_for description condition =
  let deadline = Unix.gettimeofday () +. 10. in
  let rec loop () =
    if condition () then
      ()
    else if Unix.gettimeofday () > deadline then
      Alcotest.fail ("timed out: " ^ description)
    else (
      Thread.delay 0.02;
      loop ()
    )
  in
  loop ()

let make ?observe_live dev =
  Server.create
    {
      (Silica_server.default_config ~origins:(Silica_server.Origins [ origin ])
         ~secret:"0123456789abcdef0123"
      )
      with
      dev;
      observer =
        { Silica_server.stderr_observer with log = (fun ~source:_ _ -> ()) };
    }
    ?observe_live
    ~read:(fun _ -> None)
    ~handlers:[]
    ~islands:[ Server.Island tally_live ]
    ~topics:[]
    ~page:(fun (page : Server.page) ->
      { Silica_server.status = 200; headers = []; body = page.csrf }
    )
    ()

let request ?(headers = []) path =
  { Silica_server.meth = "GET"; path; query = []; headers; body = "" }

let press_add server ~instance =
  let response = Server.handle server (request "/") in
  let cookie =
    match List.assoc_opt "set-cookie" response.headers with
    | Some cookie ->
        List.hd (String.split_on_char ';' cookie)
    | None ->
        Alcotest.fail "no cookie"
  in
  match
    Server.upgrade server
      (request
         ~headers:[ ("origin", origin); ("cookie", cookie) ]
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
      socket.receive
        (Protocol.encode_client
           (Protocol.Hello
              {
                v = Protocol.version;
                csrf = response.body;
                sub = Protocol.Island { channel = "tally"; instance };
              }
           )
        );
      socket.receive (Protocol.encode_event event_codec ~id:1 Add);
      List.filter_map
        (fun text ->
          match Protocol.decode_server text with
          | Ok (Protocol.State { state; _ }) -> (
              match Protocol.decode_state tally_codec state with
              | Ok (Counting count) ->
                  Some count
              | Error message ->
                  Alcotest.fail message
            )
          | Ok _ | Error _ ->
              None
        )
        (List.rev !sent)

let without_time line =
  match Inspect.of_string line with
  | Ok (Event { event = Step { transition; owns }; target; id }) ->
      Inspect.to_string
        (Event
           {
             id;
             target;
             event =
               Step { transition = { transition with time_ms = None }; owns };
           }
        )
  | Ok _ ->
      line
  | Error message ->
      Alcotest.fail message

let dev_server () =
  let hub = fake_hub () in
  let records = ref [] in
  let server =
    make
      ~observe_live:(fun _ ~channel:_ ->
        Transition.observer (fun pending ->
            records := (Transition.resolve pending).event :: !records
        )
      )
      (Some
         {
           state_file = Filename.concat (Filename.dirname hub.path) "state.json";
           inspect = Some hub.path;
         }
      )
  in
  wait_for "the server connects" (fun () -> List.length (lines hub) >= 2);
  ignore (press_add server ~instance:"a1b2");
  wait_for "the steps arrive" (fun () -> List.length (lines hub) >= 5);
  Alcotest.(check (list string))
    "hello, the tree, then each start and step of the island"
    [
      {|{"t":"hello","role":"target","version":1,"target":"server","kind":"server"}|};
      {|{"t":"tree","target":"server","instances":[]}|};
      {|{"t":"mount","target":"server","path":"tally#a1b2","machine":"tally","state":"counting","data":0,"owns":[]}|};
      {|{"t":"step","target":"server","seq":1,"instance":"tally#a1b2","number":1,"entry":0,"machine":"tally","from":"","to":"counting","change":"started","event":"","snapshot":0,"cause":{"type":"start"},"owns":[]}|};
      {|{"t":"step","target":"server","seq":2,"instance":"tally#a1b2","number":1,"entry":0,"machine":"tally","from":"counting","to":"counting","change":"kept","event":"add","data":"add","snapshot":1,"cause":{"type":"message"},"owns":[]}|};
    ]
    (List.map without_time (lines hub));
  Alcotest.(check (list string))
    "the observer of the app still gets each step" [ ""; "add" ]
    (List.rev !records);
  Server.shutdown server

let release_server () =
  let hub = fake_hub () in
  Unix.putenv "SILICA_INSPECT" hub.path;
  let server = make None in
  ignore (press_add server ~instance:"a1b2");
  Thread.delay 0.2;
  Alcotest.(check int)
    "no connection to the inspector socket" 0 (connections hub);
  ( match
      Server.upgrade server
        (request ~headers:[ ("origin", origin) ] "/_silica/inspect")
    with
  | Server.Refused response ->
      Alcotest.(check int) "no inspector WebSocket" 404 response.status
  | Server.Accepted _ ->
      Alcotest.fail "the release server accepted an inspector WebSocket"
  );
  Unix.putenv "SILICA_INSPECT" ""

let environment () =
  Unix.putenv "SILICA_DEV_STATE" "";
  Unix.putenv "SILICA_INSPECT" "_build/silica-dev/inspect.sock";
  Alcotest.(check bool)
    "no dev, no inspector" true
    (Silica_server.dev_of_env () = None);
  Unix.putenv "SILICA_DEV_STATE" "_build/state.json";
  Alcotest.(check (option string))
    "dev reads the socket" (Some "_build/silica-dev/inspect.sock")
    (Option.bind (Silica_server.dev_of_env ()) (fun (dev : Silica_server.dev) ->
         dev.inspect
     )
    );
  Unix.putenv "SILICA_INSPECT" "";
  Alcotest.(check (option string))
    "an empty one is none" None
    (Option.bind (Silica_server.dev_of_env ()) (fun (dev : Silica_server.dev) ->
         dev.inspect
     )
    );
  Unix.putenv "SILICA_DEV_STATE" ""

let no_hub () =
  let directory = short_directory () in
  let server =
    make
      (Some
         {
           state_file = Filename.concat directory "state.json";
           inspect = Some (Filename.concat directory "missing.sock");
         }
      )
  in
  Alcotest.(check (list int))
    "the island still runs and answers" [ 0; 1 ]
    (press_add server ~instance:"c3d4");
  Server.shutdown server

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let test name run = Alcotest.test_case name `Quick run in
  Alcotest.run "inspect server"
    [
      ( "target",
        [
          test "a dev server sends its islands to the inspector" dev_server;
          test "a release server has no inspector" release_server;
          test "SILICA_INSPECT counts only in dev" environment;
          test "with no hub the islands still run" no_hub;
        ]
      );
    ]
