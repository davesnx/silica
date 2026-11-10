open Silica
open Fixture
module Json = Silica_json.Json

let pending : (unit -> unit) Queue.t = Queue.create ()
let sleeps : float list ref = ref []

module Io = struct
  type 'a t = 'a

  let return x = x
  let bind x f = f x

  let sleep s = sleeps := s :: !sleeps

  let async f = Queue.add f pending
  let now () = 0.

  type 'a promise = 'a option ref

  let promise () = ref None
  let resolve p v = if !p = None then p := Some v

  let await p ~timeout:_ =
    while not (Queue.is_empty pending) do
      (Queue.take pending) ()
    done;
    !p
end

module S = Silica_server.Make (Io)

let flush () =
  while not (Queue.is_empty pending) do
    (Queue.take pending) ()
  done

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

let check_contains msg s sub =
  Alcotest.(check bool) (msg ^ ": " ^ sub) true (contains s sub)
let check_absent msg s sub =
  Alcotest.(check bool) (msg ^ ": no " ^ sub) false (contains s sub)

let host_testable =
  Alcotest.testable
    (fun f h ->
      Format.pp_print_string f (Json.to_string (Protocol.encode_host h))
    )
    ( = )

let all_hosts =
  Protocol.
    [
      Focus (Ref.make "a");
      Blur (Ref.make "b");
      Scroll_into_view (Ref.make "c");
      Navigate (Url.of_string "/x/y?q=1#h");
      Replace (Url.of_string "/z");
      Back;
      Forward;
      Reload;
      Scroll_to { target = Some (Ref.make "d"); left = 1; top = 2 };
      Scroll_to { target = None; left = 0; top = 30 };
    ]

let protocol_tests =
  let open Alcotest in
  [
    test_case "host cmds round trip" `Quick (fun () ->
        List.iter
          (fun h ->
            check
              (result host_testable string)
              "rt" (Ok h)
              (Protocol.decode_host (Protocol.encode_host h))
          )
          all_hosts
    );
    test_case "host cmd json shapes" `Quick (fun () ->
        let shape h = Json.to_string (Protocol.encode_host h) in
        check string "focus" {|{"focus":"a"}|} (shape (List.nth all_hosts 0));
        check string "blur" {|{"blur":"b"}|} (shape (List.nth all_hosts 1));
        check string "scroll" {|{"scroll":"c"}|} (shape (List.nth all_hosts 2));
        check string "navigate" {|{"navigate":"/x/y?q=1#h"}|}
          (shape (List.nth all_hosts 3));
        check string "replace" {|{"replace":"/z"}|}
          (shape (List.nth all_hosts 4));
        check string "back" {|"back"|} (shape Protocol.Back);
        check string "forward" {|"forward"|} (shape Protocol.Forward);
        check string "reload" {|"reload"|} (shape Protocol.Reload);
        check string "scroll_to"
          {|{"scroll_to":{"target":"d","left":1,"top":2}}|}
          (shape (List.nth all_hosts 8));
        check string "scroll_to the window"
          {|{"scroll_to":{"left":0,"top":30}}|}
          (shape (List.nth all_hosts 9))
    );
    test_case "commands with a result do not cross the wire" `Quick (fun () ->
        check
          (list (option host_testable))
          "measure, copy and load_script are dropped" [ None; None; None ]
          (List.map Protocol.host_of_cmd
             [
               Cmd.Run
                 (Cmd.Measure { target = Ref.make "a"; on_rect = (fun _ -> ()) });
               Cmd.Run (Cmd.Copy { text = "t"; on_result = None });
               Cmd.Run (Cmd.Load_script { src = "/s.js"; on_result = ignore });
             ]
          )
    );
    test_case "timers, calls and stops are not host cmds" `Quick (fun () ->
        let start, _ = Machine.start machine in
        let mixing, entered, _ = Machine.send machine start Mixed in
        check int "focus, navigate, timer, call" 4 (List.length entered);
        check
          (list (option host_testable))
          "only commands cross the wire"
          [
            Some (Protocol.Focus x);
            Some (Protocol.Navigate (Url.of_string "/a?b=c"));
            None;
            None;
          ]
          (List.map Protocol.host_of_cmd entered);
        let _, left, _ = Machine.send machine mixing Cancel_later in
        check
          (list (option host_testable))
          "stops are not commands" [ None; None ]
          (List.map Protocol.host_of_cmd left);
        List.iter
          (fun h ->
            check bool "host_to_cmd inverts" true
              (Protocol.host_of_cmd
                 (Cmd.Run (Protocol.host_to_cmd h : unit Cmd.t))
              = Some h
              )
          )
          all_hosts
    );
  ]

let page url =
  S.render_page app ~status ~scripts:[ "/a.js"; "/b\"c.js" ]
    ~head:"<meta name=\"x\">" url

let page_tests =
  let open Alcotest in
  [
    test_case "known route is 200 with escaped content" `Quick (fun () ->
        let r = page (Url.of_string "/") in
        check int "status" 200 r.status;
        check_contains "doc" r.body "<!doctype html>";
        check_contains "title" r.body "<title>Count &amp; &lt;7&gt;</title>";
        check_contains "root" r.body
          {|<div id="silica-root"><p>count &lt;7&gt;</p></div>|};
        check_absent "raw" r.body "count <7>";
        check_contains "script" r.body
          {|<script type="module" src="/a.js"></script>|};
        check_contains "escaped script" r.body {|src="/b&quot;c.js"|};
        check_contains "head" r.body {|<meta name="x">|};
        check_contains "preload in the head before the given head" r.body
          {|<link rel="modulepreload" href="/a.js"><link rel="modulepreload" href="/b&quot;c.js"><meta name="x"></head>|}
    );
    test_case "unknown route is 404 with not_found view" `Quick (fun () ->
        let r = page (Url.of_string "/nope") in
        check int "status" 404 r.status;
        check_contains "title" r.body "<title>Not found</title>";
        check_contains "view" r.body "Nothing here";
        check_absent "page" r.body "count"
    );
    test_case "an island is filled with the first view of its component" `Quick
      (fun () ->
        let shell : (int, ev) Component.t =
          {
            machine =
              Silica_test_support.Simple.machine ~id:"shell" ~init:0 (fun _ _ ->
                  None
              );
            view = (fun _ -> Node.element "main" [] [ Live.slot live ]);
          }
        in
        let render ?islands () =
          (S.render_page
             (fun _ -> shell)
             ~status:(fun _ -> 200)
             ~scripts:[] ?islands (Url.of_string "/")
          )
            .body
        in
        check_contains "filled"
          (render ~islands:[ S.Island live ] ())
          {|<main><div data-silica-live="counter"><p>count &lt;0&gt;</p></div></main>|};
        check_contains "empty without the island" (render ())
          {|<main><div data-silica-live="counter"></div></main>|};
        check_contains "empty for another channel"
          (render
             ~islands:
               [
                 S.Island
                   (Live.declare ~channel:"other" ~state:live.state
                      ~event:live.event live.component
                   );
               ]
             ()
          )
          {|<div data-silica-live="counter"></div>|};
        let r =
          S.render_component shell ~title:"t" ~scripts:[]
            ~islands:[ S.Island live ] ()
        in
        check_contains "component page" r.body "count &lt;0&gt;"
    );
    test_case "the CSRF token goes into the head as a meta tag" `Quick
      (fun () ->
        let r =
          S.render_page app ~status ~scripts:[] ~csrf:"tok\"en"
            (Url.of_string "/")
        in
        check_contains "meta" r.body
          {|<meta name="silica-csrf" content="tok&quot;en">|};
        check_absent "none without a token"
          (S.render_page app ~status ~scripts:[] (Url.of_string "/")).body
          "silica-csrf"
    );
    test_case "component page" `Quick (fun () ->
        let r =
          S.render_component component ~title:"T <1>" ~scripts:[ "/m.js" ] ()
        in
        check int "status" 200 r.status;
        check_contains "title" r.body "<title>T &lt;1&gt;</title>";
        check_contains "view" r.body "count &lt;0&gt;"
    );
  ]

let session_record =
  { Silica_server.Session.id = "s1"; created = 0.; last_seen = 0.; data = [] }

let context =
  {
    S.request =
      {
        Silica_server.meth = "GET";
        path = "/";
        query = [];
        headers = [];
        body = "";
      };
    session = session_record;
  }

let handlers =
  [
    S.implement double (fun _ n -> Ok (n * 2));
    S.implement boom (fun _ _ -> Error "kaput");
  ]

type run = { session : S.runner; sent : string list ref; mutable next_id : int }

let start ?(handlers = handlers) () =
  Queue.clear pending;
  sleeps := [];
  let sent = ref [] in
  let session =
    S.live live ~handlers ~context ~send:(fun s -> sent := s :: !sent)
  in
  { session; sent; next_id = 0 }

let capture_stderr f =
  let path = Filename.temp_file "silica" ".err" in
  let fd = Unix.openfile path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  let saved = Unix.dup Unix.stderr in
  Stdlib.flush stderr;
  Unix.dup2 fd Unix.stderr;
  Unix.close fd;
  let restore () =
    Stdlib.flush stderr;
    Unix.dup2 saved Unix.stderr;
    Unix.close saved
  in
  Fun.protect ~finally:restore f;
  let ic = open_in_bin path in
  let out = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Sys.remove path;
  out

let state_messages r =
  List.rev_map
    (fun s ->
      match Protocol.decode_server s with
      | Ok (Protocol.State { seq; ack; state; cmds }) -> (
          match Protocol.decode_state Codec.int state with
          | Ok v ->
              (v, cmds, seq, ack)
          | Error e ->
              Alcotest.fail e
        )
      | Ok _ ->
          Alcotest.fail "not a state message"
      | Error e ->
          Alcotest.fail e
    )
    !(r.sent)

let states r = List.map (fun (v, cmds, _, _) -> (v, cmds)) (state_messages r)

let send r e =
  r.next_id <- r.next_id + 1;
  S.receive r.session (Protocol.encode_event ev_codec ~id:r.next_id e)

let host =
  Alcotest.testable
    (fun f h ->
      Format.pp_print_string f (Json.to_string (Protocol.encode_host h))
    )
    ( = )
let states_t = Alcotest.(list (pair int (list host)))

let live_tests =
  let open Alcotest in
  [
    test_case "first message is the initial state" `Quick (fun () ->
        let r = start () in
        check states_t "initial" [ (0, []) ] (states r)
    );
    test_case "event steps the machine and sends state" `Quick (fun () ->
        let r = start () in
        send r Inc;
        send r Inc;
        check states_t "states" [ (0, []); (1, []); (2, []) ] (states r)
    );
    test_case "host cmds are forwarded" `Quick (fun () ->
        let r = start () in
        send r Focus_me;
        check states_t "focus" [ (0, []); (0, [ Protocol.Focus x ]) ] (states r)
    );
    test_case "call and after never cross the wire" `Quick (fun () ->
        let r = start () in
        send r Mixed;
        check states_t "only host cmds"
          [
            (0, []);
            (0, [ Protocol.Focus x; Protocol.Navigate (Url.of_string "/a?b=c") ]);
          ]
          (states r)
    );
    test_case "call runs server side and feeds the result back" `Quick
      (fun () ->
        let r = start () in
        send r Inc;
        send r Inc;
        send r Inc;
        send r Double;
        flush ();
        check states_t "double"
          [ (0, []); (1, []); (2, []); (3, []); (3, []); (6, []) ]
          (states r)
    );
    test_case "failed handler becomes an error result" `Quick (fun () ->
        let r = start () in
        send r Boom;
        flush ();
        check states_t "failed" [ (0, []); (0, []); (-1, []) ] (states r)
    );
    test_case "call to an unimplemented endpoint becomes an error result" `Quick
      (fun () ->
        let r = start () in
        send r Ghost;
        flush ();
        check states_t "failed" [ (0, []); (0, []); (-1, []) ] (states r)
    );
    test_case "after sleeps then steps" `Quick (fun () ->
        let r = start () in
        send r Later;
        check states_t "before" [ (0, []); (0, []) ] (states r);
        flush ();
        check (list (float 1e-9)) "slept" [ 0.05 ] !sleeps;
        check states_t "after" [ (0, []); (0, []); (1, []) ] (states r)
    );
    test_case "a call with a timeout ends with a timeout error" `Quick
      (fun () ->
        let r = start () in
        send r Inc;
        send r Slow;
        check int "the call and its timeout are scheduled" 2
          (Queue.length pending);
        let answer = Queue.take pending in
        flush ();
        check (list (float 1e-9)) "slept the timeout" [ 0.02 ] !sleeps;
        check states_t "the machine got the error"
          [ (0, []); (1, []); (1, []); (-1, []) ]
          (states r);
        answer ();
        check states_t "the late answer is dropped"
          [ (0, []); (1, []); (1, []); (-1, []) ]
          (states r)
    );
    test_case "an answer before the timeout wins" `Quick (fun () ->
        let r = start () in
        send r Inc;
        send r Slow;
        flush ();
        check states_t "answer, not timeout"
          [ (0, []); (1, []); (1, []); (2, []) ]
          (states r)
    );
    test_case "a call without a timeout schedules no timeout" `Quick (fun () ->
        let r = start () in
        send r Double;
        check int "only the call" 1 (Queue.length pending)
    );
    test_case "a stopped timer never steps" `Quick (fun () ->
        let r = start () in
        send r Later;
        send r Cancel_later;
        flush ();
        check states_t "no tick" [ (0, []); (0, []); (0, []) ] (states r)
    );
    test_case "the answer of a stopped call is dropped" `Quick (fun () ->
        let r = start () in
        send r Inc;
        send r Double;
        send r Cancel_double;
        flush ();
        check states_t "no answer"
          [ (0, []); (1, []); (1, []); (1, []) ]
          (states r)
    );
    test_case "starting an id again replaces the running timer" `Quick
      (fun () ->
        let r = start () in
        send r Later;
        send r Later;
        flush ();
        check states_t "one tick"
          [ (0, []); (0, []); (0, []); (1, []) ]
          (states r)
    );
    test_case "events after close are dropped" `Quick (fun () ->
        let r = start () in
        send r Later;
        send r Double;
        S.close r.session;
        flush ();
        send r Inc;
        check states_t "no new state" [ (0, []); (0, []); (0, []) ] (states r)
    );
    test_case "messages that do not decode are dropped" `Quick (fun () ->
        let r = start () in
        S.receive r.session "garbage";
        S.receive r.session {|{"t":"event","id":9,"event":"nope"}|};
        send r Inc;
        check states_t "ignored" [ (0, []); (1, []) ] (states r)
    );
    test_case "a raising send closes the session instead of escaping" `Quick
      (fun () ->
        Queue.clear pending;
        let sent = ref 0 in
        let boom_send _ =
          incr sent;
          if !sent = 2 then failwith "send failed"
        in
        let session = S.live live ~handlers ~context ~send:boom_send in
        check unit "no escape" ()
          (S.receive session (Protocol.encode_event ev_codec ~id:1 Inc));
        S.receive session (Protocol.encode_event ev_codec ~id:2 Inc);
        check int "nothing sent after the failure" 2 !sent
    );
    test_case "a bad message does not stop the session" `Quick (fun () ->
        let r = start () in
        let err =
          capture_stderr (fun () ->
              S.receive r.session "garbage";
              S.receive r.session {|{"t":"event","id":9,"event":"nope"}|}
          )
        in
        check_contains "logged" err "counter";
        check_contains "logged" err "nope";
        send r Inc;
        send r Inc;
        check states_t "still running" [ (0, []); (1, []); (2, []) ] (states r)
    );
    test_case "each state has a higher seq and the ack of the last event" `Quick
      (fun () ->
        let r = start () in
        S.receive r.session (Protocol.encode_event ev_codec ~id:4 Inc);
        S.receive r.session (Protocol.encode_event ev_codec ~id:9 Inc);
        check
          (list (pair int int))
          "seq and ack"
          [ (1, 0); (2, 4); (3, 9) ]
          (List.map (fun (_, _, seq, ack) -> (seq, ack)) (state_messages r))
    );
    test_case "an event id that is not higher is dropped" `Quick (fun () ->
        let r = start () in
        S.receive r.session (Protocol.encode_event ev_codec ~id:2 Inc);
        S.receive r.session (Protocol.encode_event ev_codec ~id:2 Inc);
        S.receive r.session (Protocol.encode_event ev_codec ~id:1 Inc);
        check states_t "one step" [ (0, []); (1, []) ] (states r)
    );
    test_case "no state is sent while no socket is attached; resend sends it"
      `Quick (fun () ->
        let r = start () in
        S.set_send r.session None;
        send r Inc;
        send r Inc;
        check int "nothing sent" 1 (List.length !(r.sent));
        S.set_send r.session (Some (fun s -> r.sent := s :: !(r.sent)));
        S.resend r.session;
        check
          (list (pair int int))
          "the state the machine reached, with a new seq"
          [ (1, 0); (2, 2) ]
          (List.map (fun (_, _, seq, ack) -> (seq, ack)) (state_messages r));
        check states_t "value" [ (0, []); (2, []) ] (states r)
    );
    test_case "sessions are independent" `Quick (fun () ->
        let a = start () in
        let b = start () in
        send a Inc;
        check states_t "a" [ (0, []); (1, []) ] (states a);
        check states_t "b" [ (0, []) ] (states b)
    );
  ]

let () =
  Alcotest.run "silica-server"
    [ ("protocol", protocol_tests); ("page", page_tests); ("live", live_tests) ]
