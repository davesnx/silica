open Silica
open Silica_dev
module Ws = Silica_server_kit.Ws
module Memory = Silica_memory

let wait_for ?(timeout = 10.) description condition =
  let deadline = Unix.gettimeofday () +. timeout in
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

type peer = {
  hub : Inspector_hub.t;
  connection : Inspector_hub.connection;
  received : string list ref;
}

let peer hub =
  let received = ref [] in
  let connection =
    Inspector_hub.connect hub ~send:(fun text ->
        received := text :: !received;
        true
    )
  in
  { hub; connection; received }

let say peer message =
  ignore
    (Inspector_hub.receive peer.hub peer.connection (Inspect.to_string message))

let take peer =
  let all = List.rev !(peer.received) in
  peer.received := [];
  all

let record ?(change = Transition.Entered) ~seq ~machine ~instance ~from ~target
    ~event () : Transition.t =
  {
    seq;
    time_ms = None;
    instance;
    number = 1;
    entry = seq;
    machine;
    from;
    target;
    change;
    event;
    data = None;
    props = None;
    snapshot = None;
    actions = [];
    cause = Message;
  }

let step ?resources transition : Inspect.message =
  Event
    { id = None; target = ""; event = Step { transition; owns = resources } }

let hello_client peer = say peer (Hello_client "test")

let hello_target peer name kind =
  say peer (Hello_target { target = name; kind; url = None })

let routing () =
  let hub = Inspector_hub.create () in
  let first = peer hub and second = peer hub in
  hello_client first;
  hello_client second;
  Alcotest.(check (list string))
    "a client first gets the targets"
    [ {|{"t":"targets","targets":[]}|} ]
    (take first);
  ignore (take second);
  say first
    (Subscribe
       {
         id = Some 1;
         target = None;
         filter = { Inspect.every with machines = [ "saver" ] };
       }
    );
  say second
    (Subscribe
       {
         id = Some 1;
         target = Some "server";
         filter = { Inspect.every with changes = [ Kept ] };
       }
    );
  let tab = peer hub and server = peer hub in
  hello_target tab "tab-1" Browser;
  hello_target server "server" Server;
  say tab
    (step
       (record ~seq:1 ~machine:"saver" ~instance:"app/0:saver" ~from:"idle"
          ~target:"saving" ~event:"save" ()
       )
    );
  say tab
    (step
       (record ~seq:2 ~machine:"list" ~instance:"app/1:list" ~from:"idle"
          ~target:"loading" ~event:"load" ()
       )
    );
  say server
    (step
       (record ~change:Kept ~seq:1 ~machine:"counter" ~instance:"counter#a"
          ~from:"counter" ~target:"counter" ~event:"incr" ()
       )
    );
  say server
    (step
       (record ~change:Unhandled ~seq:2 ~machine:"counter" ~instance:"counter#a"
          ~from:"counter" ~target:"counter" ~event:"noop" ()
       )
    );
  Alcotest.(check (list string))
    "the first client: the saver of every target"
    [
      {|{"t":"subscribed","id":1}|};
      {|{"t":"hello","role":"target","version":1,"target":"tab-1","kind":"browser"}|};
      {|{"t":"hello","role":"target","version":1,"target":"server","kind":"server"}|};
      {|{"t":"step","target":"tab-1","seq":1,"instance":"app/0:saver","number":1,"entry":1,"machine":"saver","from":"idle","to":"saving","change":"entered","event":"save","cause":{"type":"message"}}|};
    ]
    (take first);
  Alcotest.(check (list string))
    "the second client: the kept steps of the server"
    [
      {|{"t":"subscribed","id":1}|};
      {|{"t":"hello","role":"target","version":1,"target":"server","kind":"server"}|};
      {|{"t":"step","target":"server","seq":1,"instance":"counter#a","number":1,"entry":1,"machine":"counter","from":"counter","to":"counter","change":"kept","event":"incr","cause":{"type":"message"}}|};
    ]
    (take second);
  Alcotest.(check (list string)) "a target gets nothing back" [] (take tab)

let timer_resource : Inspect.resource =
  {
    resource = Machine.Timer { id = "1.1/saver/saving/t"; ms = 10 };
    since = Some 5.;
  }

let cache () =
  let hub = Inspector_hub.create () in
  let tab = peer hub and client = peer hub in
  hello_target tab "tab-1" Browser;
  say tab
    (Event
       {
         id = None;
         target = "tab-1";
         event =
           Tree
             [
               {
                 path = "app";
                 machine = "app";
                 state = "ready";
                 data = None;
                 props = None;
                 owns = [];
               };
             ];
       }
    );
  say tab
    (Event
       {
         id = None;
         target = "tab-1";
         event =
           Mount
             {
               path = "app/0:saver";
               machine = "saver";
               state = "idle";
               data = None;
               props = None;
               owns = [];
             };
       }
    );
  say tab
    (step ~resources:[ timer_resource ]
       (record ~seq:3 ~machine:"saver" ~instance:"app/0:saver" ~from:"idle"
          ~target:"saving" ~event:"save" ()
       )
    );
  hello_client client;
  ignore (take client);
  say client (Request { id = Some 7; request = Get_tree None });
  Alcotest.(check (list string))
    "the tree has the state and the timer after the step"
    [
      {|{"t":"tree","id":7,"target":"tab-1","instances":[{"path":"app","machine":"app","state":"ready","owns":[]},{"path":"app/0:saver","machine":"saver","state":"saving","owns":[{"kind":"timer","id":"1.1/saver/saving/t","ms":10,"since":5.0}]}]}|};
    ]
    (take client);
  say tab (Event { id = None; target = "tab-1"; event = Unmount "app/0:saver" });
  say client (Request { id = Some 8; request = Get_tree (Some "tab-1") });
  Alcotest.(check (list string))
    "an unmount leaves the tree"
    [
      {|{"t":"tree","id":8,"target":"tab-1","instances":[{"path":"app","machine":"app","state":"ready","owns":[]}]}|};
    ]
    (take client);
  say client (Request { id = Some 9; request = Export None });
  ( match List.map Inspect.of_string (take client) with
  | [ Ok (Trace_file { id = Some 9; target = "tab-1"; trace }) ] -> (
      match Trace.of_jsonl trace with
      | Ok trace ->
          Alcotest.(check string) "root" "saver" trace.header.root;
          Alcotest.(check (list int))
            "the steps" [ 3 ]
            (List.map
               (fun (record : Transition.t) -> record.seq)
               trace.transitions
            )
      | Error message ->
          Alcotest.fail message
    )
  | _ ->
      Alcotest.fail "expected one trace"
  );
  say client
    (Request { id = Some 10; request = Travel { target = "tab-1"; seq = 3 } });
  say client (Request { id = Some 11; request = Get_tree (Some "tab-9") });
  Alcotest.(check (list string))
    "errors carry the id"
    [
      {|{"t":"error","id":10,"message":"time travel is not available yet"}|};
      {|{"t":"error","id":11,"message":"no target tab-9"}|};
    ]
    (take client)

let props_steps () =
  let hub = Inspector_hub.create () in
  let tab = peer hub and client = peer hub in
  hello_target tab "tab-1" Browser;
  say tab
    (Event
       {
         id = None;
         target = "tab-1";
         event =
           Mount
             {
               path = "panel/2:preview";
               machine = "preview";
               state = "ready";
               data = None;
               props = Some (Silica_json.Json.declassify (`String ""));
               owns = [];
             };
       }
    );
  let props_step ~change ~seq ~target props =
    step
      {
        (record ~change ~seq ~machine:"preview" ~instance:"panel/2:preview"
           ~from:"ready" ~target ~event:"" ()
        )
        with
        props = Some (Silica_json.Json.declassify (`String props));
        cause = Props;
      }
  in
  hello_client client;
  ignore (take client);
  say tab (props_step ~change:Entered ~seq:2 ~target:"loading" "ocaml");
  say client (Request { id = Some 1; request = Get_tree None });
  say tab (props_step ~change:Unhandled ~seq:3 ~target:"loading" "ml");
  say client (Request { id = Some 2; request = Get_tree None });
  Alcotest.(check (list string))
    "the tree has the props of the last props step"
    [
      {|{"t":"tree","id":1,"target":"tab-1","instances":[{"path":"panel/2:preview","machine":"preview","state":"loading","props":"ocaml","owns":[]}]}|};
      {|{"t":"tree","id":2,"target":"tab-1","instances":[{"path":"panel/2:preview","machine":"preview","state":"loading","props":"ml","owns":[]}]}|};
    ]
    (take client)

let lifecycle () =
  let hub = Inspector_hub.create () in
  let client = peer hub in
  hello_client client;
  say client (Subscribe { id = None; target = None; filter = Inspect.every });
  let old_tab = peer hub and new_tab = peer hub in
  hello_target old_tab "tab-1" Browser;
  hello_target new_tab "tab-1" Browser;
  Inspector_hub.disconnect hub old_tab.connection;
  say client (Request { id = Some 2; request = Targets });
  Inspector_hub.disconnect hub new_tab.connection;
  say client (Request { id = Some 3; request = Targets });
  Alcotest.(check (list string))
    "a second hello replaces the first; the replaced one leaves quietly"
    [
      {|{"t":"targets","targets":[]}|};
      {|{"t":"subscribed"}|};
      {|{"t":"hello","role":"target","version":1,"target":"tab-1","kind":"browser"}|};
      {|{"t":"hello","role":"target","version":1,"target":"tab-1","kind":"browser"}|};
      {|{"t":"targets","id":2,"targets":[{"target":"tab-1","kind":"browser"}]}|};
      {|{"t":"gone","target":"tab-1"}|};
      {|{"t":"targets","id":3,"targets":[]}|};
    ]
    (take client)

let refusals () =
  let hub = Inspector_hub.create () in
  let stranger = peer hub and old = peer hub and client = peer hub in
  ignore
    (Inspector_hub.receive hub stranger.connection
       {|{"t":"hello","role":"client","version":0,"name":"old"}|}
    );
  Alcotest.(check (list string))
    "another version is refused and closed"
    [
      {|{"t":"error","message":"protocol version 0; this side speaks version 1"}|};
    ]
    (take stranger);
  Alcotest.(check bool)
    "closed" true
    (Inspector_hub.is_closed stranger.connection);
  Alcotest.(check bool)
    "a message that is not a hello is not for the inspector" true
    (Inspector_hub.receive hub old.connection {|{"t":"hot-reload"}|} = `Other);
  hello_client client;
  ignore (take client);
  say client
    (step
       (record ~seq:1 ~machine:"m" ~instance:"m" ~from:"a" ~target:"b"
          ~event:"e" ()
       )
    );
  Alcotest.(check (list string))
    "a client cannot send steps"
    [ {|{"t":"error","message":"this message is not for this role"}|} ]
    (take client)

let short_directory () =
  let name =
    Filename.concat
      (Filename.get_temp_dir_name ())
      ("si-" ^ Silica_server_kit.Random_source.hex_id ~bytes:4)
  in
  Unix.mkdir name 0o700;
  name

let line_socket path =
  let socket = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.connect socket (Unix.ADDR_UNIX path);
  socket

let write_line socket message =
  Http_head.write socket (Inspect.to_string message ^ "\n")

module Saver = struct
  type event = Save | Done
  type state = Idle | Saving

  let name = function Idle -> "idle" | Saving -> "saving"

  let states = [ "idle"; "saving" ]

  let event_name = function Save -> "save" | Done -> "done"

  let machine =
    Machine.make ~id:"saver" ~init:Idle ~name ~states ~event_name
      ~during:(function Idle -> [] | Saving -> [ Source.after 2000 Done ])
      (fun state event ->
        match (state, event) with
        | Idle, Save ->
            Machine.Go Saving
        | Saving, Done ->
            Machine.Go Idle
        | Idle, Done | Saving, Save ->
            Machine.Ignore
      )

  let component =
    Component.make machine (fun state ->
        Node.element "button"
          [ Attr.on_press Save ]
          [ Node.text (match state with Idle -> "Save" | Saving -> "Saving") ]
    )
end

let tail_json () =
  let directory = short_directory () in
  let path = Filename.concat directory "inspect.sock" in
  let hub = Inspector_hub.create () in
  let listener =
    match Inspector_hub.listen hub ~path with
    | Ok listener ->
        listener
    | Error (_, message) ->
        Alcotest.fail message
  in
  Alcotest.(check int)
    "the socket is for this user only" 0o600
    ((Unix.stat path).st_perm land 0o777);
  let app = Memory.mount Saver.component in
  Memory.press (Memory.by_role ~name:"Save" app Accessibility.Button);
  Memory.advance app 2000;
  let transitions = (Memory.trace app).transitions in
  let tail =
    Unix.open_process_args_in "../../bin/silica.exe"
      [|
        "silica";
        "inspect";
        "tail";
        "--json";
        "--socket";
        path;
        "--machine";
        "saver";
        "--count";
        "4";
      |]
  in
  wait_for "the tail subscribed" (fun () -> Inspector_hub.subscribers hub = 1);
  let target = line_socket path in
  write_line target
    (Hello_target { target = "memory"; kind = Browser; url = Some "/" });
  List.iter
    (fun transition ->
      write_line target
        (Event
           {
             id = None;
             target = "memory";
             event = Step { transition; owns = None };
           }
        )
    )
    transitions;
  let lines = In_channel.input_lines tail in
  Alcotest.(check (list string))
    "one JSON line for each step, with the target"
    [
      {|{"t":"hello","role":"target","version":1,"target":"memory","kind":"browser","url":"/"}|};
      {|{"t":"step","target":"memory","seq":1,"time_ms":0.0,"instance":"saver","number":1,"entry":0,"machine":"saver","from":"","to":"idle","change":"started","event":"","cause":{"type":"start"}}|};
      {|{"t":"step","target":"memory","seq":2,"time_ms":0.0,"instance":"saver","number":1,"entry":1,"machine":"saver","from":"idle","to":"saving","change":"entered","event":"save","actions":["start timer 1.1/saver/saving/after 2000ms 2000ms"],"cause":{"type":"element","handler":"press","role":"button","name":"Save","element":"button"}}|};
      {|{"t":"step","target":"memory","seq":3,"time_ms":2000.0,"instance":"saver","number":1,"entry":2,"machine":"saver","from":"saving","to":"idle","change":"entered","event":"done","actions":["stop 1.1/saver/saving/after 2000ms"],"cause":{"type":"timer","id":"1.1/saver/saving/after 2000ms","ms":2000}}|};
    ]
    lines;
  Alcotest.(check bool)
    "the tail ends after its count" true
    (Unix.close_process_in tail = Unix.WEXITED 0);
  Unix.close target;
  Inspector_hub.stop listener;
  Alcotest.(check bool) "the socket file is removed" false (Sys.file_exists path);
  Unix.rmdir directory

let mounted ~path ~machine ~state ~props : Inspect.message =
  Event
    {
      id = None;
      target = "tab-1";
      event =
        Mount
          {
            path;
            machine;
            state;
            data = None;
            props =
              Option.map
                (fun props -> Silica_json.Json.declassify (`String props))
                props;
            owns = [];
          };
    }

let silica_lines arguments =
  let output =
    Unix.open_process_args_in "../../bin/silica.exe"
      (Array.of_list ("silica" :: arguments))
  in
  let lines = In_channel.input_lines output in
  ignore (Unix.close_process_in output);
  lines

let graph_and_tree () =
  let directory = short_directory () in
  let path = Filename.concat directory "inspect.sock" in
  let specs = Filename.concat directory "spec" in
  Unix.mkdir specs 0o700;
  List.iter
    (fun (name, lines) ->
      Out_channel.with_open_bin (Filename.concat specs name) (fun channel ->
          Out_channel.output_string channel (String.concat "\n" lines ^ "\n")
      )
    )
    [
      ( "preview.spec.txt",
        [
          "machine preview";
          "initial loading";
          "state loading";
          "state ready";
          "edge loading -> ready on loaded";
        ]
      );
      ( "panel+preview.preview.spec.txt",
        [
          "machine preview";
          "initial loading";
          "state loading";
          "state ready";
          "edge loading -> ready on loaded";
          "edge ready -> loading on props";
        ]
      );
      ( "panel+preview.panel.spec.txt",
        [
          "machine panel";
          "initial idle";
          "state idle";
          "state typing";
          "edge idle -> typing on typed";
        ]
      );
    ];
  let hub = Inspector_hub.create () in
  let listener =
    match Inspector_hub.listen hub ~path with
    | Ok listener ->
        listener
    | Error (_, message) ->
        Alcotest.fail message
  in
  let first = peer hub in
  hello_target first "tab-1" Browser;
  List.iter (say first)
    [
      step
        (record ~change:Started ~seq:1 ~machine:"panel" ~instance:"panel"
           ~from:"" ~target:"idle" ~event:"" ()
        );
      step
        (record ~change:Started ~seq:2 ~machine:"preview"
           ~instance:"panel/2:preview" ~from:"" ~target:"loading" ~event:"" ()
        );
      step
        (record ~seq:3 ~machine:"preview" ~instance:"panel/2:preview"
           ~from:"loading" ~target:"ready" ~event:"loaded" ()
        );
      step
        {
          (record ~seq:4 ~machine:"preview" ~instance:"panel/2:preview"
             ~from:"ready" ~target:"loading" ~event:"" ()
          )
          with
          cause = Props;
        };
      step
        (record ~seq:5 ~machine:"panel" ~instance:"panel" ~from:"idle"
           ~target:"typing" ~event:"typed" ()
        );
      step
        (record ~seq:6 ~machine:"panel" ~instance:"panel" ~from:"typing"
           ~target:"idle" ~event:"settle" ()
        );
    ];
  Inspector_hub.disconnect hub first.connection;
  let again = peer hub in
  hello_target again "tab-1" Browser;
  List.iter (say again)
    [
      mounted ~path:"panel" ~machine:"panel" ~state:"idle" ~props:None;
      step
        (record ~change:Resumed ~seq:1 ~machine:"panel" ~instance:"panel"
           ~from:"" ~target:"idle" ~event:"" ()
        );
      mounted ~path:"panel/2:preview" ~machine:"preview" ~state:"loading"
        ~props:(Some "ocaml");
      step
        (record ~change:Resumed ~seq:2 ~machine:"preview"
           ~instance:"panel/2:preview" ~from:"" ~target:"loading" ~event:"" ()
        );
    ];
  let graph machine =
    silica_lines
      [ "inspect"; "graph"; machine; "--specs"; specs; "--socket"; path ]
  in
  Alcotest.(check (list string))
    "the specs of every entry, and the edges taken before a reload"
    [
      "spec panel+preview.preview.spec.txt";
      "machine preview";
      "initial loading";
      "state loading";
      "state ready";
      "edge loading -> ready on loaded";
      "edge ready -> loading on props";
      "spec preview.spec.txt";
      "machine preview";
      "initial loading";
      "state loading";
      "state ready";
      "edge loading -> ready on loaded";
      "current loading at panel/2:preview [tab-1]";
      "seen edge loading -> ready on loaded";
      "seen edge ready -> loading on props";
    ]
    (graph "preview");
  Alcotest.(check (list string))
    "the spec of a parent in a joint check, and a drift"
    [
      "spec panel+preview.panel.spec.txt";
      "machine panel";
      "initial idle";
      "state idle";
      "state typing";
      "edge idle -> typing on typed";
      "current idle at panel [tab-1]";
      "seen edge idle -> typing on typed";
      "drift edge typing -> idle on settle";
    ]
    (graph "panel");
  Alcotest.(check (list string))
    "the text tree shows the props"
    [
      "[tab-1]";
      "  panel panel idle";
      "  panel/2:preview preview loading";
      "    props \"ocaml\"";
    ]
    (silica_lines [ "inspect"; "tree"; "--socket"; path ]);
  Inspector_hub.stop listener;
  List.iter
    (fun name -> Sys.remove (Filename.concat specs name))
    (Array.to_list (Sys.readdir specs));
  Unix.rmdir specs;
  Unix.rmdir directory

let graph_of_sub_states () =
  let directory = short_directory () in
  let path = Filename.concat directory "inspect.sock" in
  let specs = Filename.concat directory "spec" in
  Unix.mkdir specs 0o700;
  let spec_file = Filename.concat specs "editor.spec.txt" in
  Out_channel.with_open_bin spec_file (fun channel ->
      Out_channel.output_string channel
        (String.concat "\n"
           [
             "machine editor";
             "initial Editing.A";
             "state Editing.A";
             "state Editing.B";
             "edge Editing.A -> Editing.B on next";
           ]
        ^ "\n"
        )
  );
  let hub = Inspector_hub.create () in
  let listener =
    match Inspector_hub.listen hub ~path with
    | Ok listener ->
        listener
    | Error (_, message) ->
        Alcotest.fail message
  in
  let target = peer hub in
  hello_target target "tab-1" Browser;
  List.iter (say target)
    [
      step
        (record ~change:Started ~seq:1 ~machine:"editor" ~instance:"editor"
           ~from:"" ~target:"Editing.A" ~event:"" ()
        );
      step
        (record ~change:Kept ~seq:2 ~machine:"editor" ~instance:"editor"
           ~from:"Editing.A" ~target:"Editing.B" ~event:"next" ()
        );
    ];
  Alcotest.(check (list string))
    "a kept step between sub-states is an edge to its target"
    [
      "spec editor.spec.txt";
      "machine editor";
      "initial Editing.A";
      "state Editing.A";
      "state Editing.B";
      "edge Editing.A -> Editing.B on next";
      "seen edge Editing.A -> Editing.B on next";
    ]
    (silica_lines
       [ "inspect"; "graph"; "editor"; "--specs"; specs; "--socket"; path ]
    );
  Alcotest.(check (list string))
    "with no spec, the graph has the edge between sub-states"
    [
      "not checked editor";
      "machine editor";
      "initial Editing.A";
      "state Editing.A";
      "state Editing.B";
      "edge Editing.A -> Editing.B on next";
    ]
    (silica_lines [ "inspect"; "graph"; "editor"; "--socket"; path ]);
  Inspector_hub.stop listener;
  Sys.remove spec_file;
  Unix.rmdir specs;
  Unix.rmdir directory

let no_hub () =
  let directory = short_directory () in
  let tail =
    Unix.open_process_args_full "../../bin/silica.exe"
      [|
        "silica";
        "inspect";
        "tree";
        "--socket";
        Filename.concat directory "inspect.sock";
      |]
      [||]
  in
  let output, _, errors = tail in
  let message = In_channel.input_all errors in
  ignore (In_channel.input_all output);
  Alcotest.(check bool)
    "exit 3" true
    (Unix.close_process_full tail = Unix.WEXITED 3);
  Alcotest.(check bool)
    "it says that no silica dev runs" true
    (String.length message > 0
    &&
    let needle = "no silica dev runs here" in
    let rec has index =
      index + String.length needle <= String.length message
      && (String.sub message index (String.length needle) = needle
         || has (index + 1)
         )
    in
    has 0
    );
  Unix.rmdir directory

let websocket_origin () =
  let hub = Hub.create () in
  let proxy =
    match
      Proxy.start ~port:0
        ~app_port:(fun ~timeout:_ -> None)
        ~on_dev_socket:(Hub.attach hub) ~client_script:""
    with
    | Ok proxy ->
        proxy
    | Error message ->
        Alcotest.fail message
  in
  let upgrade ~origin =
    let socket = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt_float socket Unix.SO_RCVTIMEO 5.;
    Unix.connect socket
      (Unix.ADDR_INET (Unix.inet_addr_loopback, Proxy.port proxy));
    Http_head.write socket
      (Printf.sprintf
         "GET /_silica/inspect HTTP/1.1\r\n\
          Host: 127.0.0.1:%d\r\n\
          Origin: %s\r\n\
          Upgrade: websocket\r\n\
          Connection: Upgrade\r\n\
          Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\
          \r\n"
         (Proxy.port proxy) origin
      );
    let buffer = Buffer.create 256 and byte = Bytes.create 1 in
    while not (String.ends_with ~suffix:"\r\n\r\n" (Buffer.contents buffer)) do
      ignore (Unix.read socket byte 0 1);
      Buffer.add_bytes buffer byte
    done;
    (socket, Buffer.contents buffer)
  in
  let refused, head = upgrade ~origin:"https://evil.example" in
  Alcotest.(check bool)
    "another origin gets 403" true
    (String.starts_with ~prefix:"HTTP/1.1 403" head);
  Unix.close refused;
  let socket, head =
    upgrade ~origin:(Printf.sprintf "http://127.0.0.1:%d" (Proxy.port proxy))
  in
  Alcotest.(check bool)
    "the page gets the WebSocket" true
    (String.starts_with ~prefix:"HTTP/1.1 101" head);
  let client = peer (Hub.inspector hub) in
  hello_client client;
  say client (Subscribe { id = None; target = None; filter = Inspect.every });
  ignore (take client);
  Http_head.write socket
    (Ws.encode_masked ~mask:"abcd"
       (Ws.Text
          (Inspect.to_string
             (Hello_target { target = "tab-7"; kind = Browser; url = Some "/" })
          )
       )
    );
  wait_for "the target reaches the client" (fun () -> !(client.received) <> []);
  Alcotest.(check (list string))
    "a page is a target over the WebSocket"
    [
      {|{"t":"hello","role":"target","version":1,"target":"tab-7","kind":"browser","url":"/"}|};
    ]
    (take client);
  Unix.close socket;
  wait_for "the target leaves" (fun () -> !(client.received) <> []);
  Alcotest.(check (list string))
    "and leaves when it closes"
    [ {|{"t":"gone","target":"tab-7"}|} ]
    (take client);
  Proxy.stop proxy

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let test name run = Alcotest.test_case name `Quick run in
  Alcotest.run "inspector"
    [
      ( "hub",
        [
          test "routes two targets to two clients with filters" routing;
          test "answers tree, export and errors from what targets sent" cache;
          test "a props step updates the props of the tree" props_steps;
          test "targets connect, are replaced and leave" lifecycle;
          test "refuses other versions and messages of the wrong role" refusals;
        ]
      );
      ( "transport",
        [
          test "silica inspect tail --json prints a Silica_memory run" tail_json;
          test "silica inspect with no silica dev exits 3" no_hub;
          test "silica inspect graph and tree after a reload" graph_and_tree;
          test "silica inspect graph of a step between sub-states"
            graph_of_sub_states;
          test "the page WebSocket refuses other origins" websocket_origin;
        ]
      );
    ]
