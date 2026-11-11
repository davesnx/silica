open Silica
open Fixture
module Adapter = Silica_example_adapter
module Server = Adapter.Server
module Kit = Silica_server_kit

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

let connect port =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt_float fd Unix.SO_RCVTIMEO 5.;
  Unix.connect fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
  fd

let write fd s =
  let rec go off =
    if off < String.length s then
      go (off + Unix.write_substring fd s off (String.length s - off))
  in
  go 0

let read_all fd =
  let b = Buffer.create 1024 and chunk = Bytes.create 4096 in
  let rec go () =
    match Unix.read fd chunk 0 4096 with
    | 0 ->
        ()
    | n ->
        Buffer.add_subbytes b chunk 0 n;
        go ()
    | exception Unix.Unix_error _ ->
        ()
  in
  go ();
  Buffer.contents b

let split_head raw =
  let rec find i =
    if i + 4 > String.length raw then
      Alcotest.failf "no header end in %S" raw
    else if String.sub raw i 4 = "\r\n\r\n" then
      i
    else
      find (i + 1)
  in
  let i = find 0 in
  (String.sub raw 0 i, String.sub raw (i + 4) (String.length raw - i - 4))

let dechunk body =
  let b = Buffer.create 64 in
  let rec go pos =
    let eol = String.index_from body pos '\r' in
    let n = int_of_string ("0x" ^ String.sub body pos (eol - pos)) in
    if n > 0 then (
      Buffer.add_string b (String.sub body (eol + 2) n);
      go (eol + 2 + n + 2)
    )
  in
  go 0;
  Buffer.contents b

type reply = { status : int; headers : (string * string) list; body : string }

let parse_head head =
  match String.split_on_char '\n' head with
  | status :: lines ->
      ( Scanf.sscanf status "HTTP/1.1 %d" Fun.id,
        List.filter_map
          (fun line ->
            let line = String.trim line in
            match String.index_opt line ':' with
            | Some i ->
                Some
                  ( String.lowercase_ascii (String.sub line 0 i),
                    String.trim
                      (String.sub line (i + 1) (String.length line - i - 1))
                  )
            | None ->
                None
          )
          lines
      )
  | [] ->
      Alcotest.fail "empty response"

let http ?(headers = []) ?content_length port meth path body =
  let fd = connect port in
  let length = Option.value content_length ~default:(String.length body) in
  write fd
    (Printf.sprintf
       "%s %s HTTP/1.1\r\n\
        Host: localhost:%d\r\n\
        Connection: close\r\n\
        %sContent-Length: %d\r\n\
        \r\n\
        %s"
       meth path port
       (String.concat ""
          (List.map (fun (k, v) -> Printf.sprintf "%s: %s\r\n" k v) headers)
       )
       length body
    );
  let raw = read_all fd in
  Unix.close fd;
  let head, body = split_head raw in
  let status, headers = parse_head head in
  let body =
    if List.assoc_opt "transfer-encoding" headers = Some "chunked" then
      dechunk body
    else
      body
  in
  { status; headers; body }

let header name r = List.assoc_opt name r.headers

let word_after marker body =
  let n = String.length marker in
  let rec find i =
    if i + n > String.length body then
      Alcotest.failf "no %S in %S" marker body
    else if String.sub body i n = marker then
      i + n
    else
      find (i + 1)
  in
  let start = find 0 in
  let stop = String.index_from body start '"' in
  String.sub body start (stop - start)

type visitor = { cookie : string; csrf : string; origin : string; port : int }

let visit port =
  let r = http port "GET" "/" "" in
  let set_cookie = Option.get (header "set-cookie" r) in
  {
    cookie = String.sub set_cookie 0 (String.index set_cookie ';');
    csrf = word_after {|name="silica-csrf" content="|} r.body;
    origin = Printf.sprintf "http://localhost:%d" port;
    port;
  }

let post ?(origin = true) ?(content_type = "application/json") v path body =
  http v.port "POST" path body
    ~headers:
      ([
         ("Cookie", v.cookie);
         ("Content-Type", content_type);
         ("X-Silica-Csrf", v.csrf);
       ]
      @
      if origin then
        [ ("Origin", v.origin) ]
      else
        []
      )

let ws_connect ?(origin = true) ?(cookie = true) v path =
  let fd = connect v.port in
  write fd
    (Printf.sprintf
       "GET %s HTTP/1.1\r\n\
        Host: localhost:%d\r\n\
        Upgrade: websocket\r\n\
        Connection: Upgrade\r\n\
        Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\
        Sec-WebSocket-Version: 13\r\n\
        %s%s\r\n"
       path v.port
       ( if origin then
           "Origin: " ^ v.origin ^ "\r\n"
         else
           ""
       )
       ( if cookie then
           "Cookie: " ^ v.cookie ^ "\r\n"
         else
           ""
       )
    );
  let ic = Unix.in_channel_of_descr fd in
  let rec head acc =
    match input_char ic with
    | c ->
        let acc = acc ^ String.make 1 c in
        if
          String.length acc >= 4
          && String.sub acc (String.length acc - 4) 4 = "\r\n\r\n"
        then
          acc
        else
          head acc
    | exception End_of_file ->
        acc
  in
  let head = head "" in
  (fd, ic, head)

let ws_send fd message =
  write fd (Kit.Ws.encode_masked ~mask:"\x01\x02\x03\x04" message)

let ws_text fd s = ws_send fd (Kit.Ws.Text s)

let read_frame ic =
  let b0 = Char.code (input_char ic) in
  let b1 = Char.code (input_char ic) in
  if b1 land 0x80 <> 0 then Alcotest.fail "server frame is masked";
  let len =
    match b1 with
    | 126 ->
        let hi = Char.code (input_char ic) in
        (hi lsl 8) lor Char.code (input_char ic)
    | 127 ->
        let b = Bytes.create 8 in
        really_input ic b 0 8;
        Int64.to_int (Bytes.get_int64_be b 0)
    | n ->
        n
  in
  (b0 land 0x80 <> 0, b0 land 0x0F, really_input_string ic len)

let recv ic =
  let fin, opcode, payload = read_frame ic in
  if not fin then Alcotest.fail "fragmented frame from the server";
  match opcode with
  | 1 ->
      `Text payload
  | 8 ->
      `Close
        ( if String.length payload >= 2 then
            (Char.code payload.[0] lsl 8) lor Char.code payload.[1]
          else
            0
        )
  | n ->
      Alcotest.failf "unexpected opcode %d" n

let server_message ic =
  match recv ic with
  | `Text text -> (
      match Protocol.decode_server text with
      | Ok m ->
          m
      | Error e ->
          Alcotest.fail e
    )
  | `Close code ->
      Alcotest.failf "closed with %d" code

let hello_text v sub =
  Protocol.encode_client
    (Protocol.Hello { v = Protocol.version; csrf = v.csrf; sub })

let state_int = function
  | Protocol.State { seq; ack; state; _ } -> (
      match Protocol.decode_state Codec.int state with
      | Ok n ->
          (seq, ack, n)
      | Error e ->
          Alcotest.fail e
    )
  | _ ->
      Alcotest.fail "expected a state message"

let append =
  Live.declare ~channel:"append" ~state:Codec.string ~event:Codec.string
    {
      Component.machine =
        Silica_test_support.Simple.machine ~id:"append" ~init:"" (fun s e ->
            Some (s ^ e, [])
        );
      view = (fun s -> Node.text s);
    }

let counts_topic = Topic.declare ~name:"counts" ~state:Codec.int
let bump = Remote.declare ~path:"/api/bump" ~input:Codec.int ~output:Codec.int
let secret = "0123456789abcdef0123"

let metafile =
  {|{"outputs":{"dist/app-4F2A9C1B.js":{"entryPoint":"out/app.mjs"}}}|}

let with_server f =
  let dir = Filename.temp_dir "silica" "static" in
  let put name content =
    let oc = open_out (Filename.concat dir name) in
    output_string oc content;
    close_out oc
  in
  put "hello.txt" "hi static";
  put "app-4F2A9C1B.js" "console.log(1)";
  let server = ref None in
  let handlers =
    [
      Server.implement double (fun _ n -> Ok (n * 2));
      Server.implement boom (fun _ _ -> Error "kaput");
      Server.implement bump (fun _ n ->
          ignore (Server.publish (Option.get !server) counts_topic ~key:"k" n);
          Ok n
      );
    ]
  in
  let islands = [ Server.Island live; Server.Island append ] in
  let config =
    {
      (Silica_server.default_config ~origins:Silica_server.Localhost ~secret) with
      observer =
        { Silica_server.stderr_observer with log = (fun ~source:_ _ -> ()) };
    }
  in
  let assets =
    match Silica_server.Assets.of_metafile ~prefix:"/static/" metafile with
    | Ok a ->
        a
    | Error e ->
        Alcotest.fail e
  in
  let t =
    Server.create config ~assets ~read:(Adapter.read_dir dir) ~handlers ~islands
      ~topics:
        [
          Server.Topic_def
            { topic = counts_topic; init = (fun _ ~key:_ -> Ok 1) };
        ]
      ~page:(fun (page : Server.page) ->
        Server.render_page_settled app ~status
          ~scripts:[ page.asset "app.js" ]
          ~csrf:page.csrf ~nonce:page.nonce ~islands ~handlers
          ~context:page.context page.url
      )
      ()
  in
  server := Some t;
  let adapter = Adapter.start t in
  Fun.protect
    ~finally:(fun () -> Adapter.stop adapter)
    (fun () -> f (Adapter.port adapter))

let io_tests =
  let open Alcotest in
  let elapsed f =
    let start = Unix.gettimeofday () in
    let result = f () in
    (result, Unix.gettimeofday () -. start)
  in
  [
    test_case "await gives the value as soon as another thread resolves it"
      `Quick (fun () ->
        let p = Adapter.Io.promise () in
        ignore
          (Thread.create
             (fun () ->
               Thread.delay 0.05;
               Adapter.Io.resolve p 7
             )
             ()
          );
        let value, seconds =
          elapsed (fun () -> Adapter.Io.await p ~timeout:5.)
        in
        check (option int) "value" (Some 7) value;
        check bool "long before the timeout" true (seconds < 1.)
    );
    test_case "await ends with None at its own deadline, not at a later one"
      `Quick (fun () ->
        let late = Adapter.Io.promise () and early = Adapter.Io.promise () in
        let late_result = ref (Some 0, 0.) in
        let waiter =
          Thread.create
            (fun () ->
              late_result :=
                elapsed (fun () ->
                    (Adapter.Io.await late ~timeout:0.6 : int option)
                )
            )
            ()
        in
        Thread.delay 0.05;
        let value, seconds =
          elapsed (fun () -> (Adapter.Io.await early ~timeout:0.1 : int option))
        in
        check (option int) "no value" None value;
        check bool "after its timeout" true (seconds >= 0.09);
        check bool "not at the later deadline" true (seconds < 0.4);
        Thread.join waiter;
        let late_value, late_seconds = !late_result in
        check (option int) "late: no value" None late_value;
        check bool "late: at its own deadline" true
          (late_seconds >= 0.59 && late_seconds < 1.2)
    );
    test_case "many awaits share one timer thread" `Quick (fun () ->
        let threads () =
          let status =
            In_channel.with_open_text "/proc/self/status" In_channel.input_all
          in
          Scanf.sscanf
            (List.find
               (fun line -> String.starts_with ~prefix:"Threads:" line)
               (String.split_on_char '\n' status)
            )
            "Threads: %d" Fun.id
        in
        let before = threads () in
        for _ = 1 to 50 do
          let p = Adapter.Io.promise () in
          Adapter.Io.resolve p ();
          ignore (Adapter.Io.await p ~timeout:30.)
        done;
        check bool "at most one more thread" true (threads () <= before + 1)
    );
  ]

let http_tests port =
  let open Alcotest in
  [
    test_case "a request is served while 100 idle connections are open" `Quick
      (fun () ->
        let idle = List.init 100 (fun _ -> connect port) in
        Fun.protect
          ~finally:(fun () -> List.iter Unix.close idle)
          (fun () ->
            let r = http port "GET" Silica_server.health_path "" in
            check int "health" 200 r.status
          )
    );
    test_case "a page has the cookie, the token and the policy headers" `Quick
      (fun () ->
        let r = http port "GET" "/" "" in
        check int "status" 200 r.status;
        check bool "root" true
          (contains r.body
             {|<div id="silica-root"><p>count &lt;7&gt;</p></div>|}
          );
        check bool "cookie" true
          (contains (Option.get (header "set-cookie" r)) "HttpOnly");
        check bool "token meta" true
          (contains r.body {|<meta name="silica-csrf" content="|});
        check bool "hashed script" true
          (contains r.body {|src="/static/app-4F2A9C1B.js"|});
        check (option string) "cache" (Some "private, no-store")
          (header "cache-control" r);
        check bool "csp" true
          (contains (Option.get (header "content-security-policy" r)) "nonce-");
        let r = http port "GET" "/missing" "" in
        check int "404 page" 404 r.status
    );
    test_case "static files: hashed is immutable, others use ETag" `Quick
      (fun () ->
        let r = http port "GET" "/static/app-4F2A9C1B.js" "" in
        check int "status" 200 r.status;
        check (option string) "cache"
          (Some "public, max-age=31536000, immutable") (header "cache-control" r);
        let r = http port "GET" "/static/hello.txt" "" in
        check string "body" "hi static" r.body;
        check (option string) "cache" (Some "no-cache")
          (header "cache-control" r);
        let etag = Option.get (header "etag" r) in
        check int "304" 304
          (http
             ~headers:[ ("If-None-Match", etag) ]
             port "GET" "/static/hello.txt" ""
          )
            .status;
        check int "traversal" 404
          (http port "GET" "/static/../dune-project" "").status
    );
    test_case "a remote call needs the session, Origin and token" `Quick
      (fun () ->
        let v = visit port in
        let r = post v "/api/double" "21" in
        check int "ok" 200 r.status;
        check string "body" "42" r.body;
        check int "no origin" 403
          (post ~origin:false v "/api/double" "21").status;
        check int "form" 415
          (post ~content_type:"text/plain" v "/api/double" "21").status;
        let r = post v "/api/boom" "1" in
        check int "handler error" 500 r.status;
        check string "fixed body" "internal error" r.body;
        check int "no cookie, no token" 403
          (http port "POST" "/api/double" "21"
             ~headers:
               [ ("Origin", v.origin); ("Content-Type", "application/json") ]
          )
            .status
    );
    test_case "a large Content-Length is refused before the body" `Quick
      (fun () ->
        let v = visit port in
        let r =
          http port "POST" "/api/double" "1" ~content_length:5_000_000
            ~headers:
              [
                ("Cookie", v.cookie);
                ("Origin", v.origin);
                ("Content-Type", "application/json");
                ("X-Silica-Csrf", v.csrf);
              ]
        in
        check int "413" 413 r.status
    );
    test_case "health" `Quick (fun () ->
        check string "ok" "ok" (http port "GET" "/_silica/health" "").body
    );
  ]

let ws_tests port =
  let open Alcotest in
  [
    test_case "upgrade needs Origin; no session closes with 4005" `Quick
      (fun () ->
        let v = visit port in
        let _, _, head = ws_connect v "/_silica/live" in
        check bool "101" true (contains head " 101 ");
        check bool "accept key" true
          (contains head "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=");
        let _, _, head = ws_connect ~origin:false v "/_silica/live" in
        check bool "no origin refused" true (contains head " 403 ");
        let fd, ic, head = ws_connect ~cookie:false v "/_silica/live" in
        check bool "no cookie: upgrade accepted" true (contains head " 101 ");
        ( match recv ic with
        | `Close code ->
            check int "no cookie: closed with 4005" Protocol.close_session code
        | `Text _ ->
            Alcotest.fail "expected a close frame"
        );
        Unix.close fd;
        let _, _, head = ws_connect v "/_silica/live/counter" in
        check bool "old path" true (contains head " 404 ")
    );
    test_case "island session: hello, events, acks, ping" `Quick (fun () ->
        let v = visit port in
        let fd, ic, _ = ws_connect v "/_silica/live" in
        ws_text fd
          (hello_text v
             (Protocol.Island { channel = "counter"; instance = "a" })
          );
        check bool "hello ok" true
          (server_message ic = Protocol.Hello_ok { v = Protocol.version });
        check (triple int int int) "initial" (1, 0, 0)
          (state_int (server_message ic));
        ws_text fd (Protocol.encode_event ev_codec ~id:1 Inc);
        check (triple int int int) "inc" (2, 1, 1)
          (state_int (server_message ic));
        ws_text fd (Protocol.encode_event ev_codec ~id:1 Inc);
        ws_text fd (Protocol.encode_event ev_codec ~id:2 Double);
        check (triple int int int) "double sent" (3, 2, 1)
          (state_int (server_message ic));
        check (triple int int int) "double result" (4, 2, 2)
          (state_int (server_message ic));
        ws_text fd (Protocol.encode_client Protocol.Ping);
        check bool "pong" true (server_message ic = Protocol.Pong);
        ws_send fd (Kit.Ws.Ping "x");
        ( match read_frame ic with
        | true, 10, "x" ->
            ()
        | _ ->
            Alcotest.fail "expected a pong frame"
        );
        Unix.close fd
    );
    test_case "a reconnect with the same instance finds the state" `Quick
      (fun () ->
        let v = visit port in
        let sub = Protocol.Island { channel = "counter"; instance = "keep" } in
        let fd, ic, _ = ws_connect v "/_silica/live" in
        ws_text fd (hello_text v sub);
        ignore (server_message ic);
        ignore (server_message ic);
        ws_text fd (Protocol.encode_event ev_codec ~id:1 Inc);
        ws_text fd (Protocol.encode_event ev_codec ~id:2 Inc);
        ignore (server_message ic);
        ignore (server_message ic);
        Unix.close fd;
        let fd, ic, _ = ws_connect v "/_silica/live" in
        ws_text fd (hello_text v sub);
        ignore (server_message ic);
        check (triple int int int) "kept" (4, 2, 2)
          (state_int (server_message ic));
        Unix.close fd;
        let other = visit port in
        let fd, ic, _ = ws_connect other "/_silica/live" in
        ws_text fd (hello_text other sub);
        ignore (server_message ic);
        check (triple int int int) "another visitor starts fresh" (1, 0, 0)
          (state_int (server_message ic));
        Unix.close fd
    );
    test_case "a bad hello closes the socket with a code" `Quick (fun () ->
        let v = visit port in
        let fd, ic, _ = ws_connect v "/_silica/live" in
        ws_text fd
          (Protocol.encode_client
             (Protocol.Hello
                {
                  v = Protocol.version;
                  csrf = "wrong";
                  sub = Protocol.Island { channel = "counter"; instance = "x" };
                }
             )
          );
        ( match recv ic with
        | `Close code ->
            check int "csrf" Protocol.close_csrf code
        | `Text _ ->
            Alcotest.fail "expected a close frame"
        );
        Unix.close fd
    );
    test_case "messages above 64 KiB get an error; large states go out whole"
      `Quick (fun () ->
        let v = visit port in
        let fd, ic, _ = ws_connect v "/_silica/live" in
        ws_text fd
          (hello_text v
             (Protocol.Island { channel = "append"; instance = "big" })
          );
        ignore (server_message ic);
        ignore (server_message ic);
        ws_text fd
          (Protocol.encode_event Codec.string ~id:1 (String.make 100_000 'a'));
        ( match server_message ic with
        | Protocol.Error { code = "too_big" } ->
            ()
        | _ ->
            Alcotest.fail "expected a too_big error"
        );
        let chunk = String.make 60_000 'b' in
        let state () =
          match server_message ic with
          | Protocol.State { state; _ } -> (
              match Protocol.decode_state Codec.string state with
              | Ok s ->
                  String.length s
              | Error e ->
                  Alcotest.fail e
            )
          | _ ->
              Alcotest.fail "expected a state"
        in
        let sizes = ref [] in
        for i = 1 to 18 do
          ws_text fd (Protocol.encode_event Codec.string ~id:i chunk);
          sizes := state () :: !sizes
        done;
        check int "18 x 60000" 1_080_000 (List.hd !sizes);
        check bool "the 16 and 64 bit length forms both worked" true
          (List.exists (fun n -> n < 65_536) !sizes
          && List.exists (fun n -> n > 65_536) !sizes
          );
        Unix.close fd
    );
    test_case "a topic: subscribe, then a remote call publishes" `Quick
      (fun () ->
        let v = visit port in
        let fd, ic, _ = ws_connect v "/_silica/live" in
        ws_text fd (hello_text v (Protocol.Topic { name = "counts"; key = "k" }));
        ignore (server_message ic);
        let seq, _, first = state_int (server_message ic) in
        check int "init value" 1 first;
        let r = post v "/api/bump" "77" in
        check int "bump" 200 r.status;
        let seq2, _, value = state_int (server_message ic) in
        check int "pushed value" 77 value;
        check bool "seq grew" true (seq2 > seq);
        Unix.close fd
    );
    test_case "a close frame is answered" `Quick (fun () ->
        let v = visit port in
        let fd, ic, _ = ws_connect v "/_silica/live" in
        ws_send fd (Kit.Ws.Close { code = Some 1000; reason = "" });
        ( match recv ic with
        | `Close 1000 ->
            ()
        | _ ->
            Alcotest.fail "expected close 1000"
        );
        Unix.close fd
    );
  ]

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  with_server (fun port ->
      Alcotest.run "silica-adapter"
        [
          ("http", http_tests port);
          ("websocket", ws_tests port);
          ("io", io_tests);
        ]
  )
