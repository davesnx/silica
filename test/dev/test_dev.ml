open Silica_dev
module Ws = Silica_server_kit.Ws

let contains text part =
  let length = String.length part in
  let rec search index =
    index + length <= String.length text
    && (String.sub text index length = part || search (index + 1))
  in
  search 0

let listen handler =
  let listener = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen listener 16;
  let port =
    match Unix.getsockname listener with
    | Unix.ADDR_INET (_, port) ->
        port
    | Unix.ADDR_UNIX _ ->
        assert false
  in
  ignore
    (Thread.create
       (fun () ->
         while true do
           let socket, _ = Unix.accept ~cloexec:true listener in
           ignore
             (Thread.create
                (fun () ->
                  (try handler socket with Unix.Unix_error _ -> ());
                  try Unix.close socket with Unix.Unix_error _ -> ()
                )
                ()
             )
         done
       )
       ()
    );
  port

let connect port =
  let socket = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt_float socket Unix.SO_RCVTIMEO 5.;
  Unix.connect socket (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
  socket

let read_all socket =
  let buffer = Buffer.create 1024 and chunk = Bytes.create 4096 in
  let rec loop () =
    match Unix.read socket chunk 0 4096 with
    | 0 ->
        Buffer.contents buffer
    | count ->
        Buffer.add_subbytes buffer chunk 0 count;
        loop ()
    | exception Unix.Unix_error (Unix.EAGAIN, _, _) ->
        Alcotest.fail ("no end of answer after 5 s: " ^ Buffer.contents buffer)
  in
  loop ()

let exchange port request =
  let socket = connect port in
  Http_head.write socket request;
  let answer = read_all socket in
  Unix.close socket;
  answer

let read_head socket =
  match Http_head.read socket with
  | Http_head.Head (head, raw, rest) ->
      (head, raw, rest)
  | _ ->
      Alcotest.fail "no request head"

let body answer =
  match String.split_on_char '\n' answer |> List.rev with
  | last :: _ ->
      last
  | [] ->
      ""

let start_proxy ?(client_script = "script") ?(on_dev_socket = fun _ _ -> ())
    app_port =
  match Proxy.start ~port:0 ~app_port ~on_dev_socket ~client_script with
  | Ok proxy ->
      proxy
  | Error message ->
      Alcotest.fail message

let echo_head_app () =
  listen (fun socket ->
      let _, raw, _ = read_head socket in
      Http_head.write socket
        (Printf.sprintf "HTTP/1.1 200 OK\r\nContent-Length: %d\r\n\r\n%s"
           (String.length raw) raw
        );
      if not (contains raw "Connection: close") then ignore (read_all socket)
  )

let fixed port ~timeout:_ = Some port

let proxy_tests =
  let open Alcotest in
  [
    test_case
      "a request reaches the app with Connection: close, then the connection \
       ends"
      `Quick (fun () ->
        let proxy = start_proxy (fixed (echo_head_app ())) in
        let answer =
          exchange (Proxy.port proxy)
            "GET /page HTTP/1.1\r\n\
             Host: 127.0.0.1\r\n\
             Connection: keep-alive\r\n\
             Keep-Alive: timeout=5\r\n\
             \r\n"
        in
        check bool "the app got Connection: close" true
          (contains answer "Connection: close");
        check bool "keep-alive is gone" false (contains answer "keep-alive");
        check bool "the path is kept" true (contains answer "GET /page HTTP/1.1");
        Proxy.stop proxy
    );
    test_case "a WebSocket upgrade passes unchanged and carries bytes both ways"
      `Quick (fun () ->
        let app =
          listen (fun socket ->
              let head, _, _ = read_head socket in
              if Http_head.is_upgrade head then (
                Http_head.write socket
                  "HTTP/1.1 101 Switching Protocols\r\n\
                   Upgrade: websocket\r\n\
                   Connection: Upgrade\r\n\
                   \r\n";
                let buffer = Bytes.create 64 in
                let count = Unix.read socket buffer 0 64 in
                Http_head.write socket (Bytes.sub_string buffer 0 count)
              ) else
                Http_head.write socket "HTTP/1.1 400 Bad Request\r\n\r\n"
          )
        in
        let proxy = start_proxy (fixed app) in
        let socket = connect (Proxy.port proxy) in
        Http_head.write socket
          "GET /_silica/live HTTP/1.1\r\n\
           Host: 127.0.0.1\r\n\
           Upgrade: websocket\r\n\
           Connection: Upgrade\r\n\
           Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\
           \r\n";
        let buffer = Bytes.create 256 in
        let count = Unix.read socket buffer 0 256 in
        check bool "101" true
          (contains (Bytes.sub_string buffer 0 count) "101 Switching Protocols");
        Http_head.write socket "frame-bytes";
        let count = Unix.read socket buffer 0 256 in
        check string "echo" "frame-bytes" (Bytes.sub_string buffer 0 count);
        Unix.close socket;
        Proxy.stop proxy
    );
    test_case "the proxy answers the dev client script itself" `Quick (fun () ->
        let proxy =
          start_proxy ~client_script:"console.log(1)" (fun ~timeout:_ ->
              Alcotest.fail "the app was asked"
          )
        in
        let answer =
          exchange (Proxy.port proxy)
            "GET /_silica/dev/client.js HTTP/1.1\r\nHost: x\r\n\r\n"
        in
        check bool "javascript" true (contains answer "text/javascript");
        check string "body" "console.log(1)" (body answer);
        Proxy.stop proxy
    );
    test_case "with no app, a page loads the dev client" `Quick (fun () ->
        let proxy = start_proxy (fun ~timeout:_ -> None) in
        let answer =
          exchange (Proxy.port proxy) "GET / HTTP/1.1\r\nHost: x\r\n\r\n"
        in
        check bool "503" true (contains answer "503 Service Unavailable");
        check bool "script" true (contains answer "/_silica/dev/client.js");
        Proxy.stop proxy
    );
  ]

let fake_app = Filename.concat (Sys.getcwd ()) "fake_app.exe"

let temporary_directory () =
  let path = Filename.temp_file "silica-dev" "" in
  Sys.remove path;
  Unix.mkdir path 0o755;
  path

let write path text =
  Out_channel.with_open_bin path (fun channel -> output_string channel text)

let copy_executable source target =
  write target (In_channel.with_open_bin source In_channel.input_all);
  Unix.chmod target 0o755

type recorder = { lock : Mutex.t; mutable events : Dev_event.t list }

let recorder () = { lock = Mutex.create (); events = [] }

let record recorder event =
  Mutex.protect recorder.lock (fun () ->
      recorder.events <- recorder.events @ [ event ]
  )

let events recorder = Mutex.protect recorder.lock (fun () -> recorder.events)

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

let started recorder =
  List.length
    (List.filter
       (function Dev_event.Server_started _ -> true | _ -> false)
       (events recorder)
    )

let crashes recorder =
  List.filter_map
    (function
      | Dev_event.Server_crashed { code; output } ->
          Some (code, output)
      | _ ->
          None
      )
    (events recorder)

let alive pid =
  match Unix.kill pid 0 with
  | () ->
      true
  | exception Unix.Unix_error (Unix.ESRCH, _, _) ->
      false

let server_pids recorder =
  List.filter_map
    (function Dev_event.Server_started { pid; _ } -> Some pid | _ -> None)
    (events recorder)

let make_supervisor ~mode =
  let directory = temporary_directory () in
  let server = Filename.concat directory "app.exe" in
  let metafile = Filename.concat directory "meta.json" in
  copy_executable fake_app server;
  write metafile {|{"outputs":{"dist/main-AAAA.js":{}}}|};
  let recorder = recorder () in
  let supervisor =
    Supervisor.create ~server ~metafile
      ~environment:(fun ~port ->
        [| "PORT=" ^ string_of_int port; "FAKE_APP_MODE=" ^ !mode |]
      )
      ~emit:(record recorder) ~echo:ignore
  in
  (supervisor, metafile, recorder)

let supervisor_tests =
  let open Alcotest in
  [
    test_case "a request during a restart waits for the new server" `Quick
      (fun () ->
        let supervisor, metafile, recorder = make_supervisor ~mode:(ref "ok") in
        let proxy = start_proxy (Supervisor.app_port supervisor) in
        Supervisor.build_succeeded supervisor;
        wait_for "first start" (fun () -> started recorder = 1);
        let first =
          exchange (Proxy.port proxy) "GET / HTTP/1.1\r\nHost: x\r\n\r\n"
        in
        write metafile {|{"outputs":{"dist/main-BBBB.js":{}}}|};
        Supervisor.build_succeeded supervisor;
        wait_for "the restart begins" (fun () ->
            Supervisor.app_port supervisor ~timeout:0. = None
        );
        let second =
          exchange (Proxy.port proxy) "GET / HTTP/1.1\r\nHost: x\r\n\r\n"
        in
        let pids = server_pids recorder in
        check int "two starts" 2 (List.length pids);
        check string "first server"
          ("pid " ^ string_of_int (List.nth pids 0))
          (body first);
        check string "the new server answered"
          ("pid " ^ string_of_int (List.nth pids 1))
          (body second);
        check bool "the old server is gone" false (alive (List.nth pids 0));
        check bool "the page reloads after each start" true
          (List.length
             (List.filter
                (function Dev_event.Reload Dev_event.Page -> true | _ -> false)
                (events recorder)
             )
          = 2
          );
        Supervisor.stop supervisor;
        check bool "stop ends the server" false (alive (List.nth pids 1));
        Proxy.stop proxy
    );
    test_case "a build with the same files keeps the server" `Quick (fun () ->
        let supervisor, _, recorder = make_supervisor ~mode:(ref "ok") in
        Supervisor.build_succeeded supervisor;
        wait_for "first start" (fun () -> started recorder = 1);
        Supervisor.build_succeeded supervisor;
        Thread.delay 0.3;
        check int "one start" 1 (started recorder);
        Supervisor.stop supervisor
    );
    test_case
      "a server that stops at start is reported once, then retried on the next \
       build"
      `Quick (fun () ->
        let mode = ref "crash" in
        let supervisor, _, recorder = make_supervisor ~mode in
        Supervisor.build_succeeded supervisor;
        wait_for "crash" (fun () -> crashes recorder <> []);
        Thread.delay 0.5;
        ( match crashes recorder with
        | [ (code, output) ] ->
            check string "code" "exit 3" code;
            check string "output" "boom" output
        | list ->
            failf "%d crash reports" (List.length list)
        );
        check int "no start" 0 (started recorder);
        check bool "no app port" true
          (Supervisor.app_port supervisor ~timeout:0. = None);
        mode := "ok";
        Supervisor.build_succeeded supervisor;
        wait_for "start" (fun () -> started recorder = 1);
        Supervisor.stop supervisor
    );
    test_case "a server that stops later is reported" `Quick (fun () ->
        let supervisor, _, recorder = make_supervisor ~mode:(ref "ok") in
        Supervisor.build_succeeded supervisor;
        wait_for "start" (fun () -> started recorder = 1);
        Unix.kill (List.hd (server_pids recorder)) Sys.sigkill;
        wait_for "crash" (fun () -> crashes recorder <> []);
        check string "code" "signal SIGKILL" (fst (List.hd (crashes recorder)));
        Supervisor.stop supervisor
    );
  ]

let reload_tests =
  let open Alcotest in
  let reload before after =
    Supervisor.reload_of ~before:(Some before) ~after:(Some after)
  in
  [
    test_case "a new script reloads the page" `Quick (fun () ->
        check bool "page" true
          (reload
             [ "dist/main-AAAA.js"; "dist/app-CCCC.css" ]
             [ "dist/main-BBBB.js"; "dist/app-CCCC.css" ]
          = Some Dev_event.Page
          )
    );
    test_case "a new stylesheet alone is swapped" `Quick (fun () ->
        check bool "css" true
          (reload
             [ "dist/main-AAAA.js"; "dist/app-CCCC.css" ]
             [ "dist/main-AAAA.js"; "dist/app-DDDD.css" ]
          = Some
              (Dev_event.Stylesheets
                 [
                   {
                     from = "/static/app-CCCC.css";
                     target = "/static/app-DDDD.css";
                   };
                 ]
              )
          )
    );
    test_case "the same outputs need no reload" `Quick (fun () ->
        check bool "none" true
          (reload [ "dist/main-AAAA.js" ] [ "dist/main-AAAA.js" ] = None)
    );
  ]

let read_frame socket =
  let read count =
    let buffer = Bytes.create count in
    let rec loop offset =
      if offset < count then
        loop (offset + Unix.read socket buffer offset (count - offset))
    in
    loop 0;
    Bytes.to_string buffer
  in
  let header = read 2 in
  let length =
    match Char.code header.[1] land 0x7f with
    | 126 ->
        let extended = read 2 in
        (Char.code extended.[0] lsl 8) lor Char.code extended.[1]
    | length ->
        length
  in
  read length

let open_dev_socket ?(origin = "") port =
  let socket = connect port in
  Http_head.write socket
    (Printf.sprintf
       "GET /_silica/dev HTTP/1.1\r\n\
        Host: 127.0.0.1:%d\r\n\
        Upgrade: websocket\r\n\
        Connection: Upgrade\r\n\
        Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\
        %s\r\n"
       port
       ( if origin = "" then
           ""
         else
           "Origin: " ^ origin ^ "\r\n"
       )
    );
  let rec head buffer =
    let byte = Bytes.create 1 in
    ignore (Unix.read socket byte 0 1);
    Buffer.add_bytes buffer byte;
    if String.ends_with ~suffix:"\r\n\r\n" (Buffer.contents buffer) then
      Buffer.contents buffer
    else
      head buffer
  in
  (socket, head (Buffer.create 256))

let hub_tests =
  let open Alcotest in
  [
    test_case "a new dev socket gets the current status, then each event" `Quick
      (fun () ->
        let hub = Hub.create () in
        let proxy =
          start_proxy ~on_dev_socket:(Hub.attach hub) (fun ~timeout:_ -> None)
        in
        let error =
          {
            Dev_event.location = None;
            severity = Dev_event.Error;
            message = "esbuild failed";
          }
        in
        Hub.publish hub Dev_event.Build_started;
        Hub.publish hub (Dev_event.Build_failed [ error ]);
        let socket, head = open_dev_socket (Proxy.port proxy) in
        check bool "101" true (contains head "101 Switching Protocols");
        check string "current status"
          {|{"t":"build","status":"failed","errors":[{"severity":"error","message":"esbuild failed"}]}|}
          (read_frame socket);
        wait_for "registered" (fun () -> Hub.client_count hub = 1);
        Hub.publish hub Dev_event.Build_ok;
        check string "next event" {|{"t":"build","status":"ok"}|}
          (read_frame socket);
        Unix.close socket;
        Proxy.stop proxy
    );
    test_case "a crash stays in the status until a server starts" `Quick
      (fun () ->
        let hub = Hub.create () in
        let proxy =
          start_proxy ~on_dev_socket:(Hub.attach hub) (fun ~timeout:_ -> None)
        in
        Hub.publish hub Dev_event.Build_ok;
        Hub.publish hub
          (Dev_event.Server_crashed { code = "exit 2"; output = "bad" });
        let socket, _ = open_dev_socket (Proxy.port proxy) in
        ignore (read_frame socket);
        check string "crash"
          {|{"t":"server","status":"crashed","code":"exit 2","output":"bad"}|}
          (read_frame socket);
        Unix.close socket;
        Hub.publish hub
          (Dev_event.Server_started
             { pid = 1; milliseconds = 1; restart = true }
          );
        let socket, _ = open_dev_socket (Proxy.port proxy) in
        ignore (read_frame socket);
        Unix.setsockopt_float socket Unix.SO_RCVTIMEO 0.2;
        check bool "no crash" true
          ( match read_frame socket with
          | _ ->
              false
          | exception Unix.Unix_error (Unix.EAGAIN, _, _) ->
              true
          );
        Unix.close socket;
        Proxy.stop proxy
    );
    test_case "a dev socket from another origin is refused" `Quick (fun () ->
        let hub = Hub.create () in
        let proxy =
          start_proxy ~on_dev_socket:(Hub.attach hub) (fun ~timeout:_ -> None)
        in
        let socket, head =
          open_dev_socket ~origin:"https://evil.example" (Proxy.port proxy)
        in
        check bool "403" true (contains head "403 Forbidden");
        Unix.close socket;
        check int "no client" 0 (Hub.client_count hub);
        Proxy.stop proxy
    );
  ]

let source_tests =
  let open Alcotest in
  [
    test_case "the excerpt has two lines on each side" `Quick (fun () ->
        let directory = temporary_directory () in
        let path = Filename.concat directory "file.ml" in
        write path "one\ntwo\nthree\nfour\nfive\nsix\n";
        check
          (list (pair int string))
          "middle"
          [ (2, "two"); (3, "three"); (4, "four"); (5, "five"); (6, "six") ]
          (Source_location.excerpt ~path ~line:4);
        check
          (list (pair int string))
          "start"
          [ (1, "one"); (2, "two"); (3, "three") ]
          (Source_location.excerpt ~path ~line:1)
    );
    test_case "a file that dune copied maps to its source" `Quick (fun () ->
        let root = temporary_directory () in
        List.iter
          (fun path -> Unix.mkdir (Filename.concat root path) 0o755)
          [
            "app";
            "app/shared";
            "app/melange";
            "_build";
            "_build/default";
            "_build/default/app";
            "_build/default/app/melange";
          ];
        write (Filename.concat root "app/shared/view.mlx") "let x = 1\n";
        write
          (Filename.concat root "_build/default/app/melange/view.mlx")
          "let x = 1\n";
        check string "source" "app/shared/view.mlx"
          (Source_location.source ~root
             (Filename.concat root "app/melange/view.mlx")
          );
        check string "a source file stays" "app/shared/view.mlx"
          (Source_location.source ~root
             (Filename.concat root "app/shared/view.mlx")
          )
    );
  ]

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Alcotest.run "dev"
    [
      ("proxy", proxy_tests);
      ("supervisor", supervisor_tests);
      ("reload", reload_tests);
      ("hub", hub_tests);
      ("source", source_tests);
    ]
