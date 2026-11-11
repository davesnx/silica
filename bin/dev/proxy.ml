type t = { listener : Unix.file_descr; port : int; closed : bool Atomic.t }

let socket_path = "/_silica/dev"
let client_path = socket_path ^ "/client.js"
let app_wait = 10.

let waiting_page =
  "<!doctype html><html><head><meta charset=\"utf-8\"><title>silica \
   dev</title></head><body><p>No server runs yet. This page reloads when the \
   build is ready.</p><script type=\"module\" src=\"" ^ client_path
  ^ "\"></script></body></html>"

let close_quietly socket = try Unix.close socket with Unix.Unix_error _ -> ()

let shutdown_quietly socket command =
  try Unix.shutdown socket command with Unix.Unix_error _ -> ()

let copy ~source ~target =
  let buffer = Bytes.create 65536 in
  let rec loop () =
    match Unix.read source buffer 0 (Bytes.length buffer) with
    | 0 ->
        ()
    | count ->
        Http_head.write target (Bytes.sub_string buffer 0 count);
        loop ()
    | exception Unix.Unix_error (Unix.EINTR, _, _) ->
        loop ()
  in
  try loop () with Unix.Unix_error _ -> ()

let forward client ~port ~first =
  let app = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
  match Unix.connect app (Unix.ADDR_INET (Unix.inet_addr_loopback, port)) with
  | exception Unix.Unix_error _ ->
      close_quietly app;
      Http_head.respond client ~status:"502 Bad Gateway"
        ~content_type:"text/plain" "the app does not accept connections"
  | () ->
      let upstream =
        Thread.create
          (fun () ->
            (try Http_head.write app first with Unix.Unix_error _ -> ());
            copy ~source:client ~target:app;
            shutdown_quietly app Unix.SHUTDOWN_SEND
          )
          ()
      in
      copy ~source:app ~target:client;
      shutdown_quietly client Unix.SHUTDOWN_ALL;
      shutdown_quietly app Unix.SHUTDOWN_ALL;
      Thread.join upstream;
      close_quietly app

let serve ~app_port ~on_dev_socket ~client_script client =
  match Http_head.read client with
  | Http_head.Closed ->
      ()
  | Too_large ->
      Http_head.respond client ~status:"431 Request Header Fields Too Large"
        ~content_type:"text/plain" "request head too large"
  | Malformed ->
      Http_head.respond client ~status:"400 Bad Request"
        ~content_type:"text/plain" "bad request"
  | Head (head, raw, rest) -> (
      match Http_head.path head with
      | path when path = socket_path ->
          on_dev_socket client head
      | path when path = client_path ->
          Http_head.respond client ~status:"200 OK"
            ~content_type:"text/javascript; charset=utf-8" client_script
      | _ -> (
          let first =
            ( if Http_head.is_upgrade head then
                raw
              else
                Http_head.closing head
            )
            ^ rest
          in
          match app_port ~timeout:app_wait with
          | Some port ->
              forward client ~port ~first
          | None ->
              if head.meth = "GET" then
                Http_head.respond client ~status:"503 Service Unavailable"
                  ~content_type:"text/html; charset=utf-8" waiting_page
              else
                Http_head.respond client ~status:"503 Service Unavailable"
                  ~content_type:"text/plain" "no server runs; see silica dev"
        )
    )

let start ~port ~app_port ~on_dev_socket ~client_script =
  let listener = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt listener Unix.SO_REUSEADDR true;
  match Unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, port)) with
  | exception Unix.Unix_error (error, _, _) ->
      close_quietly listener;
      Error (Unix.error_message error)
  | () ->
      Unix.listen listener 128;
      let port =
        match Unix.getsockname listener with
        | Unix.ADDR_INET (_, port) ->
            port
        | Unix.ADDR_UNIX _ ->
            port
      in
      let proxy = { listener; port; closed = Atomic.make false } in
      let rec accept () =
        match Unix.accept ~cloexec:true listener with
        | client, _ ->
            ignore
              (Thread.create
                 (fun () ->
                   ( try serve ~app_port ~on_dev_socket ~client_script client
                     with Unix.Unix_error _ -> ()
                   );
                   close_quietly client
                 )
                 ()
              );
            accept ()
        | exception Unix.Unix_error (Unix.EINTR, _, _) ->
            accept ()
        | exception Unix.Unix_error _ when not (Atomic.get proxy.closed) ->
            Thread.delay 0.05;
            accept ()
        | exception Unix.Unix_error _ ->
            ()
      in
      ignore (Thread.create accept ());
      Ok proxy

let port proxy = proxy.port

let stop proxy =
  Atomic.set proxy.closed true;
  shutdown_quietly proxy.listener Unix.SHUTDOWN_ALL;
  close_quietly proxy.listener
