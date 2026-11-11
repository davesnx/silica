module Ws = Silica_server_kit.Ws

type client = { socket : Unix.file_descr; write_lock : Mutex.t }

type t = {
  lock : Mutex.t;
  mutable clients : client list;
  mutable build : Dev_event.t option;
  mutable crash : Dev_event.t option;
}

let create () =
  { lock = Mutex.create (); clients = []; build = None; crash = None }

let send client frame =
  Mutex.protect client.write_lock (fun () ->
      try
        Http_head.write client.socket frame;
        true
      with Unix.Unix_error _ -> false
  )

let remember hub event =
  match event with
  | Dev_event.Build_started | Build_progress _ | Build_failed _ | Build_ok ->
      hub.build <- Some event
  | Server_crashed _ ->
      hub.crash <- Some event
  | Server_started _ ->
      hub.crash <- None
  | Listening _ | Reload _ ->
      ()

let publish hub event =
  let frame = Ws.encode (Ws.Text (Dev_event.to_string event)) in
  let clients =
    Mutex.protect hub.lock (fun () ->
        remember hub event;
        hub.clients
    )
  in
  let gone = List.filter (fun client -> not (send client frame)) clients in
  if gone <> [] then
    Mutex.protect hub.lock (fun () ->
        hub.clients <-
          List.filter (fun client -> not (List.memq client gone)) hub.clients
    )

let same_origin (head : Http_head.t) =
  match (Http_head.header head "origin", Http_head.header head "host") with
  | None, _ ->
      true
  | Some origin, Some host ->
      origin = "http://" ^ host || origin = "https://" ^ host
  | Some _, None ->
      false

let read_exactly socket count =
  let buffer = Bytes.create count in
  let rec loop offset =
    if offset = count then
      Some (Bytes.to_string buffer)
    else
      match Unix.read socket buffer offset (count - offset) with
      | 0 ->
          None
      | read ->
          loop (offset + read)
      | exception Unix.Unix_error _ ->
          None
  in
  loop 0

let rec serve client =
  match Ws.read_message ~max:65536 ~read:(read_exactly client.socket) with
  | Ok (Some (Ws.Ping payload)) ->
      ignore (send client (Ws.encode (Ws.Pong payload)));
      serve client
  | Ok (Some (Ws.Close _)) ->
      ignore
        (send client (Ws.encode (Ws.Close { code = Some 1000; reason = "" })))
  | Ok (Some (Ws.Text _ | Ws.Binary _ | Ws.Pong _)) ->
      serve client
  | Ok None | Error _ ->
      ()

let attach hub socket (head : Http_head.t) =
  match Http_head.header head "sec-websocket-key" with
  | _ when not (same_origin head) ->
      Http_head.respond socket ~status:"403 Forbidden"
        ~content_type:"text/plain" "the Origin is not this host"
  | None ->
      Http_head.respond socket ~status:"400 Bad Request"
        ~content_type:"text/plain" "a WebSocket upgrade is expected"
  | Some key ->
      Http_head.write socket
        (Printf.sprintf
           "HTTP/1.1 101 Switching Protocols\r\n\
            Upgrade: websocket\r\n\
            Connection: Upgrade\r\n\
            Sec-WebSocket-Accept: %s\r\n\
            \r\n"
           (Ws.accept_key key)
        );
      let client = { socket; write_lock = Mutex.create () } in
      Mutex.protect client.write_lock (fun () ->
          let current =
            Mutex.protect hub.lock (fun () ->
                hub.clients <- client :: hub.clients;
                List.filter_map Fun.id [ hub.build; hub.crash ]
            )
          in
          try
            List.iter
              (fun event ->
                Http_head.write socket
                  (Ws.encode (Ws.Text (Dev_event.to_string event)))
              )
              current
          with Unix.Unix_error _ -> ()
      );
      serve client;
      Mutex.protect hub.lock (fun () ->
          hub.clients <- List.filter (fun other -> other != client) hub.clients
      )

let client_count hub = Mutex.protect hub.lock (fun () -> List.length hub.clients)
