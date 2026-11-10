type config = { ping_ms : int; pong_ms : int }

let default_config =
  { ping_ms = Protocol.ping_interval_ms; pong_ms = Protocol.pong_timeout_ms }

type fatal = Close_code of int | Server_error of string

type t = {
  config : config;
  sub : Protocol.subscription;
  on_state :
    seq:int -> state:Silica_json.Json.t -> cmds:Protocol.host list -> unit;
  on_down : unit -> unit;
  on_fatal : fatal -> unit;
  io : Platform.io;
  mutable stopped : bool;
  mutable socket : Platform.socket option;
  mutable ready : bool;
  mutable down : bool;
  mutable attempt : int;
  mutable next_id : int;
  mutable cancel_timer : unit -> unit;
  mutable detach : unit -> unit;
}

let fatal_code code =
  code = Protocol.close_version
  || code = Protocol.close_csrf
  || code = Protocol.close_unknown
  || code = Protocol.close_session

let finish_socket t =
  t.detach ();
  t.detach <- ignore;
  t.cancel_timer ();
  t.cancel_timer <- ignore;
  t.ready <- false;
  Option.iter (fun (ws : Platform.socket) -> ws.close 1000) t.socket;
  t.socket <- None

let stop t =
  t.stopped <- true;
  finish_socket t

let fail t fatal =
  if not t.stopped then (
    stop t;
    ( match fatal with
    | Close_code code when code = Protocol.close_session ->
        t.io.session_gone ()
    | _ ->
        ()
    );
    t.on_fatal fatal
  )

let rec connect t =
  let current = ref true in
  let last_seq = ref 0 in
  let socket = ref None in
  let send message =
    Option.iter (fun (ws : Platform.socket) -> ws.send message) !socket
  in
  t.detach <- (fun () -> current := false);
  let wait_ping () =
    t.cancel_timer <-
      t.io.schedule t.config.ping_ms (fun () ->
          if !current then (
            send (Protocol.encode_client Protocol.Ping);
            t.cancel_timer <-
              t.io.schedule t.config.pong_ms (fun () -> if !current then lost t)
          )
      )
  in
  let on_message data =
    if !current then
      match Protocol.decode_server data with
      | Error message ->
          t.io.log ("silica: bad live message: " ^ message)
      | Ok (Protocol.Hello_ok _) ->
          t.ready <- true
      | Ok Protocol.Pong ->
          t.cancel_timer ();
          wait_ping ()
      | Ok (Protocol.Error { code }) ->
          fail t (Server_error code)
      | Ok (Protocol.State { seq; state; cmds; _ }) ->
          if t.ready && seq > !last_seq then (
            last_seq := seq;
            t.attempt <- 0;
            t.down <- false;
            t.on_state ~seq ~state ~cmds
          )
  in
  let ws =
    t.io.connect
      {
        Platform.on_open =
          (fun () ->
            if !current then (
              send
                (Protocol.encode_client
                   (Protocol.Hello
                      { v = Protocol.version; csrf = t.io.csrf (); sub = t.sub }
                   )
                );
              wait_ping ()
            )
          );
        on_message;
        on_close =
          (fun code ->
            if !current then
              if fatal_code code then
                fail t (Close_code code)
              else
                lost t
          );
        on_error = (fun () -> if !current then lost t);
      }
  in
  socket := Some ws;
  t.socket <- Some ws

and lost t =
  finish_socket t;
  if not t.stopped then (
    if not t.down then (
      t.down <- true;
      t.on_down ()
    );
    if not t.stopped then (
      let delay =
        Backoff.delay_ms ~attempt:t.attempt ~jitter:(Random.float 1.)
      in
      t.attempt <- t.attempt + 1;
      t.cancel_timer <- t.io.schedule delay (fun () -> connect t)
    )
  )

let start ?(config = default_config) ~io ~sub ~on_state ~on_down ~on_fatal () =
  let t =
    {
      config;
      sub;
      on_state;
      on_down;
      on_fatal;
      io;
      stopped = false;
      socket = None;
      ready = false;
      down = false;
      attempt = 0;
      next_id = 1;
      cancel_timer = ignore;
      detach = ignore;
    }
  in
  connect t;
  t

let send_event t codec event =
  match t.socket with
  | Some ws when t.ready && not t.stopped ->
      ws.Platform.send (Protocol.encode_event codec ~id:t.next_id event);
      t.next_id <- t.next_id + 1
  | _ ->
      ()

let subscribe ?config ~io topic key ~deliver =
  let last = ref None in
  deliver { Topic.seq = 0; status = Status.Loading None };
  let failed error =
    deliver { Topic.seq = 0; status = Status.Failed { error; last = !last } }
  in
  let client =
    start ?config ~io
      ~sub:(Protocol.Topic { name = Topic.name topic; key })
      ~on_state:(fun ~seq ~state ~cmds:_ ->
        match Protocol.decode_state (Topic.state topic) state with
        | Ok data ->
            last := Some data;
            deliver { Topic.seq; status = Status.Ready { data; stale = false } }
        | Error message ->
            failed (Remote.Decode message)
      )
      ~on_down:(fun () ->
        Option.iter
          (fun data ->
            deliver
              { Topic.seq = 0; status = Status.Ready { data; stale = true } }
          )
          !last
      )
      ~on_fatal:(fun fatal ->
        failed
          ( match fatal with
          | Close_code code when code = Protocol.close_session ->
              Remote.Http 401
          | Close_code code when code = Protocol.close_csrf ->
              Remote.Http 403
          | Close_code code when code = Protocol.close_unknown ->
              Remote.Http 404
          | Close_code _ ->
              Remote.Http 426
          | Server_error code ->
              Remote.Network code
          )
      )
      ()
  in
  fun () -> stop client
