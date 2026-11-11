module Ws = Silica_server_kit.Ws

module Timers : sig
  val at : float -> (unit -> unit) -> unit
end = struct
  let lock = Mutex.create ()
  let due : (float * (unit -> unit)) list ref = ref []
  let wake = ref None

  let rec insert deadline f = function
    | ((d, _) as entry) :: rest when d <= deadline ->
        entry :: insert deadline f rest
    | rest ->
        (deadline, f) :: rest

  let take_due now =
    Mutex.protect lock (fun () ->
        let rec split acc = function
          | (d, f) :: rest when d <= now ->
              split (f :: acc) rest
          | rest ->
              (List.rev acc, rest)
        in
        let fire, rest = split [] !due in
        due := rest;
        (fire, match rest with (d, _) :: _ -> Some d | [] -> None)
    )

  let run reader =
    let scratch = Bytes.create 64 in
    while true do
      let now = Unix.gettimeofday () in
      let fire, next = take_due now in
      List.iter (fun f -> try f () with _ -> ()) fire;
      let timeout =
        match next with Some d -> Float.max 0. (d -. now) | None -> -1.
      in
      match Unix.select [ reader ] [] [] timeout with
      | [], _, _ ->
          ()
      | _ ->
          ignore (Unix.read reader scratch 0 (Bytes.length scratch))
      | exception Unix.Unix_error (Unix.EINTR, _, _) ->
          ()
    done

  let at deadline f =
    let writer =
      Mutex.protect lock (fun () ->
          due := insert deadline f !due;
          match !wake with
          | Some writer ->
              writer
          | None ->
              let reader, writer = Unix.pipe ~cloexec:true () in
              Unix.set_nonblock writer;
              wake := Some writer;
              ignore (Thread.create run reader);
              writer
      )
    in
    try ignore (Unix.write_substring writer "x" 0 1)
    with Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
end

module Io = struct
  type 'a t = 'a

  let return x = x
  let bind x f = f x
  let sleep = Thread.delay
  let async f = ignore (Thread.create f ())
  let now = Unix.gettimeofday

  type 'a promise = {
    lock : Mutex.t;
    changed : Condition.t;
    mutable value : 'a option;
  }

  let promise () =
    { lock = Mutex.create (); changed = Condition.create (); value = None }

  let resolve p v =
    Mutex.protect p.lock (fun () ->
        if p.value = None then (
          p.value <- Some v;
          Condition.broadcast p.changed
        )
    )

  let await p ~timeout =
    let deadline = Unix.gettimeofday () +. timeout in
    Timers.at deadline (fun () ->
        Mutex.protect p.lock (fun () -> Condition.broadcast p.changed)
    );
    Mutex.protect p.lock (fun () ->
        while p.value = None && Unix.gettimeofday () < deadline do
          Condition.wait p.changed p.lock
        done;
        p.value
    )
end

module Server = Silica_server.Make (Io)

type drain = {
  requests : int Atomic.t;
  lock : Mutex.t;
  sockets : (int, Server.socket) Hashtbl.t;
  next : int Atomic.t;
}

type t = { server : Tiny_httpd.t; running : bool Atomic.t; drain : drain }

let read_dir dir name =
  let path = Filename.concat dir name in
  match In_channel.with_open_bin path In_channel.input_all with
  | content ->
      Some content
  | exception Sys_error _ ->
      None

let to_response (r : Silica_server.response) =
  Tiny_httpd.Response.make_raw ~headers:r.headers ~code:r.status r.body

let request_of (req : string Tiny_httpd.Request.t) =
  {
    Silica_server.meth = Tiny_httpd.Meth.to_string (Tiny_httpd.Request.meth req);
    path = Tiny_httpd.Request.path req;
    query = Tiny_httpd.Request.query req;
    headers = Tiny_httpd.Request.headers req;
    body = Tiny_httpd.Request.body req;
  }

let bodyless (req : unit Tiny_httpd.Request.t) =
  {
    Silica_server.meth = Tiny_httpd.Meth.to_string (Tiny_httpd.Request.meth req);
    path = Tiny_httpd.Request.path req;
    query = Tiny_httpd.Request.query req;
    headers = Tiny_httpd.Request.headers req;
    body = "";
  }

let too_big ~max_body (req : unit Tiny_httpd.Request.t) =
  match Tiny_httpd.Request.get_header req "content-length" with
  | Some length
    when Option.value (int_of_string_opt length) ~default:0 > max_body ->
      Error (413, "payload too large")
  | _ ->
      Ok ()

let serve_socket drain (attach : Server.socket -> Server.connection) ic oc =
  let lock = Mutex.create () in
  let dead = Atomic.make false in
  let write_parts parts =
    if not (Atomic.get dead) then
      try
        Mutex.protect lock (fun () ->
            List.iter (Tiny_httpd.IO.Output.output_string oc) parts;
            Tiny_httpd.IO.Output.flush oc
        )
      with _ -> Atomic.set dead true
  in
  let write message = write_parts [ Ws.encode message ] in
  let socket =
    {
      Server.send = (fun text -> write_parts [ Ws.text_header text; text ]);
      close =
        (fun code ->
          write (Ws.Close { code = Some code; reason = "" });
          Atomic.set dead true
        );
    }
  in
  let id = Atomic.fetch_and_add drain.next 1 in
  Mutex.protect drain.lock (fun () -> Hashtbl.replace drain.sockets id socket);
  let connection = attach socket in
  let read n =
    let buffer = Bytes.create n in
    match Tiny_httpd.IO.Input.really_input ic buffer 0 n with
    | () ->
        Some (Bytes.unsafe_to_string buffer)
    | exception End_of_file ->
        None
  in
  let rec loop () =
    if not (Atomic.get dead) then
      match Ws.read_message ~max:Silica.Protocol.hard_limit ~read with
      | Ok None ->
          ()
      | Ok (Some (Ws.Text text)) ->
          connection.receive text;
          loop ()
      | Ok (Some (Ws.Binary _)) ->
          socket.close 1003
      | Ok (Some (Ws.Ping payload)) ->
          write (Ws.Pong payload);
          loop ()
      | Ok (Some (Ws.Pong _)) ->
          loop ()
      | Ok (Some (Ws.Close _)) ->
          write (Ws.Close { code = Some 1000; reason = "" })
      | Error Ws.Too_big ->
          socket.close Silica.Protocol.close_too_big
      | Error (Ws.Protocol _) ->
          socket.close 1002
      | exception _ ->
          ()
  in
  Fun.protect
    ~finally:(fun () ->
      Atomic.set dead true;
      Mutex.protect drain.lock (fun () -> Hashtbl.remove drain.sockets id);
      connection.closed ()
    )
    loop

let ws_upgrade drain server : Tiny_httpd.upgrade_handler =
  (module struct
    type handshake_state = Server.socket -> Server.connection

    let name = "websocket"

    let handshake _addr req =
      match
        ( Tiny_httpd.Request.get_header req "sec-websocket-key",
          Server.upgrade server (bodyless req)
        )
      with
      | _, Server.Refused r ->
          Error (Printf.sprintf "refused with status %d" r.status)
      | None, _ ->
          Error "need sec-websocket-key"
      | Some key, Server.Accepted attach ->
          Ok ([ ("sec-websocket-accept", Ws.accept_key key) ], attach)

    let handle_connection attach ic oc = serve_socket drain attach ic oc
  end
)

let configure ~max_body drain server http =
  Tiny_httpd.add_route_handler ~accept:(too_big ~max_body) http
    Tiny_httpd.Route.rest_of_path (fun _rest req ->
      Atomic.incr drain.requests;
      Fun.protect
        ~finally:(fun () -> Atomic.decr drain.requests)
        (fun () -> to_response (Server.handle server (request_of req)))
  );
  Tiny_httpd.add_upgrade_handler http
    ~accept:(fun req ->
      if Tiny_httpd.Request.path req <> Silica_server.live_path then
        Ok ()
      else
        match Server.upgrade server (bodyless req) with
        | Server.Refused r ->
            Error (r.status, r.body)
        | Server.Accepted _ ->
            Ok ()
    )
    (Tiny_httpd.Route.exact_path "_silica/live" Tiny_httpd.Route.return)
    (ws_upgrade drain server)

let sweeper server running =
  ignore
    (Thread.create
       (fun () ->
         while Atomic.get running do
           Thread.delay 5.;
           if Atomic.get running then Server.sweep server
         done
       )
       ()
    )

let default_max_connections = 1024

let create ~port ~addr ~max_body ~max_connections server =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let http = Tiny_httpd.create ~addr ~port ~max_connections () in
  let drain =
    {
      requests = Atomic.make 0;
      lock = Mutex.create ();
      sockets = Hashtbl.create 16;
      next = Atomic.make 0;
    }
  in
  configure ~max_body drain server http;
  let running = Atomic.make true in
  sweeper server running;
  { server = http; running; drain }

let start ?(port = 0) ?(addr = "127.0.0.1") ?(max_body = 1_048_576)
    ?(max_connections = default_max_connections) server =
  let t = create ~port ~addr ~max_body ~max_connections server in
  let ready = Atomic.make false in
  ignore
    (Thread.create
       (fun () ->
         Tiny_httpd.run_exn
           ~after_init:(fun () -> Atomic.set ready true)
           t.server
       )
       ()
    );
  while not (Atomic.get ready) do
    Thread.delay 0.005
  done;
  t

let port t = Tiny_httpd.port t.server

let stop t =
  Atomic.set t.running false;
  Tiny_httpd.stop t.server

let run ?(port = 8080) ?(addr = "127.0.0.1") ?(max_body = 1_048_576)
    ?(max_connections = default_max_connections) ?(grace = 10.)
    ?(on_stop = fun () -> ()) server =
  ignore (Thread.sigmask Unix.SIG_BLOCK [ Sys.sigterm ]);
  let t = create ~port ~addr ~max_body ~max_connections server in
  ignore
    (Thread.create
       (fun () ->
         ignore (Thread.wait_signal [ Sys.sigterm ]);
         stop t;
         let sockets =
           Mutex.protect t.drain.lock (fun () ->
               List.of_seq (Hashtbl.to_seq_values t.drain.sockets)
           )
         in
         List.iter (fun (socket : Server.socket) -> socket.close 1001) sockets;
         let deadline = Unix.gettimeofday () +. grace in
         while
           Atomic.get t.drain.requests > 0 && Unix.gettimeofday () < deadline
         do
           Thread.delay 0.05
         done;
         match on_stop () with
         | () ->
             exit 0
         | exception exn ->
             prerr_endline ("on_stop: " ^ Printexc.to_string exn);
             exit 1
       )
       ()
    );
  Tiny_httpd.run_exn
    ~after_init:(fun () ->
      Printf.printf "listening on http://%s:%d\n%!" addr
        (Tiny_httpd.port t.server)
    )
    t.server
