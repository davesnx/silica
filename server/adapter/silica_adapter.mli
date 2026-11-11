(** {!Silica_server} over tiny_httpd with threads, with its own WebSocket frame
    codec (see [Silica_server_kit.Ws]). [Io.t] is the identity: the server runs
    in the thread of the connection. Frames sent to the client are text frames,
    one for each message. *)

module Io : Silica_server.IO with type 'a t = 'a
module Server : module type of Silica_server.Make (Io)

val read_dir : string -> string -> string option
(** [read_dir dir name] reads the file [name] under [dir], or [None] when it
    does not exist. The server has already refused names with [..]. *)

type t

val start :
  ?port:int ->
  ?addr:string ->
  ?max_body:int ->
  ?max_connections:int ->
  Server.t ->
  t
(** Listens on [addr] (default 127.0.0.1) in a background thread; returns once
    it accepts connections. [port] 0 (the default) picks a free port. A request
    with a [content-length] above [max_body] (default 1 MiB) is refused before
    its body is read. At most [max_connections] (default 1024) connections are
    open at once, each with its thread: an open Live socket and an idle
    keep-alive connection count; a client above the limit waits. A WebSocket
    request goes to {!Server.val-upgrade}; every other request goes to
    {!Server.handle}. A thread calls {!Server.sweep} every 5 seconds. *)

val port : t -> int
val stop : t -> unit

val run :
  ?port:int ->
  ?addr:string ->
  ?max_body:int ->
  ?max_connections:int ->
  ?grace:float ->
  ?on_stop:(unit -> unit) ->
  Server.t ->
  unit
(** Like [start], then blocks. On [SIGTERM] it stops accepting connections,
    closes each Live socket with code 1001 (going away), waits for the requests
    in progress, at most [grace] seconds (default 10), calls [on_stop] one time,
    and exits with code 0. An idle keep-alive connection does not hold it. A
    second [SIGTERM] has no effect. When [on_stop] raises, the exception goes to
    stderr and the exit code is 1. *)
