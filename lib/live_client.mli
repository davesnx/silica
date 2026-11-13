(** One live subscription over a WebSocket, as the Live protocol describes it
    ({!Protocol}). The client sends the hello, keeps the heartbeat, drops old
    states, and connects again after a loss. It reaches the platform only
    through {!Platform.type-io}.

    Experimental: Live updates can change. The checker and the runtime do not
    agree yet on the order of the first update of a subscription. *)

type config = { ping_ms : int; pong_ms : int }
(** [ping_ms] is the wait before each ping. [pong_ms] is the wait for the pong
    before the client closes the socket and connects again. *)

val default_config : config
(** The values of {!Protocol.ping_interval_ms} and {!Protocol.pong_timeout_ms}.
*)

type fatal =
  | Close_code of int
      (** The server closed the socket with {!Protocol.close_version},
          {!Protocol.close_session}, {!Protocol.close_csrf} or
          {!Protocol.close_unknown}. *)
  | Server_error of string  (** The server sent an [Error] message. *)

type t

val start :
  ?config:config ->
  io:Platform.io ->
  sub:Protocol.subscription ->
  on_state:
    (seq:int -> state:Silica_json.Json.t -> actions:Protocol.host list -> unit) ->
  on_down:(unit -> unit) ->
  on_fatal:(fatal -> unit) ->
  unit ->
  t
(** Connect with [io.connect] and keep the connection. [on_state] runs for each
    state with a [seq] higher than the last one of the same socket. After a
    loss, the client first schedules the next connect with [io.schedule] (the
    {!Backoff} delay), so that connect is the first task that the loss
    schedules; then [on_down] runs, once for a socket that had no loss before.
    The next state runs [on_state]. [on_fatal] runs once, and the client does
    not connect again. After a close with {!Protocol.close_session},
    [io.session_gone] runs first. After {!stop}, no callback runs. *)

val send_event : t -> 'e Codec.t -> 'e -> unit
(** Send the event with the next event id. Before the first state of the socket
    (no [Hello_ok] yet, no state yet, or lost and not back), the client holds
    the event and sends it, in order, right after the next first state: at most
    16 events, the oldest dropped first, and an event held for more than 10
    seconds is dropped. The ids continue across reconnects, and never go below
    the [ack] of a state plus one: a new client for an island that the server
    already knows, such as after a hot reload, does not reuse ids the server has
    seen. After {!stop} an event is dropped. *)

val stop : t -> unit
(** Close the socket with code 1000, cancel all timers, and never connect again.
*)

val subscribe :
  ?config:config ->
  io:Platform.io ->
  'a Topic.t ->
  string ->
  deliver:('a Topic.update -> unit) ->
  unit ->
  unit
(** Subscribe to the topic under the key. [deliver] gets the updates that
    {!Action.Start_live} describes. The result stops the subscription. *)
