(** Live protocol, version 2. Wire messages between a live client and the
    server, shared by both sides. One socket serves one subscription: an island
    or a topic key. Every message is one JSON object with a ["t"] tag.

    The client sends first [{"t":"hello","v":2,"csrf":...,"sub":...}] where
    [sub] is [{"island":channel,"id":instance}] or [{"topic":name,"key":key}].
    The server answers [{"t":"hello","v":2}], or closes the socket with a close
    code. Then: [{"t":"state","seq":n,"ack":n,"state":...,"actions":[...]}] from
    the server, [{"t":"event","id":n,"event":...}] from the client,
    [{"t":"error","code":...}] from the server, and [{"t":"ping"}] from the
    client, answered by [{"t":"pong"}].

    Experimental: Live updates can change. The checker and the runtime do not
    agree yet on the order of the first update of a subscription. *)

val version : int
(** The protocol version, [2]. A hello with another version closes the socket.
*)

type host =
  | Focus of Ref.t
  | Blur of Ref.t
  | Scroll_into_view of Ref.t
  | Scroll_to of { target : Ref.t option; left : int; top : int }
  | Navigate of Url.t
  | Replace of Url.t
  | Back
  | Forward
  | Reload

val host_of_host_action : 'e Action.host -> host option
(** [None] for [Start_timer], [Start_call], [Start_live], [Start_held], [Stop],
    [Set_title] and [Remount], which never cross the wire, and for the actions
    with a result event ([Measure], [Copy], [Load_script]): the event would be
    made by a function on the server, and the protocol carries no answer from
    the client to an action. [None] too for the navigation and page actions that
    only make sense for the page that owns them: [Resume] answers a [Block]
    resource, which a Live machine cannot hold; [Revert_navigation] belongs to
    the router of the page; [Preload] fills the call cache of the page runtime,
    which a Live machine does not use; [Set_status] is for a server render, and
    an island renders from its initial state. The checker ([Check.live]) reports
    a Live machine that runs one of these. *)

val host_to_action : host -> 'e Action.t
(** The action that a host message stands for. *)

val encode_host : host -> Silica_json.Json.t
(** The JSON form of a host action. *)

val decode_host : Silica_json.Json.t -> (host, string) result
(** Read a host action from JSON. *)

type subscription =
  | Island of { channel : string; instance : string }
      (** [instance] is chosen by the client and kept across reconnects, so the
          server finds the state again. *)
  | Topic of { name : string; key : string }

type client =
  | Hello of { v : int; csrf : string; sub : subscription }
  | Event of { id : int; event : Silica_json.Json.t }
      (** [id] grows by one for each event of one subscription. *)
  | Ping

type server =
  | Hello_ok of { v : int }
  | State of {
      seq : int;
      ack : int;
      state : Silica_json.Json.t;
      actions : host list;
    }
      (** The full state. [seq] grows with each state of the subscription; [ack]
          is the id of the last event the server handled (0 for none). *)
  | Error of { code : string }
  | Pong

val encode_client : client -> string
(** The JSON text of a client message. *)

val decode_client : string -> (client, string) result
(** Read a client message. [Error] holds the reason. *)

val encode_server : server -> string
(** The JSON text of a server message. *)

val decode_server : string -> (server, string) result
(** Read a server message. [Error] holds the reason. *)

val encode_event : 'e Codec.t -> id:int -> 'e -> string
(** [encode_client (Event ...)] with the event encoded by the codec. *)

val encode_state : 's Codec.t -> seq:int -> ack:int -> 's -> host list -> string
(** [encode_server (State ...)] with the state encoded by the codec. *)

val decode_state : 's Codec.t -> Silica_json.Json.t -> ('s, string) result
(** The state of a {!State} message. *)

val decode_event_payload :
  'e Codec.t -> Silica_json.Json.t -> ('e, string) result
(** The event of an {!Event} message, decoded with the codec. *)

type close_code = int

val close_version : close_code
(** 4001: the server does not speak the version of the hello. *)

val close_hello : close_code
(** 4002: the first message is not a valid hello. *)

val close_csrf : close_code
(** 4003: the CSRF token is missing or wrong. *)

val close_unknown : close_code
(** 4004: no island or topic with that name. *)

val close_session : close_code
(** 4005: the request has no live session, for example after the server
    restarted. The server accepts the upgrade and closes at once, because a
    browser cannot read the status of a refused upgrade. The client does not
    connect again: it reloads the page to get a new session and token. *)

val close_idle : close_code
(** 4008: no traffic for two heartbeats. *)

val close_too_big : close_code
(** 1009: a message above the limit. *)

val close_server_error : close_code
(** 1011: a step of the server raised. *)

val max_message : int
(** 65536. An incoming message above it is dropped and answered with
    [{"t":"error","code":"too_big"}]. *)

val hard_limit : int
(** 1048576. A message above it closes the socket with {!close_too_big}. *)

val ping_interval_ms : int
(** 30000. *)

val pong_timeout_ms : int
(** 10000. *)
