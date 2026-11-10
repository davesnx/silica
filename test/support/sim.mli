(** A simulated host around {!Silica.Instance}, for tests. It keeps the timers
    and calls that the commands started, and a log of every command. *)

type ('s, 'e) t

val mount : ?dev:bool -> ?props:'e -> ('s, 'e) Silica.Component.t -> ('s, 'e) t

val html : ('s, 'e) t -> string
(** The HTML that a server render of the view prints: portals are left out. *)

val page : ('s, 'e) t -> string
(** The HTML of the whole page: like {!html}, with the children of each portal
    printed where the portal is, inside a [<div data-silica-portal>], as the
    browser host paints them into the page. *)

val node : ('s, 'e) t -> Silica.Instance.message Silica.Node.t
(** The resolved view. *)

val submit : ('s, 'e) t -> unit
(** Submit the first form that has a submit handler. *)

val snapshot : ('s, 'e) t -> 's
val count : ('s, 'e) t -> int

val send : ('s, 'e) t -> 'e -> unit
(** An event for the root component. *)

val press : ('s, 'e) t -> string -> unit
(** Press the first pressable element whose text contains the text.

    @raise Failure when there is none. *)

val input : ('s, 'e) t -> label:string -> string -> unit
(** Type into the first element with this [aria-label]. *)

val running : ('s, 'e) t -> string list
(** The ids of the running timers and calls, sorted. *)

val sources : ('s, 'e) t -> (string * string) list
(** The running held sources, as (id, {!Silica.Cmd.source_name}), in start
    order. *)

val press_key : ('s, 'e) t -> Silica.Key.t -> bool
(** A key press in the page: the first running [Global_keys] source that binds
    the key gets it. [false] when none does. *)

val outside_press : ('s, 'e) t -> unit
(** A press outside the elements of every running [Outside_press] source and
    [Dismiss] layer: each one with an outside event gets it, newest first.

    @raise Failure when none is running. *)

val escape : ('s, 'e) t -> bool
(** An Escape press in the page, given to the newest running [Dismiss] layer
    with an Escape event. [false] when there is none. *)

val key_down : ('s, 'e) t -> id:string -> Silica.Key.t -> bool
(** A key press on the element with this id: the key handlers of the element and
    then of its ancestors are tried in that order, as in the browser renderer.
    [false] when none handles it.

    @raise Failure when no element has the id. *)

val press_id : ('s, 'e) t -> id:string -> unit
(** A press on the element with this id: the press handler of the element or of
    its nearest ancestor that has one.

    @raise Failure when there is none. *)

val focus : ('s, 'e) t -> id:string -> unit
val blur : ('s, 'e) t -> id:string -> unit
val pointer_enter : ('s, 'e) t -> id:string -> unit
val pointer_leave : ('s, 'e) t -> id:string -> unit

val attribute : ('s, 'e) t -> id:string -> string -> string option
(** The value of an attribute of the element with this id; [Some ""] for a true
    boolean attribute. [None] when there is no such element or attribute. *)

val exists : ('s, 'e) t -> id:string -> bool
(** An element with this id is in the view. *)

val fire : ('s, 'e) t -> string -> unit
(** The timer with this id fires.

    @raise Failure when it is not running. *)

val fire_after_stop : ('s, 'e) t -> string -> unit
(** Deliver the message of the timer or call with this id even when it was
    stopped: a host that ignores [Stop]. *)

val answer : ('s, 'e) t -> ('i, 'o) Silica.Remote.endpoint -> 'o -> unit
(** The running call to the endpoint ends with this value. *)

val answer_error :
  ('s, 'e) t -> ('i, 'o) Silica.Remote.endpoint -> Silica.Remote.error -> unit

val subscriptions : ('s, 'e) t -> (string * string) list
(** The running subscriptions as (topic name, key), in start order. *)

val publish :
  ('s, 'e) t ->
  'a Silica.Topic.t ->
  key:string ->
  'a Silica.Topic.update ->
  unit
(** Deliver the update to every running subscription with this topic name and
    key. The value goes through the codecs, as it does on the wire. *)

val call_timeouts : ('s, 'e) t -> int option list
(** The [timeout_ms] of the running calls, in start order. *)

val expire_call : ('s, 'e) t -> ('i, 'o) Silica.Remote.endpoint -> unit
(** The running call to the endpoint times out, as a host does when [timeout_ms]
    passes: the machine gets [Error (Network "timeout")].
    @raise Failure when the call has no timeout. *)

val call_inputs : ('s, 'e) t -> string list
(** Encoded inputs of the running calls, in start order. *)

val log : ('s, 'e) t -> string list
(** ["start <id>"], ["stop <id>"], ["focus <ref>"], ["navigate <url>"],
    ["title <text>"] and so on, in the order the host got them. *)

val clear_log : ('s, 'e) t -> unit
val unmount : ('s, 'e) t -> unit
