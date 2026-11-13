(** A simulated host around {!Silica.Instance}, for tests. It keeps the timers
    and calls that the actions started, and a log of every action. *)

type ('p, 's, 'e) t
(** A mounted component with props ['p], snapshots ['s] and events ['e]. *)

val mount : ?dev:bool -> (unit, 's, 'e) Silica.Component.t -> (unit, 's, 'e) t

val mount_with_props :
  ?dev:bool -> props:'p -> ('p, 's, 'e) Silica.Component.t -> ('p, 's, 'e) t
(** Mount a component that has props. *)

val set_props : ('p, 's, 'e) t -> 'p -> unit
(** Give the component new props, as a parent does, and run the actions that
    follow. *)

val html : ('p, 's, 'e) t -> string
(** The HTML that a server render of the view prints: portals are left out. *)

val page : ('p, 's, 'e) t -> string
(** The HTML of the whole page: like {!html}, with the children of each portal
    printed where the portal is, inside a [<div data-silica-portal>], as the
    browser host paints them into the page. *)

val node : ('p, 's, 'e) t -> Silica.Instance.message Silica.Node.t
(** The resolved view. *)

val submit : ('p, 's, 'e) t -> unit
(** Submit the first form that has a submit handler. *)

val snapshot : ('p, 's, 'e) t -> 's
val count : ('p, 's, 'e) t -> int

val send : ('p, 's, 'e) t -> 'e -> unit
(** An event for the root component. *)

val press : ('p, 's, 'e) t -> string -> unit
(** Press the first pressable element whose text contains the text.

    @raise Failure when there is none. *)

val input : ('p, 's, 'e) t -> label:string -> string -> unit
(** Type into the first element with this [aria-label]. *)

val running : ('p, 's, 'e) t -> string list
(** The ids of the running timers and calls, sorted. *)

val held : ('p, 's, 'e) t -> (string * string) list
(** The running held resources, as (id, {!Silica.Action.held_name}), in start
    order. *)

val press_key : ('p, 's, 'e) t -> Silica.Key.t -> bool
(** A key press in the page: the first running [Global_keys] resource that binds
    the key gets it. [false] when none does. *)

val outside_press : ('p, 's, 'e) t -> unit
(** A press outside the elements of every running [Outside_press] resource and
    [Dismiss] layer: each one with an outside event gets it, newest first.

    @raise Failure when none is running. *)

val escape : ('p, 's, 'e) t -> bool
(** An Escape press in the page, given to the newest running [Dismiss] layer
    with an Escape event. [false] when there is none. *)

val key_down : ('p, 's, 'e) t -> id:string -> Silica.Key.t -> bool
(** A key press on the element with this id: the key handlers of the element and
    then of its ancestors are tried in that order, as in the browser renderer.
    [false] when none handles it.

    @raise Failure when no element has the id. *)

val press_id : ('p, 's, 'e) t -> id:string -> unit
(** A press on the element with this id: the press handler of the element or of
    its nearest ancestor that has one.

    @raise Failure when there is none. *)

val focus : ('p, 's, 'e) t -> id:string -> unit
val blur : ('p, 's, 'e) t -> id:string -> unit
val pointer_enter : ('p, 's, 'e) t -> id:string -> unit
val pointer_leave : ('p, 's, 'e) t -> id:string -> unit

val attribute : ('p, 's, 'e) t -> id:string -> string -> string option
(** The value of an attribute of the element with this id; [Some ""] for a true
    boolean attribute. [None] when there is no such element or attribute. *)

val exists : ('p, 's, 'e) t -> id:string -> bool
(** An element with this id is in the view. *)

val fire : ('p, 's, 'e) t -> string -> unit
(** The timer with this id fires.

    @raise Failure when it is not running. *)

val fire_after_stop : ('p, 's, 'e) t -> string -> unit
(** Deliver the message of the timer or call with this id even when it was
    stopped: a host that ignores [Stop]. *)

val answer : ('p, 's, 'e) t -> ('i, 'o, 'x) Silica.Remote.endpoint -> 'o -> unit
(** The running call to the endpoint ends with [Ok value]. *)

val reject : ('p, 's, 'e) t -> ('i, 'o, 'x) Silica.Remote.endpoint -> 'x -> unit
(** The running call to the endpoint ends with [Rejected x]. The value goes
    through the [rejected] codec, as it does on the wire. *)

val answer_error :
  ('p, 's, 'e) t ->
  ('i, 'o, 'x) Silica.Remote.endpoint ->
  Silica.Remote.error ->
  unit
(** The running call to the endpoint ends with [Failed error]. *)

val subscriptions : ('p, 's, 'e) t -> (string * string) list
(** The running subscriptions as (topic name, key), in start order. *)

val publish :
  ('p, 's, 'e) t ->
  'a Silica.Topic.t ->
  key:string ->
  'a Silica.Topic.update ->
  unit
(** Deliver the update to every running subscription with this topic name and
    key. The value goes through the codecs, as it does on the wire. *)

val call_timeouts : ('p, 's, 'e) t -> int option list
(** The [timeout_ms] of the running calls, in start order. *)

val expire_call : ('p, 's, 'e) t -> ('i, 'o, 'x) Silica.Remote.endpoint -> unit
(** The running call to the endpoint times out, as a host does when [timeout_ms]
    passes: the machine gets [Failed (Network "timeout")].
    @raise Failure when the call has no timeout. *)

val call_inputs : ('p, 's, 'e) t -> string list
(** Encoded inputs of the running calls, in start order. *)

val log : ('p, 's, 'e) t -> string list
(** ["start <id>"], ["stop <id>"], ["focus <ref>"], ["navigate <url>"],
    ["title <text>"] and so on, in the order the host got them. *)

val clear_log : ('p, 's, 'e) t -> unit
val unmount : ('p, 's, 'e) t -> unit
