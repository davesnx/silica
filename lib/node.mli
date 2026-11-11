(** Immutable view trees. Handlers are data: an event, or a function from the
    input value to an event. A tree can hold component nodes: each one runs its
    own machine while it is rendered (see {!Instance}). *)

type ('p, 'e) handler = { run : 'p -> 'e option; prevent : bool }
(** A handler for an event with payload ['p]. [run] sees the payload at the
    moment the event happens. It returns [None] to ignore the event. [prevent]
    asks the renderer to cancel the default behavior of the platform event at
    that moment, but only when [run] returned [Some _]. The answer to [prevent]
    does not wait for the machine. *)

type error = { id : string; message : string; detail : string option }
(** What a boundary fallback gets. [id] names the failure in the console and in
    [on_error] hooks. [message] is short and safe to show. [detail] is the
    exception text, the machine, its state and a trace: it is [Some _] only when
    the host runs in dev mode. *)

type 'e attr =
  | String of string * string
  | Bool of string * bool
  | Ref of Ref.t
  | On_press of 'e
  | On_double_press of 'e
  | On_input of (string -> 'e)
  | On_submit of 'e
  | On_focus of 'e
  | On_blur of 'e
  | On_key_down of (Key.t, 'e) handler
  | On_key_up of (Key.t, 'e) handler
  | On_pointer_enter of (Pointer.t, 'e) handler
  | On_pointer_leave of (Pointer.t, 'e) handler
  | On_reset
      (** A press on the element resets the {!val-boundary} whose fallback holds
          it. Outside a fallback it does nothing. *)
  | Rated of Rate.t * 'e attr
      (** The wrapped event handler is rate limited (see {!Rate}): [run] still
          sees every payload at once, and the event it returns is held back. *)

type ('s, 'e) component = { machine : ('s, 'e) Machine.t; view : 's -> 'e t }
(** A machine together with the view of its states. Define it once, at the top
    level: a rendered component is the same one as before when its [machine] is
    physically equal to the earlier one. *)

and 'e t =
  | Element of string * 'e attr list * 'e t list
  | Text of string
  | List of 'e t list
  | Keyed of string * 'e t
  | Portal of string option * 'e t list
      (** The children are painted into the element with this id, or into the
          end of the document body when it is [None], and not at the place of
          the node. A server render leaves them out. With [None], the browser
          renderer gives the container in the body the [data-theme] attribute of
          the nearest element above the node that has one, so the children keep
          the theme of their place (see [Silica_ui.Theme.scope]). *)
  | Boundary of 'e boundary
  | Component : ('s, 'ce, 'e) child -> 'e t
  | Chunk of 'e chunk

and 'e chunk =
  | Chunk_view : {
      chunk : 'a Chunk.t;
      loading : 'e t;
      render : 'a -> 'e t;
    }
      -> 'e chunk
      (** [render value] when the value of [chunk] is here; [loading] while it
          loads (see {!val-chunk}). *)

and 'e boundary = { fallback : error -> reset:'e attr -> 'e t; body : 'e t }
(** The components under [body] run as usual. When an exception escapes the
    machine step or the view of one of them, the boundary ends all of them,
    shows [fallback error ~reset] in place of [body], and keeps the state of the
    components above it. [reset] is {!On_reset}: pressing an element that has it
    renders [body] again, and the components under it start from their initial
    state. An exception in [fallback] goes to the next boundary above. *)

and ('s, 'ce, 'e) child = {
  key : string option;
  component : ('s, 'ce) component;
  props : 'ce option;
  output : 'ce -> 's -> 'e list;
  on_final : 's -> 'e list;
}
(** A component node in the view of a parent whose events are ['e]. The child
    machine has snapshots ['s] and events ['ce]. [props] is an event that
    carries the props: the host sends it to the child machine right after the
    start and again each time it changes (compared with [=]). [output] turns
    each event the child handled, with the snapshot after it, into events for
    the parent: this is how function props reach the parent. [on_final] does the
    same once, at the moment the child machine enters a final state (at its
    start or by a step); the parent gets the events at once, after the handler
    that caused it. *)

type 'e shape = 'e t

val element : string -> 'e attr list -> 'e t list -> 'e t
(** [element tag attrs children] is an HTML or SVG element. *)

val text : string -> 'e t
(** A text node. Hosts escape the text. *)

val list : 'e t list -> 'e t
(** A group of nodes with no element around them. *)

val keyed : string -> 'e t -> 'e t
(** Identity for the keyed diff in hosts. *)

val portal : ?target:string -> 'e t list -> 'e t
(** [target] is the id of an element of the page. *)

val boundary : fallback:(error -> reset:'e attr -> 'e t) -> 'e t -> 'e t
(** A {!type-boundary} around a part of the view. See {!type-boundary}. *)

val component :
  ?key:string ->
  ?props:'ce ->
  ?output:('ce -> 's -> 'e list) ->
  ?on_final:('s -> 'e list) ->
  ('s, 'ce) component ->
  'e t
(** A component node. [key] keeps the component through reordering. Without
    [output] the parent hears nothing. *)

val chunk : 'a Chunk.t -> loading:'e t -> ('a -> 'e t) -> 'e t
(** [chunk c ~loading render] shows [render value] once the value of [c] is
    here, and [loading] until then. The host starts the load when it renders the
    node, and renders again when the load ends. In the browser, the first paint
    waits for every chunk of the first view, so the server HTML is adopted with
    the code of its page. A load that fails raises {!Chunk_failed} in the view:
    the nearest {!val-boundary} shows its fallback, and its reset loads again.
*)

exception Chunk_failed of string

val chunk_view : 'e chunk -> 'e t
(** The node a chunk shows now: [render value] when the value is here, [loading]
    otherwise. For tools that walk a view, such as a checker. *)

val view : 'e t -> 'e shape
(** The node as a value to match on. A host uses it to walk the tree. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Change the events of a view, for example to put a child view into a parent
    view. *)

val map_attr : ('a -> 'b) -> 'a attr -> 'b attr
(** Change the event of an attribute, as {!map} does for a node. *)
