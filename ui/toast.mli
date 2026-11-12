(** Toasts (Base UI Toast).

    The parent owns the list of toasts and gives it in the props; the component
    paints them in a viewport ([role="region"], [aria-live="polite"]) and tells
    the parent which toast to close with [on_close]. The parent removes it.

    A toast closes on a press of its close button, on Escape while focus is in
    it, and by itself: while the list is not empty and the pointer and focus are
    not in the viewport, the oldest toast (the first of the list) closes after
    [timeout_ms]. Pointer or focus in the viewport pauses the time; when both
    leave, the time starts again from zero. A new list starts the time again
    too. The time counts in steps of {!tick_ms}, so a toast closes up to one
    step after [timeout_ms].

    Element ids: the toast with key [k] is [id ^ "-toast-" ^ k], its title
    [id ^ "-toast-" ^ k ^ "-title"], its description [... ^ "-description"] and
    its close button [... ^ "-close"].

    Styling: classes per part ({!type-classes}), [data-priority="low"] or
    ["high"] on each toast, [data-paused] on the viewport while the time is
    paused. *)

type priority = Low | High

type toast = {
  key : string;  (** Unique in the list. *)
  title : string;  (** The heading; the toast is labelled by it. *)
  description : string option;
  priority : priority;
      (** [High]: [role="alertdialog"]; [Low]: [role="dialog"]. *)
}
[@@deriving fingerprint]

type classes = {
  viewport : string;
  toast : string;
  title : string;
  description : string;
  close : string;
}

val no_classes : classes
(** All empty. *)

type props = {
  id : string;  (** Unique in the page; the ids of the parts start with it. *)
  toasts : toast list;  (** Oldest first. *)
  timeout_ms : int;  (** More than 0. *)
  label : string;  (** The accessible name of the viewport. *)
  close_label : string;  (** The accessible name of each close button. *)
  classes : classes;
}

type event =
  | Close of string
      (** The close button or Escape in the toast with this key. *)
  | Tick
  | Hover of bool  (** The pointer enters ([true]) or leaves the viewport. *)
  | Focus of bool  (** Focus enters ([true]) or leaves the viewport. *)

type model = { hovered : bool; focused : bool }

type state =
  | Empty of model  (** No toast. *)
  | Running of model * int
      (** The time runs; the [int] is the time since it started, in ms. *)
  | Paused of model  (** The pointer or focus is in the viewport. *)
  | Expired of model
      (** The time ran out and the component told the parent to close the oldest
          toast. It waits for a new list. *)

val tick_ms : int
(** The step of the time: 100. *)

val machine : (props, state, event) Silica.Machine.t
(** The machine of the component. The checker reads it. *)

val component : (props, state, event) Silica.Component.t
(** The machine with its view. *)

val view : props -> state -> event Silica.Node.t
(** The view of a state. *)

val toast :
  ?description:string -> ?priority:priority -> key:string -> string -> toast
(** [toast ~key title]. [priority] defaults to [Low]. *)

val props :
  id:string ->
  ?timeout_ms:int ->
  ?label:string ->
  ?close_label:string ->
  ?classes:classes ->
  toast list ->
  props
(** [timeout_ms] defaults to 5000, [label] to ["Notifications"], [close_label]
    to ["Close"].

    Raises [Invalid_argument] when [timeout_ms] is not more than 0. *)

val make : ?key:string -> ?on_close:(string -> 'e) -> props -> 'e Silica.Node.t
(** The node. [on_close] gets the key of each toast to remove. *)
