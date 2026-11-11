(** A switch (WAI-ARIA switch pattern; Base UI Switch).

    A [<button type="button" role="switch">] whose text is the label, so the
    label is its accessible name. A press, Space and Enter toggle it (the native
    button turns both keys into a press). With a [name], a hidden input carries
    the value ["on"] in a form while the switch is on, as a native checkbox
    does; there is no input while it is off.

    Styling: classes per part ({!type-classes}) and the attributes
    [data-checked], [data-unchecked] and [data-disabled] on the button and on
    the thumb, a [<span data-ui="switch-thumb">] in a
    [<span data-ui="switch-track">]. *)

type classes = { root : string; track : string; thumb : string }

val no_classes : classes
(** All empty. *)

type props = {
  id : string;  (** Unique in the page; the ids of the parts start with it. *)
  label : string;  (** The visible text and the accessible name. *)
  checked : bool;
      (** When this changes, the switch takes it. A parent that sets it must
          follow the changes it gets, or it cannot set the same value again. *)
  disabled : bool;
  name : string option;  (** The name of the hidden input for forms. *)
  classes : classes;
}

type event = Toggle
type state = Unchecked | Checked

val machine : (props, state, event) Silica.Machine.t
(** The machine of the component. The checker reads it. *)

val component : (props, state, event) Silica.Component.t
(** The machine with its view. *)

val view : props -> state -> event Silica.Node.t
(** The view of a state. *)

val props :
  id:string ->
  label:string ->
  ?checked:bool ->
  ?disabled:bool ->
  ?name:string ->
  ?classes:classes ->
  unit ->
  props
(** [checked] and [disabled] default to [false]. *)

val make :
  ?key:string -> ?on_checked_change:(bool -> 'e) -> props -> 'e Silica.Node.t
(** The node. [on_checked_change] hears each change that a press made, not those
    that the props asked for. *)
