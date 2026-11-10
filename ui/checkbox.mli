(** A checkbox (WAI-ARIA checkbox pattern; Base UI Checkbox).

    A [<button type="button" role="checkbox">] whose text is the label, so the
    label is its accessible name and a press on it toggles the box. Space and a
    press toggle it; Enter does nothing (as in Base UI). With a [name], a hidden
    input carries the value ["on"] in a form while the box is checked, as a
    native checkbox does; there is no input while it is not checked.

    Styling: classes per part ({!type-classes}) and the attributes
    [data-checked], [data-unchecked], [data-indeterminate] and [data-disabled]
    on the button. The indicator is a [<span data-ui="checkbox-indicator">]. *)

type classes = { root : string; indicator : string }

val no_classes : classes
(** All empty. *)

type props = {
  id : string;  (** Unique in the page; the ids of the parts start with it. *)
  label : string;  (** The visible text and the accessible name. *)
  checked : bool;
      (** When this changes, the box takes it. A parent that sets it must follow
          the changes it gets, or it cannot set the same value again. *)
  indeterminate : bool;
      (** When this changes to [true], the box shows the mixed state
          ([aria-checked="mixed"]); when it changes to [false], it shows
          [checked]. A press on a mixed box checks it. *)
  disabled : bool;
  name : string option;  (** The name of the hidden input for forms. *)
  classes : classes;
}

type event = Props of props | Toggle | Enter_key

module State : sig
  type _ t =
    | Idle : unit t
    | Unchecked : props t
    | Checked : props t
    | Indeterminate : props t
end

type snapshot = At : 'data State.t * 'data -> snapshot

val machine : (snapshot, event) Silica.Machine.t
(** The machine of the component. The checker reads it. *)

val component : (snapshot, event) Silica.Component.t
(** The machine with its view. *)

val view : snapshot -> event Silica.Node.t
(** The view of a snapshot. *)

val props :
  id:string ->
  label:string ->
  ?checked:bool ->
  ?indeterminate:bool ->
  ?disabled:bool ->
  ?name:string ->
  ?classes:classes ->
  unit ->
  props
(** [checked], [indeterminate] and [disabled] default to [false]. *)

val make :
  ?key:string -> ?on_checked_change:(bool -> 'e) -> props -> 'e Silica.Node.t
(** The node. [on_checked_change] hears each change that a press made, not those
    that the props asked for. *)
