(** A select-only combobox (WAI-ARIA select-only combobox pattern; Base UI
    Select).

    A trigger button, [role="combobox"], shows the label of the selected option
    or a placeholder and opens a list of options, [role="listbox"], painted in a
    portal at the end of the body and kept below the trigger. Keyboard focus
    stays on the trigger; [aria-activedescendant] names the highlighted option.
    Enter, Space, ArrowDown and ArrowUp on the closed trigger open the list on
    the selected option (or the first enabled one). While it is open, ArrowDown
    and ArrowUp move (no wrap), Home and End go to the ends, a letter goes to
    the next option whose label starts with it, Enter and Space select the
    highlighted option and close, Tab selects it, closes and lets focus move on,
    Escape and a press outside close with no change. Disabled options are
    skipped and cannot be selected.

    With a [name], a hidden input carries the value for a form.

    Styling: classes per part ({!type-classes}); [data-placeholder] on the
    trigger when no option is selected, [data-popup-open] on the trigger,
    [data-open] and [data-closed] on the list, [data-selected],
    [data-highlighted] and [data-disabled] on the options. *)

type item = { value : string; label : string; disabled : bool }

type classes = { trigger : string; popup : string; option : string }

val no_classes : classes
(** All empty. *)

type props = {
  id : string;  (** Unique in the page; the ids of the parts start with it. *)
  label : string;  (** The accessible name of the trigger and the list. *)
  items : item list;
  value : string option;
      (** When this changes, the select takes it as its value. *)
  placeholder : string;  (** Shown when no option is selected. *)
  name : string option;  (** The name of the hidden input for forms. *)
  classes : classes;
}

type nav = Next | Previous | First | Last | Letter of char  (** Lowercase. *)

type event =
  | Trigger_press
  | Open_key of Popup.reason
  | Move of nav
  | Highlight of int  (** The pointer entered the option at this position. *)
  | Select of int * Popup.reason
      (** The option at this position was selected: by a press or Enter and
          Space ([Item_press]) or by Tab ([Focus_out]). *)
  | Close of Popup.reason

type state =
  | Closed of string option  (** The selected value. *)
  | Open of string option * int option
      (** The selected value and the position of the highlighted option. *)

val machine : (props, state, event) Silica.Machine.t
(** The machine of the component. The checker reads it. *)

val component : (props, state, event) Silica.Component.t
(** The machine with its view. *)

val view : props -> state -> event Silica.Node.t
(** The view of a state. *)

val option_id : string -> int -> string
(** [option_id id i] is the element id and ref name of the option at position
    [i] of the select [id]. *)

val props :
  id:string ->
  label:string ->
  ?value:string ->
  ?placeholder:string ->
  ?name:string ->
  ?classes:classes ->
  item list ->
  props
(** [placeholder] defaults to [""]. *)

val make :
  ?key:string ->
  ?on_open_change:(bool -> Popup.reason -> 'e) ->
  ?on_value_change:(string -> 'e) ->
  props ->
  'e Silica.Node.t
(** The node. [on_value_change] hears each value that the user selected and that
    differs from the current one, not the values that [value] asked for.
    [on_open_change] hears each open or close that the select did by itself. *)
