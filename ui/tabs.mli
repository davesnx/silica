(** Tabs with manual activation (WAI-ARIA tabs pattern; Base UI Tabs with
    [activateOnFocus = false]).

    The component paints the tab list: a [role="tablist"] of
    [<button role="tab">]. One tab is the tab stop ([tabindex="0"]): the focused
    one. Arrow keys along the orientation move focus to the next or previous
    enabled tab, with wrap; Home and End move it to the first and last enabled
    tab. A press, Enter or Space (the native button turns both keys into a
    press) selects the focused tab. Moving focus does not select.

    The caller paints the panels with {!panel}, from the value it follows with
    [on_value_change].

    Element ids: the tab of value [v] is [id ^ "-tab-" ^ v] and its panel is
    [id ^ "-panel-" ^ v], so values must be valid in an id.

    Styling: classes per part ({!type-classes}), [data-active] on the selected
    tab, [data-disabled] on disabled tabs, [data-orientation] on the list. *)

type orientation = Horizontal | Vertical

type tab = {
  value : string;  (** Unique among the tabs. *)
  label : string;  (** The text of the tab and its accessible name. *)
  disabled : bool;
}

type classes = { list : string; tab : string }

val no_classes : classes
(** All empty. *)

type props = {
  id : string;  (** Unique in the page; the ids of the parts start with it. *)
  tabs : tab list;
  value : string;
      (** The selected tab. When this changes, the list takes it. A parent that
          sets it must follow the changes it gets, or it cannot set the same
          value again. *)
  orientation : orientation;
  classes : classes;
}

type move = Next | Previous | First | Last

type event =
  | Select of string  (** A press on the tab with this value. *)
  | Move of move  (** A key on the list moves focus. *)

type model = { selected : string; focused : string  (** The tab stop. *) }
type state = Ready of model

val machine : (props, state, event) Silica.Machine.t
(** The machine of the component. The checker reads it. *)

val component : (props, state, event) Silica.Component.t
(** The machine with its view. *)

val view : props -> state -> event Silica.Node.t
(** The view of a state. *)

val tab : ?disabled:bool -> value:string -> string -> tab
(** [tab ~value label]. [disabled] defaults to [false]. *)

val props :
  id:string ->
  ?orientation:orientation ->
  ?classes:classes ->
  value:string ->
  tab list ->
  props
(** [orientation] defaults to [Horizontal]. *)

val make :
  ?key:string -> ?on_value_change:(string -> 'e) -> props -> 'e Silica.Node.t
(** The node. [on_value_change] hears each selection that a press made, not
    those that [value] asked for, and not a press on the selected tab. *)

val panel :
  ?class_:string ->
  id:string ->
  value:string ->
  selected:string ->
  'e Silica.Node.t list ->
  'e Silica.Node.t
(** [panel ~id ~value ~selected children] is the [role="tabpanel"] of the tab
    [value] of the tabs [id], labelled by its tab, [hidden] unless [value] is
    [selected]. *)
