(** A menu button (WAI-ARIA menu button pattern; Base UI Menu).

    A trigger button opens a list of items, [role="menu"], painted in a portal
    at the end of the body and kept below the trigger. Keyboard focus moves to
    the highlighted item: Enter, Space and ArrowDown on the trigger open the
    menu on the first enabled item, ArrowUp on the last. In the menu, ArrowDown
    and ArrowUp move (and wrap), Home and End go to the ends, a letter goes to
    the next item whose label starts with it, Enter and Space choose the item.
    Disabled items are skipped. Escape closes the menu and focuses the trigger,
    Tab closes it and lets focus move on, and a press outside closes it.

    Styling: classes per part ({!type-classes}); [data-open] and [data-closed]
    on the popup, [data-popup-open] on the trigger, [data-highlighted] and
    [data-disabled] on the items. *)

type item = {
  value : string;  (** Given to [on_select] when the item is chosen. *)
  label : string;
  disabled : bool;
}

type classes = { trigger : string; popup : string; item : string }

val no_classes : classes
(** All empty. *)

type props = {
  id : string;  (** Unique in the page; the ids of the parts start with it. *)
  label : string;  (** The text of the trigger; the menu is labelled by it. *)
  items : item list;
  open_ : bool option;
      (** When this changes to [Some b], the menu opens or closes. With [None]
          only the user opens it. *)
  classes : classes;
}

type nav = Next | Previous | First | Last | Letter of char  (** Lowercase. *)

type edge = Start | End

type event =
  | Props of props
  | Trigger_press
  | Open_key of Popup.reason * edge
      (** A key on the closed trigger: open on the first ([Start]) or the last
          ([End]) enabled item. *)
  | Move of nav
  | Highlight of int  (** The pointer entered the item at this position. *)
  | Choose of int  (** The item at this position was chosen. *)
  | Close of Popup.reason

type opened = { props : props; highlighted : int option }

module State : sig
  type _ t = Idle : unit t | Closed : props t | Open : opened t
end

type snapshot = At : 'data State.t * 'data -> snapshot

val machine : (snapshot, event) Silica.Machine.t
(** The machine of the component. The checker reads it. *)

val component : (snapshot, event) Silica.Component.t
(** The machine with its view. *)

val view : snapshot -> event Silica.Node.t
(** The view of a snapshot. *)

val item_id : string -> int -> string
(** [item_id id i] is the element id and ref name of the item at position [i] of
    the menu [id]. *)

val props :
  id:string ->
  label:string ->
  ?open_:bool ->
  ?classes:classes ->
  item list ->
  props
(** [open_] defaults to [None]: only the trigger opens the menu. [classes]
    defaults to {!no_classes}. *)

val make :
  ?key:string ->
  ?on_open_change:(bool -> Popup.reason -> 'e) ->
  ?on_select:(string -> 'e) ->
  props ->
  'e Silica.Node.t
(** The node. [on_select] gets the value of each chosen item. [on_open_change]
    hears each open or close that the menu did by itself, not those that [open_]
    asked for. *)
