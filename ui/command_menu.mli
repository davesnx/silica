(** A command menu (command palette): a modal {!Dialog} that holds an inline
    {!Combobox} with the first match always highlighted. Base UI has no such
    component; this is the composition that its documentation suggests.

    The parent opens it with [open_] (for example from a global shortcut such as
    Ctrl+K, with {!Silica.Source.keys}) and follows [on_open_change]. When it
    opens, focus goes to the search input, with empty text. Enter or a press on
    an option runs the option: the menu reports its value and closes. Escape
    (with empty text; with text, the first Escape clears it) and a press outside
    close it. On close, focus goes back to the element that had it. *)

type classes = { dialog : Dialog.classes; combobox : Combobox.classes }

val no_classes : classes
(** All empty. *)

type props = {
  id : string;  (** Unique in the page; the ids of the parts start with it. *)
  title : string;  (** The heading of the dialog and the name of the input. *)
  placeholder : string;
  empty : string;  (** The text shown when nothing matches. *)
  items : Combobox.item list;
  open_ : bool option;
      (** When this changes to [Some b], the menu opens or closes. The parent
          must follow the open changes it gets, as with {!Dialog}. *)
  classes : classes;
}

val dialog : (Combobox.props, string) Dialog.t
(** The dialog whose content is the combobox; its event is the value of the
    chosen option. Its machine is the machine of the command menu. *)

val props :
  id:string ->
  title:string ->
  ?placeholder:string ->
  ?empty:string ->
  ?open_:bool ->
  ?classes:classes ->
  Combobox.item list ->
  props
(** [placeholder] defaults to [""], [empty] to ["No results"]. *)

val menu_props : props -> (Combobox.props, string) Dialog.props
(** The props of the dialog that {!make} renders. *)

val make :
  ?key:string ->
  ?on_open_change:(bool -> Popup.reason -> 'e) ->
  ?on_run:(string -> 'e) ->
  props ->
  'e Silica.Node.t
(** The node. [on_run] hears the value of each option the user ran.
    [on_open_change] hears each open change the menu did by itself: a close
    after a run has the reason {!Popup.Close_press}. *)
