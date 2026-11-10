(** A text input with a list of options that filters as you type (WAI-ARIA
    combobox pattern with list autocomplete; Base UI Combobox).

    Focus stays in the input. The highlighted option is shown by
    [aria-activedescendant] on the input and [data-highlighted] on the option.
    Typing filters the items by their label and keywords (case-insensitive
    substring) and opens the list. Keys in the input: ArrowDown opens the list
    and moves the highlight down, ArrowUp moves it up (both wrap), Alt+ArrowDown
    opens the list and does not move it, Enter chooses the highlighted option,
    Escape closes the list or, when it is closed, clears the text. An Escape
    that the combobox does not use reaches the dismiss layers around it. A press
    on an option chooses it. Choosing puts the label of the option in the input
    and closes the list.

    Two modes ([inline] in {!type-props}):
    - popup: the list is painted in a portal, under the input, while open. A
      press outside the input and the list closes it.
    - inline: the list is always painted under the input, in place, and the
      first match is always highlighted. For a list inside a dialog, such as
      {!Command_menu}.

    Styling: classes per part ({!type-classes}), [data-open] or [data-closed] on
    the list, [data-highlighted], [data-selected] and [data-disabled] on the
    options. *)

type item = {
  value : string;  (** What {!make} reports when the item is chosen. *)
  label : string;  (** The text of the option; the filter reads it. *)
  keywords : string list;  (** More words that the filter matches. *)
  disabled : bool;  (** Shown, but not highlighted or chosen. *)
}

val item : ?keywords:string list -> ?disabled:bool -> string -> string -> item
(** [item value label]. *)

val matches : string -> item -> bool
(** [matches query item]: the query is a substring of the label or of a keyword,
    ignoring case. The empty query matches every item. *)

type classes = {
  root : string;
  input : string;
  list : string;
  option : string;
  empty : string;
}

val no_classes : classes
(** All empty. *)

type props = {
  id : string;  (** Unique in the page; the ids of the parts start with it. *)
  label : string;  (** The accessible name of the input ([aria-label]). *)
  placeholder : string;
  empty : string;  (** The text of the status shown when nothing matches. *)
  items : item list;
  inline : bool;  (** See the two modes above. *)
  classes : classes;
}

type data = {
  props : props;
  query : string;  (** The text of the input. *)
  highlighted : string option;  (** The value of the highlighted option. *)
  selected : string option;  (** The value of the last chosen option. *)
}

type event =
  | Props of props
  | Typed of string  (** The text of the input changed. *)
  | Move of [ `Next | `Previous | `First | `Last ]
      (** ArrowDown or ArrowUp: open the list and move the highlight. Home or
          End, while an option is highlighted: highlight the first or the last
          option. *)
  | Show  (** Alt+ArrowDown: open the list. *)
  | Highlight of string  (** The pointer is on the option with this value. *)
  | Choose_highlighted  (** Enter. *)
  | Choose of string  (** A press on the option with this value. *)
  | Escape
  | Outside_press
  | Focus_out  (** Tab, or focus moved outside the input and the list. *)

module State : sig
  type _ t =
    | Idle : unit t  (** Before the first props. *)
    | Closed : data t  (** Popup mode, list not shown. *)
    | Open : data t  (** Popup mode, list shown. *)
    | Inline : data t  (** Inline mode: the list is always shown. *)
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
  ?placeholder:string ->
  ?empty:string ->
  ?inline:bool ->
  ?classes:classes ->
  item list ->
  props
(** [placeholder] defaults to [""], [empty] to ["No results"], [inline] to
    [false]. *)

val make :
  ?key:string -> ?on_value_change:(string -> 'e) -> props -> 'e Silica.Node.t
(** The node. [on_value_change] hears the value of each option the user chose.
*)
