(** Parts that every popup component shares: the reason of an open change, the
    refs and ids of the trigger and the popup, the ARIA attributes of the
    trigger, the [data-*] state attributes and the dismiss layer. *)

type reason =
  | Trigger_press  (** The trigger was pressed. *)
  | Trigger_hover  (** The pointer rested on the trigger. *)
  | Trigger_focus  (** The trigger got keyboard focus. *)
  | Outside_press  (** A press started outside the popup and the trigger. *)
  | Escape_key
  | Close_press  (** A close part, or content that closes the popup. *)
  | Item_press  (** An item of a list was chosen. *)
  | Focus_out  (** Focus left the popup. *)
  | List_navigation  (** A key on the trigger opened the list. *)

val reason_name : reason -> string
(** ["trigger-press"], ["escape-key"] and so on, as in Base UI. *)

type parts = {
  trigger : Silica.Ref.t;
  popup : Silica.Ref.t;
  trigger_id : string;
  popup_id : string;
}

val parts : string -> parts
(** The refs and element ids of the popup with this id: [id ^ "-trigger"] and
    [id ^ "-popup"]. The id must be unique in the page. *)

val open_attrs : bool -> 'e Silica.Node.attr list
(** [data-open] or [data-closed]. *)

val trigger_attrs :
  parts -> haspopup:string -> open_:bool -> 'e Silica.Node.attr list
(** The id and ref of the trigger, [aria-haspopup], [aria-expanded],
    [aria-controls] (while open) and [data-popup-open] (while open). *)

val dismiss :
  ?escape:'e -> ?outside:'e -> ?focus_out:'e -> parts -> 'e Silica.Host.t
(** The dismiss layer of the popup: the trigger and the popup are inside (see
    {!Silica.Host.dismiss}). *)

val layer :
  ?escape:'e ->
  ?outside:'e ->
  ?focus_out:'e ->
  ?place:Silica.Place.t ->
  parts ->
  'e Silica.Host.t list
(** The resources of an open popup next to its trigger: {!dismiss} and
    {!Silica.Host.anchor} of the popup to the trigger. *)

val close : ?focus:bool -> parts -> 's -> ('s, 'e) Silica.Machine.next
(** [close parts closed] goes to the closed snapshot and, unless [focus] is
    [false], moves focus to the trigger. A close by an outside press passes
    [~focus:false]: focus stays where the press put it. *)

val class_attr : string -> 'e Silica.Node.attr list
(** [class] when the string is not empty. *)

val open_change : ('p -> bool option) -> 'p option -> 'p -> bool option
(** [open_change open_ previous props] is the open state that a parent asks for
    with a controlled prop [open_ props]: [Some b] when it is [Some b] and the
    previous props ([None] at the start) did not ask for [b] already, [None]
    otherwise. Use it in the [on_props] of a popup machine (see
    {!Silica.Machine.make_with_props}). *)

val report : (bool -> reason -> 'e) option -> bool -> reason -> 'e list
(** [report on_open_change open_ reason] is the output of an open change for the
    parent: [[f open_ reason]] when [on_open_change] is [Some f]. *)
