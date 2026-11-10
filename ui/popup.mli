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

val template : parts
(** Parts whose refs are named ["trigger"] and ["popup"], for the [invoke] list
    of a state, where the id is not known yet. See {!scoped}. *)

val scoped :
  ('p -> string) -> ('p, 'e) Silica.Source.t -> ('p, 'e) Silica.Source.t
(** [scoped id source] renames each ref [r] of the source to
    [id data ^ "-" ^ r], so that {!template} refs become the refs of
    [parts (id data)]. *)

val dismiss : ?escape:'e -> ?outside:'e -> parts -> ('p, 'e) Silica.Source.t
(** The dismiss layer of the popup: the trigger and the popup are inside. *)

val class_attr : string -> 'e Silica.Node.attr list
(** [class] when the string is not empty. *)
