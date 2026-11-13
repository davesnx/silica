open Silica

type reason =
  | Trigger_press
  | Trigger_hover
  | Trigger_focus
  | Outside_press
  | Escape_key
  | Close_press
  | Item_press
  | Focus_out
  | List_navigation

let reason_name = function
  | Trigger_press ->
      "trigger-press"
  | Trigger_hover ->
      "trigger-hover"
  | Trigger_focus ->
      "trigger-focus"
  | Outside_press ->
      "outside-press"
  | Escape_key ->
      "escape-key"
  | Close_press ->
      "close-press"
  | Item_press ->
      "item-press"
  | Focus_out ->
      "focus-out"
  | List_navigation ->
      "list-navigation"

type parts = {
  trigger : Ref.t;
  popup : Ref.t;
  trigger_id : string;
  popup_id : string;
}

let parts id =
  let trigger_id = id ^ "-trigger" and popup_id = id ^ "-popup" in
  {
    trigger = Ref.make trigger_id;
    popup = Ref.make popup_id;
    trigger_id;
    popup_id;
  }

let open_attrs open_ =
  if open_ then
    [ Attr.string "data-open" "" ]
  else
    [ Attr.string "data-closed" "" ]

let trigger_attrs parts ~haspopup ~open_ =
  [
    Attr.string "id" parts.trigger_id;
    Attr.ref_ parts.trigger;
    Attr.string "aria-haspopup" haspopup;
    Attr.string "aria-expanded" (string_of_bool open_);
  ]
  @
  if open_ then
    [
      Attr.string "aria-controls" parts.popup_id;
      Attr.string "data-popup-open" "";
    ]
  else
    []

let dismiss ?escape ?outside ?focus_out parts =
  Host.dismiss ?escape ?outside ?focus_out [ parts.trigger; parts.popup ]

let layer ?escape ?outside ?focus_out ?place parts =
  [
    dismiss ?escape ?outside ?focus_out parts;
    Host.anchor ?place ~anchor:parts.trigger parts.popup;
  ]

let close ?(focus = true) parts closed : (_, _) Machine.next =
  if focus then
    Go_with (closed, [ Action.Focus parts.trigger ])
  else
    Go closed

let class_attr = function "" -> [] | value -> [ Attr.string "class" value ]

let open_change open_ previous props =
  match (Option.map open_ previous, open_ props) with
  | Some before, Some value when before = Some value ->
      None
  | (Some _ | None), Some value ->
      Some value
  | (Some _ | None), None ->
      None

let report on_open_change value reason =
  match on_open_change with Some f -> [ f value reason ] | None -> []
