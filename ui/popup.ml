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

let template =
  {
    trigger = Ref.make "trigger";
    popup = Ref.make "popup";
    trigger_id = "trigger";
    popup_id = "popup";
  }

let scoped id source =
  Source.map_refs (fun data r -> Ref.make (id data ^ "-" ^ Ref.name r)) source

let dismiss ?escape ?outside parts =
  Source.dismiss ?escape ?outside [ parts.trigger; parts.popup ]

let class_attr = function "" -> [] | value -> [ Attr.string "class" value ]
