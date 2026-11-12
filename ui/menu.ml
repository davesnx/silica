open Silica

type item = { value : string; label : string; disabled : bool }
type classes = { trigger : string; popup : string; item : string }

let no_classes = { trigger = ""; popup = ""; item = "" }

type props = {
  id : string;
  label : string;
  items : item list;
  open_ : bool option;
  classes : classes;
}

type nav = Next | Previous | First | Last | Letter of char
type edge = Start | End

let nav_name = function
  | Next ->
      "next"
  | Previous ->
      "previous"
  | First ->
      "first"
  | Last ->
      "last"
  | Letter _ ->
      "letter"

let edge_name = function Start -> "first" | End -> "last"

type event =
  | Trigger_press
  | Open_key of Popup.reason * edge
  | Move of nav
  | Highlight of int
  | Choose of int
  | Close of Popup.reason

type state = Closed | Open of int option

let name = function Closed -> "closed" | Open _ -> "open"
let states = [ "closed"; "open" ]

let event_name = function
  | Trigger_press ->
      "trigger_press"
  | Open_key (reason, edge) ->
      "open_key " ^ edge_name edge ^ " " ^ Popup.reason_name reason
  | Move nav ->
      "move " ^ nav_name nav
  | Highlight _ ->
      "highlight"
  | Choose _ ->
      "choose"
  | Close reason ->
      "close " ^ Popup.reason_name reason

let item_id id i = id ^ "-item-" ^ string_of_int i
let parts (props : props) = Popup.parts props.id

let enabled (props : props) i =
  match List.nth_opt props.items i with
  | Some item ->
      not item.disabled
  | None ->
      false

let starts_with letter (item : item) =
  item.label <> "" && Char.lowercase_ascii item.label.[0] = letter

let find (props : props) pred order =
  List.find_opt (fun i -> enabled props i && pred i) order

let range from until = List.init (max 0 (until - from)) (fun i -> from + i)

let navigate (props : props) highlighted nav =
  let n = List.length props.items in
  let at = Option.value highlighted ~default:(-1) in
  let any _ = true in
  let found =
    match nav with
    | First ->
        find props any (range 0 n)
    | Last ->
        find props any (List.rev (range 0 n))
    | Next ->
        find props any (range (at + 1) n @ range 0 (at + 1))
    | Previous ->
        let at = Option.value highlighted ~default:n in
        find props any (List.rev (range 0 at) @ List.rev (range at n))
    | Letter letter ->
        find props
          (fun i -> starts_with letter (List.nth props.items i))
          (range (at + 1) n @ range 0 (at + 1))
  in
  match found with Some _ -> found | None -> highlighted

let focus (props : props) = function
  | Some i ->
      Action.Focus (Ref.make (item_id props.id i))
  | None ->
      Action.Focus (parts props).popup

let show props highlighted : (state, event) Machine.next =
  Go_with (Open highlighted, [ focus props highlighted ])

let close ?focus props : (state, event) Machine.next =
  Popup.close ?focus (parts props) Closed

let machine =
  Machine.make_with_props ~id:"menu" ~init:Closed ~name ~states ~event_name
    ~during:(fun props -> function
      | Open _ ->
          Popup.layer ~escape:(Close Popup.Escape_key)
            ~outside:(Close Popup.Outside_press) (parts props)
      | Closed ->
          []
      )
    ~on_props:(fun previous props state ->
      match
        ( Popup.open_change (fun (props : props) -> props.open_) previous props,
          state
        )
      with
      | Some true, Closed ->
          show props None
      | Some false, Open _ ->
          close props
      | (Some _ | None), Open highlighted -> (
          match
            Option.bind highlighted (fun i ->
                if enabled props i then
                  Some i
                else
                  None
            )
          with
          | kept when kept = highlighted ->
              Ignore
          | kept ->
              Go (Open kept)
        )
      | (Some _ | None), Closed ->
          Ignore
    )
    (fun props state event ->
      match (state, event) with
      | Closed, Trigger_press ->
          show props None
      | Closed, Open_key (_, edge) ->
          let nav = match edge with Start -> First | End -> Last in
          show props (navigate props None nav)
      | Open _, Trigger_press ->
          close props
      | Open highlighted, Move nav -> (
          match navigate props highlighted nav with
          | next when next = highlighted ->
              Stay []
          | next ->
              show props next
        )
      | Open highlighted, Highlight i when enabled props i ->
          if highlighted = Some i then
            Stay []
          else
            show props (Some i)
      | Open _, Choose i when enabled props i ->
          close props
      | Open _, Close reason ->
          close ~focus:(reason <> Popup.Outside_press) props
      | Closed, (Move _ | Highlight _ | Choose _ | Close _)
      | Open _, (Highlight _ | Choose _ | Open_key _) ->
          Ignore
    )

let el = Node.element
let text = Node.text
let plain (key : Key.t) = not (key.ctrl || key.meta || key.alt)

let trigger_key (key : Key.t) =
  if not (plain key) then
    None
  else
    match key.key with
    | "Enter" | " " ->
        Some (Open_key (Popup.Trigger_press, Start))
    | "ArrowDown" ->
        Some (Open_key (Popup.List_navigation, Start))
    | "ArrowUp" ->
        Some (Open_key (Popup.List_navigation, End))
    | _ ->
        None

let menu_key highlighted (key : Key.t) =
  if not (plain key) then
    None
  else
    match key.key with
    | "ArrowDown" ->
        Some (Move Next)
    | "ArrowUp" ->
        Some (Move Previous)
    | "Home" ->
        Some (Move First)
    | "End" ->
        Some (Move Last)
    | "Enter" | " " ->
        Option.map (fun i -> Choose i) highlighted
    | k when String.length k = 1 && k.[0] > ' ' && k.[0] <= '~' ->
        Some (Move (Letter (Char.lowercase_ascii k.[0])))
    | _ ->
        None

let tab_key (key : Key.t) =
  if key.key = "Tab" then
    Some (Close Popup.Focus_out)
  else
    None

let trigger (props : props) ~open_ =
  el "button"
    (Attr.string "type" "button"
     :: Attr.on_press Trigger_press
     :: Attr.on_key_down ~prevent:true (fun key ->
         if open_ then
           None
         else
           trigger_key key
     )
     :: Popup.trigger_attrs (parts props) ~haspopup:"menu" ~open_
    @ Popup.open_attrs open_
    @ Popup.class_attr props.classes.trigger
    )
    [ text props.label ]

let item (props : props) highlighted i (item : item) =
  let id = item_id props.id i in
  let highlighted = highlighted = Some i in
  el "div"
    ([
       Attr.string "id" id;
       Attr.ref_ (Ref.make id);
       Attr.string "role" "menuitem";
       Attr.string "tabindex"
         ( if highlighted then
             "0"
           else
             "-1"
         );
     ]
    @ ( if highlighted then
          [ Attr.string "data-highlighted" "" ]
        else
          []
      )
    @ ( if item.disabled then
          [ Attr.string "aria-disabled" "true"; Attr.string "data-disabled" "" ]
        else
          [
            Attr.on_press (Choose i);
            Attr.on_pointer_enter (fun _ -> Some (Highlight i));
          ]
      )
    @ Popup.class_attr props.classes.item
    )
    [ text item.label ]

let popup (props : props) highlighted =
  let parts = parts props in
  Node.portal
    [
      el "div"
        ([
           Attr.string "id" parts.popup_id;
           Attr.ref_ parts.popup;
           Attr.string "role" "menu";
           Attr.string "aria-labelledby" parts.trigger_id;
           Attr.string "tabindex" "-1";
           Attr.on_key_down ~prevent:true (menu_key highlighted);
           Attr.on_key_down tab_key;
         ]
        @ Popup.open_attrs true
        @ Popup.class_attr props.classes.popup
        )
        (List.mapi (item props highlighted) props.items);
    ]

let view props = function
  | Closed ->
      trigger props ~open_:false
  | Open highlighted ->
      Node.list [ trigger props ~open_:true; popup props highlighted ]

let component = Component.make_with_props machine view

let props ~id ~label ?open_ ?(classes = no_classes) items =
  { id; label; items; open_; classes }

let make ?key ?on_open_change ?on_select (props : props) =
  let opened = Popup.report on_open_change in
  let output event state =
    match (event, state) with
    | Trigger_press, Open _ ->
        opened true Popup.Trigger_press
    | Trigger_press, Closed ->
        opened false Popup.Trigger_press
    | Open_key (reason, _), Open _ ->
        opened true reason
    | Choose i, Closed ->
        Option.to_list
          (Option.map (fun f -> f (List.nth props.items i).value) on_select)
        @ opened false Popup.Item_press
    | Close reason, Closed ->
        opened false reason
    | Open_key _, Closed
    | (Move _ | Highlight _), (Closed | Open _)
    | (Choose _ | Close _), Open _ ->
        []
  in
  Component.node_with_props ?key ~props ~output component
