open Silica

type item = { value : string; label : string; disabled : bool }
type classes = { trigger : string; popup : string; option : string }

let no_classes = { trigger = ""; popup = ""; option = "" }

type props = {
  id : string;
  label : string;
  items : item list;
  value : string option;
  placeholder : string;
  name : string option;
  classes : classes;
}

type nav = Next | Previous | First | Last | Letter of char

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

type event =
  | Trigger_press
  | Open_key of Popup.reason
  | Move of nav
  | Highlight of int
  | Select of int * Popup.reason
  | Close of Popup.reason

type state = Closed of string option | Open of string option * int option

let name = function Closed _ -> "closed" | Open _ -> "open"
let states = [ "closed"; "open" ]

let event_name = function
  | Trigger_press ->
      "trigger_press"
  | Open_key reason ->
      "open_key " ^ Popup.reason_name reason
  | Move nav ->
      "move " ^ nav_name nav
  | Highlight _ ->
      "highlight"
  | Select (_, reason) ->
      "select " ^ Popup.reason_name reason
  | Close reason ->
      "close " ^ Popup.reason_name reason

type selection = { props : props; value : string option }

let option_id id i = id ^ "-option-" ^ string_of_int i
let parts (props : props) = Popup.parts props.id

let enabled (props : props) i =
  match List.nth_opt props.items i with
  | Some item ->
      not item.disabled
  | None ->
      false

let range from until = List.init (max 0 (until - from)) (fun i -> from + i)

let find (props : props) pred order =
  List.find_opt (fun i -> enabled props i && pred i) order

let starts_with letter (item : item) =
  item.label <> "" && Char.lowercase_ascii item.label.[0] = letter

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
        find props any (range (at + 1) n)
    | Previous ->
        find props any (List.rev (range 0 (Option.value highlighted ~default:n)))
    | Letter letter ->
        find props
          (fun i -> starts_with letter (List.nth props.items i))
          (range (at + 1) n @ range 0 (at + 1))
  in
  match found with Some _ -> found | None -> highlighted

let selected_index (props : props) value =
  List.find_index (fun (item : item) -> Some item.value = value) props.items

let scroll (props : props) = function
  | Some i ->
      [ Action.Scroll_into_view (Ref.make (option_id props.id i)) ]
  | None ->
      []

let show props value : (state, event) Machine.next =
  let highlighted =
    match selected_index props value with
    | Some i when enabled props i ->
        Some i
    | Some _ | None ->
        navigate props None First
  in
  Go_with (Open (value, highlighted), scroll props highlighted)

let close ?focus props value : (state, event) Machine.next =
  Popup.close ?focus (parts props) (Closed value)

let machine =
  Machine.make_with_props ~id:"select" ~init:(Closed None) ~name ~states
    ~event_name
    ~during:(fun props -> function
      | Open _ ->
          Popup.layer ~escape:(Close Popup.Escape_key)
            ~outside:(Close Popup.Outside_press) (parts props)
      | Closed _ ->
          []
      )
    ~on_props:(fun previous (props : props) state ->
      let take current =
        match previous with
        | Some (previous : props) when previous.value = props.value ->
            current
        | Some _ | None ->
            props.value
      in
      match state with
      | Closed value when take value = value ->
          Ignore
      | Closed value ->
          Go (Closed (take value))
      | Open (value, highlighted) ->
          let next_value = take value
          and next_highlighted =
            Option.bind highlighted (fun i ->
                if enabled props i then
                  Some i
                else
                  None
            )
          in
          if next_value = value && next_highlighted = highlighted then
            Ignore
          else
            Go (Open (next_value, next_highlighted))
    )
    (fun props state event ->
      match (state, event) with
      | Closed value, (Trigger_press | Open_key _) ->
          show props value
      | Open (value, _), Trigger_press ->
          close props value
      | Open (value, highlighted), Move nav -> (
          match navigate props highlighted nav with
          | next when next = highlighted ->
              Stay []
          | next ->
              Go_with (Open (value, next), scroll props next)
        )
      | Open (value, highlighted), Highlight i when enabled props i ->
          if highlighted = Some i then
            Stay []
          else
            Go (Open (value, Some i))
      | Open _, Select (i, _) when enabled props i ->
          close props (Some (List.nth props.items i).value)
      | Open (value, _), Close reason ->
          close ~focus:(reason <> Popup.Outside_press) props value
      | Closed _, (Move _ | Highlight _ | Select _ | Close _)
      | Open _, (Highlight _ | Select _ | Open_key _) ->
          Ignore
    )

let el = Node.element
let text = Node.text
let plain (key : Key.t) = not (key.ctrl || key.meta || key.alt)

let choose (sel : selection) i reason =
  if Some (List.nth sel.props.items i).value = sel.value then
    Close reason
  else
    Select (i, reason)

let closed_key (key : Key.t) =
  if not (plain key) then
    None
  else
    match key.key with
    | "Enter" | " " ->
        Some (Open_key Popup.Trigger_press)
    | "ArrowDown" | "ArrowUp" ->
        Some (Open_key Popup.List_navigation)
    | _ ->
        None

let open_key sel highlighted (key : Key.t) =
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
        Option.map (fun i -> choose sel i Popup.Item_press) highlighted
    | k when String.length k = 1 && k.[0] > ' ' && k.[0] <= '~' ->
        Some (Move (Letter (Char.lowercase_ascii k.[0])))
    | _ ->
        None

let tab_key sel highlighted (key : Key.t) =
  match key.key with
  | "Tab" -> (
      match highlighted with
      | Some i ->
          Some (choose sel i Popup.Focus_out)
      | None ->
          Some (Close Popup.Focus_out)
    )
  | _ ->
      None

let trigger (sel : selection) ~highlighted ~open_ =
  let parts = parts sel.props in
  let shown =
    Option.bind sel.value (fun value ->
        List.find_opt (fun (item : item) -> item.value = value) sel.props.items
    )
  in
  let key =
    if open_ then
      open_key sel highlighted
    else
      closed_key
  in
  el "button"
    ([
       Attr.string "type" "button";
       Attr.string "role" "combobox";
       Attr.string "aria-label" sel.props.label;
       Attr.on_press Trigger_press;
       Attr.on_key_down ~prevent:true key;
     ]
    @ Popup.trigger_attrs parts ~haspopup:"listbox" ~open_
    @ ( match highlighted with
      | Some i when open_ ->
          [ Attr.string "aria-activedescendant" (option_id sel.props.id i) ]
      | _ ->
          []
      )
    @ ( match shown with
      | Some _ ->
          []
      | None ->
          [ Attr.string "data-placeholder" "" ]
      )
    @ Popup.open_attrs open_
    @ Popup.class_attr sel.props.classes.trigger
    )
    [
      text
        ( match shown with
        | Some item ->
            item.label
        | None ->
            sel.props.placeholder
        );
    ]

let hidden_input (sel : selection) =
  match sel.props.name with
  | Some name ->
      el "input"
        [
          Attr.string "type" "hidden";
          Attr.string "id" (sel.props.id ^ "-input");
          Attr.string "name" name;
          Attr.string "value" (Option.value sel.value ~default:"");
        ]
        []
  | None ->
      Node.list []

let option (sel : selection) highlighted i (item : item) =
  let id = option_id sel.props.id i in
  let selected = sel.value = Some item.value in
  let flag name on =
    if on then
      [ Attr.string name "" ]
    else
      []
  in
  el "div"
    ([
       Attr.string "id" id;
       Attr.ref_ (Ref.make id);
       Attr.string "role" "option";
       Attr.string "aria-selected" (string_of_bool selected);
     ]
    @ flag "data-selected" selected
    @ flag "data-highlighted" (highlighted = Some i)
    @ ( if item.disabled then
          [ Attr.string "aria-disabled" "true"; Attr.string "data-disabled" "" ]
        else
          [
            Attr.on_press (choose sel i Popup.Item_press);
            Attr.on_pointer_enter (fun _ -> Some (Highlight i));
          ]
      )
    @ Popup.class_attr sel.props.classes.option
    )
    [ text item.label ]

let popup (sel : selection) highlighted =
  let parts = parts sel.props in
  Node.portal
    [
      el "div"
        ([
           Attr.string "id" parts.popup_id;
           Attr.ref_ parts.popup;
           Attr.string "role" "listbox";
           Attr.string "aria-label" sel.props.label;
         ]
        @ Popup.open_attrs true
        @ Popup.class_attr sel.props.classes.popup
        )
        (List.mapi (option sel highlighted) sel.props.items);
    ]

let root (sel : selection) ~highlighted ~open_ =
  el "span"
    ( if open_ then
        [ Attr.on_key_down (tab_key sel highlighted) ]
      else
        []
    )
    [ trigger sel ~highlighted ~open_; hidden_input sel ]

let view props = function
  | Closed value ->
      root { props; value } ~highlighted:None ~open_:false
  | Open (value, highlighted) ->
      let sel = { props; value } in
      Node.list [ root sel ~highlighted ~open_:true; popup sel highlighted ]

let component = Component.make_with_props machine view

let props ~id ~label ?value ?(placeholder = "") ?name ?(classes = no_classes)
    items =
  { id; label; items; value; placeholder; name; classes }

let make ?key ?on_open_change ?on_value_change (props : props) =
  let opened = Popup.report on_open_change in
  let output event state =
    match (event, state) with
    | Trigger_press, Open _ ->
        opened true Popup.Trigger_press
    | Trigger_press, Closed _ ->
        opened false Popup.Trigger_press
    | Open_key reason, Open _ ->
        opened true reason
    | Select (i, reason), Closed _ ->
        Option.to_list
          (Option.map
             (fun f -> f (List.nth props.items i).value)
             on_value_change
          )
        @ opened false reason
    | Close reason, Closed _ ->
        opened false reason
    | Open_key _, Closed _
    | (Move _ | Highlight _), (Closed _ | Open _)
    | (Select _ | Close _), Open _ ->
        []
  in
  Component.node_with_props ?key ~props ~output component
