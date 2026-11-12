open Silica

type orientation = Horizontal | Vertical
type tab = { value : string; label : string; disabled : bool }
type classes = { list : string; tab : string }

let no_classes = { list = ""; tab = "" }

type props = {
  id : string;
  tabs : tab list;
  value : string;
  orientation : orientation;
  classes : classes;
}

type move = Next | Previous | First | Last
type event = Select of string | Move of move

type model = { selected : string; focused : string }
type state = Ready of model [@@deriving fingerprint]

let name = function Ready _ -> "ready"
let states = [ "ready" ]

let event_name = function
  | Select value ->
      "select " ^ value
  | Move Next ->
      "move next"
  | Move Previous ->
      "move previous"
  | Move First ->
      "move first"
  | Move Last ->
      "move last"

let tab_id id value = id ^ "-tab-" ^ value
let panel_id id value = id ^ "-panel-" ^ value

let enabled (props : props) =
  List.filter_map
    (fun (tab : tab) ->
      if tab.disabled then
        None
      else
        Some tab.value
    )
    props.tabs

let selectable (props : props) value = List.mem value (enabled props)

let take (props : props) =
  let focused =
    match enabled props with
    | first :: _ when not (selectable props props.value) ->
        first
    | _ ->
        props.value
  in
  { selected = props.value; focused }

let index_of value values =
  let rec go i = function
    | [] ->
        None
    | v :: rest ->
        if v = value then
          Some i
        else
          go (i + 1) rest
  in
  go 0 values

let target props model move =
  match enabled props with
  | [] ->
      None
  | values ->
      let count = List.length values in
      let at = index_of model.focused values in
      let i =
        match (move, at) with
        | First, _ | Next, None ->
            0
        | Last, _ | Previous, None ->
            count - 1
        | Next, Some i ->
            (i + 1) mod count
        | Previous, Some i ->
            (i + count - 1) mod count
      in
      Some (List.nth values i)

let machine =
  Machine.make_with_props ~id:"tabs"
    ~init:(Ready { selected = ""; focused = "" })
    ~name ~states ~event_name ~fingerprint:state_fingerprint
    ~on_props:(fun previous (props : props) _ ->
      match previous with
      | Some (previous : props) when previous.value = props.value ->
          Ignore
      | Some _ | None ->
          Go (Ready (take props))
    )
    (fun (props : props) (Ready model) event ->
      match event with
      | Select value when value <> model.selected && selectable props value ->
          Go (Ready { selected = value; focused = value })
      | Select _ ->
          Ignore
      | Move move -> (
          match target props model move with
          | Some value ->
              Go_with
                ( Ready { model with focused = value },
                  [ Action.Focus (Ref.make (tab_id props.id value)) ]
                )
          | None ->
              Ignore
        )
    )

let el = Node.element
let text = Node.text

let key_move orientation (key : Key.t) =
  if key.ctrl || key.meta || key.alt || key.shift then
    None
  else
    match (orientation, key.key) with
    | Horizontal, "ArrowRight" | Vertical, "ArrowDown" ->
        Some (Move Next)
    | Horizontal, "ArrowLeft" | Vertical, "ArrowUp" ->
        Some (Move Previous)
    | (Horizontal | Vertical), "Home" ->
        Some (Move First)
    | (Horizontal | Vertical), "End" ->
        Some (Move Last)
    | (Horizontal | Vertical), _ ->
        None

let flag name on =
  if on then
    [ Attr.string name "" ]
  else
    []

let tab_button (props : props) model (tab : tab) =
  let id = props.id in
  let selected = tab.value = model.selected in
  el "button"
    ([
       Attr.string "type" "button";
       Attr.string "role" "tab";
       Attr.string "id" (tab_id id tab.value);
       Attr.ref_ (Ref.make (tab_id id tab.value));
       Attr.string "aria-selected" (string_of_bool selected);
       Attr.string "aria-controls" (panel_id id tab.value);
       Attr.string "tabindex"
         ( if tab.value = model.focused then
             "0"
           else
             "-1"
         );
       Attr.bool "disabled" tab.disabled;
     ]
    @ ( if selected then
          []
        else
          [ Attr.on_press (Select tab.value) ]
      )
    @ flag "data-active" selected
    @ flag "data-disabled" tab.disabled
    @ Popup.class_attr props.classes.tab
    )
    [ text tab.label ]

let view (props : props) (Ready model) =
  let orientation =
    match props.orientation with
    | Horizontal ->
        "horizontal"
    | Vertical ->
        "vertical"
  in
  el "div"
    ([
       Attr.string "id" props.id;
       Attr.string "role" "tablist";
       Attr.string "aria-orientation" orientation;
       Attr.string "data-orientation" orientation;
       Attr.on_key_down ~prevent:true (key_move props.orientation);
     ]
    @ Popup.class_attr props.classes.list
    )
    (List.map (tab_button props model) props.tabs)

let component = Component.make_with_props machine view
let tab ?(disabled = false) ~value label = { value; label; disabled }

let props ~id ?(orientation = Horizontal) ?(classes = no_classes) ~value tabs =
  { id; tabs; value; orientation; classes }

let make ?key ?on_value_change props =
  let output event (Ready model) =
    match (on_value_change, event) with
    | Some f, Select _ ->
        [ f model.selected ]
    | Some _, Move _ | None, (Select _ | Move _) ->
        []
  in
  Component.node_with_props ?key ~props ~output component

let panel ?(class_ = "") ~id ~value ~selected children =
  el "div"
    ([
       Attr.string "role" "tabpanel";
       Attr.string "id" (panel_id id value);
       Attr.string "aria-labelledby" (tab_id id value);
       Attr.string "tabindex" "0";
       Attr.bool "hidden" (value <> selected);
     ]
    @ Popup.class_attr class_
    )
    children
