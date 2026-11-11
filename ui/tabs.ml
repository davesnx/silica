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
type event = Props of props | Select of string | Move of move
type model = { props : props; selected : string; focused : string }

module State = struct
  type _ t = Idle : unit t | Ready : model t
end

module Tabs_machine = Machine.Make (State)

type snapshot = Tabs_machine.snapshot = At : 'data State.t * 'data -> snapshot

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
  { props; selected = props.value; focused }

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

let target model move =
  match enabled model.props with
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
  Tabs_machine.make ~id:"tabs" ~init:(Tabs_machine.at Idle ())
    [
      Tabs_machine.state Idle "idle" ~to_:[ Ready ] (fun () -> function
        | Props props ->
            Tabs_machine.goto Ready (take props)
        | _ ->
            Tabs_machine.unhandled
        );
      Tabs_machine.state Ready "ready" (fun model -> function
        | Props props when props.value <> model.props.value ->
            Tabs_machine.goto Ready (take props)
        | Props props ->
            Tabs_machine.goto Ready { model with props }
        | Select value
          when value <> model.selected && selectable model.props value ->
            Tabs_machine.goto Ready
              { model with selected = value; focused = value }
        | Select _ ->
            Tabs_machine.unhandled
        | Move move -> (
            match target model move with
            | Some value ->
                Tabs_machine.goto Ready
                  { model with focused = value }
                  ~actions:
                    [ Action.Focus (Ref.make (tab_id model.props.id value)) ]
            | None ->
                Tabs_machine.unhandled
          )
        );
    ]

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
    | _, "Home" ->
        Some (Move First)
    | _, "End" ->
        Some (Move Last)
    | _ ->
        None

let flag name on =
  if on then
    [ Attr.string name "" ]
  else
    []

let tab_button model (tab : tab) =
  let id = model.props.id in
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
       Attr.on_press (Select tab.value);
     ]
    @ flag "data-active" selected
    @ flag "data-disabled" tab.disabled
    @ Popup.class_attr model.props.classes.tab
    )
    [ text tab.label ]

let view : snapshot -> event Node.t = function
  | At (Idle, ()) ->
      Node.list []
  | At (Ready, model) ->
      let orientation =
        match model.props.orientation with
        | Horizontal ->
            "horizontal"
        | Vertical ->
            "vertical"
      in
      el "div"
        ([
           Attr.string "id" model.props.id;
           Attr.string "role" "tablist";
           Attr.string "aria-orientation" orientation;
           Attr.string "data-orientation" orientation;
           Attr.on_key_down ~prevent:true (key_move model.props.orientation);
         ]
        @ Popup.class_attr model.props.classes.list
        )
        (List.map (tab_button model) model.props.tabs)

let component = Component.make machine view
let tab ?(disabled = false) ~value label = { value; label; disabled }

let props ~id ?(orientation = Horizontal) ?(classes = no_classes) ~value tabs =
  { id; tabs; value; orientation; classes }

let make ?key ?on_value_change props =
  let output event (snapshot : snapshot) =
    match (on_value_change, event, snapshot) with
    | Some f, Select _, At (Ready, model) ->
        [ f model.selected ]
    | _ ->
        []
  in
  Component.node ?key ~props:(Props props) ~output component

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
