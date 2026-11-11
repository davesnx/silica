open Silica

type classes = { root : string; indicator : string }

let no_classes = { root = ""; indicator = "" }

type props = {
  id : string;
  label : string;
  checked : bool;
  indeterminate : bool;
  disabled : bool;
  name : string option;
  classes : classes;
}

type event = Toggle | Enter_key [@event_name "enter"] [@@deriving event_shape]
type state = Unchecked | Checked | Indeterminate [@@deriving state_shape]

let of_props (props : props) =
  if props.indeterminate then
    Indeterminate
  else if props.checked then
    Checked
  else
    Unchecked

let requested (previous : props) (props : props) =
  props.checked <> previous.checked
  || props.indeterminate <> previous.indeterminate

let machine =
  Machine.make_with_props ~id:"checkbox" ~init:Unchecked ~shape:state_shape
    ~events:event_shape
    ~on_props:(fun previous props _ ->
      match previous with
      | Some previous when not (requested previous props) ->
          Ignore
      | Some _ | None ->
          Go (of_props props)
    )
    (fun (props : props) state event ->
      match (event, state) with
      | Toggle, (Unchecked | Checked | Indeterminate) when props.disabled ->
          Ignore
      | Toggle, (Unchecked | Indeterminate) ->
          Go Checked
      | Toggle, Checked ->
          Go Unchecked
      | Enter_key, (Unchecked | Checked | Indeterminate) ->
          Stay []
    )

let el = Node.element

let box (props : props) ~aria ~data ~checked =
  Node.list
    [
      el "button"
        ([
           Attr.string "type" "button";
           Attr.string "id" props.id;
           Attr.string "role" "checkbox";
           Attr.string "aria-checked" aria;
           Attr.string data "";
           Attr.bool "disabled" props.disabled;
           Attr.on_press Toggle;
           Attr.on_key_down ~prevent:true (fun (key : Key.t) ->
               if key.key = "Enter" then
                 Some Enter_key
               else
                 None
           );
         ]
        @ ( if props.disabled then
              [ Attr.string "data-disabled" "" ]
            else
              []
          )
        @ Popup.class_attr props.classes.root
        )
        [
          el "span"
            (Attr.string "data-ui" "checkbox-indicator"
            :: Attr.string "aria-hidden" "true"
            :: Popup.class_attr props.classes.indicator
            )
            [];
          Node.text props.label;
        ];
      ( match props.name with
      | Some name when checked ->
          el "input"
            [
              Attr.string "type" "hidden";
              Attr.string "id" (props.id ^ "-input");
              Attr.string "name" name;
              Attr.string "value" "on";
            ]
            []
      | _ ->
          Node.list []
      );
    ]

let view props = function
  | Unchecked ->
      box props ~aria:"false" ~data:"data-unchecked" ~checked:false
  | Checked ->
      box props ~aria:"true" ~data:"data-checked" ~checked:true
  | Indeterminate ->
      box props ~aria:"mixed" ~data:"data-indeterminate" ~checked:false

let component = Component.make_with_props machine view

let props ~id ~label ?(checked = false) ?(indeterminate = false)
    ?(disabled = false) ?name ?(classes = no_classes) () =
  { id; label; checked; indeterminate; disabled; name; classes }

let make ?key ?on_checked_change props =
  let output event state =
    match (on_checked_change, event, state) with
    | Some f, Toggle, Checked ->
        [ f true ]
    | Some f, Toggle, Unchecked ->
        [ f false ]
    | Some _, Toggle, Indeterminate | Some _, Enter_key, _ | None, _, _ ->
        []
  in
  Component.node_with_props ?key ~props ~output component
