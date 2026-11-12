open Silica

type classes = { root : string; track : string; thumb : string }

let no_classes = { root = ""; track = ""; thumb = "" }

type props = {
  id : string;
  label : string;
  checked : bool;
  disabled : bool;
  name : string option;
  classes : classes;
}

type event = Toggle
type state = Unchecked | Checked

let name = function Unchecked -> "unchecked" | Checked -> "checked"
let states = [ "unchecked"; "checked" ]
let event_name = function Toggle -> "toggle"

let of_props (props : props) =
  if props.checked then
    Checked
  else
    Unchecked

let machine =
  Machine.make_with_props ~id:"switch" ~init:Unchecked ~name ~states ~event_name
    ~on_props:(fun previous props _ ->
      match previous with
      | Some (previous : props) when previous.checked = props.checked ->
          Ignore
      | Some _ | None ->
          Go (of_props props)
    )
    (fun (props : props) state Toggle ->
      if props.disabled then
        Ignore
      else
        match state with Checked -> Go Unchecked | Unchecked -> Go Checked
    )

let el = Node.element

let switch (props : props) ~checked =
  let state =
    Attr.string
      ( if checked then
          "data-checked"
        else
          "data-unchecked"
      )
      ""
    ::
    ( if props.disabled then
        [ Attr.string "data-disabled" "" ]
      else
        []
    )
  in
  Node.list
    [
      el "button"
        ([
           Attr.string "type" "button";
           Attr.string "id" props.id;
           Attr.string "role" "switch";
           Attr.string "aria-checked" (string_of_bool checked);
           Attr.bool "disabled" props.disabled;
           Attr.on_press Toggle;
         ]
        @ state
        @ Popup.class_attr props.classes.root
        )
        [
          el "span"
            (Attr.string "data-ui" "switch-track"
            :: Attr.string "aria-hidden" "true"
            :: Popup.class_attr props.classes.track
            )
            [
              el "span"
                (Attr.string "data-ui" "switch-thumb"
                 :: Attr.string "id" (props.id ^ "-thumb")
                 :: state
                @ Popup.class_attr props.classes.thumb
                )
                [];
            ];
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

let view props state = switch props ~checked:(state = Checked)
let component = Component.make_with_props machine view

let props ~id ~label ?(checked = false) ?(disabled = false) ?name
    ?(classes = no_classes) () =
  { id; label; checked; disabled; name; classes }

let make ?key ?on_checked_change props =
  let output Toggle state =
    match on_checked_change with
    | Some f ->
        [ f (state = Checked) ]
    | None ->
        []
  in
  Component.node_with_props ?key ~props ~output component
