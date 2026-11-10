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

type event = Props of props | Toggle | Enter_key

module State = struct
  type _ t =
    | Idle : unit t
    | Unchecked : props t
    | Checked : props t
    | Indeterminate : props t
end

module M = Machine.Make (State)

type snapshot = M.snapshot = At : 'data State.t * 'data -> snapshot

let take (props : props) =
  if props.indeterminate then
    M.goto Indeterminate props
  else if props.checked then
    M.goto Checked props
  else
    M.goto Unchecked props

let requested (previous : props) (props : props) =
  props.checked <> previous.checked
  || props.indeterminate <> previous.indeterminate

let handle current ~toggle (previous : props) = function
  | Props props when requested previous props ->
      take props
  | Props props ->
      M.goto current props
  | Toggle when previous.disabled ->
      M.unhandled
  | Toggle ->
      toggle previous
  | Enter_key ->
      M.stay []

let machine =
  M.make ~id:"checkbox" ~init:(M.at Idle ())
    [
      M.state Idle "idle" ~to_:[ Unchecked; Checked; Indeterminate ]
        (fun () -> function
        | Props props ->
            take props
        | _ ->
            M.unhandled
        );
      M.state Unchecked "unchecked" ~to_:[ Checked; Indeterminate ]
        (handle Unchecked ~toggle:(M.goto Checked));
      M.state Checked "checked"
        ~to_:[ Unchecked; Indeterminate ]
        (handle Checked ~toggle:(M.goto Unchecked));
      M.state Indeterminate "indeterminate" ~to_:[ Unchecked; Checked ]
        (handle Indeterminate ~toggle:(M.goto Checked));
    ]

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

let view : snapshot -> event Node.t = function
  | At (Idle, ()) ->
      Node.list []
  | At (Unchecked, props) ->
      box props ~aria:"false" ~data:"data-unchecked" ~checked:false
  | At (Checked, props) ->
      box props ~aria:"true" ~data:"data-checked" ~checked:true
  | At (Indeterminate, props) ->
      box props ~aria:"mixed" ~data:"data-indeterminate" ~checked:false

let component = Component.make machine view

let props ~id ~label ?(checked = false) ?(indeterminate = false)
    ?(disabled = false) ?name ?(classes = no_classes) () =
  { id; label; checked; indeterminate; disabled; name; classes }

let make ?key ?on_checked_change props =
  let output event (snapshot : snapshot) =
    match (on_checked_change, event, snapshot) with
    | Some f, Toggle, At (Checked, _) ->
        [ f true ]
    | Some f, Toggle, At (Unchecked, _) ->
        [ f false ]
    | _ ->
        []
  in
  Component.node ?key ~props:(Props props) ~output component
