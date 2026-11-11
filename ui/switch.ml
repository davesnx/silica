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

type event = Props of props | Toggle

module State = struct
  type _ t = Idle : unit t | Unchecked : props t | Checked : props t
end

module Switch_machine = Machine.Make (State)

type snapshot = Switch_machine.snapshot =
  | At : 'data State.t * 'data -> snapshot

let take (props : props) =
  if props.checked then
    Switch_machine.goto Checked props
  else
    Switch_machine.goto Unchecked props

let handle current ~toggled (previous : props) = function
  | Props props when props.checked <> previous.checked ->
      take props
  | Props props ->
      Switch_machine.goto current props
  | Toggle when previous.disabled ->
      Switch_machine.unhandled
  | Toggle ->
      Switch_machine.goto toggled previous

let machine =
  Switch_machine.make ~id:"switch"
    ~init:(Switch_machine.at Idle ())
    [
      Switch_machine.state Idle "idle" ~to_:[ Unchecked; Checked ]
        (fun () -> function
        | Props props ->
            take props
        | Toggle ->
            Switch_machine.unhandled
        );
      Switch_machine.state Unchecked "unchecked" ~to_:[ Checked ]
        (handle Unchecked ~toggled:Checked);
      Switch_machine.state Checked "checked" ~to_:[ Unchecked ]
        (handle Checked ~toggled:Unchecked);
    ]

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

let view : snapshot -> event Node.t = function
  | At (Idle, ()) ->
      Node.list []
  | At (Unchecked, props) ->
      switch props ~checked:false
  | At (Checked, props) ->
      switch props ~checked:true

let component = Component.make machine view

let props ~id ~label ?(checked = false) ?(disabled = false) ?name
    ?(classes = no_classes) () =
  { id; label; checked; disabled; name; classes }

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
