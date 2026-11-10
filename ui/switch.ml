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

module M = Machine.Make (State)

type snapshot = M.snapshot = At : 'data State.t * 'data -> snapshot

let take (props : props) =
  if props.checked then
    M.goto Checked props
  else
    M.goto Unchecked props

let handle current ~toggled (previous : props) = function
  | Props props when props.checked <> previous.checked ->
      take props
  | Props props ->
      M.goto current props
  | Toggle when previous.disabled ->
      M.unhandled
  | Toggle ->
      M.goto toggled previous

let machine =
  M.make ~id:"switch" ~init:(M.at Idle ())
    [
      M.state Idle "idle" ~to_:[ Unchecked; Checked ] (fun () -> function
        | Props props ->
            take props
        | Toggle ->
            M.unhandled
        );
      M.state Unchecked "unchecked" ~to_:[ Checked ]
        (handle Unchecked ~toggled:Checked);
      M.state Checked "checked" ~to_:[ Unchecked ]
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
