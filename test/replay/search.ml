open Silica

type event = Run [@@deriving event_shape]
type state = Ready | Done [@@deriving state_shape]

let machine =
  Machine.make ~id:"search" ~init:Ready ~shape:state_shape ~events:event_shape
    (fun state Run ->
      match state with Ready -> Machine.Go Done | Done -> Machine.Ignore
  )

let component =
  Component.make machine (fun state ->
      Node.element "div" []
        [
          Node.element "input"
            [
              Attr.string "aria-label" "Query";
              Attr.on_key_down (fun (key : Key.t) ->
                  if key.key = "Enter" then
                    Some Run
                  else
                    None
              );
            ]
            [];
          Node.element "p" []
            [ Node.text (match state with Ready -> "ready" | Done -> "done") ];
        ]
  )

let check = Check.component ~keys:[ Key.enter ] component
