open Silica

type event = Run

let event_name = function Run -> "run"

type state = Ready | Done

let name = function Ready -> "ready" | Done -> "done"

let states = [ "ready"; "done" ]

let machine =
  Machine.make ~id:"search" ~init:Ready ~name ~states ~event_name
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
