open Silica

type event = Typed of string | Increment
type state = { count : int; step : int }

let event_name = function Typed _ -> "typed" | Increment -> "increment"

let machine =
  Machine.make ~id:"stepper" ~init:{ count = 0; step = 1 }
    ~name:(fun _ -> "counting")
    ~states:[ "counting" ] ~event_name
    (fun state event ->
      match event with
      | Typed text -> (
          match int_of_string_opt text with
          | Some step ->
              Machine.Go { state with step }
          | None ->
              Machine.Ignore
        )
      | Increment ->
          Machine.Go { state with count = state.count + state.step }
    )

let component =
  Component.make machine (fun state ->
      Node.element "div" []
        [
          Node.element "p" [] [ Node.text (string_of_int state.count) ];
          Node.element "input"
            [
              Attr.string "type" "number";
              Attr.string "aria-label" "Step size";
              Attr.string "value" (string_of_int state.step);
              Attr.on_input (fun text -> Typed text);
            ]
            [];
          Node.element "button" [ Attr.on_press Increment ] [ Node.text "Add" ];
        ]
  )

let positive =
  Check.always ~name:"the step is positive"
    (Check.state (fun state -> state.step > 0))

let check =
  Check.component
    ~within:(fun state -> state.count <= 2)
    ~inputs:(fun _ -> [ Typed "1"; Typed "0" ])
    ~properties:[ positive ] component

let typing =
  Check.component
    ~within:(fun state -> state.count <= 2)
    ~texts:[ "1"; "0" ] ~properties:[ positive ] component
