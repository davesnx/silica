open Silica

type event = Divide

let event_name = function Divide -> "divide"

type state = Ready of int

let name = function Ready _ -> "ready"

let states = [ "ready" ]

let component =
  Component.make
    (Machine.make ~id:"crash" ~init:(Ready 0) ~name ~states ~event_name
       (fun (Ready count) Divide -> Machine.Go (Ready (10 / count))
     )
    )
    (fun (Ready count) ->
      Node.element "div" []
        [
          Node.element "p" [] [ Node.text (string_of_int count) ];
          Node.element "button" [ Attr.on_press Divide ] [ Node.text "Divide" ];
        ]
    )

let check = Check.component component
