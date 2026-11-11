open Silica

type event = Divide [@@deriving event_shape]
type state = Ready of int [@@deriving state_shape]

let component =
  Component.make
    (Machine.make ~id:"crash" ~init:(Ready 0) ~shape:state_shape
       ~events:event_shape (fun (Ready count) Divide ->
         Machine.Go (Ready (10 / count))
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
