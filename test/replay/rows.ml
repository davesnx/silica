open Silica

type event = Pick
type state = Shown

let shape = Machine.shape ~name:(fun Shown -> "shown") ~names:[ "shown" ] ()
let events = Machine.shape ~name:(fun Pick -> "pick") ()

let numbered =
  Component.make_with_props
    (Machine.make_with_props ~id:"numbered" ~init:Shown ~shape ~events
       (fun _ Shown Pick -> Machine.Stay []
     )
    )
    (fun number Shown ->
      Node.element "button"
        [ Attr.on_press Pick ]
        [
          Node.text
            ( if number = 0 then
                ""
              else
                string_of_int number
            );
        ]
    )

let numbered_check = Check.component ~props:[ 1; 0 ] numbered

type row = { title : string; count : int }

let titled =
  Component.make_with_props
    (Machine.make_with_props ~id:"titled" ~init:Shown ~shape ~events
       (fun _ Shown Pick -> Machine.Stay []
     )
    )
    (fun row Shown ->
      Node.element "div" []
        [
          Node.element "p" [] [ Node.text (string_of_int row.count) ];
          Node.element "button" [ Attr.on_press Pick ] [ Node.text row.title ];
        ]
    )

let titled_check =
  Check.component
    ~props:[ { title = "Open"; count = 1 }; { title = ""; count = 0 } ]
    titled
