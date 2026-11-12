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

type changes = Changes of int

let changing ~counts =
  Component.make_with_props
    (Machine.make_with_props ~id:"changing" ~init:(Changes 0)
       ~shape:(Machine.shape ~name:(fun _ -> "shown") ~names:[ "shown" ] ())
       ~events
       ~on_props:(fun previous _ (Changes count) ->
         match previous with
         | Some _ when counts ->
             Machine.Go (Changes (count + 1))
         | Some _ | None ->
             Machine.Ignore
       )
       (fun _ (Changes _) Pick -> Machine.Stay [])
    )
    (fun route (Changes count) ->
      Node.element "div" []
        [
          Node.element "p" [] [ Node.text route ];
          Node.element "p" [] [ Node.text (string_of_int count) ];
          Node.element "button" [ Attr.on_press Pick ] [ Node.text "Pick" ];
        ]
    )

let changing_check_of component =
  Check.component
    ~within:(fun (Changes count) -> count <= 3)
    ~props:[ "home"; "about" ]
    ~properties:
      [
        Check.always ~name:"props change at most once"
          (Check.state (fun (Changes count) -> count < 2));
      ]
    component

let changing_bug = changing ~counts:true
let changing_fixed = changing ~counts:false
let changing_check = changing_check_of changing_bug
let changing_fixed_check = changing_check_of changing_fixed
