open Silica

let element = Node.element
let text = Node.text
let id value = Attr.string "id" value

type props = { key : int; label : int }
type state = { clicks : int; seen : int }
type event = Bump

let machine =
  Machine.make_with_props ~id:"row" ~init:{ clicks = 0; seen = -1 }
    ~shape:
      (Machine.shape
         ~name:(fun state ->
           if state.clicks >= 2 then
             "done"
           else
             "counting"
         )
         ~names:[ "counting"; "done" ] ()
      )
    ~events:(Machine.shape ~name:(fun Bump -> "bump") ())
    ~final:(fun state -> state.clicks >= 2)
    ~on_props:(fun _ props state -> Go { state with seen = props.label })
    (fun _ state Bump -> Go { state with clicks = state.clicks + 1 })

let view props state =
  let key = string_of_int props.key in
  element "li" []
    [
      element "span"
        [ id ("row-text-" ^ key) ]
        [
          text
            (Printf.sprintf "row %d label %d seen %d clicks %d" props.key
               props.label state.seen state.clicks
            );
        ];
      ( if state.clicks < 2 then
          element "button"
            [ id ("bump-" ^ key); Attr.on_press Bump ]
            [ text ("bump " ^ key) ]
        else
          Node.list []
      );
    ]

let component = Component.make_with_props machine view
