open Silica

type event = Edit | Done [@@deriving event_shape]
type state = Viewing | Editing [@@deriving state_shape]

let machine =
  Machine.make ~id:"nameless" ~init:Viewing ~shape:state_shape
    ~events:event_shape (fun state event ->
      match (state, event) with
      | Viewing, Edit ->
          Machine.Go Editing
      | Editing, Done ->
          Machine.Go Viewing
      | _ ->
          Machine.Ignore
  )

let make ~label =
  Component.make machine (function
    | Viewing ->
        Node.element "button" [ Attr.on_press Edit ] [ Node.text "Edit" ]
    | Editing ->
        Node.element "button" [ Attr.on_press Done ] [ Node.text label ]
    )

let component = make ~label:""
let fixed = make ~label:"Done"
let check = Check.component component
