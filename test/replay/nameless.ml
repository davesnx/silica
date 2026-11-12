open Silica

type event = Edit | Done

let event_name = function Edit -> "edit" | Done -> "done"

type state = Viewing | Editing

let name = function Viewing -> "viewing" | Editing -> "editing"

let states = [ "viewing"; "editing" ]

let machine =
  Machine.make ~id:"nameless" ~init:Viewing ~name ~states ~event_name
    (fun state event ->
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
