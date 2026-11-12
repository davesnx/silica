open Silica
open Jsonkit.Primitives

type event = Edit | Save [@@deriving json]

let event_name = function Edit -> "edit" | Save -> "save"

type state = Editing of int | Saved [@@deriving json]

let name = function Editing _ -> "editing" | Saved -> "saved"

let states = [ "editing"; "saved" ]

let machine ~fixed =
  let machine =
    Machine.make ~id:"save" ~init:(Editing 0) ~name ~states ~event_name
      (fun state event ->
        match (state, event) with
        | Editing count, Edit ->
            Machine.Go (Editing ((count + 1) mod 2))
        | Editing _, Save ->
            Machine.Go Saved
        | Saved, Save when fixed ->
            Machine.Go (Editing 0)
        | _ ->
            Machine.Ignore
    )
  in
  Machine.register_codecs machine
    ~state:(Silica_jsonkit.codec ~to_json:state_to_json ~of_json:state_of_json)
    ~event:(Silica_jsonkit.codec ~to_json:event_to_json ~of_json:event_of_json)
    ();
  machine

let button label event =
  Node.element "button" [ Attr.on_press event ] [ Node.text label ]

let view = function
  | Editing count ->
      Node.element "form" []
        [
          Node.element "p" [] [ Node.text (string_of_int count ^ " edits") ];
          button "Edit" Edit;
          button "Save" Save;
        ]
  | Saved ->
      Node.element "div" []
        [ Node.element "p" [] [ Node.text "Saved" ]; button "Save" Save ]

let component = Component.make (machine ~fixed:false) view
let fixed = Component.make (machine ~fixed:true) view

let disabled =
  Component.make (machine ~fixed:false) (function
    | Editing _ as state ->
        view state
    | Saved ->
        Node.element "div" []
          [
            Node.element "p" [] [ Node.text "Saved" ];
            Node.element "button"
              [ Attr.on_press Save; Attr.bool "disabled" true ]
              [ Node.text "Save" ];
          ]
    )
let check = Check.component component
