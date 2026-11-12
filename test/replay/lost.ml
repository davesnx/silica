open Silica

type event = Save | Answer of (int, string) Remote.answer
type state = Editing | Saving | Saved | Failed

let name = function
  | Editing ->
      "editing"
  | Saving ->
      "saving"
  | Saved ->
      "saved"
  | Failed ->
      "failed"

let states = [ "editing"; "saving"; "saved"; "failed" ]

let endpoint =
  Remote.declare ~path:"/api/save" ~input:Codec.unit ~output:Codec.int
    ~rejected:Codec.string

let event_name = function
  | Save ->
      "save"
  | Answer (Ok _) ->
      "answer ok"
  | Answer _ ->
      "answer failed"

let machine =
  Machine.make ~id:"lost" ~init:Editing ~name ~states ~event_name
    ~final:(function Saved -> true | Editing | Saving | Failed -> false)
    ~during:(function
      | Saving ->
          [ Remote.call endpoint () (fun answer -> Answer answer) ]
      | Editing | Saved | Failed ->
          []
      )
    (fun state event ->
      match (state, event) with
      | (Editing | Failed), Save ->
          Machine.Go Saving
      | Saving, Answer (Ok _) ->
          Machine.Stay []
      | Saving, Answer _ ->
          Machine.Go Failed
      | _ ->
          Machine.Ignore
    )

let component =
  Component.make machine (function
    | Editing | Failed ->
        Node.element "button" [ Attr.on_press Save ] [ Node.text "Save" ]
    | Saving ->
        Node.text "saving"
    | Saved ->
        Node.text "saved"
    )

let check =
  Check.component
    ~allow:
      [
        Check.allow ~rule:"unreached-state" ~state:"saved"
          ~reason:"The lost answer never reaches Saved." ();
      ]
    ~samples:[ Check.answers endpoint [ 1 ] ]
    ~properties:
      [
        Check.leads_to ~name:"save leads to saved or failed"
          ~from:(Check.event (( = ) Save))
          ~to_:Check.(state_named "saved" ||| state_named "failed")
          ();
      ]
    component
