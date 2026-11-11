open Silica
open Jsonkit.Primitives

type event = Save | Done | Rejected_with of string | Failed_call | Retry
[@@deriving json, event_shape]

type state = Idle | Saving | Saved | Failed of string
[@@deriving json, state_shape]

let api =
  Remote.declare ~path:"/api/save" ~input:Codec.int ~output:Codec.unit
    ~rejected:Codec.string

let machine =
  let machine =
    Machine.make ~id:"upload" ~init:Idle ~shape:state_shape ~events:event_shape
      ~owns:(function
        | Saving ->
            [
              Remote.call api 1 (function
                | Remote.Ok () ->
                    Done
                | Rejected reason ->
                    Rejected_with reason
                | Failed _ ->
                    Failed_call
                );
            ]
        | Idle | Saved | Failed _ ->
            []
        )
      (fun state event ->
        match (state, event) with
        | (Idle | Saved), Save ->
            Machine.Go Saving
        | Saving, Done ->
            Machine.Go Saved
        | Saving, Rejected_with reason ->
            Machine.Go (Failed reason)
        | Saving, Failed_call ->
            Machine.Go (Failed "down")
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

let component =
  Component.make machine (function
    | Idle ->
        button "Save" Save
    | Saving ->
        Node.element "p" [] [ Node.text "Saving" ]
    | Saved ->
        Node.element "div" []
          [ Node.element "p" [] [ Node.text "Saved" ]; button "Save" Save ]
    | Failed reason ->
        Node.element "div" []
          [ Node.element "p" [] [ Node.text reason ]; button "Retry" Retry ]
    )

let check =
  Check.component
    ~samples:[ Check.answers api ~rejected:[ "too many" ] [ () ] ]
    ~properties:
      [
        Check.always ~name:"short-error"
          ~hint:"Keep the error under five letters."
          (Check.state (function
            | Failed reason ->
                String.length reason < 5
            | Idle | Saving | Saved ->
                true
            ));
      ]
    component
