open Silica
open Jsonkit.Primitives

type event = Save | Done | Rejected_with of string | Failed_call | Retry
[@@deriving json]

let event_name = function
  | Save ->
      "save"
  | Done ->
      "done"
  | Rejected_with _ ->
      "rejected_with"
  | Failed_call ->
      "failed_call"
  | Retry ->
      "retry"

type state = Idle | Saving | Saved | Failed of string [@@deriving json]

let name = function
  | Idle ->
      "idle"
  | Saving ->
      "saving"
  | Saved ->
      "saved"
  | Failed _ ->
      "failed"

let states = [ "idle"; "saving"; "saved"; "failed" ]

let api =
  Remote.declare ~path:"/api/save" ~input:Codec.int ~output:Codec.unit
    ~rejected:Codec.string

let make_machine ~shorten =
  let machine =
    Machine.make ~id:"upload" ~init:Idle ~name ~states ~event_name
      ~during:(function
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
            Machine.Go
              (Failed
                 ( if shorten then
                     String.sub reason 0 (min 4 (String.length reason))
                   else
                     reason
                 )
              )
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

let machine = make_machine ~shorten:false
let shortening = make_machine ~shorten:true

let view ~loud = function
  | Idle ->
      button "Save" Save
  | Saving ->
      Node.element "p" [] [ Node.text "Saving" ]
  | Saved ->
      Node.element "div" []
        [ Node.element "p" [] [ Node.text "Saved" ]; button "Save" Save ]
  | Failed reason ->
      Node.element "div" []
        [
          Node.element
            ( if loud then
                "strong"
              else
                "p"
            )
            []
            [
              Node.text
                ( if loud then
                    String.uppercase_ascii reason
                  else
                    reason
                );
            ];
          button "Retry" Retry;
        ]

let component = Component.make machine (view ~loud:false)
let restyled = Component.make machine (view ~loud:true)
let fixed = Component.make shortening (view ~loud:false)

let check_of component =
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

let check = check_of component
let restyled_check = check_of restyled
let fixed_check = check_of fixed
