open Silica

type event = Load | Got of string
type state = Idle | Loading | Result of string

let endpoint =
  Remote.declare ~path:"/api/load" ~input:Codec.unit ~output:Codec.int
    ~rejected:Codec.string

let kind = function
  | Remote.Ok _ ->
      "ok"
  | Rejected reason ->
      "rejected_" ^ reason
  | Failed (Http _) ->
      "http"
  | Failed (Network _) ->
      "network"
  | Failed (Decode _) ->
      "decode"

let kinds =
  [ "ok"; "rejected_busy"; "rejected_full"; "http"; "network"; "decode" ]

let machine =
  Machine.make ~id:"answers" ~init:Idle
    ~shape:
      (Machine.shape
         ~name:(function
           | Idle -> "idle" | Loading -> "loading" | Result kind -> kind
           )
         ~names:([ "idle"; "loading" ] @ kinds)
         ()
      )
    ~events:
      (Machine.shape ~name:(function Load -> "load" | Got kind -> kind) ())
    ~owns:(function
      | Loading ->
          [ Remote.call endpoint () (fun answer -> Got (kind answer)) ]
      | Idle | Result _ ->
          []
      )
    (fun state event ->
      match (state, event) with
      | (Idle | Result _), Load ->
          Machine.Go Loading
      | Loading, Got kind ->
          Machine.Go (Result kind)
      | _ ->
          Machine.Ignore
    )

let button label event =
  Node.element "button" [ Attr.on_press event ] [ Node.text label ]

let view ~retry = function
  | Idle ->
      button "Load" Load
  | Loading ->
      Node.element "p" [] [ Node.text "Loading" ]
  | Result kind ->
      Node.element "div" []
        (Node.element "p" [] [ Node.text kind ]
        ::
        ( if retry then
            [ button "Retry" Load ]
          else
            []
        )
        )

let component = Component.make machine (view ~retry:false)
let fixed = Component.make machine (view ~retry:true)

let check_of component =
  Check.component
    ~allow:
      [ Check.allow ~rule:"dead-end" ~reason:"The bug leaves no retry." () ]
    ~samples:
      [ Check.answers ~decode:true ~rejected:[ "busy"; "full" ] endpoint [ 1 ] ]
    ~properties:
      [
        Check.always ~name:"a result offers a retry"
          Check.(
            not_ (state (function Result _ -> true | Idle | Loading -> false))
            ||| shows "Retry"
          );
      ]
    component

let check = check_of component
let fixed_check = check_of fixed
