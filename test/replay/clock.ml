open Silica

type event = Start | Short | Long | Again [@@deriving json]

let event_name = function
  | Start ->
      "start"
  | Short ->
      "short"
  | Long ->
      "long"
  | Again ->
      "again"

type state = Idle | Waiting | Ready | Timed_out [@@deriving json]

let name = function
  | Idle ->
      "idle"
  | Waiting ->
      "waiting"
  | Ready ->
      "ready"
  | Timed_out ->
      "timed_out"

let states = [ "idle"; "waiting"; "ready"; "timed_out" ]

let machine =
  let machine =
    Machine.make ~id:"clock" ~init:Idle ~name ~states ~event_name
      ~during:(function
        | Waiting ->
            [ Host.after 300 Short; Host.after 2000 Long ]
        | Idle | Ready | Timed_out ->
            []
        )
      (fun state event ->
        match (state, event) with
        | Idle, Start ->
            Machine.Go Waiting
        | Waiting, Short ->
            Machine.Go Ready
        | Waiting, Long ->
            Machine.Go Timed_out
        | Timed_out, Again ->
            Machine.Go Idle
        | _ ->
            Machine.Ignore
      )
  in
  Machine.register_codecs machine
    ~state:(Silica_jsonkit.codec ~to_json:state_to_json ~of_json:state_of_json)
    ~event:(Silica_jsonkit.codec ~to_json:event_to_json ~of_json:event_of_json)
    ();
  machine

let component =
  Component.make machine (function
    | Idle ->
        Node.element "button" [ Attr.on_press Start ] [ Node.text "Start" ]
    | Waiting ->
        Node.element "p" [] [ Node.text "Waiting" ]
    | Ready ->
        Node.element "div" []
          [
            Node.element "p" [] [ Node.text "Ready" ];
            Node.element "button" [ Attr.on_press Again ] [ Node.text "Again" ];
          ]
    | Timed_out ->
        Node.element "div" []
          [
            Node.element "p" [] [ Node.text "Too late" ];
            Node.element "button" [ Attr.on_press Again ] [];
          ]
    )

let check = Check.component component
