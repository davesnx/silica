open Silica

let element = Node.element
let text = Node.text
let id value = Attr.string "id" value

type state = Idle | Running | Finished of string
type event = Start | Answered of (string, string) Remote.answer

let endpoint =
  Remote.declare ~path:"/api/job" ~input:Codec.int ~output:Codec.string
    ~rejected:Codec.string

let name = function
  | Idle ->
      "idle"
  | Running ->
      "running"
  | Finished _ ->
      "finished"

let machine =
  Machine.make ~id:"job" ~init:Idle
    ~shape:(Machine.shape ~name ~names:[ "idle"; "running"; "finished" ] ())
    ~events:
      (Machine.shape
         ~name:(function Start -> "start" | Answered _ -> "answered")
         ()
      )
    ~owns:(function
      | Running ->
          [ Remote.call endpoint 1 (fun answer -> Answered answer) ]
      | Idle | Finished _ ->
          []
      )
    (fun state event ->
      match (state, event) with
      | Idle, Start ->
          Go Running
      | Running, Answered (Ok value) | Running, Answered (Rejected value) ->
          Go (Finished value)
      | Running, Answered (Failed _) ->
          Go (Finished "failed")
      | _ ->
          Ignore
    )

let view state =
  element "div"
    [ id "job" ]
    [
      element "p"
        [ id "job-state" ]
        [
          text
            ( match state with
            | Finished value ->
                "finished " ^ value
            | Idle | Running ->
                name state
            );
        ];
      ( match state with
      | Idle ->
          element "button"
            [ id "job-start"; Attr.on_press Start ]
            [ text "start job" ]
      | Running | Finished _ ->
          Node.list []
      );
    ]

let component = Component.make machine view
