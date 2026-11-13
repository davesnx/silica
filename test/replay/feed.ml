open Silica

type event = Update of int Topic.update
type state = { score : int option; offline : bool }

let topic = Topic.declare ~name:"score" ~state:Codec.int

let event_name = function
  | Update { status = Idle | Loading _; _ } ->
      "connecting"
  | Update { status = Ready { stale = false; _ }; _ } ->
      "published"
  | Update { status = Ready { stale = true; _ }; _ } ->
      "offline"
  | Update { status = Failed _; _ } ->
      "lost"

let make ~clears_offline ~keeps_last =
  Machine.make ~id:"feed"
    ~init:{ score = None; offline = false }
    ~name:(fun _ -> "watching")
    ~states:[ "watching" ] ~event_name
    ~during:(fun _ ->
      [ Topic.subscribe topic "main" (fun update -> Update update) ]
    )
    (fun state (Update { status; _ }) ->
      match status with
      | Idle | Loading _ ->
          Machine.Go state
      | Ready { data; stale = false } ->
          Machine.Go
            {
              score = Some data;
              offline =
                ( if clears_offline then
                    false
                  else
                    state.offline
                );
            }
      | Ready { data; stale = true } ->
          Machine.Go { score = Some data; offline = true }
      | Failed { last; _ } ->
          Machine.Go
            {
              score =
                ( if keeps_last then
                    last
                  else
                    None
                );
              offline = true;
            }
    )

let view state =
  Node.element "main" []
    [
      Node.element "p" []
        [
          Node.text
            ( match state.score with
            | Some score ->
                "Score " ^ string_of_int score
            | None ->
                "No score"
            );
        ];
      ( if state.offline then
          Node.element "p" [] [ Node.text "Offline" ]
        else
          Node.list []
      );
    ]

let component =
  Component.make (make ~clears_offline:false ~keeps_last:false) view

let fixed = Component.make (make ~clears_offline:true ~keeps_last:true) view

let make_check component =
  Check.component
    ~allow:
      [
        Check.allow ~rule:"dead-end" ~state:"watching"
          ~reason:"After a fatal failure the feed has nothing more to do." ();
      ]
    ~samples:[ Check.states topic [ 3 ] ]
    ~properties:
      [
        Check.always ~name:"a new score is not offline"
          Check.(
            not_
              (event (function
                | Update { status = Ready { stale = false; _ }; _ } ->
                    true
                | Update _ ->
                    false
                ))
            ||| state (fun state -> not state.offline)
          );
        Check.always ~name:"a lost feed keeps the last score"
          Check.(
            not_
              (event (function
                | Update { status = Failed { last = Some _; _ }; _ } ->
                    true
                | Update _ ->
                    false
                ))
            ||| state (fun state -> state.score <> None)
          );
      ]
    component

let check = make_check component
let fixed_check = make_check fixed
