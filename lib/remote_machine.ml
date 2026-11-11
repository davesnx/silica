type ('i, 'o, 'x) state =
  | Idle
  | Loading of { input : 'i; last : 'o option }
  | Ready of { input : 'i; data : 'o }
  | Rejected of { input : 'i; reason : 'x; last : 'o option }
  | Failed of { input : 'i; error : Remote.error; last : 'o option }

type ('i, 'o, 'x) event =
  | Load of 'i
  | Retry
  | Settled of ('o, 'x) Remote.answer
  | Reset

let data = function
  | Idle ->
      None
  | Ready { data; _ } ->
      Some data
  | Loading { last; _ } | Rejected { last; _ } | Failed { last; _ } ->
      last

let name = function
  | Idle ->
      "idle"
  | Loading _ ->
      "loading"
  | Ready _ ->
      "ready"
  | Rejected _ ->
      "rejected"
  | Failed _ ->
      "failed"

let make ?timeout_ms ~id endpoint =
  Machine.make ~id ~init:Idle
    ~shape:
      (Machine.shape ~name
         ~names:[ "idle"; "loading"; "ready"; "rejected"; "failed" ]
         ()
      )
    ~owns:(function
      | Loading { input; _ } ->
          [
            Remote.call ?timeout_ms endpoint input (fun answer -> Settled answer);
          ]
      | Idle | Ready _ | Rejected _ | Failed _ ->
          []
      )
    (fun state event ->
      match (state, event) with
      | _, Load input ->
          Machine.Reenter (Loading { input; last = data state })
      | _, Reset ->
          Machine.Go Idle
      | (Rejected { input; last; _ } | Failed { input; last; _ }), Retry ->
          Machine.Reenter (Loading { input; last })
      | Loading { input; _ }, Settled (Remote.Ok data) ->
          Machine.Go (Ready { input; data })
      | Loading { input; last }, Settled (Remote.Rejected reason) ->
          Machine.Go (Rejected { input; reason; last })
      | Loading { input; last }, Settled (Remote.Failed error) ->
          Machine.Go (Failed { input; error; last })
      | (Idle | Ready _ | Loading _), Retry
      | (Idle | Ready _ | Rejected _ | Failed _), Settled _ ->
          Machine.Ignore
    )
