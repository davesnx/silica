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

let event_name = function
  | Load _ ->
      "load"
  | Retry ->
      "retry"
  | Settled (Remote.Ok _) ->
      "settled ok"
  | Settled (Remote.Rejected _) ->
      "settled rejected"
  | Settled (Remote.Failed _) ->
      "settled error"
  | Reset ->
      "reset"

let ( let* ) = Result.bind
let unknown name = Error ("unknown constructor " ^ name)
let fields pairs = Silica_json.Json.declassify (`Assoc pairs)

let event_codec endpoint =
  let input = Remote.input endpoint
  and answer =
    Remote.answer_codec (Remote.output endpoint) (Remote.rejected endpoint)
  in
  Codec.make
    ~encode:(function
      | Load value ->
          Codec.constructor "Load" [ input.encode value ]
      | Retry ->
          Codec.constructor "Retry" []
      | Settled value ->
          Codec.constructor "Settled" [ answer.encode value ]
      | Reset ->
          Codec.constructor "Reset" []
      )
    ~decode:(fun json ->
      let* name, arguments = Codec.constructor_of json in
      match (name, arguments) with
      | "Load", [ value ] ->
          Result.map (fun value -> Load value) (input.decode value)
      | "Retry", [] ->
          Ok Retry
      | "Settled", [ value ] ->
          Result.map (fun value -> Settled value) (answer.decode value)
      | "Reset", [] ->
          Ok Reset
      | name, _ ->
          unknown name
    )

let state_codec endpoint =
  let input = Remote.input endpoint
  and output = Remote.output endpoint
  and rejected = Remote.rejected endpoint in
  let last = Codec.option output in
  Codec.make
    ~encode:(function
      | Idle ->
          Codec.constructor "Idle" []
      | Loading { input = value; last = old } ->
          Codec.constructor "Loading"
            [
              fields
                [ ("input", input.encode value); ("last", last.encode old) ];
            ]
      | Ready { input = value; data } ->
          Codec.constructor "Ready"
            [
              fields
                [ ("input", input.encode value); ("data", output.encode data) ];
            ]
      | Rejected { input = value; reason; last = old } ->
          Codec.constructor "Rejected"
            [
              fields
                [
                  ("input", input.encode value);
                  ("reason", rejected.encode reason);
                  ("last", last.encode old);
                ];
            ]
      | Failed { input = value; error; last = old } ->
          Codec.constructor "Failed"
            [
              fields
                [
                  ("input", input.encode value);
                  ("error", Remote.error_codec.encode error);
                  ("last", last.encode old);
                ];
            ]
      )
    ~decode:(fun json ->
      let* name, arguments = Codec.constructor_of json in
      match (name, arguments) with
      | "Idle", [] ->
          Ok Idle
      | "Loading", [ record ] ->
          let* value = Codec.field "input" input record in
          let* old = Codec.field "last" last record in
          Ok (Loading { input = value; last = old })
      | "Ready", [ record ] ->
          let* value = Codec.field "input" input record in
          let* data = Codec.field "data" output record in
          Ok (Ready { input = value; data })
      | "Rejected", [ record ] ->
          let* value = Codec.field "input" input record in
          let* reason = Codec.field "reason" rejected record in
          let* old = Codec.field "last" last record in
          Ok (Rejected { input = value; reason; last = old })
      | "Failed", [ record ] ->
          let* value = Codec.field "input" input record in
          let* error = Codec.field "error" Remote.error_codec record in
          let* old = Codec.field "last" last record in
          Ok (Failed { input = value; error; last = old })
      | name, _ ->
          unknown name
    )

let make ?timeout_ms ~id endpoint =
  Machine.make ~id ~init:Idle
    ~shape:
      (Machine.shape ~name
         ~names:[ "idle"; "loading"; "ready"; "rejected"; "failed" ]
         ()
      )
    ~events:(Machine.shape ~name:event_name ())
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
