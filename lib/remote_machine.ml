type ('i, 'o) state = { input : 'i option; status : 'o Status.t }

type ('i, 'o) event =
  | Load of 'i
  | Retry
  | Settled of ('o, Remote.error) result
  | Reset

let make (type i o) ?timeout_ms ~id (endpoint : (i, o) Remote.endpoint) :
    ((i, o) state, (i, o) event) Machine.t =
  let module Chart = struct
    type _ t =
      | Idle : unit t
      | Loading : (i * o option) t
      | Ready : (i * o) t
      | Failed : (i * Remote.error * o option) t
  end in
  let module M = Machine.Make (Chart) in
  let machine =
    M.make ~id ~init:(M.at Idle ())
      [
        M.always ~to_:[ Loading; Idle ] (function
          | Load input ->
              M.reenter Loading (input, None)
          | Reset ->
              M.goto Idle ()
          | Retry | Settled _ ->
              M.unhandled
          );
        M.state Idle "idle" M.ignore;
        M.state Loading "loading" ~to_:[ Ready; Failed ]
          ~invoke:
            [
              Source.call ?timeout_ms endpoint fst (fun result -> Settled result);
            ]
          (fun (input, last) -> function
            | Settled (Ok output) ->
                M.goto Ready (input, output)
            | Settled (Error error) ->
                M.goto Failed (input, error, last)
            | Load next ->
                M.reenter Loading (next, last)
            | Retry | Reset ->
                M.unhandled
            );
        M.state Ready "ready" ~to_:[ Loading ] (fun (_, output) -> function
          | Load next ->
              M.reenter Loading (next, Some output)
          | Retry | Settled _ | Reset ->
              M.unhandled
          );
        M.state Failed "failed" ~to_:[ Loading ]
          (fun (input, _, last) -> function
          | Retry ->
              M.reenter Loading (input, last)
          | Load next ->
              M.reenter Loading (next, last)
          | Settled _ | Reset ->
              M.unhandled
          );
      ]
  in
  Machine.map_snapshot machine
    ~get:(fun (snapshot : M.snapshot) : (i, o) state ->
      match snapshot with
      | M.At (Chart.Idle, ()) ->
          { input = None; status = Status.Idle }
      | M.At (Chart.Loading, (input, last)) ->
          { input = Some input; status = Status.Loading last }
      | M.At (Chart.Ready, (input, data)) ->
          { input = Some input; status = Status.Ready { data; stale = false } }
      | M.At (Chart.Failed, (input, error, last)) ->
          { input = Some input; status = Status.Failed { error; last } }
    )
    ~set:(fun (state : (i, o) state) : M.snapshot ->
      match (state.input, state.status) with
      | None, _ | _, Status.Idle ->
          M.at Chart.Idle ()
      | Some input, Status.Loading last ->
          M.at Chart.Loading (input, last)
      | Some input, Status.Ready { data; _ } ->
          M.at Chart.Ready (input, data)
      | Some input, Status.Failed { error; last } ->
          M.at Chart.Failed (input, error, last)
    )
