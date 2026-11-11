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
  let module Chart_machine = Machine.Make (Chart) in
  let machine =
    Chart_machine.make ~id ~init:(Chart_machine.at Idle ())
      [
        Chart_machine.always ~to_:[ Loading; Idle ] (function
          | Load input ->
              Chart_machine.reenter Loading (input, None)
          | Reset ->
              Chart_machine.goto Idle ()
          | Retry | Settled _ ->
              Chart_machine.unhandled
          );
        Chart_machine.state Idle "idle" Chart_machine.ignore;
        Chart_machine.state Loading "loading" ~to_:[ Ready; Failed ]
          ~invoke:
            [
              Source.call ?timeout_ms endpoint fst (fun result -> Settled result);
            ]
          (fun (input, last) -> function
            | Settled (Ok output) ->
                Chart_machine.goto Ready (input, output)
            | Settled (Error error) ->
                Chart_machine.goto Failed (input, error, last)
            | Load next ->
                Chart_machine.reenter Loading (next, last)
            | Retry | Reset ->
                Chart_machine.unhandled
            );
        Chart_machine.state Ready "ready" ~to_:[ Loading ]
          (fun (_, output) -> function
          | Load next ->
              Chart_machine.reenter Loading (next, Some output)
          | Retry | Settled _ | Reset ->
              Chart_machine.unhandled
          );
        Chart_machine.state Failed "failed" ~to_:[ Loading ]
          (fun (input, _, last) -> function
          | Retry ->
              Chart_machine.reenter Loading (input, last)
          | Load next ->
              Chart_machine.reenter Loading (next, last)
          | Settled _ | Reset ->
              Chart_machine.unhandled
          );
      ]
  in
  Machine.map_snapshot machine
    ~get:(fun (snapshot : Chart_machine.snapshot) : (i, o) state ->
      match snapshot with
      | Chart_machine.At (Chart.Idle, ()) ->
          { input = None; status = Status.Idle }
      | Chart_machine.At (Chart.Loading, (input, last)) ->
          { input = Some input; status = Status.Loading last }
      | Chart_machine.At (Chart.Ready, (input, data)) ->
          { input = Some input; status = Status.Ready { data; stale = false } }
      | Chart_machine.At (Chart.Failed, (input, error, last)) ->
          { input = Some input; status = Status.Failed { error; last } }
    )
    ~set:(fun (state : (i, o) state) : Chart_machine.snapshot ->
      match (state.input, state.status) with
      | None, _ | _, Status.Idle ->
          Chart_machine.at Chart.Idle ()
      | Some input, Status.Loading last ->
          Chart_machine.at Chart.Loading (input, last)
      | Some input, Status.Ready { data; _ } ->
          Chart_machine.at Chart.Ready (input, data)
      | Some input, Status.Failed { error; last } ->
          Chart_machine.at Chart.Failed (input, error, last)
    )
