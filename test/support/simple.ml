open Silica

let machine (type s e) ~id ~(init : s) ?final
    (step : s -> e -> (s * e Action.t list) option) : (s, e) Machine.t =
  let module Phase = struct
    type _ t = Active : s t | Final : s t
  end in
  let module Phase_machine = Machine.Make (Phase) in
  let is_final data =
    match final with Some final -> final data | None -> false
  in
  let at data =
    if is_final data then
      Phase_machine.at Phase.Final data
    else
      Phase_machine.at Phase.Active data
  in
  let handle data event =
    match step data event with
    | None ->
        Phase_machine.unhandled
    | Some (data, actions) ->
        if is_final data then
          Phase_machine.goto ~actions Phase.Final data
        else
          Phase_machine.goto ~actions Phase.Active data
  in
  let table =
    Phase_machine.make ~id ~init:(at init)
      ( match final with
      | None ->
          [ Phase_machine.state Phase.Active "active" handle ]
      | Some _ ->
          [
            Phase_machine.state Phase.Active "active" ~to_:[ Phase.Final ]
              handle;
            Phase_machine.state Phase.Final "final" ~final:true
              Phase_machine.ignore;
          ]
      )
  in
  Machine.map_snapshot table
    ~get:(fun (Phase_machine.At (phase, data) : Phase_machine.snapshot) : s ->
      match phase with Phase.Active -> data | Phase.Final -> data
    )
    ~set:at
