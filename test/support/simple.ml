open Silica

let machine (type s e) ~id ~(init : s) ?final
    (step : s -> e -> (s * e Cmd.t list) option) : (s, e) Machine.t =
  let module Phase = struct
    type _ t = Active : s t | Final : s t
  end in
  let module M = Machine.Make (Phase) in
  let is_final data =
    match final with Some final -> final data | None -> false
  in
  let at data =
    if is_final data then
      M.at Phase.Final data
    else
      M.at Phase.Active data
  in
  let handle data event =
    match step data event with
    | None ->
        M.unhandled
    | Some (data, cmds) ->
        if is_final data then
          M.goto ~cmds Phase.Final data
        else
          M.goto ~cmds Phase.Active data
  in
  let table =
    M.make ~id ~init:(at init)
      ( match final with
      | None ->
          [ M.state Phase.Active "active" handle ]
      | Some _ ->
          [
            M.state Phase.Active "active" ~to_:[ Phase.Final ] handle;
            M.state Phase.Final "final" ~final:true M.ignore;
          ]
      )
  in
  Machine.map_snapshot table
    ~get:(fun (M.At (phase, data) : M.snapshot) : s ->
      match phase with Phase.Active -> data | Phase.Final -> data
    )
    ~set:at
