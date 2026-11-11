open Silica

let machine ~id ~init ?final step =
  let is_final data =
    match final with Some final -> final data | None -> false
  in
  let name data =
    if is_final data then
      "final"
    else
      "active"
  in
  let states =
    match final with None -> [ "active" ] | Some _ -> [ "active"; "final" ]
  in
  Machine.make ~id ~init ~shape:(Machine.shape ~name ~names:states ())
    ~final:is_final (fun data event ->
      match step data event with
      | None ->
          Machine.Ignore
      | Some (data, []) ->
          Machine.Go data
      | Some (data, actions) ->
          Machine.Go_with (data, actions)
  )
