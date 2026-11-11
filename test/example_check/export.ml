open Silica

let write name text =
  Out_channel.with_open_text name (fun channel ->
      Out_channel.output_string channel text
  )

let round_trip spec labels =
  let expected = Ok (spec, labels) in
  if Spec.of_text (Spec.to_text ~labels spec) <> expected then
    failwith ("the text of " ^ spec.Spec.id ^ " does not read back");
  let json = Silica_json.Json.to_string (Spec.to_json ~labels spec) in
  if Result.bind (Silica_json.Json.of_string json) Spec.of_json <> expected then
    failwith ("the JSON of " ^ spec.Spec.id ^ " does not read back")

let export name = function
  | Check.Ok { spec; edges; _ } ->
      write (spec.Spec.id ^ ".qnt") (Spec.to_quint ~labels:edges spec);
      write (spec.Spec.id ^ ".dot") (Spec.to_dot ~labels:edges spec);
      round_trip spec edges;
      write (spec.Spec.id ^ ".txt") (Spec.to_text ~labels:edges spec);
      write (spec.Spec.id ^ ".mmd") (Spec.to_mermaid ~labels:edges spec);
      write (spec.Spec.id ^ ".layout")
        (Statechart_layout.to_string
           (Statechart_layout.layout ~labels:edges spec)
        )
  | _ ->
      failwith ("the check of " ^ name ^ " did not pass")

let () =
  let open Example_checks in
  let router_report = router () in
  let pages = children router_report in
  export "router" router_report;
  export "home" (home ());
  export "todo" (todo ());
  export "item" (item ~props:(Check.props pages Example_item.component) ());
  export "about" (about ());
  export "not_found" (not_found ());
  export "votes" (votes ());
  export "vote" (vote ());
  export "counter" (counter_bounded ())
