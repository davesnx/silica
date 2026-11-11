open Silica

let write name text =
  Out_channel.with_open_text name (fun channel ->
      Out_channel.output_string channel text
  )

let round_trip spec =
  if Spec.of_text (Spec.to_text spec) <> Ok spec then
    failwith ("the text of " ^ spec.Spec.id ^ " does not read back");
  let json = Silica_json.Json.to_string (Spec.to_json spec) in
  if Result.bind (Silica_json.Json.of_string json) Spec.of_json <> Ok spec then
    failwith ("the JSON of " ^ spec.Spec.id ^ " does not read back")

let export name (report : (_, _) Check.report) =
  if not (Check.passed report) then
    failwith
      (Format.asprintf "the check of %s did not pass:@\n%a" name Check.pp_report
         report
      );
  let spec = List.hd report.specs in
  write (spec.Spec.id ^ ".qnt") (Spec.to_quint spec);
  write (spec.Spec.id ^ ".dot") (Spec.to_dot spec);
  round_trip spec;
  write (spec.Spec.id ^ ".txt") (Spec.to_text spec);
  write (spec.Spec.id ^ ".mmd") (Spec.to_mermaid spec);
  write (spec.Spec.id ^ ".layout")
    (Statechart_layout.to_string (Statechart_layout.layout spec))

let () =
  let open Example_checks in
  let router_report = Check.run (router ()) in
  let pages = children router_report in
  export "router" router_report;
  export "home" (Check.run (home ()));
  export "todo" (Check.run (todo ()));
  export "item"
    (Check.run (item ~props:(Check.props pages Example_item.component) ()));
  export "about" (Check.run (about ()));
  export "not_found" (Check.run (not_found ()));
  export "votes" (Check.run (votes ()));
  export "vote" (Check.run (vote ()));
  export "counter" (Check.run (counter_bounded ()))
