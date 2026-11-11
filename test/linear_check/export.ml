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

let passed name report =
  if not (Check.passed report) then
    failwith
      (Format.asprintf "the check of %s did not pass:@.%a" name Check.pp_report
         report
      )

let export name (report : _ Check.report) =
  passed name report;
  let spec = List.hd report.specs in
  write (spec.Spec.id ^ ".qnt") (Spec.to_quint spec);
  write (spec.Spec.id ^ ".dot") (Spec.to_dot spec);
  round_trip spec;
  write (spec.Spec.id ^ ".txt") (Spec.to_text spec);
  write (spec.Spec.id ^ ".mmd") (Spec.to_mermaid spec);
  write (spec.Spec.id ^ ".layout")
    (Statechart_layout.to_string (Statechart_layout.layout spec))

let children_of name (report : _ Check.report) =
  passed name report;
  report.children

let () =
  let open Linear_checks in
  let router_report = router () in
  let router_children = children_of "router" router_report in
  export "router" router_report;
  export "issue-list" (list ());
  export "issue-detail"
    (detail ~props:(Check.props router_children Linear_detail.component) ());
  export "unsaved" (unsaved ());
  let create_report =
    create ~props:(Check.props router_children Linear_create.component) ()
  in
  export "create-issue" create_report;
  export "create-issue-dialog"
    (create_dialog
       ~props:
         (Check.props
            (children_of "create-issue" create_report)
            Linear_create.dialog_component.component
         )
       ()
    );
  export "me" (me ());
  export "comments" (comments ());
  export "comment-composer" (composer ());
  export "comment" (comment ())
