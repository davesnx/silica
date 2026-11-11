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

let export name check =
  let report = Check.run check in
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
  let open Ui_checks in
  export "dialog" (dialog ());
  export "popover" (popover ());
  export "tooltip" (tooltip ());
  export "menu" (menu ());
  export "select" (select ());
  export "combobox" (combobox ());
  export "command menu" (command_menu ());
  export "checkbox" (checkbox ());
  export "switch" (switch ());
  export "theme" (theme ());
  export "tabs" (tabs ());
  export "toast" (toast ())
