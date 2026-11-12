type check = Check : ('s, 'ev) Check.t -> check

let checks =
  [
    ("save", Check Save.check);
    ("nameless", Check Nameless.check);
    ("crash", Check Crash.check);
    ("upload", Check Upload.check);
    ("clock", Check Clock.check);
    ("labelled", Check Labelled.check);
    ("lost", Check Lost.check);
    ("sync", Check Sync.check);
    ("pages", Check Pages.check);
    ("search", Check Search.check);
    ("typing", Check Stepper.typing);
  ]

let () =
  match Array.to_list Sys.argv with
  | _ :: name :: rule :: component -> (
      let (Check check) = List.assoc name checks in
      let report = Check.run check in
      let violation =
        List.find
          (fun (violation : (_, _) Check.violation) -> violation.rule = rule)
          report.violations
      in
      let trace =
        Check.export check ~rule:violation.rule violation.counterexample
      in
      match Check.script ?component:(List.nth_opt component 0) check trace with
      | Ok text ->
          print_string text
      | Error message ->
          prerr_endline message;
          exit 1
    )
  | _ ->
      prerr_endline "usage: gen_script.exe CHECK RULE [COMPONENT]";
      exit 2
