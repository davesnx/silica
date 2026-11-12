let () =
  let report = Check.run Joint_cases.bumped in
  match report.violations with
  | violation :: _ -> (
      let trace =
        Check.export Joint_cases.bumped ~rule:violation.rule
          violation.counterexample
      in
      match
        Check.script ~check:"Joint_cases.bumped" Joint_cases.bumped trace
      with
      | Ok text ->
          print_string text
      | Error message ->
          prerr_endline message;
          exit 1
    )
  | [] ->
      prerr_endline "the joint fixture has no violation";
      exit 1
