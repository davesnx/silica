type check = Check : ('s, 'ev) Check.t * string -> check

let checks =
  [
    ("save", Check (Save.check, "Save.check"));
    ("nameless", Check (Nameless.check, "Nameless.check"));
    ("crash", Check (Crash.check, "Crash.check"));
    ("upload", Check (Upload.check, "Upload.check"));
    ("clock", Check (Clock.check, "Clock.check"));
    ("labelled", Check (Labelled.check, "Labelled.check"));
    ("lost", Check (Lost.check, "Lost.check"));
    ("sync", Check (Sync.check, "Sync.check"));
    ("pages", Check (Pages.check, "Pages.check"));
    ("search", Check (Search.check, "Search.check"));
    ("typing", Check (Stepper.typing, "Stepper.typing"));
    ("secret", Check (Stepper.secret_typing, "Stepper.secret_typing"));
    ("badge", Check (Badge.check, "Badge.check"));
    ("answers", Check (Answer_kinds.check, "Answer_kinds.check"));
    ("gestures", Check (Gestures.check, "Gestures.check"));
    ("changing", Check (Rows.changing_check, "Rows.changing_check"));
    ("numbered", Check (Rows.numbered_check, "Rows.numbered_check"));
    ("titled", Check (Rows.titled_check, "Rows.titled_check"));
    ("feed", Check (Feed.check, "Feed.check"));
  ]

let () =
  match Array.to_list Sys.argv with
  | _ :: name :: rule :: rest -> (
      let component = List.nth_opt rest 0 in
      let expression_override =
        Option.value (List.nth_opt rest 1) ~default:"" |> function
        | "" ->
            None
        | expression ->
            Some expression
      in
      let (Check (check, expression)) = List.assoc name checks in
      let override = expression_override in
      let report = Check.run check in
      let rule, state =
        match String.index_opt rule '@' with
        | Some at ->
            ( String.sub rule 0 at,
              Some (String.sub rule (at + 1) (String.length rule - at - 1))
            )
        | None ->
            (rule, None)
      in
      let violation =
        List.find
          (fun (violation : (_, _) Check.violation) ->
            violation.rule = rule
            &&
            match state with
            | Some state ->
                violation.state_name = state
            | None ->
                true
          )
          report.violations
      in
      let trace =
        Check.export check ~rule:violation.rule violation.counterexample
      in
      match
        Check.script ?component
          ~check:(Option.value override ~default:expression)
          check trace
      with
      | Ok text ->
          print_string text
      | Error message ->
          prerr_endline message;
          exit 1
    )
  | _ ->
      prerr_endline
        "usage: gen_script.exe CHECK RULE [COMPONENT [CHECK_EXPRESSION]]";
      exit 2
