let () =
  let report =
    Check.run
      (Check.component
         ~within:(fun state -> Counter.count state <= 3)
         ~samples:[ Check.answers Api.save ~rejected:[ "too many" ] [ () ] ]
         Counter.component
      )
  in
  if Check.passed report then
    Printf.printf "counter: %d states, %d transitions\n" report.states
      report.transitions
  else (
    Format.eprintf "%a@." Check.pp_report report;
    exit 1
  )
