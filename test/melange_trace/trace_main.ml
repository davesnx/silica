let () =
  let count = Trace_cases.run () in
  print_endline
    ("melange trace: " ^ string_of_int count
   ^ " records and two files read back"
    )
