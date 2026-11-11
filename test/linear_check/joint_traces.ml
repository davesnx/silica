let seeded =
  let open Linear_routes in
  Linear_checks.joint_router
    ~pages:[ Linear_checks.joint_create; Linear_checks.joint_dialog ]
    ~paths:[]
    ~routes:[ Issues list; Issues { list with create = true } ]
    ~properties:
      [
        Check.always ~name:"no title error"
          (Check.view (fun view ->
               not (Linear_checks.shows view "Title is required")
           )
          );
      ]
    ()

let read file =
  match
    Silica.Trace.of_jsonl (In_channel.with_open_bin file In_channel.input_all)
  with
  | Ok trace ->
      trace
  | Error message ->
      failwith message

let print_point (point : (_, _) Check.replayed_step) =
  print_endline
    ("checker: "
    ^ ( match point.step with
      | Some step ->
          Printf.sprintf "%s at %s: %s -> %s" step.machine step.instance
            step.from step.target
      | None ->
          "start"
      )
    ^
    match point.failing with
    | [] ->
        ""
    | failing ->
        ", fails "
        ^ String.concat ", "
            (List.map (fun (failure : Check.failure) -> failure.rule) failing)
    )

let replay file =
  let trace = read file in
  ( match Check.replay_trace seeded trace with
  | Error message ->
      print_endline ("checker: " ^ message)
  | Ok (Not_enabled { at; _ }) ->
      Printf.printf "checker: step %d is not enabled\n" at
  | Ok (Replayed points) ->
      List.iter print_point points
  );
  let page = Silica_memory.mount_app ~url:"/" Linear_app.app in
  match Silica_memory.replay page trace with
  | Replayed ->
      print_endline "memory: replayed"
  | Different { seq; expected; got } ->
      Printf.printf "memory: different at %d: expected %s, got %s\n" seq
        expected got
  | Not_realisable { seq; reason } ->
      Printf.printf "memory: not realisable at %d: %s\n" seq reason
  | Not_found { seq; error } ->
      Printf.printf "memory: not found at %d: %s\n" seq error

let script_of trace =
  match Check.script ~component:"Linear_app.app" seeded trace with
  | Ok text ->
      print_string text
  | Error message ->
      print_endline ("script: " ^ message)

let generated () =
  match (Check.run seeded).violations with
  | violation :: _ ->
      script_of
        (Check.export seeded ~rule:violation.rule violation.counterexample)
  | [] ->
      print_endline "script: the seeded check has no violation"

let () =
  match Array.to_list Sys.argv with
  | [ _; "generate" ] ->
      generated ()
  | [ _; "replay"; file ] ->
      replay file
  | [ _; "script"; file ] ->
      script_of (read file)
  | argv ->
      Check_suite.main ~argv:(Array.of_list argv) [ Check_suite.entry seeded ]
