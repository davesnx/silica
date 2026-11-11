let list f items = "[" ^ String.concat "; " (List.map f items) ^ "]"
let quoted = Printf.sprintf "%S"

let state (state : Silica.Spec.state) =
  Printf.sprintf
    "{ name = %S; final = %b; timers = %s; sources = %s; held = %s }" state.name
    state.final
    (list string_of_int state.timers)
    (list quoted state.sources)
    (list quoted state.held)

let event formatter (event : Landing_save.event) =
  Format.pp_print_string formatter
    (match event with Save -> "save" | Answer _ -> "answer" | Reset -> "reset")

let () =
  match
    Check.component
      ~samples:
        [ Check.answers ~rejected:[ "rejected" ] Landing_save.save [ "12:00" ] ]
      Landing_save.component
  with
  | Ok { spec; _ } ->
      print_string "[@@@ocamlformat \"disable\"]\n\n";
      Printf.printf
        "let spec : Silica.Spec.t = { id = %S; init = %S; states = %s; edges = \
         %s }\n"
        spec.id spec.init (list state spec.states)
        (list
           (fun (from, target) -> Printf.sprintf "(%S, %S)" from target)
           spec.edges
        )
  | report ->
      Format.eprintf "landing_gen: %a@."
        (fun formatter report ->
          Check.pp_report ~pp_event:(Check.pp_input event) formatter report
        )
        report;
      exit 1
