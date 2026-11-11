let list f items = "[" ^ String.concat "; " (List.map f items) ^ "]"
let quoted = Printf.sprintf "%S"

let state (state : Silica.Spec.state) =
  Printf.sprintf
    "{ name = %S; final = %b; timers = %s; sources = %s; held = %s }" state.name
    state.final
    (list string_of_int state.timers)
    (list quoted state.sources)
    (list quoted state.held)

let label (label : Silica.Spec.label) =
  Printf.sprintf "{ from = %S; target = %S; event = %S; kept = %b }" label.from
    label.target label.event label.kept

let () =
  let report =
    Check.run
      (Check.component
         ~samples:
           [
             Check.answers ~rejected:[ "rejected" ] Landing_save.save
               [ "12:00" ];
           ]
         Landing_save.component
      )
  in
  match report.specs with
  | spec :: _ when Check.passed report ->
      print_string "[@@@ocamlformat \"disable\"]\n\n";
      Printf.printf
        "let spec : Silica.Spec.t = { id = %S; init = %S; states = %s; edges = \
         %s }\n"
        spec.id spec.init (list state spec.states) (list label spec.edges)
  | _ ->
      Format.eprintf "landing_gen: %a@." Check.pp_report report;
      exit 1
