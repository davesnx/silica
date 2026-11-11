let print_event formatter event =
  Format.pp_print_string formatter
    (Silica.Machine.event_name Counter.machine event)

let () =
  match
    Check.component
      ~within:(fun state -> Counter.count state <= 3)
      ~samples:[ Check.answers Api.save ~rejected:[ "too many" ] [ () ] ]
      Counter.component
  with
  | Check.Ok { states; transitions; _ } ->
      Printf.printf "counter: %d states, %d transitions\n" states transitions
  | report ->
      Check.pp_report
        ~pp_event:(Check.pp_input print_event)
        Format.err_formatter report;
      Format.pp_print_newline Format.err_formatter ();
      exit 1
