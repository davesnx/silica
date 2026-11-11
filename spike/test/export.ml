open Silica

let write name text =
  Out_channel.with_open_text name (fun channel ->
      Out_channel.output_string channel text
  )

let export = function
  | Check.Ok { edges; spec; _ } ->
      write (spec.Spec.id ^ ".qnt") (Spec.to_quint ~labels:edges spec);
      write (spec.Spec.id ^ ".dot") (Spec.to_dot ~labels:edges spec)
  | _ ->
      failwith "a check of the spike did not pass"

let () =
  export (Spike_checks.Landing.port_check ());
  export (Spike_checks.Dialogs.port_check ());
  export (Spike_checks.Issue_list.port_check ())
