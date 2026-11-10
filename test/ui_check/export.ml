open Silica

let write name text =
  Out_channel.with_open_text name (fun channel ->
      Out_channel.output_string channel text
  )

let export spec = function
  | Check.Ok { edges; _ } ->
      write (spec.Spec.id ^ ".qnt") (Spec.to_quint ~labels:edges spec);
      write (spec.Spec.id ^ ".dot") (Spec.to_dot ~labels:edges spec)
  | _ ->
      failwith ("the check of " ^ spec.Spec.id ^ " did not pass")

let () =
  let open Ui_checks in
  export (Machine.spec D.machine) (dialog ());
  export (Machine.spec P.machine) (popover ());
  export (Machine.spec Silica_ui.Tooltip.machine) (tooltip ());
  export (Machine.spec Mn.machine) (menu ());
  export (Machine.spec Sl.machine) (select ());
  export (Machine.spec Cb.machine) (combobox ());
  export (Machine.spec Cm.machine) (command_menu ());
  export (Machine.spec C.machine) (checkbox ());
  export (Machine.spec Sw.machine) (switch ());
  export (Machine.spec Th.machine) (theme ());
  export (Machine.spec T.machine) (tabs ());
  export (Machine.spec To.machine) (toast ())
