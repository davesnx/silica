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
  export (Machine.spec Ui_samples.Note_dialog.machine) (dialog ());
  export (Machine.spec Filter_popover.machine) (popover ());
  export (Machine.spec Silica_ui.Tooltip.machine) (tooltip ());
  export (Machine.spec Silica_ui.Menu.machine) (menu ());
  export (Machine.spec Silica_ui.Select.machine) (select ());
  export (Machine.spec Silica_ui.Combobox.machine) (combobox ());
  export (Machine.spec Silica_ui.Command_menu.Menu.machine) (command_menu ());
  export (Machine.spec Silica_ui.Checkbox.machine) (checkbox ());
  export (Machine.spec Silica_ui.Switch.machine) (switch ());
  export (Machine.spec Silica_ui.Theme.machine) (theme ());
  export (Machine.spec Silica_ui.Tabs.machine) (tabs ());
  export (Machine.spec Silica_ui.Toast.machine) (toast ())
