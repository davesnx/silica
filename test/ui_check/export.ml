open Silica

let write name text =
  Out_channel.with_open_text name (fun channel ->
      Out_channel.output_string channel text
  )

let export name = function
  | Check.Ok { spec; edges; _ } ->
      write (spec.Spec.id ^ ".qnt") (Spec.to_quint ~labels:edges spec);
      write (spec.Spec.id ^ ".dot") (Spec.to_dot ~labels:edges spec)
  | _ ->
      failwith ("the check of " ^ name ^ " did not pass")

let () =
  let open Ui_checks in
  export "dialog" (dialog ());
  export "popover" (popover ());
  export "tooltip" (tooltip ());
  export "menu" (menu ());
  export "select" (select ());
  export "combobox" (combobox ());
  export "command menu" (command_menu ());
  export "checkbox" (checkbox ());
  export "switch" (switch ());
  export "theme" (theme ());
  export "tabs" (tabs ());
  export "toast" (toast ())
