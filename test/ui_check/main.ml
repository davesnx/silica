let () =
  let open Ui_checks in
  Check_suite.main
    [
      Check_suite.entry (dialog ());
      Check_suite.entry (popover ());
      Check_suite.entry (tooltip ());
      Check_suite.entry (menu ());
      Check_suite.entry (select ());
      Check_suite.entry (combobox ());
      Check_suite.entry (command_menu ());
      Check_suite.entry (checkbox ());
      Check_suite.entry (switch ());
      Check_suite.entry (theme ());
      Check_suite.entry (tabs ());
      Check_suite.entry (toast ());
    ]
