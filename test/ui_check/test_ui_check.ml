open Ui_checks

let expect_pass name check =
  let report = Check.run check in
  if Check.passed report then
    Printf.printf "%s: %d states, %d transitions\n" name report.states
      report.transitions
  else
    Alcotest.fail (Format.asprintf "%a" Check.pp_report report);
  report

let test name check () = ignore (expect_pass name (check ()))

let test_command_menu () =
  let report = expect_pass "command menu" (command_menu ()) in
  Alcotest.(check (list string))
    "the search is a child" [ "component combobox" ] report.not_explored;
  ignore
    (expect_pass "command menu search" (command_menu_search report.children))

let () =
  let case name f = Alcotest.test_case name `Quick f in
  Alcotest.run "silica-ui check"
    [
      ( "check",
        [
          case "dialog" (test "dialog" dialog);
          case "popover" (test "popover" popover);
          case "tooltip" (test "tooltip" tooltip);
          case "menu" (test "menu" menu);
          case "select" (test "select" select);
          case "combobox" (test "combobox" combobox);
          case "command menu" test_command_menu;
          case "checkbox" (test "checkbox" checkbox);
          case "switch" (test "switch" switch);
          case "theme" (test "theme" theme);
          case "tabs" (test "tabs" tabs);
          case "toast" (test "toast" toast);
        ]
      );
    ]
