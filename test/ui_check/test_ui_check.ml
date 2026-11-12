open Ui_checks

let expect_pass name check =
  let report = Check.run check in
  if Check.passed report then
    Printf.printf "%s: %d states, %d transitions\n" name report.states
      report.transitions
  else
    Alcotest.fail (Format.asprintf "%a" Check.pp_report report);
  report

let test_command_menu () =
  let report = expect_pass "command menu" (command_menu ()) in
  Alcotest.(check (list string))
    "the search is a child" [ "component combobox" ] report.not_explored;
  ignore
    (expect_pass "command menu search" (command_menu_search report.children))

let () =
  Alcotest.run "silica-ui check"
    [
      ("check", [ Alcotest.test_case "command menu" `Quick test_command_menu ]);
    ]
