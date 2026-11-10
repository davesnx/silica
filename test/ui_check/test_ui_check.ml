open Ui_checks

let print name = function
  | Check.Ok { states; transitions; _ } ->
      Printf.printf "%s: %d states, %d transitions\n" name states transitions
  | _ ->
      ()

let test_dialog () =
  match dialog () with
  | Check.Ok _ as r ->
      print "dialog" r
  | r ->
      Alcotest.fail (print_report pp_dialog_event r)

let test_popover () =
  match popover () with
  | Check.Ok _ as r ->
      print "popover" r
  | r ->
      Alcotest.fail (print_report pp_popover_event r)

let test_tooltip () =
  match tooltip () with
  | Check.Ok _ as r ->
      print "tooltip" r
  | r ->
      Alcotest.fail (print_report pp_tooltip_event r)

let test_menu () =
  match menu () with
  | Check.Ok _ as r ->
      print "menu" r
  | r ->
      Alcotest.fail (print_report pp_menu_event r)

let test_select () =
  match select () with
  | Check.Ok _ as r ->
      print "select" r
  | r ->
      Alcotest.fail (print_report pp_select_event r)

let test_combobox () =
  match combobox () with
  | Check.Ok _ as r ->
      print "combobox" r
  | r ->
      Alcotest.fail (print_report pp_combobox_event r)

let test_command_menu () =
  match command_menu () with
  | Check.Ok { not_explored; children; _ } as r -> (
      print "command menu" r;
      Alcotest.(check (list string))
        "the search is a child" [ "component combobox" ] not_explored;
      match command_menu_search children with
      | Check.Ok _ as r ->
          print "command menu search" r
      | r ->
          Alcotest.fail (print_report pp_combobox_event r)
    )
  | r ->
      Alcotest.fail (print_report pp_command_menu_event r)

let test_checkbox () =
  match checkbox () with
  | Check.Ok _ as r ->
      print "checkbox" r
  | r ->
      Alcotest.fail (print_report pp_checkbox_event r)

let test_switch () =
  match switch () with
  | Check.Ok _ as r ->
      print "switch" r
  | r ->
      Alcotest.fail (print_report pp_switch_event r)

let test_theme () =
  match theme () with
  | Check.Ok _ as r ->
      print "theme" r
  | r ->
      Alcotest.fail (print_report pp_theme_event r)

let test_tabs () =
  match tabs () with
  | Check.Ok _ as r ->
      print "tabs" r
  | r ->
      Alcotest.fail (print_report pp_tabs_event r)

let test_toast () =
  match toast () with
  | Check.Ok _ as r ->
      print "toast" r
  | r ->
      Alcotest.fail (print_report pp_toast_event r)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "silica-ui check"
    [
      ( "check",
        [
          t "dialog" test_dialog;
          t "popover" test_popover;
          t "tooltip" test_tooltip;
          t "menu" test_menu;
          t "select" test_select;
          t "combobox" test_combobox;
          t "command menu" test_command_menu;
          t "checkbox" test_checkbox;
          t "switch" test_switch;
          t "theme" test_theme;
          t "tabs" test_tabs;
          t "toast" test_toast;
        ]
      );
    ]
