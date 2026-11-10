open Silica
open Silica_test_support
open Ui_samples.Tabs_samples

let opt = Alcotest.(option string)
let attr sim id name = Sim.attribute sim ~id name
let tab v = "set-tab-" ^ v
let panel v = "set-panel-" ^ v
let key name = Key.make name

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let has_log sim line =
  if not (contains (Sim.html sim) line) then
    Alcotest.failf "log has no %S in %s" line (Sim.html sim)

let stops sim =
  List.filter
    (fun v -> attr sim (tab v) "tabindex" = Some "0")
    [ "a"; "b"; "c"; "d" ]

let check_stop sim v =
  Alcotest.(check (list string)) "one tab stop" [ v ] (stops sim)

let focused_last sim v =
  match List.rev (List.filter (fun l -> contains l "focus") (Sim.log sim)) with
  | last :: _ ->
      Alcotest.(check string) "focus command" ("focus " ^ tab v) last
  | [] ->
      Alcotest.fail "no focus command"

let roles_at_start () =
  let sim = Sim.mount page in
  Alcotest.check opt "tablist" (Some "tablist") (attr sim "set" "role");
  Alcotest.check opt "orientation" (Some "horizontal")
    (attr sim "set" "aria-orientation");
  Alcotest.check opt "tab role" (Some "tab") (attr sim (tab "a") "role");
  Alcotest.check opt "selected" (Some "true")
    (attr sim (tab "a") "aria-selected");
  Alcotest.check opt "not selected" (Some "false")
    (attr sim (tab "b") "aria-selected");
  Alcotest.check opt "controls"
    (Some (panel "a"))
    (attr sim (tab "a") "aria-controls");
  Alcotest.check opt "data-active" (Some "") (attr sim (tab "a") "data-active");
  Alcotest.check opt "no data-active" None (attr sim (tab "b") "data-active");
  Alcotest.check opt "disabled" (Some "") (attr sim (tab "c") "disabled");
  Alcotest.check opt "data-disabled" (Some "")
    (attr sim (tab "c") "data-disabled");
  check_stop sim "a";
  Alcotest.check opt "panel role" (Some "tabpanel") (attr sim (panel "a") "role");
  Alcotest.check opt "labelled by its tab"
    (Some (tab "a"))
    (attr sim (panel "a") "aria-labelledby");
  Alcotest.check opt "panel tabindex" (Some "0")
    (attr sim (panel "a") "tabindex");
  Alcotest.check opt "shown" None (attr sim (panel "a") "hidden");
  Alcotest.check opt "others hidden" (Some "") (attr sim (panel "b") "hidden")

let arrows_move_focus () =
  let sim = Sim.mount page in
  Alcotest.(check bool)
    "ArrowRight" true
    (Sim.key_down sim ~id:(tab "a") (key "ArrowRight"));
  focused_last sim "b";
  check_stop sim "b";
  Alcotest.check opt "focus does not select" (Some "true")
    (attr sim (tab "a") "aria-selected");
  ignore (Sim.key_down sim ~id:(tab "b") (key "ArrowRight"));
  focused_last sim "d";
  check_stop sim "d";
  ignore (Sim.key_down sim ~id:(tab "d") (key "ArrowRight"));
  focused_last sim "a";
  ignore (Sim.key_down sim ~id:(tab "a") (key "ArrowLeft"));
  focused_last sim "d";
  ignore (Sim.key_down sim ~id:(tab "d") (key "Home"));
  focused_last sim "a";
  ignore (Sim.key_down sim ~id:(tab "a") (key "End"));
  focused_last sim "d";
  Alcotest.(check bool)
    "ArrowDown is not for a horizontal list" false
    (Sim.key_down sim ~id:(tab "d") Key.arrow_down);
  Alcotest.(check bool) "no output" false (contains (Sim.html sim) "value ")

let press_selects () =
  let sim = Sim.mount page in
  ignore (Sim.key_down sim ~id:(tab "a") (key "ArrowRight"));
  Sim.press_id sim ~id:(tab "b");
  has_log sim "value b";
  Alcotest.check opt "selected" (Some "true")
    (attr sim (tab "b") "aria-selected");
  Alcotest.check opt "panel shown" None (attr sim (panel "b") "hidden");
  Alcotest.check opt "old panel hidden" (Some "") (attr sim (panel "a") "hidden");
  check_stop sim "b";
  Sim.press_id sim ~id:(tab "b");
  Alcotest.(check bool)
    "a press on the selected tab says nothing" false
    (contains (Sim.html sim) "value b; value b");
  Sim.press_id sim ~id:(tab "c");
  Alcotest.check opt "a disabled tab is not selected" (Some "false")
    (attr sim (tab "c") "aria-selected")

let props_request () =
  let sim = Sim.mount page in
  Sim.send sim (Set_value "d");
  Alcotest.check opt "selected by the parent" (Some "true")
    (attr sim (tab "d") "aria-selected");
  check_stop sim "d";
  Alcotest.(check bool) "no output" false (contains (Sim.html sim) "value ")

let vertical () =
  let sim = Sim.mount vertical_page in
  Alcotest.check opt "orientation" (Some "vertical")
    (attr sim "set" "aria-orientation");
  Alcotest.(check bool)
    "ArrowRight is not for a vertical list" false
    (Sim.key_down sim ~id:(tab "a") (key "ArrowRight"));
  Alcotest.(check bool)
    "ArrowDown" true
    (Sim.key_down sim ~id:(tab "a") Key.arrow_down);
  focused_last sim "b";
  ignore (Sim.key_down sim ~id:(tab "b") Key.arrow_up);
  focused_last sim "a"

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "silica-ui tabs"
    [
      ( "tabs",
        [
          t "roles and states at start" roles_at_start;
          t "arrows, Home and End move focus" arrows_move_focus;
          t "a press selects" press_selects;
          t "the parent sets the value" props_request;
          t "vertical" vertical;
        ]
      );
    ]
