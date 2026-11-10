open Silica
open Silica_test_support
open Ui_samples.Switch_samples

let opt = Alcotest.(option string)
let aria sim = Sim.attribute sim ~id:"wifi" "aria-checked"
let log sim = Sim.html sim

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let has_log sim line =
  if not (contains (log sim) line) then
    Alcotest.failf "log has no %S in %s" line (log sim)

let off_at_start () =
  let sim = Sim.mount page in
  Alcotest.check opt "role" (Some "switch") (Sim.attribute sim ~id:"wifi" "role");
  Alcotest.check opt "aria-checked" (Some "false") (aria sim);
  Alcotest.check opt "data-unchecked" (Some "")
    (Sim.attribute sim ~id:"wifi" "data-unchecked");
  Alcotest.check opt "thumb" (Some "")
    (Sim.attribute sim ~id:"wifi-thumb" "data-unchecked");
  Alcotest.(check bool)
    "the label is the text of the button" true
    (contains (Sim.html sim) "Wi-Fi</button>");
  Alcotest.(check bool)
    "no form value while off" false
    (Sim.exists sim ~id:"wifi-input")

let press_toggles () =
  let sim = Sim.mount page in
  Sim.press_id sim ~id:"wifi";
  Alcotest.check opt "on" (Some "true") (aria sim);
  Alcotest.check opt "data-checked" (Some "")
    (Sim.attribute sim ~id:"wifi" "data-checked");
  Alcotest.check opt "thumb checked" (Some "")
    (Sim.attribute sim ~id:"wifi-thumb" "data-checked");
  Alcotest.check opt "input value" (Some "on")
    (Sim.attribute sim ~id:"wifi-input" "value");
  Alcotest.check opt "input name" (Some "wifi")
    (Sim.attribute sim ~id:"wifi-input" "name");
  Sim.press_id sim ~id:"wifi";
  Alcotest.check opt "off" (Some "false") (aria sim);
  has_log sim "checked true; checked false"

let keys_left_to_the_button () =
  let sim = Sim.mount page in
  Alcotest.(check bool)
    "Enter is a native press" false
    (Sim.key_down sim ~id:"wifi" Key.enter);
  Alcotest.(check bool)
    "Space is a native press" false
    (Sim.key_down sim ~id:"wifi" (Key.make " "))

let props_request () =
  let sim = Sim.mount page in
  Sim.send sim (Set_checked true);
  Alcotest.check opt "on by the parent" (Some "true") (aria sim);
  Alcotest.(check bool)
    "no output for the parent's own request" false
    (contains (log sim) "checked true");
  Sim.send sim (Set_checked false);
  Alcotest.check opt "off by the parent" (Some "false") (aria sim)

let disabled_ignores () =
  let sim = Sim.mount page in
  Sim.send sim (Set_disabled true);
  Alcotest.check opt "disabled" (Some "")
    (Sim.attribute sim ~id:"wifi" "disabled");
  Alcotest.check opt "data-disabled" (Some "")
    (Sim.attribute sim ~id:"wifi" "data-disabled");
  Sim.press_id sim ~id:"wifi";
  Alcotest.check opt "not toggled" (Some "false") (aria sim)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "silica-ui switch"
    [
      ( "switch",
        [
          t "off at start" off_at_start;
          t "a press toggles it" press_toggles;
          t "Enter and Space are native presses" keys_left_to_the_button;
          t "the parent sets checked" props_request;
          t "disabled ignores presses" disabled_ignores;
        ]
      );
    ]
