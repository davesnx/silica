open Silica
open Silica_test_support
open Ui_samples.Checkbox_samples

let opt = Alcotest.(option string)
let aria sim = Sim.attribute sim ~id:"terms" "aria-checked"
let data sim name = Sim.attribute sim ~id:"terms" name
let log sim = Sim.html sim

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let has_log sim line =
  if not (contains (log sim) line) then
    Alcotest.failf "log has no %S in %s" line (log sim)

let unchecked_at_start () =
  let sim = Sim.mount page in
  Alcotest.check opt "role" (Some "checkbox")
    (Sim.attribute sim ~id:"terms" "role");
  Alcotest.check opt "a button" (Some "button")
    (Sim.attribute sim ~id:"terms" "type");
  Alcotest.check opt "aria-checked" (Some "false") (aria sim);
  Alcotest.check opt "data-unchecked" (Some "") (data sim "data-unchecked");
  Alcotest.check opt "no data-checked" None (data sim "data-checked");
  Alcotest.(check bool)
    "the label is the text of the button" true
    (contains (Sim.html sim) "Accept the terms</button>");
  Alcotest.(check bool)
    "no form value while unchecked" false
    (Sim.exists sim ~id:"terms-input")

let press_toggles () =
  let sim = Sim.mount page in
  Sim.press_id sim ~id:"terms";
  Alcotest.check opt "aria-checked" (Some "true") (aria sim);
  Alcotest.check opt "data-checked" (Some "") (data sim "data-checked");
  Alcotest.check opt "hidden input" (Some "hidden")
    (Sim.attribute sim ~id:"terms-input" "type");
  Alcotest.check opt "input name" (Some "terms")
    (Sim.attribute sim ~id:"terms-input" "name");
  Alcotest.check opt "input value" (Some "on")
    (Sim.attribute sim ~id:"terms-input" "value");
  Sim.press_id sim ~id:"terms";
  Alcotest.check opt "unchecked again" (Some "false") (aria sim);
  has_log sim "checked true; checked false"

let enter_does_nothing () =
  let sim = Sim.mount page in
  Alcotest.(check bool)
    "Enter is taken, so the native click does not happen" true
    (Sim.key_down sim ~id:"terms" Key.enter);
  Alcotest.check opt "still unchecked" (Some "false") (aria sim);
  Alcotest.(check bool)
    "Space is left to the native button" false
    (Sim.key_down sim ~id:"terms" (Key.make " "));
  Alcotest.(check bool)
    "nothing logged" false
    (contains (log sim) "checked true" || contains (log sim) "checked false")

let props_request () =
  let sim = Sim.mount page in
  Sim.send sim (Set_checked true);
  Alcotest.check opt "checked by the parent" (Some "true") (aria sim);
  Alcotest.(check bool)
    "no output for the parent's own request" false
    (contains (log sim) "checked true");
  Sim.send sim (Set_indeterminate true);
  Alcotest.check opt "mixed" (Some "mixed") (aria sim);
  Alcotest.check opt "data-indeterminate" (Some "")
    (data sim "data-indeterminate");
  Alcotest.check opt "no data-checked" None (data sim "data-checked");
  Sim.press_id sim ~id:"terms";
  Alcotest.check opt "a press on a mixed box checks it" (Some "true") (aria sim);
  has_log sim "checked true"

let disabled_ignores () =
  let sim = Sim.mount page in
  Sim.send sim (Set_disabled true);
  Alcotest.check opt "disabled" (Some "")
    (Sim.attribute sim ~id:"terms" "disabled");
  Alcotest.check opt "data-disabled" (Some "") (data sim "data-disabled");
  Sim.press_id sim ~id:"terms";
  Alcotest.check opt "not toggled" (Some "false") (aria sim)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "silica-ui checkbox"
    [
      ( "checkbox",
        [
          t "unchecked at start" unchecked_at_start;
          t "a press toggles it" press_toggles;
          t "Enter does not toggle" enter_does_nothing;
          t "the parent sets checked and indeterminate" props_request;
          t "disabled ignores presses" disabled_ignores;
        ]
      );
    ]
