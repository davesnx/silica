open Silica_test_support
open Ui_samples

let opt = Alcotest.(option string)
let strings = Alcotest.(list string)
let log = Sim.html

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let has_log sim line =
  if not (contains (log sim) line) then
    Alcotest.failf "log has no %S in %s" line (log sim)

let source_names sim = List.map snd (Sim.sources sim)

let closed_at_start () =
  let sim = Sim.mount page in
  Alcotest.check opt "trigger has a popup" (Some "dialog")
    (Sim.attribute sim ~id:"note-trigger" "aria-haspopup");
  Alcotest.check opt "collapsed" (Some "false")
    (Sim.attribute sim ~id:"note-trigger" "aria-expanded");
  Alcotest.check opt "no controls while closed" None
    (Sim.attribute sim ~id:"note-trigger" "aria-controls");
  Alcotest.(check bool) "no popup" false (Sim.exists sim ~id:"note-popup");
  Alcotest.check strings "no sources" [] (source_names sim)

let trigger_opens () =
  let sim = Sim.mount page in
  Sim.press_id sim ~id:"note-trigger";
  Alcotest.check opt "role" (Some "dialog")
    (Sim.attribute sim ~id:"note-popup" "role");
  Alcotest.check opt "modal" (Some "true")
    (Sim.attribute sim ~id:"note-popup" "aria-modal");
  Alcotest.check opt "labelled by the title" (Some "note-title")
    (Sim.attribute sim ~id:"note-popup" "aria-labelledby");
  Alcotest.check opt "described" (Some "note-description")
    (Sim.attribute sim ~id:"note-popup" "aria-describedby");
  Alcotest.check opt "title text exists" (Some "note-title")
    (Sim.attribute sim ~id:"note-title" "id");
  Alcotest.check opt "data-open" (Some "")
    (Sim.attribute sim ~id:"note-popup" "data-open");
  Alcotest.check opt "expanded" (Some "true")
    (Sim.attribute sim ~id:"note-trigger" "aria-expanded");
  Alcotest.check opt "controls" (Some "note-popup")
    (Sim.attribute sim ~id:"note-trigger" "aria-controls");
  Alcotest.check strings "trap, lock, dismiss"
    [ "focus_trap"; "scroll_lock"; "dismiss" ]
    (source_names sim);
  has_log sim "open true trigger-press"

let escape_closes () =
  let sim = Sim.mount page in
  Sim.press_id sim ~id:"note-trigger";
  Alcotest.(check bool) "handled" true (Sim.escape sim);
  Alcotest.(check bool) "closed" false (Sim.exists sim ~id:"note-popup");
  Alcotest.check strings "sources stopped" [] (source_names sim);
  has_log sim "open false escape-key";
  Alcotest.(check bool)
    "focus goes to the trigger" true
    (List.mem "focus note-trigger" (Sim.log sim))

let outside_press_closes () =
  let sim = Sim.mount page in
  Sim.press_id sim ~id:"note-trigger";
  Sim.outside_press sim;
  Alcotest.(check bool) "closed" false (Sim.exists sim ~id:"note-popup");
  has_log sim "open false outside-press"

let close_part_closes () =
  let sim = Sim.mount page in
  Sim.press_id sim ~id:"note-trigger";
  Sim.press_id sim ~id:"note-close";
  Alcotest.(check bool) "closed" false (Sim.exists sim ~id:"note-popup");
  has_log sim "open false close-press"

let content_events () =
  let sim = Sim.mount page in
  Sim.press_id sim ~id:"note-trigger";
  Sim.input sim ~label:"Note" "changed";
  has_log sim "edit changed";
  Alcotest.(check bool) "still open" true (Sim.exists sim ~id:"note-popup");
  Sim.submit sim;
  has_log sim "save; open false close-press";
  Alcotest.(check bool)
    "closed by the content" false
    (Sim.exists sim ~id:"note-popup")

let alert_ignores_outside () =
  let sim = Sim.mount alert_page in
  Sim.press_id sim ~id:"note-trigger";
  Alcotest.check opt "alertdialog" (Some "alertdialog")
    (Sim.attribute sim ~id:"note-popup" "role");
  Sim.outside_press sim;
  Alcotest.(check bool)
    "an outside press does not close it" true
    (Sim.exists sim ~id:"note-popup");
  Alcotest.(check bool) "Escape reaches it" true (Sim.escape sim);
  Alcotest.(check bool)
    "Escape closes it" false
    (Sim.exists sim ~id:"note-popup");
  has_log sim "open false escape-key"

let locked_stays () =
  let sim = Sim.mount locked_page in
  Sim.press_id sim ~id:"note-trigger";
  ignore (Sim.escape sim);
  Sim.outside_press sim;
  Alcotest.(check bool)
    "Escape and outside presses do not close it" true
    (Sim.exists sim ~id:"note-popup");
  Alcotest.(check bool)
    "no close reported" false
    (contains (log sim) "open false");
  Sim.press_id sim ~id:"note-close";
  Alcotest.(check bool)
    "the close part still closes" false
    (Sim.exists sim ~id:"note-popup")

let parent_sets_open () =
  let sim = Sim.mount page in
  Sim.send sim (Set_open (Some true));
  Alcotest.(check bool)
    "opened by the parent" true
    (Sim.exists sim ~id:"note-popup");
  Alcotest.(check bool)
    "no open change for the parent's own request" false
    (contains (log sim) "open true");
  Sim.send sim (Set_open (Some false));
  Alcotest.(check bool)
    "closed by the parent" false
    (Sim.exists sim ~id:"note-popup");
  Sim.send sim (Set_open (Some true));
  ignore (Sim.escape sim);
  has_log sim "open false escape-key";
  Sim.send sim (Set_open (Some true));
  Alcotest.(check bool)
    "the same value again does not reopen" false
    (Sim.exists sim ~id:"note-popup");
  Sim.send sim (Set_open None);
  Sim.send sim (Set_open (Some true));
  Alcotest.(check bool)
    "a change to Some true reopens" true
    (Sim.exists sim ~id:"note-popup")

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "silica-ui dialog"
    [
      ( "dialog",
        [
          t "closed at start" closed_at_start;
          t "the trigger opens it" trigger_opens;
          t "Escape closes it" escape_closes;
          t "an outside press closes it" outside_press_closes;
          t "the close part closes it" close_part_closes;
          t "content events reach the parent" content_events;
          t "an alert dialog ignores outside presses" alert_ignores_outside;
          t "not dismissible" locked_stays;
          t "the parent sets the open state" parent_sets_open;
        ]
      );
    ]
