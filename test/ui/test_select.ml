open Silica
open Silica_test_support
open Select_samples

let opt = Alcotest.(option string)
let strings = Alcotest.(list string)

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let has_log sim line =
  if not (contains (Sim.html sim) line) then
    Alcotest.failf "page log has no %S in %s" line (Sim.html sim)

let held_names sim = List.map snd (Sim.held sim)
let option i = Silica_ui.Select.option_id "status" i
let trigger = "status-trigger"
let key name = Key.make name

let press sim k =
  Alcotest.(check bool)
    ("handled " ^ k) true
    (Sim.key_down sim ~id:trigger (key k))

let active sim = Sim.attribute sim ~id:trigger "aria-activedescendant"

let check_active sim name i =
  Alcotest.check opt
    (name ^ ": active descendant")
    (Some (option i))
    (active sim);
  Alcotest.check opt
    (name ^ ": data-highlighted")
    (Some "")
    (Sim.attribute sim ~id:(option i) "data-highlighted");
  Alcotest.(check bool)
    (name ^ ": scrolled into view")
    true
    (List.mem ("scroll " ^ option i) (Sim.log sim))

let focuses sim = List.filter (String.starts_with ~prefix:"focus ") (Sim.log sim)

let opened () =
  let sim = Sim.mount page in
  press sim "Enter";
  sim

let closed_at_start () =
  let sim = Sim.mount page in
  let attr = Sim.attribute sim ~id:trigger in
  Alcotest.check opt "role" (Some "combobox") (attr "role");
  Alcotest.check opt "haspopup" (Some "listbox") (attr "aria-haspopup");
  Alcotest.check opt "collapsed" (Some "false") (attr "aria-expanded");
  Alcotest.check opt "named" (Some "Status") (attr "aria-label");
  Alcotest.check opt "no controls" None (attr "aria-controls");
  Alcotest.check opt "placeholder state" (Some "") (attr "data-placeholder");
  Alcotest.(check bool)
    "shows the placeholder" true
    (contains (Sim.html sim) "Pick a status");
  Alcotest.check opt "hidden input name" (Some "status")
    (Sim.attribute sim ~id:"status-input" "name");
  Alcotest.check opt "hidden input type" (Some "hidden")
    (Sim.attribute sim ~id:"status-input" "type");
  Alcotest.check opt "empty value" (Some "")
    (Sim.attribute sim ~id:"status-input" "value");
  Alcotest.(check bool)
    "no input without a name" false
    (Sim.exists sim ~id:"priority-input");
  Alcotest.(check bool) "no listbox" false (Sim.exists sim ~id:"status-popup");
  Alcotest.check strings "no held resources" [] (held_names sim)

let keys_open () =
  List.iter
    (fun (k, why) ->
      let sim = Sim.mount page in
      press sim k;
      Alcotest.check opt (k ^ ": role") (Some "listbox")
        (Sim.attribute sim ~id:"status-popup" "role");
      Alcotest.check opt (k ^ ": expanded") (Some "true")
        (Sim.attribute sim ~id:trigger "aria-expanded");
      Alcotest.check opt (k ^ ": controls") (Some "status-popup")
        (Sim.attribute sim ~id:trigger "aria-controls");
      check_active sim k 0;
      Alcotest.check strings (k ^ ": focus stays") [] (focuses sim);
      Alcotest.check strings
        (k ^ ": dismiss and anchor")
        [ "dismiss"; "anchor" ] (held_names sim);
      has_log sim ("status open true " ^ why)
    )
    [
      ("Enter", "trigger-press");
      (" ", "trigger-press");
      ("ArrowDown", "list-navigation");
      ("ArrowUp", "list-navigation");
    ];
  let sim = Sim.mount page in
  Alcotest.(check bool)
    "a letter on the closed trigger is not handled" false
    (Sim.key_down sim ~id:trigger (key "d"))

let arrows_move () =
  let sim = opened () in
  Alcotest.check opt "option role" (Some "option")
    (Sim.attribute sim ~id:(option 0) "role");
  Alcotest.check opt "disabled" (Some "true")
    (Sim.attribute sim ~id:(option 2) "aria-disabled");
  press sim "ArrowDown";
  check_active sim "down" 1;
  press sim "ArrowDown";
  check_active sim "skips the disabled option" 3;
  press sim "ArrowDown";
  press sim "ArrowDown";
  check_active sim "last" 5;
  press sim "ArrowDown";
  check_active sim "no wrap at the end" 5;
  press sim "ArrowUp";
  press sim "ArrowUp";
  press sim "ArrowUp";
  check_active sim "skips the disabled option up" 1;
  press sim "Home";
  check_active sim "Home" 0;
  press sim "ArrowUp";
  check_active sim "no wrap at the start" 0;
  press sim "End";
  check_active sim "End" 5

let typeahead () =
  let sim = opened () in
  press sim "d";
  check_active sim "d" 4;
  press sim "d";
  check_active sim "d again" 5;
  press sim "D";
  check_active sim "D wraps" 4;
  press sim "b";
  check_active sim "b skips Blocked (disabled)" 0

let enter_selects () =
  let sim = opened () in
  press sim "ArrowDown";
  press sim "ArrowDown";
  press sim "Enter";
  Alcotest.(check bool) "closed" false (Sim.exists sim ~id:"status-popup");
  has_log sim "status value progress; status open false item-press";
  Alcotest.check opt "hidden input value" (Some "progress")
    (Sim.attribute sim ~id:"status-input" "value");
  Alcotest.check opt "no placeholder state" None
    (Sim.attribute sim ~id:trigger "data-placeholder");
  Alcotest.(check bool)
    "shows the label" true
    (contains (Sim.html sim) "In progress");
  Alcotest.check opt "no active descendant" None (active sim);
  Alcotest.check strings "focus on the trigger"
    [ "focus " ^ trigger ]
    (focuses sim);
  press sim "ArrowDown";
  check_active sim "reopens on the selected option" 3;
  Alcotest.check opt "aria-selected" (Some "true")
    (Sim.attribute sim ~id:(option 3) "aria-selected");
  Alcotest.check opt "data-selected" (Some "")
    (Sim.attribute sim ~id:(option 3) "data-selected");
  Alcotest.check opt "others not selected" (Some "false")
    (Sim.attribute sim ~id:(option 0) "aria-selected");
  press sim " ";
  Alcotest.(check bool)
    "Space on the selected option closes" false
    (Sim.exists sim ~id:"status-popup");
  Alcotest.(check bool)
    "with no second value change" false
    (contains (Sim.html sim)
       "status value progress; status open false item-press; status open true \
        list-navigation; status value"
    )

let tab_selects () =
  let sim = opened () in
  press sim "ArrowDown";
  press sim "Tab";
  Alcotest.(check bool) "closed" false (Sim.exists sim ~id:"status-popup");
  has_log sim "status value todo; status open false focus-out"

let escape_keeps () =
  let sim = opened () in
  press sim "ArrowDown";
  Alcotest.(check bool) "handled" true (Sim.escape sim);
  Alcotest.(check bool) "closed" false (Sim.exists sim ~id:"status-popup");
  has_log sim "status open false escape-key";
  Alcotest.(check bool) "no value" false (contains (Sim.html sim) "status value");
  Alcotest.check strings "focus on the trigger"
    [ "focus " ^ trigger ]
    (focuses sim)

let outside_keeps () =
  let sim = opened () in
  press sim "ArrowDown";
  Sim.outside_press sim;
  Alcotest.(check bool) "closed" false (Sim.exists sim ~id:"status-popup");
  has_log sim "status open false outside-press";
  Alcotest.(check bool) "no value" false (contains (Sim.html sim) "status value")

let press_selects () =
  let sim = Sim.mount page in
  Sim.press_id sim ~id:trigger;
  has_log sim "status open true trigger-press";
  Sim.pointer_enter sim ~id:(option 4);
  Alcotest.check opt "the pointer highlights" (Some (option 4)) (active sim);
  Alcotest.check_raises "a disabled option has no pointer handler"
    (Failure "Sim.pointer_enter: no handler at or above #status-option-2")
    (fun () -> Sim.pointer_enter sim ~id:(option 2)
  );
  Alcotest.check opt "not a disabled option" (Some (option 4)) (active sim);
  Alcotest.(check bool)
    "a press on a disabled option does nothing" true
    ( match Sim.press_id sim ~id:(option 2) with
    | () ->
        false
    | exception Failure _ ->
        Sim.exists sim ~id:"status-popup"
    );
  Sim.press_id sim ~id:(option 4);
  has_log sim "status value done; status open false item-press";
  Sim.press_id sim ~id:trigger;
  Sim.press_id sim ~id:trigger;
  has_log sim "status open true trigger-press; status open false trigger-press"

let parent_sets_value () =
  let sim = Sim.mount page in
  Sim.send sim (Set_value (Some "done"));
  Alcotest.check opt "taken" (Some "done")
    (Sim.attribute sim ~id:"status-input" "value");
  Alcotest.(check bool)
    "no value change" false
    (contains (Sim.html sim) "status value");
  press sim "Enter";
  check_active sim "opens on the requested value" 4;
  press sim "ArrowUp";
  press sim "Enter";
  Alcotest.check opt "user value" (Some "progress")
    (Sim.attribute sim ~id:"status-input" "value");
  Sim.send sim (Set_value None);
  Alcotest.check opt "cleared" (Some "")
    (Sim.attribute sim ~id:"status-input" "value");
  Alcotest.check opt "placeholder again" (Some "")
    (Sim.attribute sim ~id:trigger "data-placeholder")

let instances () =
  let sim = Sim.mount page in
  Alcotest.(check bool)
    "handled" true
    (Sim.key_down sim ~id:"priority-trigger" Key.enter);
  Alcotest.(check bool) "its own list" true (Sim.exists sim ~id:"priority-popup");
  Alcotest.(check bool)
    "the other is closed" false
    (Sim.exists sim ~id:"status-popup");
  Alcotest.check opt "its own option ids"
    (Some (Silica_ui.Select.option_id "priority" 0))
    (Sim.attribute sim ~id:"priority-trigger" "aria-activedescendant")

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "silica-ui select"
    [
      ( "select",
        [
          t "closed at start" closed_at_start;
          t "Enter, Space and arrows open it" keys_open;
          t "arrows, Home and End move without wrap" arrows_move;
          t "a letter highlights the next matching option" typeahead;
          t "Enter and Space select and close" enter_selects;
          t "Tab selects and closes" tab_selects;
          t "Escape closes with no change" escape_keeps;
          t "an outside press closes with no change" outside_keeps;
          t "presses and the pointer" press_selects;
          t "the parent sets the value" parent_sets_value;
          t "each instance has its own ids" instances;
        ]
      );
    ]
