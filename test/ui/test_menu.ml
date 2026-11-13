open Silica
open Silica_test_support
open Menu_samples

let opt = Alcotest.(option string)
let strings = Alcotest.(list string)

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let page_log = Sim.html

let has_log sim line =
  if not (contains (page_log sim) line) then
    Alcotest.failf "page log has no %S in %s" line (page_log sim)

let held_names sim = List.map snd (Sim.held sim)
let item i = Silica_ui.Menu.item_id "actions" i
let key name = Key.make name

let highlighted sim =
  List.filter
    (fun i -> Sim.attribute sim ~id:(item i) "data-highlighted" = Some "")
    [ 0; 1; 2; 3 ]

let tabindexes sim =
  List.map
    (fun i ->
      Option.value ~default:"none" (Sim.attribute sim ~id:(item i) "tabindex")
    )
    [ 0; 1; 2; 3 ]

let last_focus sim =
  List.fold_left
    (fun found line ->
      if String.starts_with ~prefix:"focus " line then
        Some (String.sub line 6 (String.length line - 6))
      else
        found
    )
    None (Sim.log sim)

let check_highlight sim name i =
  Alcotest.(check (list int)) (name ^ ": highlighted") [ i ] (highlighted sim);
  Alcotest.check opt (name ^ ": focus") (Some (item i)) (last_focus sim)

let down sim k =
  Alcotest.(check bool)
    ("handled " ^ k) true
    (Sim.key_down sim ~id:(item (List.hd (highlighted sim))) (key k))

let open_with_enter () =
  let sim = Sim.mount page in
  Alcotest.(check bool)
    "Enter handled" true
    (Sim.key_down sim ~id:"actions-trigger" Key.enter);
  sim

let closed_at_start () =
  let sim = Sim.mount page in
  Alcotest.check opt "haspopup" (Some "menu")
    (Sim.attribute sim ~id:"actions-trigger" "aria-haspopup");
  Alcotest.check opt "collapsed" (Some "false")
    (Sim.attribute sim ~id:"actions-trigger" "aria-expanded");
  Alcotest.check opt "no controls" None
    (Sim.attribute sim ~id:"actions-trigger" "aria-controls");
  Alcotest.(check bool) "no popup" false (Sim.exists sim ~id:"actions-popup");
  Alcotest.check strings "no held resources" [] (held_names sim)

let press_opens () =
  let sim = Sim.mount page in
  Sim.press_id sim ~id:"actions-trigger";
  Alcotest.check opt "role" (Some "menu")
    (Sim.attribute sim ~id:"actions-popup" "role");
  Alcotest.check opt "labelled by the trigger" (Some "actions-trigger")
    (Sim.attribute sim ~id:"actions-popup" "aria-labelledby");
  Alcotest.check opt "expanded" (Some "true")
    (Sim.attribute sim ~id:"actions-trigger" "aria-expanded");
  Alcotest.check opt "controls" (Some "actions-popup")
    (Sim.attribute sim ~id:"actions-trigger" "aria-controls");
  Alcotest.check strings "dismiss and anchor" [ "dismiss"; "anchor" ]
    (held_names sim);
  Alcotest.(check (list int)) "nothing highlighted" [] (highlighted sim);
  Alcotest.check strings "no item in the tab order" [ "-1"; "-1"; "-1"; "-1" ]
    (tabindexes sim);
  Alcotest.check opt "focus on the popup" (Some "actions-popup") (last_focus sim);
  Alcotest.check opt "item role" (Some "menuitem")
    (Sim.attribute sim ~id:(item 0) "role");
  Alcotest.check opt "disabled item" (Some "true")
    (Sim.attribute sim ~id:(item 1) "aria-disabled");
  Alcotest.check opt "disabled data" (Some "")
    (Sim.attribute sim ~id:(item 1) "data-disabled");
  has_log sim "actions open true trigger-press";
  Alcotest.(check bool)
    "the other menu stays closed" false
    (Sim.exists sim ~id:"more-popup")

let keys_open () =
  let sim = open_with_enter () in
  check_highlight sim "Enter" 0;
  Alcotest.check strings "roving tabindex" [ "0"; "-1"; "-1"; "-1" ]
    (tabindexes sim);
  has_log sim "actions open true trigger-press";
  let sim = Sim.mount page in
  ignore (Sim.key_down sim ~id:"actions-trigger" (key " "));
  check_highlight sim "Space" 0;
  let sim = Sim.mount page in
  ignore (Sim.key_down sim ~id:"actions-trigger" Key.arrow_down);
  check_highlight sim "ArrowDown" 0;
  has_log sim "actions open true list-navigation";
  let sim = Sim.mount page in
  ignore (Sim.key_down sim ~id:"actions-trigger" Key.arrow_up);
  check_highlight sim "ArrowUp" 3;
  Alcotest.(check bool)
    "a letter on the trigger is not handled" false
    (Sim.key_down sim ~id:"more-trigger" (key "a"))

let arrows_move () =
  let sim = open_with_enter () in
  down sim "ArrowDown";
  check_highlight sim "skips the disabled item" 2;
  down sim "ArrowDown";
  check_highlight sim "down" 3;
  down sim "ArrowDown";
  check_highlight sim "wraps to the first" 0;
  down sim "ArrowUp";
  check_highlight sim "wraps to the last" 3;
  down sim "ArrowUp";
  check_highlight sim "up" 2;
  down sim "ArrowUp";
  check_highlight sim "skips the disabled item up" 0;
  down sim "End";
  check_highlight sim "End" 3;
  down sim "Home";
  check_highlight sim "Home" 0

let typeahead () =
  let sim = open_with_enter () in
  down sim "d";
  check_highlight sim "d skips Duplicate (disabled)" 3;
  down sim "A";
  check_highlight sim "uppercase A" 2;
  down sim "e";
  check_highlight sim "e" 0;
  down sim "e";
  check_highlight sim "e again stays on the only match" 0;
  Alcotest.(check bool)
    "Ctrl+a is not handled" false
    (Sim.key_down sim ~id:(item 0) (Key.make ~ctrl:true "a"))

let enter_chooses () =
  let sim = open_with_enter () in
  down sim "ArrowDown";
  Sim.clear_log sim;
  down sim "Enter";
  Alcotest.(check bool) "closed" false (Sim.exists sim ~id:"actions-popup");
  has_log sim "actions select archive; actions open false item-press";
  Alcotest.check opt "focus on the trigger" (Some "actions-trigger")
    (last_focus sim);
  Alcotest.check strings "held resources stopped" [] (held_names sim);
  let sim = open_with_enter () in
  down sim " ";
  has_log sim "actions select edit; actions open false item-press"

let press_chooses () =
  let sim = Sim.mount page in
  Sim.press_id sim ~id:"actions-trigger";
  Sim.press_id sim ~id:(item 3);
  Alcotest.(check bool) "closed" false (Sim.exists sim ~id:"actions-popup");
  has_log sim "actions select delete; actions open false item-press";
  Sim.press_id sim ~id:"actions-trigger";
  Alcotest.(check bool)
    "a press on a disabled item does nothing" true
    ( match Sim.press_id sim ~id:(item 1) with
    | () ->
        false
    | exception Failure _ ->
        Sim.exists sim ~id:"actions-popup"
    )

let pointer_highlights () =
  let sim = Sim.mount page in
  Sim.press_id sim ~id:"actions-trigger";
  Sim.pointer_enter sim ~id:(item 2);
  check_highlight sim "pointer" 2;
  Alcotest.check_raises "a disabled item has no pointer handler"
    (Failure "Sim.pointer_enter: no handler at or above #actions-item-1")
    (fun () -> Sim.pointer_enter sim ~id:(item 1)
  );
  check_highlight sim "a disabled item is not highlighted" 2

let escape_closes () =
  let sim = open_with_enter () in
  Sim.clear_log sim;
  Alcotest.(check bool) "handled" true (Sim.escape sim);
  Alcotest.(check bool) "closed" false (Sim.exists sim ~id:"actions-popup");
  has_log sim "actions open false escape-key";
  Alcotest.check opt "focus on the trigger" (Some "actions-trigger")
    (last_focus sim)

let tab_closes () =
  let sim = open_with_enter () in
  Sim.clear_log sim;
  down sim "Tab";
  Alcotest.(check bool) "closed" false (Sim.exists sim ~id:"actions-popup");
  has_log sim "actions open false focus-out";
  Alcotest.check opt "focus on the trigger, so Tab moves on from it"
    (Some "actions-trigger") (last_focus sim)

let outside_closes () =
  let sim = open_with_enter () in
  Sim.clear_log sim;
  Sim.outside_press sim;
  Alcotest.(check bool) "closed" false (Sim.exists sim ~id:"actions-popup");
  has_log sim "actions open false outside-press";
  Alcotest.check opt "focus does not move" None (last_focus sim)

let trigger_toggles () =
  let sim = Sim.mount page in
  Sim.press_id sim ~id:"actions-trigger";
  Sim.press_id sim ~id:"actions-trigger";
  Alcotest.(check bool) "closed" false (Sim.exists sim ~id:"actions-popup");
  has_log sim
    "actions open true trigger-press; actions open false trigger-press"

let instances () =
  let sim = Sim.mount page in
  ignore (Sim.key_down sim ~id:"more-trigger" Key.enter);
  Alcotest.(check bool) "more is open" true (Sim.exists sim ~id:"more-popup");
  Alcotest.(check bool)
    "actions is closed" false
    (Sim.exists sim ~id:"actions-popup");
  Alcotest.check opt "its own item ids" (Some "")
    (Sim.attribute sim ~id:"more-item-0" "data-highlighted");
  Alcotest.check opt "its own focus" (Some "more-item-0") (last_focus sim);
  Sim.press_id sim ~id:"actions-trigger";
  Alcotest.(check bool)
    "both open" true
    (Sim.exists sim ~id:"actions-popup" && Sim.exists sim ~id:"more-popup")

let parent_sets_open () =
  let sim = Sim.mount page in
  Sim.send sim (Set_open (Some true));
  Alcotest.(check bool) "opened" true (Sim.exists sim ~id:"actions-popup");
  Alcotest.(check bool)
    "no open change for the parent's request" false
    (contains (page_log sim) "open true");
  Sim.send sim (Set_open (Some false));
  Alcotest.(check bool) "closed" false (Sim.exists sim ~id:"actions-popup");
  Alcotest.(check bool)
    "no close reported" false
    (contains (page_log sim) "open false")

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "silica-ui menu"
    [
      ( "menu",
        [
          t "closed at start" closed_at_start;
          t "a press opens it with the popup focused" press_opens;
          t "Enter, Space and arrows on the trigger open it" keys_open;
          t "arrows, Home and End move, wrap and skip disabled items"
            arrows_move;
          t "a letter highlights the next matching item" typeahead;
          t "Enter and Space choose the highlighted item" enter_chooses;
          t "a press on an item chooses it" press_chooses;
          t "the pointer highlights an enabled item" pointer_highlights;
          t "Escape closes and focuses the trigger" escape_closes;
          t "Tab closes" tab_closes;
          t "an outside press closes" outside_closes;
          t "the trigger toggles" trigger_toggles;
          t "each instance has its own ids" instances;
          t "the parent sets the open state" parent_sets_open;
        ]
      );
    ]
