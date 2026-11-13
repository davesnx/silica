open Silica
open Silica_test_support
open Popover_samples

let opt = Alcotest.(option string)
let strings = Alcotest.(list string)

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let has_log sim line =
  if not (contains (Sim.html sim) line) then
    Alcotest.failf "log has no %S in %s" line (Sim.html sim)

let source_names sim = List.map snd (Sim.sources sim)
let focused sim id = List.mem ("focus " ^ id) (Sim.log sim)
let popup_shown sim = Sim.exists sim ~id:"filter-popup"

let closed_at_start () =
  let sim = Sim.mount page in
  Alcotest.check opt "trigger has a dialog popup" (Some "dialog")
    (Sim.attribute sim ~id:"filter-trigger" "aria-haspopup");
  Alcotest.check opt "collapsed" (Some "false")
    (Sim.attribute sim ~id:"filter-trigger" "aria-expanded");
  Alcotest.check opt "no controls while closed" None
    (Sim.attribute sim ~id:"filter-trigger" "aria-controls");
  Alcotest.check opt "no data-popup-open" None
    (Sim.attribute sim ~id:"filter-trigger" "data-popup-open");
  Alcotest.(check bool) "no popup" false (popup_shown sim);
  Alcotest.check strings "no sources" [] (source_names sim)

let trigger_opens () =
  let sim = Sim.mount page in
  Sim.press_id sim ~id:"filter-trigger";
  Alcotest.check opt "role" (Some "dialog")
    (Sim.attribute sim ~id:"filter-popup" "role");
  Alcotest.check opt "not modal" None
    (Sim.attribute sim ~id:"filter-popup" "aria-modal");
  Alcotest.check opt "labelled by the title" (Some "filter-title")
    (Sim.attribute sim ~id:"filter-popup" "aria-labelledby");
  Alcotest.check opt "title exists" (Some "filter-title")
    (Sim.attribute sim ~id:"filter-title" "id");
  Alcotest.check opt "focusable by script" (Some "-1")
    (Sim.attribute sim ~id:"filter-popup" "tabindex");
  Alcotest.check opt "no style from the view" None
    (Sim.attribute sim ~id:"filter-popup" "style");
  Alcotest.check opt "data-open" (Some "")
    (Sim.attribute sim ~id:"filter-popup" "data-open");
  Alcotest.check opt "expanded" (Some "true")
    (Sim.attribute sim ~id:"filter-trigger" "aria-expanded");
  Alcotest.check opt "controls" (Some "filter-popup")
    (Sim.attribute sim ~id:"filter-trigger" "aria-controls");
  Alcotest.check opt "data-popup-open" (Some "")
    (Sim.attribute sim ~id:"filter-trigger" "data-popup-open");
  Alcotest.check strings "dismiss and anchor; no trap, no scroll lock"
    [ "dismiss"; "anchor" ] (source_names sim);
  Alcotest.(check bool)
    "focus moves into the popup" true
    (focused sim "filter-popup");
  has_log sim "open true trigger-press"

let trigger_closes () =
  let sim = Sim.mount page in
  Sim.press_id sim ~id:"filter-trigger";
  Sim.press_id sim ~id:"filter-trigger";
  Alcotest.(check bool) "closed" false (popup_shown sim);
  Alcotest.check strings "sources stopped" [] (source_names sim);
  has_log sim "open true trigger-press; open false trigger-press"

let escape_closes () =
  let sim = Sim.mount page in
  Sim.press_id sim ~id:"filter-trigger";
  Sim.clear_log sim;
  Alcotest.(check bool) "handled" true (Sim.escape sim);
  Alcotest.(check bool) "closed" false (popup_shown sim);
  has_log sim "open false escape-key";
  Alcotest.(check bool)
    "focus goes to the trigger" true
    (focused sim "filter-trigger")

let outside_press_closes () =
  let sim = Sim.mount page in
  Sim.press_id sim ~id:"filter-trigger";
  Sim.clear_log sim;
  Sim.outside_press sim;
  Alcotest.(check bool) "closed" false (popup_shown sim);
  has_log sim "open false outside-press";
  Alcotest.(check bool)
    "focus does not move" false
    (List.exists (fun line -> contains line "focus") (Sim.log sim))

let close_part_and_content () =
  let sim = Sim.mount page in
  Sim.press_id sim ~id:"filter-trigger";
  Sim.press_id sim ~id:"filter-close";
  Alcotest.(check bool) "closed by the close part" false (popup_shown sim);
  has_log sim "open false close-press";
  Sim.press_id sim ~id:"filter-trigger";
  Sim.input sim ~label:"Query" "crash";
  has_log sim "query crash";
  Alcotest.(check bool) "still open" true (popup_shown sim);
  Sim.submit sim;
  has_log sim "apply; open false close-press";
  Alcotest.(check bool) "closed by the content" false (popup_shown sim)

let parent_sets_open () =
  let sim = Sim.mount page in
  Sim.send sim (Set_open (Some true));
  Alcotest.(check bool) "opened by the parent" true (popup_shown sim);
  Alcotest.(check bool) "focus moves in" true (focused sim "filter-popup");
  Alcotest.(check bool)
    "no open change for the parent's own request" false
    (contains (Sim.html sim) "open true");
  Sim.send sim (Set_open (Some false));
  Alcotest.(check bool) "closed by the parent" false (popup_shown sim);
  Sim.send sim (Set_open (Some true));
  ignore (Sim.escape sim);
  has_log sim "open false escape-key";
  Sim.send sim (Set_open (Some true));
  Alcotest.(check bool)
    "the same value again does not reopen" false (popup_shown sim);
  Sim.send sim (Set_open None);
  Sim.send sim (Set_open (Some true));
  Alcotest.(check bool) "a change to Some true reopens" true (popup_shown sim)

let anchor_uses_props () =
  let props = props ~open_:true () in
  let sim = Sim.mount_with_props ~props filter_popover.component in
  let anchors =
    List.filter_map
      (function
        | Action.Anchor { anchor; popup; place } ->
            Some (Ref.name anchor, Ref.name popup, place.side)
        | _ ->
            None
        )
      (Machine.held_with_props filter_popover.component.machine props
         (Sim.snapshot sim)
      )
  in
  Alcotest.(check bool)
    "the popup is anchored to the trigger, on the side from the props" true
    (anchors = [ ("filter-trigger", "filter-popup", Place.Top) ])

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "silica-ui popover"
    [
      ( "popover",
        [
          t "closed at start" closed_at_start;
          t "the trigger opens it and focus moves in" trigger_opens;
          t "the trigger closes it" trigger_closes;
          t "Escape closes it and focuses the trigger" escape_closes;
          t "an outside press closes it and keeps focus" outside_press_closes;
          t "the close part and the content close it" close_part_and_content;
          t "the parent sets the open state" parent_sets_open;
          t "the anchor takes the place from the props" anchor_uses_props;
        ]
      );
    ]
