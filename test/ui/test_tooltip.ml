open Silica_test_support
open Silica_ui
open Tooltip_samples

let opt = Alcotest.(option string)
let strings = Alcotest.(list string)

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let log = Sim.html

let has_log sim line =
  if not (contains (log sim) line) then
    Alcotest.failf "log has no %S in %s" line (log sim)

let no_log sim text =
  if contains (log sim) text then
    Alcotest.failf "log has %S in %s" text (log sim)

let timers sim = List.filter (contains "/after") (Sim.running sim)

let raises f = match f () with exception Failure _ -> true | () -> false

let shown sim = Sim.exists sim ~id:"save-popup"
let source_names sim = List.map snd (Sim.sources sim)

let ends_with suffix s =
  let n = String.length suffix and l = String.length s in
  l >= n && String.sub s (l - n) n = suffix

let fire sim state delay =
  let index =
    match delay with
    | Tooltip.Instant ->
        0
    | Short ->
        1
    | Medium ->
        2
    | Long ->
        3
  in
  let suffix = Printf.sprintf "/%s/after%d" state index in
  match List.find_opt (ends_with suffix) (Sim.running sim) with
  | Some id ->
      Sim.fire sim id
  | None ->
      Alcotest.failf "no running timer %s in [%s]" suffix
        (String.concat "; " (Sim.running sim))

let closed_at_start () =
  let sim = Sim.mount (page ()) in
  Alcotest.check opt "trigger" (Some "button")
    (Sim.attribute sim ~id:"save-trigger" "type");
  Alcotest.check opt "not described" None
    (Sim.attribute sim ~id:"save-trigger" "aria-describedby");
  Alcotest.(check bool) "no popup" false (shown sim);
  Alcotest.check strings "no timers" [] (timers sim);
  Alcotest.check strings "no sources" [] (source_names sim)

let hover_waits_for_the_delay () =
  let sim = Sim.mount (page ()) in
  Sim.pointer_enter sim ~id:"save-trigger";
  Alcotest.(check bool) "not shown before the delay" false (shown sim);
  fire sim "opening" Tooltip.Short;
  Alcotest.(check bool) "a shorter timer does not open it" false (shown sim);
  fire sim "opening" Tooltip.Medium;
  Alcotest.check opt "role" (Some "tooltip")
    (Sim.attribute sim ~id:"save-popup" "role");
  Alcotest.check opt "not focusable" None
    (Sim.attribute sim ~id:"save-popup" "tabindex");
  Alcotest.check opt "no style from the view" None
    (Sim.attribute sim ~id:"save-popup" "style");
  Alcotest.check opt "described by the popup" (Some "save-popup")
    (Sim.attribute sim ~id:"save-trigger" "aria-describedby");
  Alcotest.check opt "data-popup-open" (Some "")
    (Sim.attribute sim ~id:"save-trigger" "data-popup-open");
  Alcotest.check strings "dismiss and anchor" [ "dismiss"; "anchor" ]
    (source_names sim);
  Alcotest.check strings "no timers while open" [] (timers sim);
  Alcotest.(check bool)
    "focus does not move" false
    (List.exists (fun line -> contains line "focus") (Sim.log sim));
  has_log sim "open true trigger-hover"

let leave_before_the_delay () =
  let sim = Sim.mount (page ()) in
  Sim.pointer_enter sim ~id:"save-trigger";
  Sim.pointer_leave sim ~id:"save-trigger";
  Alcotest.(check bool) "not shown" false (shown sim);
  Alcotest.check strings "timers stopped" [] (timers sim);
  no_log sim "open"

let leave_closes_at_once () =
  let sim = Sim.mount (page ~delay:Tooltip.Instant ()) in
  Sim.pointer_enter sim ~id:"save-trigger";
  Alcotest.(check bool) "an instant delay opens at once" true (shown sim);
  Sim.pointer_leave sim ~id:"save-trigger";
  Alcotest.(check bool) "closed at once" false (shown sim);
  has_log sim "open true trigger-hover; open false trigger-hover"

let hoverable_while_closing () =
  let sim =
    Sim.mount (page ~delay:Tooltip.Instant ~close_delay:Tooltip.Short ())
  in
  Sim.pointer_enter sim ~id:"save-trigger";
  Sim.pointer_leave sim ~id:"save-trigger";
  Alcotest.(check bool) "still shown while it closes" true (shown sim);
  Sim.pointer_enter sim ~id:"save-popup";
  Alcotest.check strings "the close timer stopped" [] (timers sim);
  Sim.pointer_leave sim ~id:"save-popup";
  Alcotest.(check bool) "still shown" true (shown sim);
  fire sim "closing" Tooltip.Short;
  Alcotest.(check bool) "closed after the close delay" false (shown sim);
  has_log sim "open true trigger-hover; open false trigger-hover"

let focus_and_blur () =
  let sim = Sim.mount (page ()) in
  Sim.focus sim ~id:"save-trigger";
  Alcotest.(check bool) "shown at once" true (shown sim);
  Sim.blur sim ~id:"save-trigger";
  Alcotest.(check bool) "hidden" false (shown sim);
  has_log sim "open true trigger-focus; open false trigger-focus"

let focus_while_opening () =
  let sim = Sim.mount (page ()) in
  Sim.pointer_enter sim ~id:"save-trigger";
  Alcotest.(check bool)
    "focus during the delay is not an event: a press focuses the trigger and \
     must not flash the tooltip"
    true
    (raises (fun () -> Sim.focus sim ~id:"save-trigger"));
  Sim.press_id sim ~id:"save-trigger";
  Alcotest.check strings "the press stops the wait" [] (timers sim);
  no_log sim "open"

let escape_closes () =
  let sim = Sim.mount (page ()) in
  Sim.focus sim ~id:"save-trigger";
  Sim.clear_log sim;
  Alcotest.(check bool) "handled" true (Sim.escape sim);
  Alcotest.(check bool) "hidden" false (shown sim);
  has_log sim "open false escape-key";
  Alcotest.(check bool)
    "focus stays on the trigger: no focus command" false
    (List.exists (fun line -> contains line "focus") (Sim.log sim))

let outside_press_keeps_it () =
  let sim = Sim.mount (page ()) in
  Sim.focus sim ~id:"save-trigger";
  Alcotest.(check bool)
    "no layer takes an outside press" true
    (raises (fun () -> Sim.outside_press sim));
  Alcotest.(check bool) "still shown" true (shown sim)

let press_closes () =
  let sim = Sim.mount (page ()) in
  Sim.focus sim ~id:"save-trigger";
  Sim.press_id sim ~id:"save-trigger";
  Alcotest.(check bool) "hidden" false (shown sim);
  has_log sim "open false trigger-press";
  let sim = Sim.mount (page ()) in
  Sim.pointer_enter sim ~id:"save-trigger";
  Sim.press_id sim ~id:"save-trigger";
  Alcotest.check strings "a press before the delay stops it" [] (timers sim);
  no_log sim "open"

let delay_change_restarts () =
  let sim = Sim.mount (page ()) in
  Sim.pointer_enter sim ~id:"save-trigger";
  Sim.send sim (Set_delay Tooltip.Instant);
  fire sim "opening" Tooltip.Instant;
  Alcotest.(check bool) "opened by the new delay" true (shown sim);
  has_log sim "open true trigger-hover"

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "silica-ui tooltip"
    [
      ( "tooltip",
        [
          t "closed at start" closed_at_start;
          t "hover opens it after the delay" hover_waits_for_the_delay;
          t "leaving before the delay cancels it" leave_before_the_delay;
          t "with no close delay, leaving closes it at once"
            leave_closes_at_once;
          t "the popup keeps it open while it closes" hoverable_while_closing;
          t "focus opens it, blur closes it" focus_and_blur;
          t "a mouse press during the delay shows nothing" focus_while_opening;
          t "Escape closes it" escape_closes;
          t "an outside press does not close it" outside_press_keeps_it;
          t "a press on the trigger closes it" press_closes;
          t "a new delay restarts the wait" delay_change_restarts;
        ]
      );
    ]
