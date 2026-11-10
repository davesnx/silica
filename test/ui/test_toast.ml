open Silica
open Silica_test_support
open Silica_ui
open Ui_samples.Toast_samples

let opt = Alcotest.(option string)
let attr sim id name = Sim.attribute sim ~id name
let toast k = "note-toast-" ^ k

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let log sim =
  match String.split_on_char '>' (Sim.html sim) with _ -> Sim.html sim

let has_log sim line =
  if not (contains (log sim) line) then
    Alcotest.failf "log has no %S in %s" line (log sim)

let timers sim =
  List.filter
    (fun id -> not (List.mem_assoc id (Sim.sources sim)))
    (Sim.running sim)

let tick sim =
  match timers sim with
  | [ id ] ->
      Sim.fire sim id
  | ids ->
      Alcotest.failf "expected one timer, got [%s]" (String.concat "; " ids)

let ticks sim n =
  for _ = 1 to n do
    tick sim
  done

let empty_at_start () =
  let sim = Sim.mount page in
  Alcotest.check opt "region" (Some "region") (attr sim "note" "role");
  Alcotest.check opt "live" (Some "polite") (attr sim "note" "aria-live");
  Alcotest.check opt "name" (Some "Notifications") (attr sim "note" "aria-label");
  Alcotest.check opt "tabindex" (Some "-1") (attr sim "note" "tabindex");
  Alcotest.(check (list string)) "no timer" [] (timers sim)

let add_shows () =
  let sim = Sim.mount page in
  Sim.send sim (Add Toast.Low);
  Alcotest.check opt "dialog" (Some "dialog") (attr sim (toast "t1") "role");
  Alcotest.check opt "not modal" (Some "false")
    (attr sim (toast "t1") "aria-modal");
  Alcotest.check opt "labelled"
    (Some (toast "t1" ^ "-title"))
    (attr sim (toast "t1") "aria-labelledby");
  Alcotest.check opt "described"
    (Some (toast "t1" ^ "-description"))
    (attr sim (toast "t1") "aria-describedby");
  Alcotest.(check bool) "title painted" true (contains (Sim.html sim) "Saved t1");
  Alcotest.check opt "close name" (Some "Close")
    (attr sim (toast "t1" ^ "-close") "aria-label");
  Alcotest.(check int) "the time runs" 1 (List.length (timers sim));
  Sim.send sim (Add Toast.High);
  Alcotest.check opt "high priority" (Some "alertdialog")
    (attr sim (toast "t2") "role")

let close_button () =
  let sim = Sim.mount page in
  Sim.send sim (Add Toast.Low);
  Sim.press_id sim ~id:(toast "t1" ^ "-close");
  has_log sim "close t1";
  Alcotest.(check bool) "removed" false (Sim.exists sim ~id:(toast "t1"));
  Alcotest.(check (list string)) "no timer for an empty list" [] (timers sim)

let escape_closes () =
  let sim = Sim.mount page in
  Sim.send sim (Add Toast.Low);
  Sim.send sim (Add Toast.Low);
  Alcotest.(check bool)
    "Escape taken" true
    (Sim.key_down sim ~id:(toast "t2" ^ "-title") Key.escape);
  has_log sim "close t2";
  Alcotest.(check bool) "t2 removed" false (Sim.exists sim ~id:(toast "t2"));
  Alcotest.(check bool) "t1 stays" true (Sim.exists sim ~id:(toast "t1"))

let timeout_closes_oldest () =
  let sim = Sim.mount page in
  Sim.send sim (Add Toast.Low);
  Sim.send sim (Add Toast.Low);
  ticks sim 2;
  Alcotest.(check bool) "not yet" true (Sim.exists sim ~id:(toast "t1"));
  tick sim;
  has_log sim "close t1";
  Alcotest.(check bool) "oldest removed" false (Sim.exists sim ~id:(toast "t1"));
  Alcotest.(check bool) "newer stays" true (Sim.exists sim ~id:(toast "t2"));
  ticks sim 2;
  Alcotest.(check bool)
    "the time started again" true
    (Sim.exists sim ~id:(toast "t2"));
  tick sim;
  has_log sim "close t1; close t2"

let new_list_restarts () =
  let sim = Sim.mount page in
  Sim.send sim (Add Toast.Low);
  ticks sim 2;
  Sim.send sim (Add Toast.Low);
  ticks sim 2;
  Alcotest.(check bool)
    "the new list started the time again" false
    (contains (log sim) "close t1");
  tick sim;
  has_log sim "close t1"

let hover_pauses () =
  let sim = Sim.mount page in
  Sim.send sim (Add Toast.Low);
  ticks sim 2;
  Sim.pointer_enter sim ~id:(toast "t1");
  Alcotest.(check (list string)) "paused" [] (timers sim);
  Alcotest.check opt "data-paused" (Some "") (attr sim "note" "data-paused");
  Sim.pointer_leave sim ~id:(toast "t1");
  ticks sim 2;
  Alcotest.(check bool)
    "full time again after leave" true
    (Sim.exists sim ~id:(toast "t1"));
  tick sim;
  has_log sim "close t1"

let focus_pauses () =
  let sim = Sim.mount page in
  Sim.send sim (Add Toast.Low);
  Sim.focus sim ~id:(toast "t1");
  Alcotest.(check (list string)) "paused" [] (timers sim);
  Sim.pointer_enter sim ~id:(toast "t1");
  Sim.blur sim ~id:(toast "t1");
  Alcotest.(check (list string)) "still hovered" [] (timers sim);
  Sim.pointer_leave sim ~id:(toast "t1");
  ticks sim 3;
  has_log sim "close t1"

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "silica-ui toast"
    [
      ( "toast",
        [
          t "an empty region at start" empty_at_start;
          t "a toast shows" add_shows;
          t "the close button closes it" close_button;
          t "Escape closes the toast with focus" escape_closes;
          t "the time closes the oldest" timeout_closes_oldest;
          t "a new list starts the time again" new_list_restarts;
          t "hover pauses" hover_pauses;
          t "focus pauses" focus_pauses;
        ]
      );
    ]
