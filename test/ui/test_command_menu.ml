open Silica
open Silica_test_support
open Combobox_samples

let opt = Alcotest.(option string)
let strings = Alcotest.(list string)
let attr sim id name = Sim.attribute sim ~id name
let ctrl_k = Key.make ~ctrl:true "k"
let input = "menu-search-input"
let held_names sim = List.map snd (Sim.held sim)

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let log = Sim.html

let has_log sim line =
  if not (contains (log sim) line) then
    Alcotest.failf "page has no %S in %s" line (log sim)

let is_open sim = Sim.exists sim ~id:"menu-popup"

let closed_at_start () =
  let sim = Sim.mount menu_page in
  Alcotest.(check bool) "closed" false (is_open sim);
  Alcotest.check strings "only the shortcut" [ "keys" ] (held_names sim)

let shortcut_opens () =
  let sim = Sim.mount menu_page in
  Alcotest.(check bool) "Ctrl+K bound" true (Sim.press_key sim ctrl_k);
  Alcotest.(check bool) "open" true (is_open sim);
  Alcotest.check opt "a dialog" (Some "dialog") (attr sim "menu-popup" "role");
  Alcotest.check opt "named by the title" (Some "menu-title")
    (attr sim "menu-popup" "aria-labelledby");
  Alcotest.check opt "the search input" (Some "combobox") (attr sim input "role");
  Alcotest.check opt "named" (Some "Command menu") (attr sim input "aria-label");
  Alcotest.check opt "empty text" (Some "") (attr sim input "value");
  Alcotest.check opt "first highlighted" (Some "menu-search-option-0")
    (attr sim input "aria-activedescendant");
  Alcotest.check strings "shortcut, trap, lock, dismiss"
    [ "keys"; "focus_trap"; "scroll_lock"; "dismiss" ]
    (held_names sim);
  Alcotest.(check bool)
    "no open change for the parent's request" false
    (contains (log sim) "open true")

let meta_k_opens () =
  let sim = Sim.mount menu_page in
  Alcotest.(check bool)
    "Meta+K bound" true
    (Sim.press_key sim (Key.make ~meta:true "k"));
  Alcotest.(check bool) "open" true (is_open sim)

let enter_runs () =
  let sim = Sim.mount menu_page in
  ignore (Sim.press_key sim ctrl_k);
  Sim.input sim ~label:"Command menu" "sign";
  Alcotest.check opt "the match is highlighted" (Some "menu-search-option-2")
    (attr sim input "aria-activedescendant");
  Alcotest.(check bool) "Enter used" true (Sim.key_down sim ~id:input Key.enter);
  Alcotest.(check bool) "closed" false (is_open sim);
  has_log sim "run sign-out; open false close-press";
  Alcotest.check strings "only the shortcut" [ "keys" ] (held_names sim)

let press_runs () =
  let sim = Sim.mount menu_page in
  ignore (Sim.press_key sim ctrl_k);
  Sim.press_id sim ~id:"menu-search-option-1";
  Alcotest.(check bool) "closed" false (is_open sim);
  has_log sim "run new-issue"

let escape_clears_then_closes () =
  let sim = Sim.mount menu_page in
  ignore (Sim.press_key sim ctrl_k);
  Sim.input sim ~label:"Command menu" "new";
  Alcotest.(check bool)
    "Escape with text used by the input" true
    (Sim.key_down sim ~id:input Key.escape);
  Alcotest.(check bool) "still open" true (is_open sim);
  Alcotest.check opt "cleared" (Some "") (attr sim input "value");
  Alcotest.(check bool)
    "Escape with no text not used by the input" false
    (Sim.key_down sim ~id:input Key.escape);
  Alcotest.(check bool) "the dialog takes it" true (Sim.escape sim);
  Alcotest.(check bool) "closed" false (is_open sim);
  has_log sim "open false escape-key"

let reopen_is_empty () =
  let sim = Sim.mount menu_page in
  ignore (Sim.press_key sim ctrl_k);
  Sim.input sim ~label:"Command menu" "zz";
  Alcotest.(check bool)
    "no option" false
    (Sim.exists sim ~id:"menu-search-option-0");
  Sim.outside_press sim;
  Alcotest.(check bool) "closed" false (is_open sim);
  has_log sim "open false outside-press";
  ignore (Sim.press_key sim ctrl_k);
  Alcotest.(check bool) "open again" true (is_open sim);
  Alcotest.check opt "empty text" (Some "") (attr sim input "value");
  Alcotest.(check bool)
    "all options" true
    (Sim.exists sim ~id:"menu-search-option-2")

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "silica-ui command menu"
    [
      ( "command menu",
        [
          t "closed at start" closed_at_start;
          t "Ctrl+K opens it" shortcut_opens;
          t "Meta+K opens it" meta_k_opens;
          t "Enter runs the highlighted option" enter_runs;
          t "a press runs an option" press_runs;
          t "Escape clears, then closes" escape_clears_then_closes;
          t "each open starts empty" reopen_is_empty;
        ]
      );
    ]
