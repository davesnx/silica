open Silica
open Silica_test_support
open Combobox_samples

let opt = Alcotest.(option string)
let strings = Alcotest.(list string)
let attr sim id name = Sim.attribute sim ~id name
let mount () = Sim.mount combobox_page

let key sim key = Sim.key_down sim ~id:"fruit-input" key
let shown sim =
  List.filter
    (fun i -> Sim.exists sim ~id:(Printf.sprintf "fruit-option-%d" i))
    [ 0; 1; 2; 3 ]
let shown_ids = Alcotest.(list int)
let highlighted sim = attr sim "fruit-input" "aria-activedescendant"
let source_names sim = List.map snd (Sim.sources sim)

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let has_log sim line =
  if not (contains (Sim.html sim) line) then
    Alcotest.failf "page has no %S in %s" line (Sim.html sim)

let closed_at_start () =
  let sim = mount () in
  Alcotest.check opt "role" (Some "combobox") (attr sim "fruit-input" "role");
  Alcotest.check opt "autocomplete" (Some "list")
    (attr sim "fruit-input" "aria-autocomplete");
  Alcotest.check opt "name" (Some "Fruit") (attr sim "fruit-input" "aria-label");
  Alcotest.check opt "collapsed" (Some "false")
    (attr sim "fruit-input" "aria-expanded");
  Alcotest.check opt "no highlight" None (highlighted sim);
  Alcotest.(check bool) "no list" false (Sim.exists sim ~id:"fruit-listbox");
  Alcotest.check strings "no sources" [] (source_names sim)

let typing_filters () =
  let sim = mount () in
  Sim.input sim ~label:"Fruit" "an";
  Alcotest.(check bool) "list shown" true (Sim.exists sim ~id:"fruit-listbox");
  Alcotest.check opt "role" (Some "listbox") (attr sim "fruit-listbox" "role");
  Alcotest.check opt "expanded" (Some "true")
    (attr sim "fruit-input" "aria-expanded");
  Alcotest.check opt "controls" (Some "fruit-listbox")
    (attr sim "fruit-input" "aria-controls");
  Alcotest.check opt "text" (Some "an") (attr sim "fruit-input" "value");
  Alcotest.check shown_ids "label match" [ 1 ] (shown sim);
  Alcotest.check opt "option role" (Some "option")
    (attr sim "fruit-option-1" "role");
  Alcotest.check opt "nothing highlighted after typing" None (highlighted sim);
  Sim.input sim ~label:"Fruit" "VINE";
  Alcotest.check shown_ids "keyword match, any case" [ 3 ] (shown sim);
  Sim.input sim ~label:"Fruit" "";
  Alcotest.check shown_ids "all" [ 0; 1; 2; 3 ] (shown sim);
  Alcotest.check opt "disabled" (Some "true")
    (attr sim "fruit-option-2" "aria-disabled");
  Alcotest.check strings "dismiss and anchor" [ "dismiss"; "anchor" ]
    (source_names sim)

let empty_status () =
  let sim = mount () in
  Sim.input sim ~label:"Fruit" "zz";
  Alcotest.check shown_ids "no option" [] (shown sim);
  Alcotest.check opt "status" (Some "status") (attr sim "fruit-status" "role");
  Sim.input sim ~label:"Pick" "zz";
  Alcotest.check opt "nothing highlighted" None
    (attr sim "pick-input" "aria-activedescendant");
  has_log sim "<div id=\"pick-status\" role=\"status\">No results</div>"

let arrows_move () =
  let sim = mount () in
  Alcotest.(check bool) "ArrowDown handled" true (key sim Key.arrow_down);
  Alcotest.check opt "opened on the first" (Some "fruit-option-0")
    (highlighted sim);
  Alcotest.check opt "data-highlighted" (Some "")
    (attr sim "fruit-option-0" "data-highlighted");
  Alcotest.(check bool)
    "scrolled" true
    (List.mem "scroll fruit-option-0" (Sim.log sim));
  ignore (key sim Key.arrow_down);
  Alcotest.check opt "second" (Some "fruit-option-1") (highlighted sim);
  ignore (key sim Key.arrow_down);
  Alcotest.check opt "the disabled one is skipped" (Some "fruit-option-3")
    (highlighted sim);
  Alcotest.check opt "no longer highlighted" None
    (attr sim "fruit-option-1" "data-highlighted");
  ignore (key sim Key.arrow_down);
  Alcotest.check opt "wraps to the first" (Some "fruit-option-0")
    (highlighted sim);
  ignore (key sim Key.arrow_up);
  Alcotest.check opt "ArrowUp wraps to the last" (Some "fruit-option-3")
    (highlighted sim);
  Sim.pointer_enter sim ~id:"fruit-option-1";
  Alcotest.check opt "the pointer highlights" (Some "fruit-option-1")
    (highlighted sim)

let arrow_up_opens_on_last () =
  let sim = mount () in
  ignore (key sim Key.arrow_up);
  Alcotest.check opt "last" (Some "fruit-option-3") (highlighted sim)

let alt_down_opens () =
  let sim = mount () in
  Alcotest.(check bool) "handled" true (key sim (Key.make ~alt:true "ArrowDown"));
  Alcotest.(check bool) "open" true (Sim.exists sim ~id:"fruit-listbox");
  Alcotest.check opt "nothing highlighted" None (highlighted sim)

let enter_chooses () =
  let sim = mount () in
  Alcotest.(check bool)
    "Enter with the list closed is not used" false (key sim Key.enter);
  Sim.input sim ~label:"Fruit" "gr";
  Alcotest.(check bool)
    "Enter with no highlight is not used" false (key sim Key.enter);
  ignore (key sim Key.arrow_down);
  Alcotest.(check bool) "Enter handled" true (key sim Key.enter);
  Alcotest.check opt "the label is the text" (Some "Grape")
    (attr sim "fruit-input" "value");
  Alcotest.(check bool) "closed" false (Sim.exists sim ~id:"fruit-listbox");
  Alcotest.check opt "no highlight when closed" None (highlighted sim);
  has_log sim "value grape";
  ignore (key sim (Key.make ~alt:true "ArrowDown"));
  Alcotest.check opt "selected" (Some "true")
    (attr sim "fruit-option-3" "aria-selected")

let press_chooses () =
  let sim = mount () in
  ignore (key sim Key.arrow_down);
  Sim.press_id sim ~id:"fruit-option-1";
  Alcotest.check opt "text" (Some "Banana") (attr sim "fruit-input" "value");
  Alcotest.(check bool) "closed" false (Sim.exists sim ~id:"fruit-listbox");
  Alcotest.(check bool)
    "focus stays in the input" true
    (List.mem "focus fruit-input" (Sim.log sim));
  has_log sim "value banana"

let escape_closes_then_clears () =
  let sim = mount () in
  Sim.input sim ~label:"Fruit" "a";
  Alcotest.(check bool) "first Escape used" true (key sim Key.escape);
  Alcotest.(check bool) "closed" false (Sim.exists sim ~id:"fruit-listbox");
  Alcotest.check opt "text kept" (Some "a") (attr sim "fruit-input" "value");
  Alcotest.(check bool) "second Escape used" true (key sim Key.escape);
  Alcotest.check opt "cleared" (Some "") (attr sim "fruit-input" "value");
  Alcotest.(check bool) "third Escape not used" false (key sim Key.escape)

let outside_press_closes () =
  let sim = mount () in
  Sim.input sim ~label:"Fruit" "a";
  Sim.outside_press sim;
  Alcotest.(check bool) "closed" false (Sim.exists sim ~id:"fruit-listbox");
  Alcotest.check strings "sources stopped" [] (source_names sim);
  Alcotest.check opt "text kept" (Some "a") (attr sim "fruit-input" "value")

let tab_closes () =
  let sim = mount () in
  Alcotest.(check bool)
    "Tab with the list closed is not used" false (key sim Key.tab);
  Sim.input sim ~label:"Fruit" "a";
  ignore (key sim Key.arrow_down);
  Alcotest.(check bool) "Tab used" true (key sim Key.tab);
  Alcotest.(check bool) "closed" false (Sim.exists sim ~id:"fruit-listbox");
  Alcotest.check strings "sources stopped" [] (source_names sim);
  Alcotest.check opt "text kept" (Some "a") (attr sim "fruit-input" "value");
  Alcotest.(check bool)
    "the highlighted option is not chosen" false
    (contains (Sim.html sim) "value apple")

let home_end_move () =
  let sim = mount () in
  Alcotest.(check bool)
    "Home with the list closed is not used" false
    (key sim (Key.make "Home"));
  Sim.input sim ~label:"Fruit" "";
  Alcotest.(check bool)
    "End with no highlight is not used (caret)" false
    (key sim (Key.make "End"));
  ignore (key sim Key.arrow_down);
  Alcotest.(check bool) "End used" true (key sim (Key.make "End"));
  Alcotest.check opt "last enabled" (Some "fruit-option-3") (highlighted sim);
  Alcotest.(check bool)
    "scrolled" true
    (List.mem "scroll fruit-option-3" (Sim.log sim));
  Alcotest.(check bool) "Home used" true (key sim (Key.make "Home"));
  Alcotest.check opt "first enabled" (Some "fruit-option-0") (highlighted sim)

let inline_home_end () =
  let sim = mount () in
  let ikey k = Sim.key_down sim ~id:"pick-input" k in
  let hl () = attr sim "pick-input" "aria-activedescendant" in
  Alcotest.(check bool) "End used" true (ikey (Key.make "End"));
  Alcotest.check opt "last" (Some "pick-option-3") (hl ());
  Alcotest.(check bool) "Home used" true (ikey (Key.make "Home"));
  Alcotest.check opt "first" (Some "pick-option-0") (hl ());
  Alcotest.(check bool) "Tab not used inline" false (ikey Key.tab)

let inline_mode () =
  let sim = mount () in
  let ikey k = Sim.key_down sim ~id:"pick-input" k in
  let hl () = attr sim "pick-input" "aria-activedescendant" in
  Alcotest.(check bool) "list shown" true (Sim.exists sim ~id:"pick-listbox");
  Alcotest.check opt "expanded" (Some "true")
    (attr sim "pick-input" "aria-expanded");
  Alcotest.check opt "first highlighted" (Some "pick-option-0") (hl ());
  Sim.input sim ~label:"Pick" "e";
  Alcotest.check opt "first match highlighted" (Some "pick-option-0") (hl ());
  ignore (ikey Key.arrow_down);
  Alcotest.check opt "disabled skipped" (Some "pick-option-3") (hl ());
  Sim.input sim ~label:"Pick" "gr";
  Alcotest.check opt "the new first match" (Some "pick-option-3") (hl ());
  Alcotest.(check bool) "Escape with text used" true (ikey Key.escape);
  Alcotest.check opt "cleared" (Some "") (attr sim "pick-input" "value");
  Alcotest.(check bool) "Escape with no text not used" false (ikey Key.escape);
  Alcotest.(check bool) "still shown" true (Sim.exists sim ~id:"pick-listbox");
  Alcotest.(check bool) "Enter chooses" true (ikey Key.enter);
  has_log sim "value apple";
  Alcotest.(check bool)
    "still shown after a choice" true
    (Sim.exists sim ~id:"pick-listbox");
  Alcotest.check strings "no sources" [] (source_names sim)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "silica-ui combobox"
    [
      ( "combobox",
        [
          t "closed at start" closed_at_start;
          t "typing filters and opens" typing_filters;
          t "an empty result shows the status" empty_status;
          t "arrows move the highlight" arrows_move;
          t "ArrowUp opens on the last" arrow_up_opens_on_last;
          t "Alt+ArrowDown opens" alt_down_opens;
          t "Enter chooses" enter_chooses;
          t "a press chooses" press_chooses;
          t "Escape closes, then clears" escape_closes_then_clears;
          t "an outside press closes" outside_press_closes;
          t "Tab closes and keeps the text" tab_closes;
          t "Home and End move to the ends" home_end_move;
          t "inline mode" inline_mode;
          t "inline Home and End" inline_home_end;
        ]
      );
    ]
