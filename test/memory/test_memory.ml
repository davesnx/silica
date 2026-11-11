open Silica
module Memory = Silica_memory

let string = Alcotest.(check string)
let int = Alcotest.(check int)
let bool = Alcotest.(check bool)
let text t id = Memory.text (Memory.by_id t id)
let press t id = Memory.press (Memory.by_id t id)

let h1 t =
  match List.filter (fun e -> Memory.tag e = "h1") (Memory.all t) with
  | e :: _ ->
      Memory.text e
  | [] ->
      Alcotest.fail "no h1"

let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec at i =
    i + n <= h && (String.sub haystack i n = needle || at (i + 1))
  in
  at 0

let only = function [ x ] -> x | l -> Alcotest.failf "%d items" (List.length l)

let basic () = Memory.mount Test_apps.component

let press_re_renders () =
  let t = basic () in
  press t "inc";
  press t "inc";
  string "count" "2" (text t "count")

let input_delivers_the_value () =
  let t = basic () in
  Memory.input (Memory.by_id t "input") "hello";
  string "typed" "hello" (text t "typed");
  Alcotest.(check (option string))
    "value" (Some "hello")
    (Memory.attribute (Memory.by_id t "input") "value")

let after_fires_later () =
  let t = basic () in
  press t "ask";
  string "ticks" "0" (text t "ticks");
  Memory.advance t 4;
  string "not yet" "0" (text t "ticks");
  Memory.advance t 1;
  string "tick" "1" (text t "ticks")

let call_success () =
  let t = basic () in
  press t "ask";
  let request = only (Memory.pending t) in
  Memory.reply request "42";
  string "answer" "ok 42" (text t "answer");
  Alcotest.(check (list (pair string string)))
    "wire request"
    [ ("/api/double", "21") ]
    (List.map (fun r -> (Memory.path r, Memory.body r)) (Memory.requests t))

let call_errors () =
  let t = basic () in
  press t "ask";
  Memory.reply ~status:500 (only (Memory.pending t)) "boom";
  string "http" "http 500" (text t "answer");
  press t "ask";
  Memory.reply (only (Memory.pending t)) "{nope";
  string "decode" "decode" (text t "answer");
  press t "ask";
  Memory.network_error (only (Memory.pending t)) "offline";
  string "network" "network" (text t "answer")

let call_timeout () =
  let t = basic () in
  press t "ask-timed";
  let request = only (Memory.pending t) in
  string "nothing yet" "" (text t "answer");
  Memory.advance t 40;
  string "timeout" "timeout" (text t "answer");
  Memory.reply request "42";
  Memory.advance t 40;
  string "late answer dropped" "timeout" (text t "answer")

let answer_before_timeout () =
  let t = basic () in
  press t "ask-timed";
  Memory.reply (only (Memory.pending t)) "7";
  string "answer" "ok 7" (text t "answer");
  Memory.advance t 80;
  string "no timeout" "ok 7" (text t "answer")

let timer = Test_apps.timer_component

let timer_after_exit () =
  let t = Memory.mount timer in
  press t "leave";
  string "left" "left" (text t "phase");
  Memory.advance t 80;
  string "still left" "left" (text t "phase")

let timer_reentered () =
  let t = Memory.mount timer in
  press t "leave";
  press t "back";
  string "waiting" "waiting" (text t "phase");
  Alcotest.(check (list int)) "one timer" [ 20 ] (Memory.timers t);
  Memory.advance t 20;
  string "fired" "fired" (text t "phase");
  Memory.advance t 60;
  string "still fired" "fired" (text t "phase")

let stopped_call_dropped () =
  let t = basic () in
  press t "ask";
  press t "cancel-ask";
  Memory.reply (only (Memory.pending t)) "42";
  Memory.advance t 60;
  string "answer" "" (text t "answer");
  string "ticks" "0" (text t "ticks")

let call_again () =
  let t = basic () in
  press t "ask";
  press t "ask";
  match Memory.pending t with
  | [ first; second ] ->
      Memory.reply first "1";
      Memory.advance t 40;
      string "first dropped" "" (text t "answer");
      Memory.reply second "2";
      string "second" "ok 2" (text t "answer");
      string "ticks" "1" (text t "ticks")
  | l ->
      Alcotest.failf "%d requests" (List.length l)

let router url = Memory.mount_app ~url Test_apps.app

let link_press () =
  let t = router "/home" in
  bool "home" true (contains (Memory.html t) "home");
  string "title" "Home" (Memory.title t);
  let before = List.length (Memory.history t) in
  press t "to-about";
  string "url" "/about" (Memory.url t);
  int "pushed" (before + 1) (List.length (Memory.history t));
  string "h1" "about" (h1 t);
  string "title" "About" (Memory.title t)

let navigate_action () =
  let t = router "/home" in
  press t "action-nav";
  string "url" "/about" (Memory.url t);
  string "h1" "about" (h1 t)

let back_and_forward () =
  let t = router "/home" in
  press t "to-item";
  string "item" "item 7" (h1 t);
  Memory.back t;
  string "back" "home" (h1 t);
  string "url" "/home" (Memory.url t);
  string "title" "Home" (Memory.title t);
  Memory.forward t;
  string "forward" "item 7" (h1 t);
  string "title" "Item 7" (Memory.title t)

let not_found () =
  let t = router "/nowhere" in
  string "h1" "404" (h1 t);
  string "title" "Not found" (Memory.title t)

let strings = Alcotest.(check (list string))
let data_requests t =
  List.filter (fun r -> Memory.path r = "/api/data") (Memory.requests t)

let preload_on_hover_then_press_uses_it () =
  let t = router "/home" in
  Memory.hover (Memory.by_id t "to-data");
  let preload = only (data_requests t) in
  string "body" "\"7\"" (Memory.body preload);
  string "the page does not change" "/home" (Memory.url t);
  Memory.hover (Memory.by_id t "to-data");
  int "a second hover sends nothing" 1 (List.length (data_requests t));
  Memory.reply preload "\"seven\"";
  press t "to-data";
  string "url" "/data/7" (Memory.url t);
  string "the kept answer, no request" "seven" (text t "data");
  int "one request" 1 (List.length (data_requests t))

let call_waits_for_preload_in_flight () =
  let t = router "/home" in
  Memory.focus (Memory.by_id t "to-data");
  let preload = only (data_requests t) in
  press t "to-data";
  string "loading" "loading" (text t "data");
  int "the call waits" 1 (List.length (data_requests t));
  Memory.reply preload "\"seven\"";
  string "answer" "seven" (text t "data")

let preload_kept_for_one_use () =
  let t = router "/home" in
  Memory.hover (Memory.by_id t "to-data");
  Memory.reply (only (data_requests t)) "\"seven\"";
  press t "to-data";
  string "first use" "seven" (text t "data");
  Memory.back t;
  press t "to-data";
  int "a second call sends its own request" 2 (List.length (data_requests t))

let preload_expires () =
  let t = router "/home" in
  Memory.hover (Memory.by_id t "to-data");
  Memory.reply (only (data_requests t)) "\"seven\"";
  Memory.advance t 29_999;
  Memory.hover (Memory.by_id t "to-data");
  int "still kept" 1 (List.length (data_requests t));
  Memory.advance t 1;
  press t "to-data";
  int "expired after 30 s: a new request" 2 (List.length (data_requests t))

let failed_preload_not_kept () =
  let t = router "/home" in
  Memory.hover (Memory.by_id t "to-data");
  Memory.reply ~status:500 (only (data_requests t)) "boom";
  press t "to-data";
  int "a non-200 preload is not used" 2 (List.length (data_requests t));
  Memory.back t;
  Memory.hover (Memory.by_id t "to-data");
  Memory.network_error (List.nth (data_requests t) 2) "offline";
  press t "to-data";
  int "a failed preload is not used" 4 (List.length (data_requests t))

let waiting_call_after_failed_preload () =
  let t = router "/home" in
  Memory.hover (Memory.by_id t "to-data");
  press t "to-data";
  Memory.reply ~status:503 (only (data_requests t)) "busy";
  string "still loading" "loading" (text t "data");
  let own = only (Memory.pending t) in
  Memory.reply own "\"fresh\"";
  string "its own answer" "fresh" (text t "data")

let hover_without_preload_attribute () =
  let t = router "/home" in
  Memory.hover (Memory.by_id t "to-about");
  Memory.hover (Memory.by_id t "to-data-viewport");
  int "no request" 0 (List.length (Memory.requests t))

let edit t =
  press t "to-edit";
  Memory.input (Memory.by_id t "draft") "draft";
  string "dirty" "dirty" (text t "edit-status");
  t

let block_stops_link_press () =
  let t = edit (router "/home") in
  bool "leaving asks" true (Memory.leave_asks t);
  press t "edit-about";
  string "a link press is stopped" "asking push /about" (text t "edit-status");
  string "no URL change" "/edit" (Memory.url t);
  press t "stay";
  Memory.back t;
  string "the URL stays" "/edit" (Memory.url t);
  string "the attempt" "asking traverse -1 /home" (text t "edit-status");
  int "the position stays" 1 (Memory.history_index t);
  press t "leave";
  string "resumed" "/home" (Memory.url t);
  int "back is still back" 0 (Memory.history_index t);
  strings "no new entry" [ "/home"; "/edit" ] (Memory.history t);
  bool "leaving does not ask" false (Memory.leave_asks t)

let block_stops_actions () =
  let t = edit (router "/home") in
  press t "edit-home";
  string "navigate stopped" "asking push /home" (text t "edit-status");
  string "url" "/edit" (Memory.url t);
  press t "stay";
  string "stay" "dirty" (text t "edit-status");
  strings "history" [ "/home"; "/edit" ] (Memory.history t);
  press t "edit-home";
  press t "leave";
  string "resumed push" "/home" (Memory.url t);
  strings "pushed" [ "/home"; "/edit"; "/home" ] (Memory.history t);
  Memory.back t;
  string "back to the editor, clean" "clean" (text t "edit-status");
  Memory.forward t;
  string "forward" "/home" (Memory.url t)

let guard_cancel_reverts () =
  let t = router "/home" in
  press t "to-locked";
  string "push reverted" "/home" (Memory.url t);
  int "back one entry" 0 (Memory.history_index t);
  string "page" "home" (h1 t);
  Memory.advance t 0;
  strings "a reverted push announces nothing" [] (Memory.announcements t);
  let t = router "/locked" in
  press t "locked-home";
  Memory.back t;
  string "back reverted" "/home" (Memory.url t);
  int "the position stays" 1 (Memory.history_index t);
  string "page stays" "home" (h1 t)

let focus_after_navigation () =
  let t = router "/home" in
  press t "to-about";
  bool "not before the step ends" true (Memory.announcements t = []);
  Memory.advance t 0;
  bool "focus on the route focus target" true
    ( match Memory.focused t with
    | Some e ->
        Memory.attribute e "data-silica-route-focus" = Some ""
    | None ->
        false
    );
  strings "announced" [ "About" ] (Memory.announcements t);
  Memory.back t;
  Memory.advance t 0;
  bool "no target: no element has focus" true (Memory.focused t = None);
  strings "announced" [ "About"; "Home" ] (Memory.announcements t);
  press t "to-about-replace";
  Memory.advance t 0;
  strings "a replace announces nothing" [ "About"; "Home" ]
    (Memory.announcements t)

let parent () = Memory.mount Test_apps.parent_component

let children t section =
  List.map
    (fun child ->
      let part cls =
        Memory.text (only (Memory.select ~within:child t "class" cls))
      in
      ( Option.get (Memory.attribute child "data-n"),
        part "child-clicks",
        part "child-prop"
      )
    )
    (Memory.select ~within:(Memory.by_id t section) t "class" "child")

let child_press t section =
  Memory.press
    (only
       (Memory.select ~within:(Memory.by_id t section) t "class" "child-press")
    )

let child_triple = Alcotest.(triple string string string)

let child_renders () =
  let t = parent () in
  let single () = only (children t "single") in
  Alcotest.check child_triple "initial" ("0", "0", "0") (single ());
  child_press t "single";
  child_press t "single";
  let _, clicks, _ = single () in
  string "clicks" "2" clicks;
  string "presses" "0" (text t "presses");
  press t "parent-press";
  string "presses" "1" (text t "presses");
  let _, clicks, _ = single () in
  string "clicks kept" "2" clicks

let props_event () =
  let t = parent () in
  child_press t "single";
  press t "prop-up";
  press t "prop-up";
  Alcotest.check child_triple "props" ("2", "1", "2")
    (only (children t "single"));
  press t "parent-press";
  let _, _, prop = only (children t "single") in
  string "prop" "2" prop

let output_reaches_parent () =
  let t = parent () in
  string "heard" "0" (text t "heard");
  child_press t "single";
  string "heard" "1" (text t "heard");
  child_press t "single";
  string "heard" "2" (text t "heard");
  string "presses" "0" (text t "presses")

let keyed_children () =
  let t = parent () in
  let press_kid n =
    let kid =
      only
        (List.filter
           (fun e -> Memory.attribute e "data-n" = Some n)
           (Memory.select ~within:(Memory.by_id t "kids") t "class" "child")
        )
    in
    Memory.press (only (Memory.select ~within:kid t "class" "child-press"))
  in
  press_kid "1";
  press_kid "1";
  press_kid "3";
  let pairs () =
    List.map (fun (n, clicks, _) -> (n, clicks)) (children t "kids")
  in
  let pairs_t = Alcotest.(list (pair string string)) in
  Alcotest.check pairs_t "before"
    [ ("1", "2"); ("2", "0"); ("3", "1") ]
    (pairs ());
  press t "reverse-children";
  Alcotest.check pairs_t "after"
    [ ("3", "1"); ("2", "0"); ("1", "2") ]
    (pairs ())

let independent_timers () =
  let t = Memory.mount Test_apps.ticker_host_component in
  let active () = List.length (List.filter (( = ) 20) (Memory.timers t)) in
  let tick label =
    int_of_string
      (Memory.text
         (only
            (List.filter
               (fun e -> Memory.attribute e "data-label" = Some label)
               (Memory.select t "class" "tick")
            )
         )
      )
  in
  let missing label =
    List.for_all
      (fun e -> Memory.attribute e "data-label" <> Some label)
      (Memory.select t "class" "tick")
  in
  int "two timers" 2 (active ());
  Memory.advance t 40;
  bool "both tick" true (tick "a" >= 2 && tick "b" >= 2);
  int "two timers" 2 (active ());
  press t "toggle-a";
  bool "a gone" true (missing "a");
  int "one timer" 1 (active ());
  let before = tick "b" in
  Memory.advance t 20;
  bool "b still ticks" true (tick "b" > before);
  int "one timer" 1 (active ());
  press t "toggle-b";
  int "no timer" 0 (active ());
  press t "toggle-a";
  int "a restarts" 0 (tick "a");
  int "one timer" 1 (active ());
  press t "toggle-a";
  int "no timer" 0 (active ())

let rec canonical json =
  match Silica_json.Json.classify json with
  | `Assoc fields ->
      Silica_json.Json.declassify
        (`Assoc
           (List.sort compare (List.map (fun (k, v) -> (k, canonical v)) fields))
        )
  | `List items ->
      Silica_json.Json.declassify (`List (List.map canonical items))
  | _ ->
      json

let json text =
  match Silica_json.Json.of_string text with
  | Ok value ->
      Silica_json.Json.to_string (canonical value)
  | Error message ->
      Alcotest.failf "bad JSON %s: %s" text message

let messages socket = List.map json (Memory.sent socket)

let kind socket t =
  List.length
    (List.filter
       (fun m -> contains m (Printf.sprintf {|"t":"%s"|} t))
       (messages socket)
    )

let welcome socket =
  Memory.socket_open socket;
  Memory.socket_message socket {|{"t":"hello","v":2}|}

let state socket seq value =
  Memory.socket_message socket
    (Printf.sprintf {|{"t":"state","seq":%d,"ack":0,"state":%d,"actions":[]}|}
       seq value
    )

let socket t n = List.nth (Memory.sockets t) n
let watch =
  { Component.machine = Test_apps.watch_machine; view = Test_apps.watch_view }
let fast = { Live_client.ping_ms = 30; pong_ms = 30 }

let topic_lifecycle () =
  let t = Memory.mount ~csrf:"tok-t" watch in
  let status () = text t "status" and updates () = text t "updates" in
  string "loading" "loading seq 0" (status ());
  string "updates" "1" (updates ());
  let ws = socket t 0 in
  Memory.socket_open ws;
  Alcotest.(check (list string))
    "hello"
    [
      json
        {|{"t":"hello","v":2,"csrf":"tok-t","sub":{"topic":"news","key":"room"}}|};
    ]
    (messages ws);
  Memory.socket_message ws {|{"t":"hello","v":2}|};
  state ws 3 30;
  string "ready" "ready 30 seq 3" (status ());
  state ws 3 31;
  state ws 2 32;
  string "old seq dropped" "ready 30 seq 3" (status ());
  Memory.socket_close ws 1006;
  string "stale" "stale 30 seq 0" (status ());
  string "updates" "3" (updates ());
  Memory.advance t 300;
  string "stale is delivered once per loss" "3" (updates ());
  welcome (socket t 1);
  state (socket t 1) 1 40;
  string "fresh" "ready 40 seq 1" (status ())

let backoff () =
  let t = Memory.mount watch in
  welcome (socket t 0);
  state (socket t 0) 1 1;
  Memory.socket_close (socket t 0) 1006;
  int "not at once" 1 (List.length (Memory.sockets t));
  Memory.advance t 300;
  int "reconnected" 2 (List.length (Memory.sockets t));
  welcome (socket t 1);
  state (socket t 1) 1 2;
  Memory.socket_close (socket t 1) 1006;
  Memory.advance t 170;
  int "attempt went back to 0 after a state" 3 (List.length (Memory.sockets t))

let session_gone () =
  let t = Memory.mount watch in
  welcome (socket t 0);
  state (socket t 0) 1 5;
  Memory.socket_close (socket t 0) 4005;
  string "failed" "failed http 401 last 5 seq 0" (text t "status");
  Memory.advance t 300;
  int "no reconnect" 1 (List.length (Memory.sockets t));
  int "hook once" 1 (Memory.session_gone t)

let heartbeat_pong () =
  let t = Memory.mount ~live_config:fast watch in
  let ws = socket t 0 in
  welcome ws;
  int "no ping yet" 0 (kind ws "ping");
  Memory.advance t 30;
  int "ping" 1 (kind ws "ping");
  Memory.socket_message ws {|{"t":"pong"}|};
  Memory.advance t 30;
  int "second ping" 2 (kind ws "ping");
  Memory.socket_message ws {|{"t":"pong"}|};
  Alcotest.(check (option int)) "open" None (Memory.closed ws);
  int "one socket" 1 (List.length (Memory.sockets t))

let heartbeat_no_pong () =
  let t = Memory.mount ~live_config:fast watch in
  welcome (socket t 0);
  Memory.advance t 60;
  bool "closed after missing pong" true (Memory.closed (socket t 0) <> None);
  Memory.advance t 300;
  int "reconnect" 2 (List.length (Memory.sockets t))

let settle_replay () =
  let html, answers = Test_apps.lookup_server () in
  bool "server html" true (contains html "ready 8");
  let t = Memory.mount ~answers Test_apps.Lookup.component in
  int "the call did not run" 0 (List.length (Memory.requests t));
  string "answer" "ready 8" (text t "answer");
  string "the first paint is the server HTML" html (Memory.html t);
  int "one paint" 1 (Memory.paints t);
  press t "again";
  let request = only (Memory.requests t) in
  string "a call that is not in the table runs" "8" (Memory.body request);
  Memory.reply request "16";
  string "answer" "ready 16" (text t "answer");
  int "one call" 1 (List.length (Memory.requests t))

let error_page t =
  List.find_opt
    (fun e -> Memory.attribute e "data-silica-error" <> None)
    (Memory.all t)

let top_level_boundary () =
  let t = Memory.mount (Test_apps.Shell.component ~guarded:false) in
  press t "bump";
  string "bumps" "1" (text t "bumps");
  press t "bomb-boom";
  let page =
    match error_page t with
    | Some page ->
        page
    | None ->
        Alcotest.fail "error page"
  in
  let body = Memory.text page in
  bool "Something went wrong" true (contains body "Something went wrong");
  let id = Option.get (Memory.attribute page "data-silica-error") in
  bool "error id" true
    (String.length id = 6
    && String.for_all
         (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
         id
    && contains body ("Error id: " ^ id)
    );
  bool "prod hides the exception" false (contains body "secret boom");
  bool "the log got it" true (Memory.logs t <> []);
  Memory.press (Memory.by_text ~within:page t "Try again");
  bool "no error page" true (error_page t = None);
  string "the root starts again with fresh state" "0" (text t "bumps");
  press t "bump";
  string "and works" "1" (text t "bumps")

module Family = struct
  let views : (string, int) Hashtbl.t = Hashtbl.create 4
  let views_of name = Option.value (Hashtbl.find_opt views name) ~default:0
  let seen name = Hashtbl.replace views name (views_of name + 1)
  let el = Node.element
  let button name event =
    el "button" [ Attr.string "id" name; Attr.on_press event ] []

  type child_event = Bump | Break | Props of int

  let child name =
    {
      Component.machine =
        Test_apps.simple ~id:("family-" ^ name) ~init:(0, 0)
          (fun (bumps, props) -> function
          | Bump ->
              Some ((bumps + 1, props), [])
          | Break ->
              failwith "break"
          | Props props ->
              Some ((bumps, props), [])
          );
      view =
        (fun (bumps, props) ->
          seen name;
          el "div" []
            [
              button (name ^ "-bump") Bump;
              button (name ^ "-break") Break;
              el "span"
                [ Attr.string "id" name ]
                [ Node.text (Printf.sprintf "%d/%d" bumps props) ];
            ]
        );
    }

  let first = child "first"
  let second = child "second"

  type event = Own | Feed | Ignore
  type state = { own : int; feed : int }

  let component =
    {
      Component.machine =
        Test_apps.simple ~id:"family" ~init:{ own = 0; feed = 0 }
          (fun s -> function
          | Own ->
              Some ({ s with own = s.own + 1 }, [])
          | Feed ->
              Some ({ s with feed = s.feed + 1 }, [])
          | Ignore ->
              None
          );
      view =
        (fun s ->
          seen "parent";
          el "main" []
            [
              button "own" Own;
              button "feed" Feed;
              button "ignore" Ignore;
              el "span"
                [ Attr.string "id" "parent" ]
                [ Node.text (string_of_int s.own) ];
              Node.component ~props:(Props s.feed) first;
              Node.boundary
                ~fallback:(fun _ ~reset ->
                  el "p"
                    [ Attr.string "id" "fallback" ]
                    [
                      Node.text "failed";
                      el "button" [ Attr.string "id" "reset"; reset ] [];
                    ]
                )
                (Node.component ~props:(Props 0) second);
            ]
        );
    }

  let mount () =
    let t = Memory.mount ~on_error:(fun _ _ -> ()) component in
    Hashtbl.reset views;
    t

  let check_views label expected =
    Alcotest.(check (list (pair string int)))
      label expected
      (List.map
         (fun name -> (name, views_of name))
         [ "parent"; "first"; "second" ]
      )
end

let child_event_renders_its_branch_only () =
  let t = Family.mount () in
  press t "first-bump";
  string "the child shows its event" "1/0" (text t "first");
  Family.check_views "the child and its parent, not the sibling"
    [ ("parent", 1); ("first", 1); ("second", 0) ]

let parent_event_keeps_unchanged_children () =
  let t = Family.mount () in
  press t "own";
  string "the parent shows its event" "1" (text t "parent");
  string "the child keeps its nodes" "0/0" (text t "first");
  Family.check_views "only the parent"
    [ ("parent", 1); ("first", 0); ("second", 0) ]

let changed_props_render_the_child () =
  let t = Family.mount () in
  press t "feed";
  string "the child got the props" "0/1" (text t "first");
  Family.check_views "the parent and the fed child"
    [ ("parent", 1); ("first", 1); ("second", 0) ]

let unhandled_event_renders_nothing () =
  let t = Family.mount () in
  press t "ignore";
  Family.check_views "no view" [ ("parent", 0); ("first", 0); ("second", 0) ]

let step_failure_under_a_boundary_shows_the_fallback () =
  let t = Family.mount () in
  press t "second-break";
  string "the fallback" "failed" (text t "fallback");
  string "the sibling keeps its state" "0/0" (text t "first");
  press t "first-bump";
  string "the sibling still works" "1/0" (text t "first");
  string "the fallback stays" "failed" (text t "fallback");
  press t "reset";
  string "reset shows the child again" "0/0" (text t "second")

module Lazy_page = struct
  let el = Node.element

  type event = Show | Bump

  type loader = {
    mutable calls : int;
    mutable answer : ((int -> event Node.t, string) result -> unit) option;
  }

  let fresh () =
    let loader = { calls = 0; answer = None } in
    let chunk =
      Chunk.deferred (fun answer ->
          loader.calls <- loader.calls + 1;
          loader.answer <- Some answer
      )
    in
    (loader, chunk)

  let page count =
    el "p"
      [ Attr.string "id" "page" ]
      [ Node.text ("page " ^ string_of_int count) ]

  let finish loader result =
    match loader.answer with
    | Some answer ->
        loader.answer <- None;
        answer result
    | None ->
        Alcotest.fail "the loader did not run"

  let component ~shown chunk =
    {
      Component.machine =
        Test_apps.simple ~id:"lazy-page" ~init:(shown, 0)
          (fun (shown, count) -> function
          | Show ->
              Some ((true, count), [])
          | Bump ->
              Some ((shown, count + 1), [])
          );
      view =
        (fun (shown, count) ->
          el "main" []
            [
              el "button" [ Attr.string "id" "show"; Attr.on_press Show ] [];
              el "button" [ Attr.string "id" "bump"; Attr.on_press Bump ] [];
              ( if shown then
                  Node.boundary
                    ~fallback:(fun _ ~reset ->
                      el "p"
                        [ Attr.string "id" "fallback" ]
                        [
                          Node.text "failed";
                          el "button" [ Attr.string "id" "reset"; reset ] [];
                        ]
                    )
                    (Node.chunk chunk
                       ~loading:
                         (el "p"
                            [ Attr.string "id" "loading" ]
                            [ Node.text "loading" ]
                         )
                       (fun page -> page count)
                    )
                else
                  Node.text ""
              );
            ]
        );
    }

  let mount ~shown chunk =
    Memory.mount ~on_error:(fun _ _ -> ()) (component ~shown chunk)
end

let ids t = List.filter_map (fun e -> Memory.attribute e "id") (Memory.all t)

let first_paint_waits_for_a_chunk () =
  let loader, chunk = Lazy_page.fresh () in
  let t = Lazy_page.mount ~shown:true chunk in
  int "no paint while the chunk loads" 0 (Memory.paints t);
  int "the loader ran once" 1 loader.calls;
  Lazy_page.finish loader (Ok Lazy_page.page);
  int "one paint" 1 (Memory.paints t);
  string "the page, not the loading view" "page 0" (text t "page");
  bool "no loading view" false (List.mem "loading" (ids t))

let first_paint_waits_while_a_sibling_steps () =
  let loader, chunk = Lazy_page.fresh () in
  let shows_chunk =
    {
      Component.machine =
        Test_apps.simple ~id:"shows-chunk" ~init:()
          (fun () (_ : Lazy_page.event) -> None
        );
      view =
        (fun () ->
          Node.chunk chunk ~loading:(Node.text "loading") (fun page -> page 0)
        );
    }
  in
  let root =
    {
      Component.machine =
        Test_apps.simple ~id:"chunk-and-ticker" ~init:() (fun () () -> None);
      view =
        (fun () ->
          Node.element "main" []
            [
              Node.component ~props:(Test_apps.Label "x")
                Test_apps.ticker_component;
              Node.component shows_chunk;
            ]
        );
    }
  in
  let t = Memory.mount root in
  Memory.advance t 20;
  int "a sibling stepped, the chunk still loads: no paint" 0 (Memory.paints t);
  Lazy_page.finish loader (Ok Lazy_page.page);
  bool "painted" true (Memory.paints t > 0);
  string "the page" "page 0" (text t "page")

let a_ready_chunk_paints_at_once () =
  let t = Lazy_page.mount ~shown:true (Chunk.ready Lazy_page.page) in
  int "one paint" 1 (Memory.paints t);
  string "the page" "page 0" (text t "page")

let a_chunk_after_the_first_paint_shows_loading () =
  let loader, chunk = Lazy_page.fresh () in
  let t = Lazy_page.mount ~shown:false chunk in
  int "painted" 1 (Memory.paints t);
  int "nothing loads yet" 0 loader.calls;
  press t "show";
  string "the loading view" "loading" (text t "loading");
  int "the loader ran" 1 loader.calls;
  press t "bump";
  string "still loading after another event" "loading" (text t "loading");
  int "one load at a time" 1 loader.calls;
  Lazy_page.finish loader (Ok Lazy_page.page);
  string "the page with the newest state" "page 1" (text t "page");
  press t "bump";
  string "the page renders again" "page 2" (text t "page")

let a_failed_chunk_goes_to_the_boundary () =
  let loader, chunk = Lazy_page.fresh () in
  let t = Lazy_page.mount ~shown:false chunk in
  press t "show";
  Lazy_page.finish loader (Error "offline");
  string "the fallback" "failed" (text t "fallback");
  press t "reset";
  int "reset loads again" 2 loader.calls;
  string "loading again" "loading" (text t "loading");
  Lazy_page.finish loader (Ok Lazy_page.page);
  string "the page" "page 0" (text t "page")

module Lazy_routes = struct
  module State = struct
    type _ t = Home : unit t | Far : unit t | Other : string t

    let path : type data. data t -> data Route.t = function
      | Home ->
          Route.root
      | Far ->
          Route.(s "far")
      | Other ->
          Route.rest
  end

  module Lazy_router = Router.Make (State)

  let loader = { Lazy_page.calls = 0; answer = None }

  let far : (unit -> Router.event Node.t) Chunk.t =
    Chunk.deferred (fun answer ->
        loader.calls <- loader.calls + 1;
        answer
          (Ok
             (fun () ->
               Node.element "p" [ Attr.string "id" "far" ] [ Node.text "far" ]
             )
          )
    )

  let routes =
    Lazy_router.routes ~chunked:[ State.Far ]
      [ State.Home; State.Far; State.Other ]

  let render (snapshot : Lazy_router.snapshot) =
    match snapshot with
    | Lazy_router.At (State.Far, ()) ->
        Node.chunk far ~loading:(Node.text "loading") (fun page -> page ())
    | Lazy_router.At (State.Home, ()) | Lazy_router.At (State.Other, _) ->
        Lazy_router.link routes State.Far ()
          [ Attr.string "id" "to-far" ]
          [ Node.text "far" ]

  let app url =
    Lazy_router.make routes
      ~chunks:(function
        | Lazy_router.At (State.Far, ()) -> [ Chunk.Any far ] | _ -> []
        )
      ~render url
end

let a_link_preloads_the_chunk_of_its_route () =
  let t = Memory.mount_app ~url:"/" Lazy_routes.app in
  Alcotest.(check (option string))
    "the link preloads" (Some "intent")
    (Memory.attribute (Memory.by_id t "to-far") "data-silica-preload");
  int "not loaded yet" 0 Lazy_routes.loader.calls;
  Memory.hover (Memory.by_id t "to-far");
  int "hover loads the chunk" 1 Lazy_routes.loader.calls;
  press t "to-far";
  string "the page shows at once" "far" (text t "far");
  int "no second load" 1 Lazy_routes.loader.calls

let () =
  let case name f = Alcotest.test_case name `Quick f in
  Alcotest.run "memory"
    [
      ( "browser",
        [
          case "press re-renders" press_re_renders;
          case "input delivers the value" input_delivers_the_value;
          case "After fires later" after_fires_later;
          case "Call delivers success and sends the wire request" call_success;
          case "Call maps non-200, bad JSON and fetch failure" call_errors;
          case
            "a call with a timeout ends with a timeout error and drops the \
             late answer"
            call_timeout;
          case "an answer before the timeout wins and the timeout never fires"
            answer_before_timeout;
          case "a timer does not fire after its state exited" timer_after_exit;
          case "a timer restarted by re-entering fires once" timer_reentered;
          case "the answer of a stopped call is dropped" stopped_call_dropped;
          case "starting a call again drops the answer of the first" call_again;
          case "link press pushes history, ignores modified clicks, sets title"
            link_press;
          case "Navigate action pushes history and changes the view"
            navigate_action;
          case "popstate goes back and forward" back_and_forward;
          case "unknown URL renders not_found" not_found;
          case
            "a child component renders and its machine handles its own events"
            child_renders;
          case "the props event reaches the child when the parent re-renders"
            props_event;
          case "function-prop output reaches the parent" output_reaches_parent;
          case "keyed children keep their state when reordered" keyed_children;
          case
            "two instances of one machine have independent timers; an \
             unrendered child stops its timer"
            independent_timers;
          case
            "topic: hello, loading, ready, stale after loss, fresh after \
             reconnect"
            topic_lifecycle;
          case "reconnect waits for the backoff, and a state resets it" backoff;
          case
            "topic: close code 4005 fails with http 401, keeps the data and \
             runs the hook"
            session_gone;
          case "heartbeat: ping after the interval, a pong keeps the socket"
            heartbeat_pong;
          case "heartbeat: no pong closes the socket and reconnects"
            heartbeat_no_pong;
        ]
      );
      ( "router",
        [
          case "preload on hover, then the press uses it"
            preload_on_hover_then_press_uses_it;
          case "a call waits for the preload in flight"
            call_waits_for_preload_in_flight;
          case "a preload is used once" preload_kept_for_one_use;
          case "a preload expires after 30 s" preload_expires;
          case "a failed or non-200 preload is not kept" failed_preload_not_kept;
          case "a call that waited for a failed preload sends its own"
            waiting_call_after_failed_preload;
          case "a link with no preload attribute"
            hover_without_preload_attribute;
          case "a block stops back, and Resume goes back" block_stops_link_press;
          case "a block stops Navigate; Stay and Leave" block_stops_actions;
          case "guard Cancel reverts the push" guard_cancel_reverts;
          case "focus and announcement after navigation" focus_after_navigation;
        ]
      );
      ( "chunk",
        [
          case "the first paint waits for a chunk" first_paint_waits_for_a_chunk;
          case "the first paint waits while a sibling steps"
            first_paint_waits_while_a_sibling_steps;
          case "a ready chunk paints at once" a_ready_chunk_paints_at_once;
          case "a chunk after the first paint shows its loading view"
            a_chunk_after_the_first_paint_shows_loading;
          case "a failed chunk goes to the boundary; reset loads again"
            a_failed_chunk_goes_to_the_boundary;
          case "a link preloads the chunk of its route"
            a_link_preloads_the_chunk_of_its_route;
        ]
      );
      ( "host",
        [
          case "a child event renders the child and its parent only"
            child_event_renders_its_branch_only;
          case "a parent event keeps the children whose props did not change"
            parent_event_keeps_unchanged_children;
          case "changed props render the child" changed_props_render_the_child;
          case "an unhandled event renders no view"
            unhandled_event_renders_nothing;
          case "a step failure under a boundary shows the fallback; reset"
            step_failure_under_a_boundary_shows_the_fallback;
          case "settle answers are replayed: no second call, same nodes"
            settle_replay;
          case
            "top-level boundary: error page, no exception text in prod, Try \
             again restarts"
            top_level_boundary;
        ]
      );
    ]
