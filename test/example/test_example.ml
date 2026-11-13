open Silica
open Silica_test_support

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let has msg html needle =
  if not (contains html needle) then
    Alcotest.failf "%s: %S not found in %s" msg needle html

let lacks msg html needle =
  if contains html needle then
    Alcotest.failf "%s: %S found in %s" msg needle html

let mount url = Sim.mount (Example_app.app (Url.of_string url))
let html = Sim.html
let go sim url = Sim.send sim (Silica.Router.Url_changed (Url.of_string url))

let focus_names log =
  List.filter_map
    (fun line ->
      let prefix = "focus " in
      let n = String.length prefix in
      if String.length line >= n && String.sub line 0 n = prefix then
        Some (String.sub line n (String.length line - n))
      else
        None
    )
    log

let strings = Alcotest.(list string)

let test_home_render () =
  let html = html (mount "/") in
  has "title" html "<h1>Silica example</h1>";
  has "link with preload" html
    "<a href=\"/item/1\" data-silica-link data-silica-preload=\"intent\">Item \
     1</a>";
  has "about link" html "<a href=\"/about\" data-silica-link>About</a>";
  has "input ref" html "data-silica-ref=\"todo-input\"";
  has "input name" html "aria-label=\"New todo\"";
  has "counter island" html "<div data-silica-live=\"counter\"></div>";
  has "submit" html "<button type=\"submit\">Add</button>"

let type_todo sim text = Sim.input sim ~label:"New todo" text

let test_todo_flow () =
  let sim = mount "/" in
  type_todo sim "buy milk";
  has "draft is the input value" (html sim) "value=\"buy milk\"";
  Sim.clear_log sim;
  Sim.submit sim;
  Alcotest.check strings "focus after add" [ "todo-input" ]
    (focus_names (Sim.log sim));
  has "added" (html sim) "[ ] buy milk";
  has "draft cleared" (html sim) "value=\"\"";
  type_todo sim "walk dog";
  Sim.submit sim;
  Sim.press sim "buy milk";
  has "toggled" (html sim) "[x] buy milk";
  has "other untouched" (html sim) "[ ] walk dog";
  Alcotest.(check int)
    "root, home, todo, votes and vote button" 5 (Sim.count sim)

let test_blank_add () =
  let sim = mount "/" in
  type_todo sim "   ";
  Sim.clear_log sim;
  Sim.submit sim;
  Alcotest.check strings "still focuses" [ "todo-input" ]
    (focus_names (Sim.log sim));
  lacks "no item" (html sim) "<li>"

let item =
  Example_model.Item.
    { id = 3; title = "Mug"; price_cents = 1250; tags = [ "kitchen"; "blue" ] }

let test_item_page () =
  let sim = mount "/item/3" in
  has "loading" (html sim) "Loading item 3...";
  Alcotest.check strings "one call, with the id" [ "3" ] (Sim.call_inputs sim);
  Alcotest.check strings "the call belongs to the page instance"
    [ "2.0/item/loading//api/item 3" ]
    (Sim.running sim);
  Alcotest.(check bool)
    "title while loading" true
    (List.mem "title Item" (Sim.log sim));
  Sim.answer sim Example_model.item_endpoint item;
  let page = html sim in
  has "name" page "<h2>Mug</h2>";
  has "price" page "<p>$12.50</p>";
  has "tags" page "<li>kitchen</li><li>blue</li>";
  Alcotest.(check string)
    "title" "title Mug"
    (List.nth (Sim.log sim) (List.length (Sim.log sim) - 1));
  let failing = mount "/item/3" in
  Sim.answer_error failing Example_model.item_endpoint (Http 500);
  has "failure" (html failing) "The server answered 500";
  let missing = mount "/item/3" in
  Sim.reject missing Example_model.item_endpoint "no item 3";
  has "rejected" (html missing) "<p>no item 3</p>";
  lacks "no article" (html missing) "<article>"

let test_item_change_reloads () =
  let sim = mount "/item/3" in
  Sim.answer sim Example_model.item_endpoint item;
  go sim "/item/4";
  has "loading again" (html sim) "Loading item 4...";
  Alcotest.check strings "call for the new id" [ "4" ] (Sim.call_inputs sim);
  Alcotest.(check int) "same page instance" 2 (Sim.count sim)

let test_leaving_item_ignores_late_result () =
  let sim = mount "/item/3" in
  go sim "/about";
  Alcotest.check strings "nothing runs" [] (Sim.running sim);
  Sim.clear_log sim;
  let before = html sim in
  Sim.fire_after_stop sim "2.0/item/loading//api/item 3";
  Alcotest.(check string) "view unchanged" before (html sim);
  Alcotest.check strings "no action" [] (Sim.log sim)

let test_about_back () =
  let sim = mount "/" in
  go sim "/about";
  has "about" (html sim) "<button>Back</button>";
  Sim.clear_log sim;
  Sim.press sim "Back";
  Alcotest.check strings "back" [ "back" ] (Sim.log sim);
  Alcotest.(check bool)
    "no back button on home" true
    ( match Sim.press (mount "/") "Back" with
    | () ->
        false
    | exception Failure _ ->
        true
    )

let vote_count html =
  let marker = "<span id=\"votes-count\">" in
  let n = String.length marker in
  let rec find i =
    if i + n > String.length html then
      Alcotest.failf "no votes count in %s" html
    else if String.sub html i n = marker then
      i + n
    else
      find (i + 1)
  in
  let start = find 0 in
  String.sub html start (String.index_from html start '<' - start)

let test_votes_topic () =
  let sim = mount "/" in
  Alcotest.(check (list (pair string string)))
    "one subscription, under the key main"
    [ ("votes", "main") ]
    (Sim.subscriptions sim);
  Alcotest.(check string) "waiting" "..." (vote_count (html sim));
  Sim.publish sim Example_model.votes_topic ~key:"main"
    { Topic.seq = 3; status = Status.Ready { data = 4; stale = false } };
  Alcotest.(check string) "count" "4" (vote_count (html sim));
  lacks "online" (html sim) "Offline";
  Sim.publish sim Example_model.votes_topic ~key:"main"
    { Topic.seq = 0; status = Status.Ready { data = 4; stale = true } };
  Alcotest.(check string) "old data stays" "4" (vote_count (html sim));
  has "offline notice" (html sim) "Offline, showing old data";
  go sim "/about";
  Alcotest.(check (list (pair string string)))
    "leaving the page ends the subscription" [] (Sim.subscriptions sim)

let test_votes_reconnect () =
  let sim = mount "/" in
  lacks "no reconnect while live" (html sim) "Reconnect";
  Sim.publish sim Example_model.votes_topic ~key:"main"
    {
      Topic.seq = 0;
      status = Status.Failed { error = Remote.Http 403; last = None };
    };
  Sim.clear_log sim;
  Sim.press sim "Reconnect";
  Alcotest.check strings "the subscription stops and starts again"
    [
      "stop 4.0/votes/watching/live:votes main";
      "start 4.1/votes/watching/live:votes main";
    ]
    (Sim.log sim);
  Sim.publish sim Example_model.votes_topic ~key:"main"
    { Topic.seq = 1; status = Status.Ready { data = 5; stale = false } };
  Alcotest.(check string) "live again" "5" (vote_count (html sim));
  lacks "no reconnect after a state" (html sim) "Reconnect"

let test_vote_button () =
  let sim = mount "/" in
  Alcotest.check strings "no call at first" [] (Sim.call_inputs sim);
  Sim.press sim "Vote";
  Alcotest.check strings "one call" [ "null" ] (Sim.call_inputs sim);
  Sim.answer_error sim Example_model.vote_endpoint (Http 500);
  has "retry" (html sim) "Vote failed. Try again";
  Sim.press sim "Vote failed";
  Alcotest.check strings "the same call again" [ "null" ] (Sim.call_inputs sim);
  Sim.answer sim Example_model.vote_endpoint 7;
  lacks "no retry after success" (html sim) "Vote failed"

let test_not_found () =
  let sim = mount "/nope" in
  has "not found" (html sim) "<h1>Not found</h1>";
  Alcotest.(check int) "status" 404 (Example_app.status (Sim.snapshot sim));
  Alcotest.(check bool) "title" true (List.mem "title Not found" (Sim.log sim));
  go sim "/item/x";
  Alcotest.(check int)
    "bad id is no route" 404
    (Example_app.status (Sim.snapshot sim));
  go sim "/about";
  Alcotest.(check int) "about is 200" 200 (Example_app.status (Sim.snapshot sim))

let test_route_round_trip () =
  let open Example_routes in
  List.iter
    (fun route ->
      Alcotest.(check bool)
        "round trip" true
        (Router.parse routes (Router.print routes route) = Ok route)
    )
    [ Home; About; Item 0; Item 42; Edit; Locked; Not_found "/nope" ]

let rec pressables acc (n : _ Node.t) =
  match Node.view n with
  | Text _ ->
      acc
  | List l ->
      List.fold_left pressables acc l
  | Keyed (_, n) ->
      pressables acc n
  | Portal (_, l) ->
      List.fold_left pressables acc l
  | Boundary { body; _ } ->
      pressables acc body
  | Component _ | Chunk _ ->
      acc
  | Element (tag, attrs, children) ->
      let pressable =
        List.exists
          (function Node.On_press _ | On_double_press _ -> true | _ -> false)
          attrs
      in
      let acc =
        if pressable then
          (tag, attrs, children) :: acc
        else
          acc
      in
      List.fold_left pressables acc children

let rec text_of n =
  match Node.view n with
  | Text s ->
      s
  | List l ->
      String.concat "" (List.map text_of l)
  | Keyed (_, n) ->
      text_of n
  | Portal (_, l) ->
      String.concat "" (List.map text_of l)
  | Boundary { body; _ } ->
      text_of body
  | Component _ | Chunk _ ->
      ""
  | Element (_, _, c) ->
      String.concat "" (List.map text_of c)

let check_named (view : _ Node.t) =
  let ps = pressables [] view in
  Alcotest.(check bool) "has pressables" true (ps <> []);
  List.iter
    (fun (tag, attrs, children) ->
      let labelled =
        List.exists
          (function Node.String ("aria-label", _) -> true | _ -> false)
          attrs
      in
      let named =
        labelled
        || String.trim (String.concat "" (List.map text_of children)) <> ""
      in
      if not named then Alcotest.failf "unnamed pressable <%s>" tag
    )
    ps

let test_accessible_names () =
  let sim = mount "/" in
  type_todo sim "a";
  Sim.submit sim;
  check_named (Sim.node sim);
  check_named (Sim.node (mount "/about"));
  check_named (Example_counter.view 1)

let test_counter () =
  let send count event =
    let count, _, _ = Machine.step Example_counter.machine count event in
    count
  in
  let open Example_model.Counter_event in
  Alcotest.(check int) "incr" 2 (send (send 0 Incr) Incr);
  Alcotest.(check int) "decr" 1 (send 2 Decr);
  Alcotest.(check int) "decr at zero" 0 (send 0 Decr);
  Alcotest.(check int) "reset" 0 (send 5 Reset);
  Alcotest.(check bool)
    "decr at zero is ignored" false
    (Machine.can Example_counter.machine 0 Decr);
  Alcotest.(check bool)
    "decr at one is handled" true
    (Machine.can Example_counter.machine 1 Decr);
  has "disabled at 0"
    (Render.to_string (Example_counter.view 0))
    "<button disabled>Decrement</button>";
  lacks "enabled at 1" (Render.to_string (Example_counter.view 1)) "disabled";
  has "shows value" (Render.to_string (Example_counter.view 7)) "<span>7</span>"

let test_codecs () =
  let c = Remote.output Example_model.item_endpoint in
  let wire = Codec.to_string c item in
  Alcotest.(check string)
    "item wire"
    {|{"id":3,"title":"Mug","price_cents":1250,"tags":["kitchen","blue"]}|} wire;
  Alcotest.(check bool) "item round trip" true (Codec.of_string c wire = Ok item);
  Alcotest.(check bool)
    "bad field type" true
    ( match
        Codec.of_string c {|{"id":"x","title":"Mug","price_cents":1,"tags":[]}|}
      with
    | Error _ ->
        true
    | Ok _ ->
        false
    );
  Alcotest.(check bool)
    "missing field" true
    (match Codec.of_string c {|{"id":1}|} with Error _ -> true | Ok _ -> false);
  let e = Example_model.counter_event in
  List.iter
    (fun v ->
      Alcotest.(check bool)
        "event round trip" true
        (Codec.of_string e (Codec.to_string e v) = Ok v)
    )
    Example_model.Counter_event.[ Incr; Decr; Reset ];
  Alcotest.(check bool)
    "unknown event" true
    (match Codec.of_string e {|["Nope"]|} with Error _ -> true | Ok _ -> false);
  let live = Example_counter.live in
  Alcotest.(check string) "channel" "counter" live.channel;
  Alcotest.(check string) "state wire" "5" (Codec.to_string live.state 5)

let () =
  let t n f = Alcotest.test_case n `Quick f in
  Alcotest.run "silica example"
    [
      ( "app",
        [
          t "home render" test_home_render;
          t "todo flow" test_todo_flow;
          t "blank add" test_blank_add;
          t "item page" test_item_page;
          t "item change reloads" test_item_change_reloads;
          t "late result ignored" test_leaving_item_ignores_late_result;
          t "about back" test_about_back;
          t "not found" test_not_found;
          t "votes topic: updates, stale, stop on leave" test_votes_topic;
          t "votes topic: reconnect after a failure" test_votes_reconnect;
          t "vote button: call, failure, retry" test_vote_button;
          t "route round trip" test_route_round_trip;
          t "accessible names" test_accessible_names;
          t "counter" test_counter;
          t "codecs" test_codecs;
        ]
      );
    ]
