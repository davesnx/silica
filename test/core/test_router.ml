open Silica
open Silica_test_support

type status = Open | Closed

type route =
  | Home
  | Issues of status * int
  | Issue of int
  | Not_found of string

let route_list =
  [
    Router.route Route.root
      (fun () -> Home)
      (function Home -> Some () | _ -> None);
    Router.route
      Route.(
        s "issues"
        +? (choice_query "status" ~default:Open
              [ ("open", Open); ("closed", Closed) ]
           &? int_query "page" ~default:1
           )
      )
      (fun (status, page) -> Issues (status, page))
      (function Issues (status, page) -> Some (status, page) | _ -> None);
    Router.route
      Route.(s "issues" / int)
      (fun id -> Issue id)
      (function Issue id -> Some id | _ -> None);
    Router.route Route.rest
      (fun path -> Not_found path)
      (function Not_found path -> Some path | _ -> None);
  ]

let is_not_found = function Not_found _ -> true | _ -> false
let strings = Alcotest.(list string)
let u s = Url.of_string s

module Page = struct
  type event = Bump
  type state = Page of int

  let name = function Page _ -> "page"

  let states = [ "page" ]

  let machine =
    Machine.make_with_props ~id:"issue" ~init:(Page 0) ~name ~states
      ~entry:(fun _ _ -> [ Action.Set_title "issue" ])
      ~during:(fun _ _ -> [ Host.after 5 Bump ])
      (fun _ (Page bumps) Bump -> Machine.Go (Page (bumps + 1)))

  let view id (Page bumps) : event Node.t =
    Node.element "button"
      [ Attr.on_press Bump ]
      [ Node.text (Printf.sprintf "issue %d:%d" id bumps) ]

  let component = Component.make_with_props machine view
  let make ~id () = Component.node_with_props ~props:id component
end

module Static = struct
  let make name =
    let machine =
      Machine.make ~id:name ~init:()
        ~name:(fun () -> name)
        ~entry:(fun () -> [ Action.Set_title name ])
        (fun () () -> Machine.Ignore)
    in
    let view () : unit Node.t = Node.text name in
    Component.make machine view

  let home = make "home"
  let issues = make "issues"
end

let status_text = function Open -> "open" | Closed -> "closed"
let routes = Router.routes ~not_found:is_not_found route_list

let render : route -> Router.event Node.t = function
  | Home ->
      Component.node Static.home
  | Issues (status, page) ->
      Node.element "div" []
        [
          Node.text (Printf.sprintf "list %s %d" (status_text status) page);
          Component.node Static.issues;
          Page.make ~id:0 ();
        ]
  | Issue id ->
      Page.make ~id ()
  | Not_found url ->
      Node.element "p" [] [ Node.text ("nothing at " ^ url) ]

let mount ?guard ?exit ?title ?(routes = routes) url =
  Sim.mount (Router.make routes ?guard ?exit ?title ~render (u url))

let go sim url = Sim.send sim (Router.Url_changed (u url))

let has sim needle =
  let html = Sim.html sim in
  let n = String.length needle and h = String.length html in
  let rec go i = i + n <= h && (String.sub html i n = needle || go (i + 1)) in
  if not (go 0) then Alcotest.failf "%S not in %s" needle html

let url_of sim = Url.to_string (Router.print routes (Sim.snapshot sim))

let test_codecs () =
  let machine = (Router.make routes ~render (u "/")).machine in
  let state_codec = Router.state_codec routes
  and event_codec = Router.event_codec in
  Alcotest.(check string)
    "a route is its printed URL" {|"/issues?status=closed&page=2"|}
    (Codec.to_string state_codec (Issues (Closed, 2)));
  List.iter
    (fun route ->
      Alcotest.(check bool)
        "the route round trips" true
        (Codec.of_string state_codec (Codec.to_string state_codec route)
        = Ok route
        )
    )
    [ Home; Issues (Closed, 2); Issue 3; Not_found "/nowhere" ];
  Alcotest.(check string)
    "an event" {|["Url_changed","/issues/3"]|}
    (Codec.to_string event_codec (Router.Url_changed (u "/issues/3")));
  Alcotest.(check bool)
    "the event round trips" true
    ( match
        Codec.of_string event_codec
          (Codec.to_string event_codec (Router.Preload (u "/issues/3")))
      with
    | Ok (Router.Preload url) ->
        Url.to_string url = "/issues/3"
    | Ok (Router.Url_changed _) | Error _ ->
        false
    );
  Alcotest.(check (list (option string)))
    "event names"
    [ Some "url changed"; Some "preload" ]
    (List.map
       (Machine.event_name machine)
       [ Router.Url_changed (u "/"); Router.Preload (u "/") ]
    )

let test_start () =
  let sim = mount "/" in
  Alcotest.(check string) "url" "/" (url_of sim);
  has sim "home";
  Alcotest.check strings "start actions" [ "title home" ] (Sim.log sim);
  Alcotest.(check int) "router and page" 2 (Sim.count sim)

let test_url_changed_switches_page () =
  let sim = mount "/issues/7" in
  has sim "issue 7:0";
  Alcotest.(check bool)
    "page timer runs" true
    (List.exists (fun id -> String.length id > 0) (Sim.running sim));
  Sim.clear_log sim;
  go sim "/";
  has sim "home";
  Alcotest.check strings "old page stops, new page starts"
    [ "stop 2.0/issue/page/after 5ms"; "title home" ]
    (Sim.log sim);
  Alcotest.check strings "no timer left" [] (Sim.running sim)

let test_same_route_new_parameters_keeps_page () =
  let sim = mount "/issues/7" in
  Sim.press sim "issue 7";
  has sim "issue 7:1";
  let before = Sim.running sim in
  Sim.clear_log sim;
  go sim "/issues/8";
  has sim "issue 8:1";
  Alcotest.check strings "same instance, same timers" before (Sim.running sim);
  Alcotest.check strings "no action" [] (Sim.log sim);
  Alcotest.(check int) "router and page" 2 (Sim.count sim)

let test_reenter_route_restarts_page () =
  let routes =
    Router.routes ~not_found:is_not_found
      ~reenter:(function Issue _ -> true | _ -> false)
      route_list
  in
  let sim = mount ~routes "/issues/7" in
  Sim.press sim "issue 7";
  has sim "issue 7:1";
  Sim.clear_log sim;
  go sim "/issues/8";
  has sim "issue 8:0";
  Alcotest.check strings "start again"
    [
      "stop 2.0/issue/page/after 5ms";
      "title issue";
      "start 3.0/issue/page/after 5ms";
    ]
    (Sim.log sim)

let test_same_url_does_nothing () =
  let sim = mount "/issues/7" in
  Sim.press sim "issue 7";
  Sim.clear_log sim;
  go sim "/issues/7#top";
  has sim "issue 7:1";
  Alcotest.check strings "no action" [] (Sim.log sim)

let test_back_and_forward () =
  let sim = mount "/" in
  go sim "/issues/7";
  has sim "issue 7:0";
  Sim.press sim "issue 7";
  go sim "/";
  has sim "home";
  go sim "/issues/7";
  has sim "issue 7:0"

let test_bad_path_is_not_found () =
  let sim = mount "/nowhere/at/all" in
  has sim "nothing at /nowhere/at/all";
  Alcotest.(check bool)
    "flag" true
    (Router.is_not_found routes (Sim.snapshot sim));
  go sim "/issues/x";
  has sim "nothing at /issues/x";
  go sim "/issues/007";
  has sim "nothing at /issues/007";
  go sim "/";
  Alcotest.(check bool)
    "recovers" false
    (Router.is_not_found routes (Sim.snapshot sim))

let test_query () =
  let sim = mount "/issues?status=closed&page=3" in
  has sim "list closed 3";
  go sim "/issues?status=bogus&page=x";
  has sim "list open 1";
  go sim "/issues?status=closed&page=oops";
  has sim "list closed 1";
  go sim "/issues?page=2&unknown=1";
  has sim "list open 2";
  Alcotest.(check string)
    "printed canonical, defaults dropped, unknown dropped" "/issues?page=2"
    (url_of sim);
  go sim "/issues?page=2&status=closed";
  Alcotest.(check string)
    "canonical order" "/issues?status=closed&page=2" (url_of sim)

let test_query_change_keeps_page () =
  let sim = mount "/issues" in
  let before = Sim.running sim in
  Alcotest.(check int) "router, list and its page" 3 (Sim.count sim);
  go sim "/issues?status=closed";
  has sim "list closed 1";
  Alcotest.(check int) "same instances" 3 (Sim.count sim);
  Alcotest.check strings "same timers" before (Sim.running sim)

let test_route_round_trip () =
  List.iter
    (fun snapshot ->
      let printed = Router.print routes snapshot in
      Alcotest.(check string)
        "print . parse . print" (Url.to_string printed)
        (Url.to_string (Router.print routes (Router.parse_exn routes printed)))
    )
    [
      Home;
      Issues (Open, 1);
      Issues (Closed, 4);
      Issue 0;
      Issue 42;
      Not_found "/a/b";
    ];
  Alcotest.(check bool)
    "same data" true
    (Router.parse routes (u "/issues?status=closed&page=4")
    = Ok (Issues (Closed, 4))
    )

let test_guard_cancel () =
  let guard ~from:_ ~target =
    match target with Home -> Router.Cancel | _ -> Router.Allow
  in
  let sim = mount ~guard "/issues/7" in
  Sim.clear_log sim;
  go sim "/";
  has sim "issue 7:0";
  Alcotest.check strings "the navigation is reverted" [ "revert" ] (Sim.log sim)

let test_guard_redirect () =
  let guard ~from:_ ~target =
    match target with Issue 13 -> Router.Redirect Home | _ -> Router.Allow
  in
  let sim = mount ~guard "/" in
  Sim.clear_log sim;
  go sim "/issues/13";
  has sim "home";
  Alcotest.check strings "url is replaced" [ "replace /" ] (Sim.log sim)

let issue_endpoint =
  Remote.declare ~path:"/api/issue" ~input:Codec.int ~output:Codec.string
    ~rejected:Codec.string

let list_endpoint =
  Remote.declare ~path:"/api/list" ~input:Codec.int ~output:Codec.string
    ~rejected:Codec.string

let preload_routes ?base () =
  Router.routes ?base ~not_found:is_not_found
    ~preload:(function
      | Issue id ->
          [ Remote.request issue_endpoint id ]
      | Issues (_, page) ->
          [
            Remote.request list_endpoint page;
            Remote.request issue_endpoint (100 + page);
          ]
      | _ ->
          []
      )
    route_list

let html node = Render.to_string node

let test_base_path () =
  let routes = preload_routes ~base:"/app" () in
  Alcotest.(check bool)
    "parse takes the base off" true
    (Router.parse routes (u "/app/issues/7") = Ok (Issue 7));
  Alcotest.(check bool)
    "the root under the base" true
    (Router.parse routes (u "/app") = Ok Home);
  Alcotest.(check string)
    "print puts the base first" "/app/issues?page=2"
    (Url.to_string (Router.print routes (Issues (Open, 2))));
  Alcotest.(check string)
    "the root prints as the base" "/app"
    (Url.to_string (Router.print routes Home));
  ( match Router.parse routes (u "/issues/7") with
  | Error _ ->
      ()
  | Ok _ ->
      Alcotest.fail "expected Error for a path outside the base"
  );
  ( match Router.parse_exn routes (u "/issues/7") with
  | _ ->
      Alcotest.fail "expected Failure for a path outside the base"
  | exception Failure _ ->
      ()
  );
  Alcotest.(check string)
    "a link has the base" "<a href=\"/app/issues/5\" data-silica-link>go</a>"
    (html (Router.link ~preload:Never routes (Issue 5) [] [ Node.text "go" ]));
  ( match Router.navigate routes (Issue 5) with
  | Action.Navigate url ->
      Alcotest.(check string) "navigate" "/app/issues/5" (Url.to_string url)
  | _ ->
      Alcotest.fail "expected Navigate"
  );
  let sim = Sim.mount (Router.make routes ~render (u "/app/issues/7")) in
  has sim "issue 7:0";
  Sim.clear_log sim;
  go sim "/app";
  has sim "home";
  Alcotest.(check string)
    "url" "/app"
    (Url.to_string (Router.print routes (Sim.snapshot sim)))

let test_hash () =
  ( match Router.navigate ~hash:"c3" routes (Issue 5) with
  | Action.Navigate url ->
      Alcotest.(check string) "navigate" "/issues/5#c3" (Url.to_string url)
  | _ ->
      Alcotest.fail "expected Navigate"
  );
  ( match Router.replace ~hash:"top" routes Home with
  | Action.Replace url ->
      Alcotest.(check string) "replace" "/#top" (Url.to_string url)
  | _ ->
      Alcotest.fail "expected Replace"
  );
  Alcotest.(check string)
    "link" "<a href=\"/issues/5#c3\" data-silica-link>go</a>"
    (html (Router.link ~hash:"c3" routes (Issue 5) [] [ Node.text "go" ]));
  let sim = mount "/issues/5" in
  Sim.clear_log sim;
  go sim "/issues/5#c3";
  Alcotest.check strings "the router ignores the hash" [] (Sim.log sim)

let test_current () =
  let current = Issue 7 in
  Alcotest.(check string)
    "same path"
    "<a href=\"/issues/7\" data-silica-link aria-current=\"page\" \
     data-active>x</a>"
    (html (Router.link ~current routes (Issue 7) [] [ Node.text "x" ]));
  Alcotest.(check string)
    "other path" "<a href=\"/issues/8\" data-silica-link>x</a>"
    (html (Router.link ~current routes (Issue 8) [] [ Node.text "x" ]));
  Alcotest.(check string)
    "the root is active only on itself" "<a href=\"/\" data-silica-link>x</a>"
    (html (Router.link ~current routes Home [] [ Node.text "x" ]));
  Alcotest.(check string)
    "the root on itself"
    "<a href=\"/\" data-silica-link aria-current=\"page\" data-active>x</a>"
    (html (Router.link ~current:Home routes Home [] [ Node.text "x" ]));
  Alcotest.(check string)
    "parent path, query ignored"
    "<a href=\"/issues?status=closed\" data-silica-link data-active>x</a>"
    (html
       (Router.link ~current routes (Issues (Closed, 1)) [] [ Node.text "x" ])
    );
  Alcotest.(check string)
    "query ignored for the current page"
    "<a href=\"/issues?page=3\" data-silica-link aria-current=\"page\" \
     data-active>x</a>"
    (html
       (Router.link
          ~current:(Issues (Closed, 1))
          routes
          (Issues (Open, 3))
          []
          [ Node.text "x" ]
       )
    );
  Alcotest.(check string)
    "a prefix by whole segments only"
    "<a href=\"/issues/7\" data-silica-link>x</a>"
    (html
       (Router.link ~current:(Not_found "/issues/77") routes (Issue 7) []
          [ Node.text "x" ]
       )
    );
  Alcotest.(check string)
    "replace link too"
    "<a href=\"/issues/7\" data-silica-link data-silica-replace \
     aria-current=\"page\" data-active>x</a>"
    (html (Router.link_replace ~current routes (Issue 7) [] [ Node.text "x" ]))

let test_preload_attribute () =
  let routes = preload_routes () in
  Alcotest.(check string)
    "intent by default"
    "<a href=\"/issues/5\" data-silica-link \
     data-silica-preload=\"intent\">x</a>"
    (html (Router.link routes (Issue 5) [] [ Node.text "x" ]));
  Alcotest.(check string)
    "viewport"
    "<a href=\"/issues/5\" data-silica-link \
     data-silica-preload=\"viewport\">x</a>"
    (html (Router.link ~preload:Viewport routes (Issue 5) [] [ Node.text "x" ]));
  Alcotest.(check string)
    "never" "<a href=\"/issues/5\" data-silica-link>x</a>"
    (html (Router.link ~preload:Never routes (Issue 5) [] [ Node.text "x" ]));
  Alcotest.(check string)
    "a route with no calls" "<a href=\"/\" data-silica-link>x</a>"
    (html (Router.link routes Home [] [ Node.text "x" ]));
  Alcotest.(check string)
    "routes with no preload" "<a href=\"/issues/5\" data-silica-link>x</a>"
    (html
       (Router.link (Router.routes route_list) (Issue 5) [] [ Node.text "x" ])
    )

let test_preload_event () =
  let routes = preload_routes () in
  let sim = Sim.mount (Router.make routes ~render (u "/")) in
  Sim.clear_log sim;
  Sim.send sim (Router.Preload (u "/issues?page=4"));
  Alcotest.check strings "a preload for each call of the route"
    [ "preload /api/list 4"; "preload /api/issue 104" ]
    (Sim.log sim);
  has sim "home";
  Alcotest.(check bool)
    "the route does not change" true
    (Sim.snapshot sim = Home);
  Sim.clear_log sim;
  Sim.send sim (Router.Preload (u "/"));
  Alcotest.check strings "no call, no action" [] (Sim.log sim)

let test_guard_at_first_url () =
  let seen = ref [] in
  let guard ~from ~target =
    seen := (from, target) :: !seen;
    match target with
    | Issue 13 ->
        Router.Redirect Home
    | Issue 14 ->
        Router.Cancel
    | _ ->
        Router.Allow
  in
  let sim = mount ~guard "/issues/13" in
  Alcotest.(check bool)
    "the guard sees no route before" true
    (!seen = [ (None, Issue 13) ]);
  has sim "home";
  Alcotest.check strings "starts in the other route and replaces the URL"
    [ "replace /"; "title home" ]
    (Sim.log sim);
  let sim = mount ~guard "/issues/7" in
  has sim "issue 7:0";
  Alcotest.check strings "allow"
    [ "title issue"; "start 2.0/issue/page/after 5ms" ]
    (Sim.log sim);
  let sim = mount ~guard "/issues/14" in
  has sim "issue 14:0";
  Alcotest.(check bool)
    "cancel at the start stays in the route of the URL" false
    (List.mem "revert" (Sim.log sim));
  Sim.clear_log sim;
  seen := [];
  go sim "/issues/13";
  Alcotest.(check bool)
    "later, the guard sees the route before" true
    (!seen = [ (Some (Issue 14), Issue 13) ])

let test_exit_hook () =
  let exit snapshot =
    [
      Action.Focus
        (Ref.make ("left " ^ Url.to_string (Router.print routes snapshot)));
    ]
  in
  let sim = mount ~exit "/issues/7" in
  Sim.clear_log sim;
  go sim "/";
  Alcotest.check strings "exit of the route state runs"
    [ "focus left /issues/7"; "stop 2.0/issue/page/after 5ms"; "title home" ]
    (Sim.log sim)

let test_reenter_route_runs_exit () =
  let routes =
    Router.routes ~not_found:is_not_found
      ~reenter:(function Issue _ -> true | _ -> false)
      route_list
  in
  let exit snapshot =
    [
      Action.Focus
        (Ref.make ("left " ^ Url.to_string (Router.print routes snapshot)));
    ]
  in
  let sim = mount ~routes ~exit "/issues/7" in
  Sim.clear_log sim;
  go sim "/issues/8";
  Alcotest.check strings "the exit of the old parameters runs, then the remount"
    [
      "focus left /issues/7";
      "stop 2.0/issue/page/after 5ms";
      "title issue";
      "start 3.0/issue/page/after 5ms";
    ]
    (Sim.log sim)

let titles sim = List.filter (String.starts_with ~prefix:"title ") (Sim.log sim)

let test_title () =
  let title = function
    | Issue id ->
        Printf.sprintf "Issue %d" id
    | Issues _ ->
        "Issues"
    | Home | Not_found _ ->
        "Other"
  in
  let sim = mount ~title "/issues/7" in
  Alcotest.check strings "the route title, then the page title"
    [ "title Issue 7"; "title issue" ]
    (titles sim);
  Sim.clear_log sim;
  go sim "/issues/8";
  Alcotest.check strings "same route, new parameters" [ "title Issue 8" ]
    (titles sim);
  Sim.clear_log sim;
  go sim "/issues?status=closed";
  Alcotest.(check bool)
    "another route" true
    (List.mem "title Issues" (titles sim));
  Sim.clear_log sim;
  go sim "/issues?status=closed";
  Alcotest.check strings "same url, no title" [] (titles sim);
  let guard ~from:_ ~target =
    match target with Home -> Router.Redirect (Issue 1) | _ -> Router.Allow
  in
  let sim = mount ~guard ~title "/" in
  Alcotest.(check bool)
    "a redirect at the start has the title of the target" true
    (List.mem "title Issue 1" (titles sim)
    && not (List.mem "title Other" (titles sim))
    )

let test_links_and_actions () =
  Alcotest.(check string)
    "link"
    "<a href=\"/issues?status=closed\" data-silica-link class=\"x\">go</a>"
    (Render.to_string
       (Router.link routes
          (Issues (Closed, 1))
          [ Attr.string "class" "x" ]
          [ Node.text "go" ]
       )
    );
  Alcotest.(check string)
    "replace link"
    "<a href=\"/issues/5\" data-silica-link data-silica-replace>go</a>"
    (Render.to_string
       (Router.link_replace routes (Issue 5) [] [ Node.text "go" ])
    );
  ( match Router.navigate routes (Issue 5) with
  | Action.Navigate url ->
      Alcotest.(check string) "navigate" "/issues/5" (Url.to_string url)
  | _ ->
      Alcotest.fail "expected Navigate"
  );
  match Router.replace routes Home with
  | Action.Replace url ->
      Alcotest.(check string) "replace" "/" (Url.to_string url)
  | _ ->
      Alcotest.fail "expected Replace"

let test_state_names () =
  Alcotest.(check (list string))
    "the state names are the route patterns"
    [ "/"; "/issues?status&page"; "/issues/:int"; "/*" ]
    (List.map (Router.name routes)
       [ Home; Issues (Open, 1); Issue 3; Not_found "/x" ]
    );
  let sim = mount "/issues/3" in
  Alcotest.(check string)
    "the router machine names its state by route" "/issues/:int"
    (Router.name routes (Sim.snapshot sim))

let test_decode_outside_base () =
  let routes = preload_routes ~base:"/app" () in
  match Codec.of_string (Router.state_codec routes) {|"/issues/7"|} with
  | Error _ ->
      ()
  | Ok _ ->
      Alcotest.fail "expected Error for a path outside the base"

let test_no_catch_all () =
  let routes = Router.routes [ List.hd route_list ] in
  match Router.parse routes (u "/x") with
  | Error _ ->
      ()
  | Ok _ ->
      Alcotest.fail "expected Error"

let mount_app url =
  Sim.mount (Router.make (preload_routes ~base:"/app" ()) ~render (u url))

let test_start_outside_base () =
  match
    Router.make (preload_routes ~base:"/app" ()) ~render (u "/issues/7")
  with
  | _ ->
      Alcotest.fail "expected Failure for a first URL outside the base"
  | exception Failure _ ->
      ()

let test_url_changed_outside_base () =
  let sim = mount_app "/app/issues/7" in
  Sim.clear_log sim;
  go sim "/issues/8";
  Alcotest.(check bool) "the route stays" true (Sim.snapshot sim = Issue 7);
  has sim "issue 7:0";
  Alcotest.check strings "the host reloads" [ "reload" ] (Sim.log sim)

let test_preload_outside_base () =
  let sim = mount_app "/app/issues/7" in
  Sim.clear_log sim;
  Sim.send sim (Router.Preload (u "/issues/8"));
  Alcotest.(check bool) "the route stays" true (Sim.snapshot sim = Issue 7);
  Alcotest.check strings "no action" [] (Sim.log sim)

let test_url_changed_no_match () =
  let routes = Router.routes [ List.hd route_list ] in
  let sim = Sim.mount (Router.make routes ~render (u "/")) in
  Sim.clear_log sim;
  go sim "/x";
  Alcotest.(check bool) "the route stays" true (Sim.snapshot sim = Home);
  Alcotest.check strings "the host reloads" [ "reload" ] (Sim.log sim)

let test_route_units () =
  let open Route in
  let check_parse name route url expected =
    Alcotest.(check bool) name true (parse route (u url) = expected)
  in
  check_parse "literal" (s "a") "/a" (Some ());
  check_parse "literal miss" (s "a") "/b" None;
  check_parse "extra segment" (s "a") "/a/b" None;
  check_parse "int" (s "n" / int) "/n/5" (Some 5);
  check_parse "int rejects plus" (s "n" / int) "/n/+5" None;
  check_parse "int rejects leading zero" (s "n" / int) "/n/05" None;
  check_parse "suffix literal" (int // s "edit") "/3/edit" (Some 3);
  check_parse "pair" (s "p" / (int /+ string)) "/p/1/x" (Some (1, "x"));
  check_parse "root" root "/" (Some ());
  check_parse "root miss" root "/a" None;
  check_parse "rest" rest "/a/b" (Some "/a/b");
  check_parse "rest of root" rest "/" (Some "/");
  Alcotest.(check string)
    "print pair" "/p/1/x"
    (Url.to_string (print (s "p" / (int /+ string)) (1, "x")));
  Alcotest.(check string) "describe" "/n/:int" (describe (s "n" / int))

type page = { size : int; dense : bool }

let test_map_query () =
  let open Route in
  let route =
    root
    +? map_query
         (fun (size, dense) -> { size; dense })
         (fun { size; dense } -> (size, dense))
         (int_query "size" ~default:10 &? bool_query "dense" ~default:false)
  in
  Alcotest.(check bool)
    "parse to the record" true
    (parse route (u "/?dense=true&size=20") = Some { size = 20; dense = true });
  Alcotest.(check bool)
    "a bad value falls back" true
    (parse route (u "/?size=x") = Some { size = 10; dense = false });
  Alcotest.(check string)
    "print in the canonical order" "/?size=20&dense=true"
    (Url.to_string (print route { size = 20; dense = true }));
  Alcotest.(check string)
    "defaults are not printed" "/"
    (Url.to_string (print route { size = 10; dense = false }))

let () =
  let t n f = Alcotest.test_case n `Quick f in
  Alcotest.run "silica router"
    [
      ( "router",
        [
          t "start" test_start;
          t "codecs and event names" test_codecs;
          t "url changed switches the page" test_url_changed_switches_page;
          t "same route, new parameters, keeps the page"
            test_same_route_new_parameters_keeps_page;
          t "reenter route restarts the page" test_reenter_route_restarts_page;
          t "reenter route runs exit" test_reenter_route_runs_exit;
          t "same url does nothing" test_same_url_does_nothing;
          t "back and forward" test_back_and_forward;
          t "bad path is not found" test_bad_path_is_not_found;
          t "query falls back per key" test_query;
          t "query change keeps the page" test_query_change_keeps_page;
          t "route round trip" test_route_round_trip;
          t "guard cancels" test_guard_cancel;
          t "guard redirects" test_guard_redirect;
          t "guard at the first url" test_guard_at_first_url;
          t "base path" test_base_path;
          t "decode outside the base" test_decode_outside_base;
          t "hash" test_hash;
          t "current link" test_current;
          t "preload attribute" test_preload_attribute;
          t "preload event" test_preload_event;
          t "exit hook" test_exit_hook;
          t "title" test_title;
          t "links and actions" test_links_and_actions;
          t "state names" test_state_names;
          t "no catch-all route" test_no_catch_all;
          t "first url outside the base" test_start_outside_base;
          t "url changed outside the base" test_url_changed_outside_base;
          t "preload outside the base" test_preload_outside_base;
          t "url changed with no match" test_url_changed_no_match;
        ]
      );
      ("route", [ t "patterns" test_route_units; t "map query" test_map_query ]);
    ]
