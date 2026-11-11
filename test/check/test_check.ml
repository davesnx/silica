open Silica

type ev = Inc | Dec | Go

let show = function Inc -> "Inc" | Dec -> "Dec" | Go -> "Go"

let show_input show_event : (_, _) Check.input -> string = function
  | Check.Event event ->
      show_event event
  | Check.Props _ ->
      "new props"
let btn label e = Node.element "button" [ Attr.on_press e ] [ Node.text label ]

let simple = Silica_test_support.Simple.machine

let component ?(inc = fun n -> Some (n + 1, []))
    ?(dec = fun n -> Some (n - 1, [])) view =
  let machine =
    simple ~id:"c" ~init:0 (fun n -> function
      | Inc ->
          inc n
      | Dec ->
          dec n
      | Go ->
          None
      )
  in
  Component.make machine view

let counter_view n =
  Node.list
    (( if n < 2 then
         [ btn "+" Inc ]
       else
         []
     )
    @
    if n > 0 then
      [ btn "-" Dec ]
    else
      []
    )

let violation name trace = function
  | Check.Violation v ->
      Alcotest.(check string) "invariant" name v.invariant;
      Alcotest.(check (list string))
        "trace" trace
        (List.map (show_input show) v.trace)
  | Check.Ok _ ->
      Alcotest.fail "expected violation, got ok"
  | Check.Limit _ ->
      Alcotest.fail "expected violation, got limit"
  | Check.Unreached _ ->
      Alcotest.fail "expected violation, got unreached"
  | Check.Error m ->
      Alcotest.fail ("expected violation, got error: " ^ m)

let test_ok () =
  match Check.component (component counter_view) with
  | Check.Ok { states; transitions; _ } ->
      Alcotest.(check int) "states" 3 states;
      Alcotest.(check int) "transitions" 4 transitions
  | _ ->
      Alcotest.fail "expected ok"

let non_negative = { Check.name = "non-negative"; holds = (fun n _ -> n >= 0) }

let test_seeded_bug () =
  let c =
    component counter_view ~dec:(fun n ->
        Some
          ( ( if n = 2 then
                n - 3
              else
                n - 1
            ),
            []
          )
    )
  in
  violation "non-negative" [ "Inc"; "Inc"; "Dec" ]
    (Check.component ~invariants:[ non_negative ] c)

let test_no_name () =
  let c = component (fun _ -> Node.element "button" [ Attr.on_press Inc ] []) in
  violation "accessible-name" [] (Check.component c)

let test_aria_label_is_a_name () =
  let c =
    component
      (fun _ ->
        Node.element "button"
          [ Attr.on_press Inc; Attr.string "aria-label" "plus" ]
          []
      )
      ~inc:(fun n -> Some (n, []))
  in
  match Check.component c with
  | Check.Ok _ ->
      ()
  | _ ->
      Alcotest.fail "expected ok"

let test_missing_focus_ref () =
  let c =
    component
      (fun n ->
        if n = 0 then
          btn "go" Inc
        else
          Node.text "done"
      )
      ~inc:(fun _ -> Some (1, [ Action.Focus (Ref.make "x") ]))
  in
  violation "focus-target" [ "Inc" ] (Check.component ~inputs:(fun _ -> []) c)

let test_present_focus_ref () =
  let c =
    component
      (fun n ->
        if n = 0 then
          btn "go" Inc
        else
          Node.element "input"
            [ Attr.ref_ (Ref.make "x"); Attr.on_press Inc ]
            [ Node.text "x" ]
      )
      ~inc:(fun _ -> Some (1, [ Action.Focus (Ref.make "x") ]))
  in
  match Check.component c with
  | Check.Ok { states; _ } ->
      Alcotest.(check int) "states" 2 states
  | _ ->
      Alcotest.fail "expected ok"

let test_dead_end () =
  let c =
    component
      (fun n ->
        if n = 0 then
          btn "go" Go
        else
          Node.text "stuck"
      )
      ~inc:(fun _ -> None)
  in
  let machine =
    simple ~id:"c" ~init:0 (fun _ -> function Go -> Some (1, []) | _ -> None)
  in
  violation "dead-end" [ "Go" ]
    (Check.component (Component.make machine (c.Component.view ())))

let test_dead_end_final_ok () =
  let machine =
    simple ~id:"c" ~init:0
      ~final:(fun n -> n = 1)
      (fun _ -> function Go -> Some (1, []) | _ -> None)
  in
  let view n =
    if n = 0 then
      btn "go" Go
    else
      Node.text "done"
  in
  match Check.component (Component.make machine view) with
  | Check.Ok { states; _ } ->
      Alcotest.(check int) "states" 2 states
  | _ ->
      Alcotest.fail "expected ok"

let test_duplicate_refs () =
  let r = Ref.make "dup" in
  let c =
    component
      (fun _ ->
        Node.list
          [
            Node.element "input" [ Attr.ref_ r ] [];
            Node.element "input" [ Attr.ref_ r ] [];
            btn "go" Inc;
          ]
      )
      ~inc:(fun n -> Some (n, []))
  in
  violation "unique-refs" [] (Check.component c)

let test_limit () =
  match Check.component ~max_states:5 (component (fun _ -> btn "+" Inc)) with
  | Check.Limit { states } ->
      Alcotest.(check int) "states" 5 states
  | _ ->
      Alcotest.fail "expected limit"

type fstate = { f : int -> int; n : int }

let test_functional_value () =
  let machine =
    simple ~id:"c" ~init:{ f = succ; n = 0 } (fun s -> function
      | Inc ->
          Some ({ s with n = s.f s.n }, [])
      | _ ->
          None
      )
  in
  match Check.component (Component.make machine (fun _ -> btn "+" Inc)) with
  | Check.Error m ->
      Alcotest.(check bool) "message" true (String.length m > 0)
  | _ ->
      Alcotest.fail "expected error"

type wait = Waiting | Over [@@deriving state_shape]

let test_after_is_enabled () =
  let machine =
    Machine.make ~id:"c" ~init:Waiting ~shape:wait_shape
      ~owns:(function Waiting -> [ Source.after 100 Go ] | Over -> [])
      (fun state event ->
        match (state, event) with
        | Waiting, Go ->
            Machine.Go Over
        | _ ->
            Machine.Ignore
      )
  in
  let view state = Node.text (Machine.shape_name wait_shape state) in
  violation "dead-end" [ "Go" ] (Check.component (Component.make machine view))

let test_inputs () =
  let c =
    component
      (fun n ->
        Node.element "input"
          [ Attr.on_input (fun _ -> Inc) ]
          [ Node.text (string_of_int n) ]
      )
      ~inc:(fun n -> Some (min 1 (n + 1), []))
  in
  match Check.component ~inputs:(fun _ -> [ Inc ]) c with
  | Check.Ok { states; _ } ->
      Alcotest.(check int) "states" 2 states
  | _ ->
      Alcotest.fail "expected ok"

let test_pp () =
  let c =
    component counter_view ~dec:(fun n ->
        Some
          ( ( if n = 2 then
                n - 3
              else
                n - 1
            ),
            []
          )
    )
  in
  let r = Check.component ~invariants:[ non_negative ] c in
  let out =
    Format.asprintf "%a"
      (Check.pp_report
         ~pp_event:
           (Check.pp_input (fun f e -> Format.pp_print_string f (show e)))
         ~pp_state:Format.pp_print_int
      )
      r
  in
  Alcotest.(check string)
    "report"
    "invariant \"non-negative\" violated after 3 steps:\n\
    \  1. Inc\n\
    \  2. Inc\n\
    \  3. Dec\n\
     state: -1"
    out

type child_event = Set of int | Dec

let show_child = function Set n -> Printf.sprintf "Set %d" n | Dec -> "Dec"

let child_component =
  let machine =
    simple ~id:"child" ~init:0 (fun n -> function
      | Set value ->
          Some (value, [])
      | Dec ->
          Some (n - 1, [])
      )
  in
  let view n =
    if n > 0 then
      Node.element "button" [ Attr.on_press Dec ] [ Node.text "dec" ]
    else
      Node.element "button" [ Attr.on_press (Set 1) ] [ Node.text "inc" ]
  in
  Component.make machine view

let unnamed_child_component =
  let machine = simple ~id:"unnamed-child" ~init:0 (fun _ _ -> None) in
  let view _ = Node.element "button" [ Attr.on_press Dec ] [] in
  Component.make machine view

type props_child = Showing [@@deriving state_shape]

let props_child_component =
  Component.make_with_props
    (Machine.make_with_props ~id:"props-child" ~init:Showing
       ~shape:props_child_shape (fun _ Showing () -> Machine.Ignore
     )
    )
    (fun number Showing -> Node.text (string_of_int number))

let child_non_negative =
  { Check.name = "child-non-negative"; holds = (fun n _ -> n >= 0) }

let test_component_node_not_explored () =
  let machine =
    simple ~id:"root" ~init:0 (fun n -> function
      | Inc ->
          Some (min 1 (n + 1), [])
      | _ ->
          None
      )
  in
  let view _ =
    Node.list
      [
        btn "+" Inc;
        Node.component child_component;
        Node.component unnamed_child_component;
        Node.component_with_props ~props:3 props_child_component;
        Node.component_with_props ~props:5 props_child_component;
        Node.component_with_props ~props:3 props_child_component;
        Node.element "div" [ Attr.string "data-silica-live" "chat" ] [];
        Node.element "div" [ Attr.string "data-silica-live" "chat" ] [];
      ]
  in
  match Check.component (Component.make machine view) with
  | Check.Ok { states; not_explored; children; _ } ->
      Alcotest.(check int) "states of the root only" 2 states;
      Alcotest.(check (list string))
        "not explored"
        [
          "component child";
          "component props-child";
          "component unnamed-child";
          "island chat";
        ]
        not_explored;
      Alcotest.(check (list int))
        "the props given to a child, without duplicates" [ 3; 5 ]
        (Check.props children props_child_component);
      Alcotest.(check (list unit))
        "a child with no props" [ () ]
        (Check.props children child_component)
  | _ ->
      Alcotest.fail "expected ok"

let test_no_children_nothing_listed () =
  match Check.component (component counter_view) with
  | Check.Ok { not_explored; _ } ->
      Alcotest.(check (list string)) "not explored" [] not_explored
  | _ ->
      Alcotest.fail "expected ok"

type route = Items of string | Item of int | About | Not_found of string

let routes =
  Router.routes
    ~not_found:(function Not_found _ -> true | _ -> false)
    [
      Router.route
        Route.(s "items" +? string_query "sort" ~default:"name")
        (fun sort -> Items sort)
        (function Items sort -> Some sort | _ -> None);
      Router.route
        Route.(s "items" / int)
        (fun id -> Item id)
        (function Item id -> Some id | _ -> None);
      Router.route
        Route.(s "about")
        (fun () -> About)
        (function About -> Some () | _ -> None);
      Router.route Route.rest
        (fun path -> Not_found path)
        (function Not_found path -> Some path | _ -> None);
    ]

let items_default = Items "name"
let items_by_price = Items "price"
let item_seven = Item 7
let about = About
let missing = Not_found "/missing"
let all_routes = [ items_default; items_by_price; item_seven; about; missing ]

let show_router_event f = function
  | Router.Url_changed url ->
      Format.fprintf f "url_changed %s" (Url.to_string url)
  | Router.Preload url ->
      Format.fprintf f "preload %s" (Url.to_string url)

let show_app = Check.pp_app_event show_router_event

let good_links =
  [
    Router.link routes (Items "name") [] [ Node.text "items" ];
    Router.link routes (Items "price") [] [ Node.text "by price" ];
    Router.link routes (Item 7) [] [ Node.text "item" ];
    Router.link routes About [] [ Node.text "about" ];
  ]

let raw_link href =
  Node.element "a"
    [ Attr.string "href" href; Attr.bool "data-silica-link" true ]
    [ Node.text "raw" ]

let app_root ?(extra = []) () url =
  let render _ =
    Node.list (good_links @ extra @ [ Node.component child_component ])
  in
  Router.make routes ~render url

let app ?(extra = []) ?invariants ?(parse = Router.parse routes)
    ?(print = Router.print routes) ?(routes_to_check = all_routes) ?is_not_found
    () =
  Check.app ?invariants ~routes:routes_to_check ~parse ~print ?is_not_found
    ~url_event:(fun url -> Router.Url_changed url)
    (app_root ~extra ())

let app_failure report =
  Alcotest.fail
    (Format.asprintf "%a"
       (fun formatter report ->
         Check.pp_report ~pp_event:show_app formatter report
       )
       report
    )

let app_violation name trace = function
  | Check.Violation v ->
      Alcotest.(check string) "invariant" name v.invariant;
      Alcotest.(check (list string))
        "trace" trace
        (List.map (Format.asprintf "%a" show_app) v.trace)
  | report ->
      app_failure report

let test_app_ok () =
  let history_matches =
    {
      Check.name = "history entry matches route";
      holds =
        (fun (state : _ Check.app_state) _ ->
          let url = List.nth state.history state.index in
          state.router = Router.parse routes url
          && Url.to_string (Router.print routes state.router)
             = Url.to_string url
        );
    }
  in
  match
    app ~invariants:[ history_matches ]
      ~is_not_found:(Router.is_not_found routes)
      ()
  with
  | Check.Ok { states; not_explored; _ } ->
      Alcotest.(check bool) "explores history" true (states > 5);
      Alcotest.(check (list string))
        "page is not explored" [ "component child" ] not_explored
  | report ->
      app_failure report

let test_back_returns () =
  let index_in_history =
    {
      Check.name = "index in history";
      holds =
        (fun (state : _ Check.app_state) _ ->
          state.index < List.length state.history
        );
    }
  in
  match app ~invariants:[ index_in_history ] () with
  | Check.Ok _ ->
      ()
  | report ->
      app_failure report

let test_page_check_misses_child_bug () =
  match app () with
  | Check.Ok { not_explored; _ } ->
      Alcotest.(check (list string))
        "child listed" [ "component child" ] not_explored
  | report ->
      app_failure report

let test_child_alone_finds_bug () =
  match
    Check.component ~invariants:[ child_non_negative ]
      ~inputs:(fun _ -> [ Set (-1) ])
      child_component
  with
  | Check.Violation v ->
      Alcotest.(check string) "invariant" "child-non-negative" v.invariant;
      Alcotest.(check (list string))
        "trace" [ "Set -1" ]
        (List.map (show_input show_child) v.trace)
  | _ ->
      Alcotest.fail "expected violation"

let test_round_trip_query_printed_wrong () =
  let print = function
    | Items sort ->
        Router.print routes (Items (String.uppercase_ascii sort))
    | route ->
        Router.print routes route
  in
  app_violation "route-round-trip" [] (app ~print ())

let test_round_trip_parse_ignores_query () =
  let parse url =
    match Router.parse routes url with
    | Items _ ->
        items_default
    | route ->
        route
  in
  app_violation "route-round-trip" [] (app ~parse ())

let test_round_trip_item_parsed_wrong () =
  let parse url =
    match Router.parse routes url with Item _ -> Item 0 | route -> route
  in
  app_violation "route-round-trip" [] (app ~parse ())

type paged = Front | Page of int

let test_round_trip_route_pair_disagrees () =
  let paged =
    Router.routes
      [
        Router.route Route.root
          (fun () -> Front)
          (function Front -> Some () | Page _ -> None);
        Router.route
          Route.(s "page" / int)
          (fun number -> Page number)
          (function Page number -> Some (number + 1) | Front -> None);
      ]
  in
  match
    Check.app ~routes:[ Front ] ~parse:(Router.parse paged)
      ~print:(Router.print paged)
      ~inputs:(fun _ -> [ Router.Url_changed (Url.of_string "/page/1") ])
      ~url_event:(fun url -> Router.Url_changed url)
      (Router.make paged ~render:(fun _ -> Node.text "page"))
  with
  | Check.Violation v ->
      Alcotest.(check string) "invariant" "route-round-trip" v.invariant;
      Alcotest.(check bool)
        "reached the bad route" true (v.state.router = Page 1)
  | report ->
      app_failure report

let test_link_not_canonical () =
  app_violation "link-parses" [] (app ~extra:[ raw_link "/items?sort=name" ] ())

let test_link_to_not_found () =
  app_violation "link-parses" []
    (app
       ~extra:[ raw_link "/items/007" ]
       ~is_not_found:(Router.is_not_found routes)
       ()
    )

let test_link_to_not_found_allowed_without_predicate () =
  match app ~extra:[ raw_link "/items/007" ] () with
  | Check.Ok _ ->
      ()
  | report ->
      app_failure report

type stale_event = Leave | Back | Tick
type stale_state = Waiting | Left | Fired | Late [@@deriving state_shape]

let stale_component =
  let machine =
    Machine.make ~id:"timer" ~init:Waiting ~shape:stale_state_shape
      ~final:(function Fired | Late -> true | Waiting | Left -> false)
      ~owns:(function Waiting -> [ Source.after 10 Tick ] | _ -> [])
      (fun state event ->
        match (state, event) with
        | Waiting, Leave ->
            Machine.Go Left
        | Waiting, Tick ->
            Machine.Go Fired
        | Left, Back ->
            Machine.Go Waiting
        | Left, Tick ->
            Machine.Go Late
        | _ ->
            Machine.Ignore
      )
  in
  let view = function
    | Waiting ->
        Node.element "button" [ Attr.on_press Leave ] [ Node.text "leave" ]
    | Left ->
        Node.element "button" [ Attr.on_press Back ] [ Node.text "back" ]
    | Fired | Late ->
        Node.text "done"
  in
  Component.make machine view

let test_timer_stops_on_exit () =
  let never_late =
    { Check.name = "never-late"; holds = (fun s _ -> s <> Late) }
  in
  ( match Check.component ~invariants:[ never_late ] stale_component with
  | Check.Unreached { states } ->
      Alcotest.(check (list string))
        "the timer of waiting never fires in left" [ "late" ] states
  | _ ->
      Alcotest.fail "expected the state late to be unreached"
  );
  match
    Check.component ~invariants:[ never_late ] ~allow_unreached:[ "late" ]
      stale_component
  with
  | Check.Ok { states; transitions; _ } ->
      Alcotest.(check int) "states" 3 states;
      Alcotest.(check int) "transitions" 3 transitions
  | Check.Violation v ->
      Alcotest.fail
        (Printf.sprintf "violation %s after %d steps" v.invariant
           (List.length v.trace)
        )
  | _ ->
      Alcotest.fail "expected ok"

type broken = A | B [@@deriving state_shape]

let broken_component ~go =
  let machine =
    Machine.make ~id:"broken" ~init:A ~shape:broken_shape
      ~final:(function B -> true | A -> false)
      ~owns:(function A -> [ Source.after 5 (Dec : ev) ] | B -> [])
      (fun state (event : ev) ->
        match (state, event) with
        | A, Inc ->
            Machine.Go B
        | A, Go ->
            go
        | A, Dec ->
            Machine.Stay []
        | _ ->
            Machine.Ignore
      )
  in
  let view ?(disabled = false) = function
    | A ->
        Node.list
          [
            btn "inc" Inc;
            Node.element "button"
              [ Attr.on_press Go; Attr.bool "disabled" disabled ]
              [ Node.text "go" ];
          ]
    | B ->
        Node.text "done"
  in
  (machine, view)

let test_ignored_event () =
  let machine, view = broken_component ~go:Machine.Ignore in
  ( match Check.component (Component.make machine (fun state -> view state)) with
  | Check.Violation { invariant = "ignored-event"; trace; state } ->
      Alcotest.(check (list string))
        "the trace ends with the ignored event" [ "Go" ]
        (List.map (show_input show) trace);
      Alcotest.(check string)
        "the state before it" "a"
        (Machine.shape_name broken_shape state)
  | _ ->
      Alcotest.fail "expected ignored-event"
  );
  ( match
      Check.component
        (Component.make machine (fun state -> view ~disabled:true state))
    with
  | Check.Ok { states; _ } ->
      Alcotest.(check int)
        "a disabled button offers nothing: a (its timer runs again after it \
         fires), b"
        2 states
  | _ ->
      Alcotest.fail "expected ok with the button disabled"
  );
  let machine, view = broken_component ~go:(Machine.Stay []) in
  match Check.component (Component.make machine (fun state -> view state)) with
  | Check.Ok { states; transitions; _ } ->
      Alcotest.(check int) "states" 2 states;
      Alcotest.(check int) "inc, go and the timer" 3 transitions
  | _ ->
      Alcotest.fail "expected ok when the machine handles the event"

let test_ignored_timer_event () =
  let machine =
    Machine.make ~id:"timer" ~init:0
      ~shape:(Machine.shape ~name:(fun _ -> "counting") ())
      ~owns:(fun _ -> [ Source.after 5 (Dec : ev) ])
      (fun n (event : ev) ->
        match event with
        | Inc ->
            Machine.Go (min 1 (n + 1))
        | _ ->
            Machine.Ignore
      )
  in
  violation "ignored-event" [ "Dec" ]
    (Check.component (Component.make machine (fun _ -> btn "+" Inc)))

let test_ignored_held_event_is_allowed () =
  let panel = Ref.make "panel" in
  let machine =
    Machine.make ~id:"held" ~init:0
      ~shape:(Machine.shape ~name:(fun _ -> "open") ())
      ~owns:(fun _ ->
        [ Source.focus_trap panel; Source.resize panel (fun _ -> (Dec : ev)) ]
      )
      (fun n (event : ev) ->
        match event with
        | Inc ->
            Machine.Go (min 1 (n + 1))
        | _ ->
            Machine.Ignore
      )
  in
  match
    Check.component
      (Component.make machine (fun _ ->
           Node.element "div" [ Attr.ref_ panel ] [ btn "+" Inc ]
       )
      )
  with
  | Check.Ok { states; _ } ->
      Alcotest.(check int) "states" 2 states
  | Check.Violation v ->
      Alcotest.failf "violation %s: the events of held sources may be ignored"
        v.invariant
  | _ ->
      Alcotest.fail "expected ok"

module Dialog = struct
  type event = Open | Close | Resized | Seen of bool

  let show = function
    | Open ->
        "Open"
    | Close ->
        "Close"
    | Resized ->
        "Resized"
    | Seen visible ->
        "Seen " ^ string_of_bool visible

  type state = Closed | Opened [@@deriving state_shape]

  let panel = Ref.make "panel"

  let machine ~escape ~outside ~observers =
    let sources =
      [ Source.focus_trap panel; Source.scroll_lock ]
      @ ( if escape then
            [ Source.keys ~prevent:true [ (Key.escape, Close) ] ]
          else
            []
        )
      @ ( if outside then
            [ Source.outside_press panel Close ]
          else
            []
        )
      @
      if observers then
        [
          Source.resize panel (fun (_ : Rect.t) -> Resized);
          Source.intersection panel (fun visible -> Seen visible);
        ]
      else
        []
    in
    Machine.make ~id:"dialog" ~init:Closed ~shape:state_shape
      ~owns:(function Closed -> [] | Opened -> sources)
      (fun state event ->
        match (state, event) with
        | Closed, Open ->
            Machine.Go Opened
        | Opened, Close ->
            Machine.Go Closed
        | Opened, (Resized | Seen _) ->
            Machine.Stay []
        | _ ->
            Machine.Ignore
      )

  let view : state -> event Node.t = function
    | Closed ->
        Node.element "button" [ Attr.on_press Open ] [ Node.text "open" ]
    | Opened ->
        Node.element "div"
          [ Attr.ref_ panel ]
          [ Node.element "p" [] [ Node.text "dialog" ] ]

  let component ~escape ~outside ~observers =
    Component.make (machine ~escape ~outside ~observers) view

  let codec =
    Codec.make
      ~encode:(fun state ->
        Silica_json.Json.declassify
          (`Int (match state with Closed -> 0 | Opened -> 1))
      )
      ~decode:(fun _ -> Ok Closed)
end

let test_sources_give_events () =
  match
    Check.component (Dialog.component ~escape:true ~outside:true ~observers:true)
  with
  | Check.Ok { states; transitions; _ } ->
      Alcotest.(check int) "states" 2 states;
      Alcotest.(check int)
        "open; then Escape and outside press (one event, Close), resize, \
         visible, hidden"
        5 transitions
  | Check.Violation v ->
      Alcotest.failf "violation %s" v.invariant
  | _ ->
      Alcotest.fail "expected ok"

let test_escape_source_is_pressed () =
  match
    Check.component
      (Dialog.component ~escape:true ~outside:false ~observers:false)
  with
  | Check.Ok { states; transitions; _ } ->
      Alcotest.(check int) "states" 2 states;
      Alcotest.(check int) "open, then Escape" 2 transitions
  | _ ->
      Alcotest.fail "the Escape key of the source must be pressed"

let test_outside_press_source_is_pressed () =
  match
    Check.component
      (Dialog.component ~escape:false ~outside:true ~observers:false)
  with
  | Check.Ok { states; transitions; _ } ->
      Alcotest.(check int) "states" 2 states;
      Alcotest.(check int) "open, then outside press" 2 transitions
  | _ ->
      Alcotest.fail "the outside press of the source must be pressed"

let test_modal_without_a_way_out_is_a_dead_end () =
  match
    Check.component
      (Dialog.component ~escape:false ~outside:false ~observers:false)
  with
  | Check.Violation { invariant = "dead-end"; trace; _ } ->
      Alcotest.(check (list string))
        "opened and stuck" [ "Open" ]
        (List.map (show_input Dialog.show) trace)
  | _ ->
      Alcotest.fail "expected a dead end"

let key_view n =
  Node.element "input"
    [
      Attr.string "aria-label" "field";
      Attr.on_key_down (fun key ->
          if key.Key.key = "ArrowDown" then
            Some Inc
          else
            None
      );
    ]
    [ Node.text (string_of_int n) ]

let test_element_key_handler_is_pressed () =
  let c = component key_view in
  violation "below-one" [ "Inc" ]
    (Check.component
       ~invariants:[ { Check.name = "below-one"; holds = (fun n _ -> n < 1) } ]
       c
    );
  violation "dead-end" []
    (Check.component ~keys:[ Key.enter ] (component key_view))

let test_portal_handlers_are_explored () =
  let view n =
    Node.element "div" []
      [
        Node.portal
          [
            Node.element "button"
              [ Attr.on_press Inc ]
              [ Node.text "in portal" ];
          ];
        Node.text (string_of_int n);
      ]
  in
  violation "below-one" [ "Inc" ]
    (Check.component
       ~invariants:[ { Check.name = "below-one"; holds = (fun n _ -> n < 1) } ]
       (component view)
    )

let test_boundary_needs_a_reset () =
  let with_reset =
    component (fun _ ->
        Node.boundary
          ~fallback:(fun _ ~reset ->
            Node.element "button" [ reset ] [ Node.text "retry" ]
          )
          (btn "+" Inc)
    )
  in
  let without_reset =
    component (fun _ ->
        Node.boundary
          ~fallback:(fun _ ~reset:_ -> Node.text "broken")
          (btn "+" Inc)
    )
  in
  ( match Check.component ~max_states:5 with_reset with
  | Check.Ok _ | Check.Limit _ ->
      ()
  | _ ->
      Alcotest.fail "a boundary with a reset passes"
  );
  violation "boundary-reset" [] (Check.component without_reset)

let test_live_rejects_client_only_source () =
  let int_codec = Codec.int in
  let live =
    Live.declare ~channel:"dialog" ~state:Dialog.codec
      ~event:
        (Codec.make
           ~encode:(fun _ -> Silica_json.Json.declassify (`Int 0))
           ~decode:(fun _ -> Ok Dialog.Open)
        )
      (Dialog.component ~escape:true ~outside:true ~observers:true)
  in
  ( match Check.live live with
  | Check.Violation { invariant = "live-uses-client-source"; trace; state } ->
      Alcotest.(check string)
        "reached in the opened state" "opened"
        (Machine.shape_name Dialog.state_shape state);
      Alcotest.(check int) "after one event" 1 (List.length trace)
  | Check.Violation v ->
      Alcotest.failf "wrong violation %s" v.invariant
  | _ ->
      Alcotest.fail "expected a violation"
  );
  let fine =
    Live.declare ~channel:"fine" ~state:int_codec
      ~event:
        (Codec.make
           ~encode:(fun _ -> Silica_json.Json.declassify (`Int 0))
           ~decode:(fun _ -> Ok Inc)
        )
      (component counter_view)
  in
  match Check.live fine with
  | Check.Ok _ ->
      ()
  | Check.Violation v ->
      Alcotest.failf "unexpected violation %s" v.invariant
  | _ ->
      Alcotest.fail "expected ok"

module Gadget = struct
  type event = Ask | Got of string

  let show = function Ask -> "Ask" | Got s -> "Got " ^ s

  let ask =
    [
      Action.Measure
        {
          target = Ref.make "box";
          on_rect =
            (function
            | Some (r : Rect.t) ->
                Got (Printf.sprintf "rect:%.0f" r.width)
            | None ->
                Got "rect:none"
            );
        };
      Action.Copy
        {
          text = "t";
          on_result = Some (fun ok -> Got (Printf.sprintf "copy:%b" ok));
        };
      Action.Copy { text = "quiet"; on_result = None };
      Action.Load_script
        {
          src = "/a.js";
          on_result = (fun ok -> Got (Printf.sprintf "load:%b" ok));
        };
    ]

  let machine =
    simple ~id:"gadget" ~init:(false, []) (fun (asked, got) -> function
      | Ask when not asked ->
          Some ((true, got), ask)
      | Ask ->
          Some ((asked, got), [])
      | Got line ->
          Some ((asked, List.sort_uniq compare (line :: got)), [])
      )

  let view _ =
    Node.element "div"
      [ Attr.ref_ (Ref.make "box") ]
      [ Node.element "button" [ Attr.on_press Ask ] [ Node.text "ask" ] ]

  let component = Component.make machine view
end

let test_action_results_arrive () =
  ( match Check.component Gadget.component with
  | Check.Ok { states; _ } ->
      Alcotest.(check int)
        "the start, then each subset of the three results after Ask" 9 states
  | report ->
      Alcotest.fail
        (Format.asprintf "%a"
           (Check.pp_report
              ~pp_event:
                (Check.pp_input (fun f e ->
                     Format.pp_print_string f (Gadget.show e)
                 )
                )
              ?pp_state:None
           )
           report
        )
  );
  let all_three =
    {
      Check.name = "not-all-three";
      holds = (fun (_, got) _ -> List.length got < 3);
    }
  in
  match Check.component ~invariants:[ all_three ] Gadget.component with
  | Check.Violation v ->
      Alcotest.(check (list string))
        "a fixed rect, copy and load succeed"
        [ "Ask"; "Got copy:true"; "Got load:true"; "Got rect:0" ]
        (List.sort compare (List.map (show_input Gadget.show) v.trace))
  | _ ->
      Alcotest.fail "expected the three results to arrive"

module Watcher = struct
  type event = Got of string
  type state = Watching of string list [@@deriving state_shape]

  let machine =
    Machine.make ~id:"watcher" ~init:(Watching []) ~shape:state_shape
      ~owns:(fun _ ->
        [
          Source.scroll (fun (s : Scroll.t) ->
              Got (Printf.sprintf "scroll:%.0f" s.top)
          );
          Source.visibility (fun v -> Got (Printf.sprintf "visible:%b" v));
          Source.online (fun v -> Got (Printf.sprintf "online:%b" v));
        ]
      )
      (fun (Watching got) (Got line) ->
        Machine.Go (Watching (List.sort_uniq compare (line :: got)))
      )

  let view _ = Node.text "watching"
  let component = Component.make machine view
end

let test_new_sources_give_events () =
  let all =
    {
      Check.name = "not-all";
      holds = (fun (Watcher.Watching got) _ -> List.length got < 5);
    }
  in
  match Check.component ~invariants:[ all ] Watcher.component with
  | Check.Violation { state = Watcher.Watching got; _ } ->
      Alcotest.(check (list string))
        "scroll gives a zero position; visibility and online give both values"
        [
          "online:false";
          "online:true";
          "scroll:0";
          "visible:false";
          "visible:true";
        ]
        got
  | _ ->
      Alcotest.fail "expected every source event"

let test_scroll_to_missing_ref () =
  let machine =
    simple ~id:"s" ~init:() (fun () -> function
      | Go ->
          Some
            ( (),
              [
                Action.Scroll_to
                  { target = Some (Ref.make "gone"); left = 0; top = 0 };
              ]
            )
      | Inc | Dec ->
          None
      )
  in
  violation "focus-target" [ "Go" ]
    (Check.component (Component.make machine (fun () -> btn "go" Go)))

let test_live_rejects_result_action () =
  let machine =
    simple ~id:"m" ~init:0 (fun n -> function
      | Go ->
          Some
            ( n,
              [
                Action.Measure
                  { target = Ref.make "box"; on_rect = (fun _ -> Inc) };
              ]
            )
      | Inc | Dec ->
          None
      )
  in
  let live =
    Live.declare ~channel:"measure" ~state:Codec.int
      ~event:
        (Codec.make
           ~encode:(fun _ -> Silica_json.Json.declassify (`Int 0))
           ~decode:(fun _ -> Ok Go)
        )
      (Component.make machine (fun _ -> btn "go" Go))
  in
  violation "live-uses-client-action" [ "Go" ] (Check.live live)

let test_live_rejects_page_actions () =
  List.iter
    (fun action ->
      let machine =
        simple ~id:"m" ~init:0 (fun n -> function
          | Go ->
              Some (n, [ action ])
          | Inc | Dec ->
              None
          )
      in
      let live =
        Live.declare ~channel:"page" ~state:Codec.int
          ~event:
            (Codec.make
               ~encode:(fun _ -> Silica_json.Json.declassify (`Int 0))
               ~decode:(fun _ -> Ok Go)
            )
          (Component.make machine (fun _ -> btn "go" Go))
      in
      violation "live-uses-client-action" [ "Go" ] (Check.live live)
    )
    [
      Action.Set_status 404;
      Action.Revert_navigation;
      Action.Resume (Nav.Push (Url.of_string "/"));
      Action.Preload
        (Remote.request
           (Remote.declare ~path:"/p" ~input:Codec.int ~output:Codec.int
              ~rejected:Codec.string
           )
           1
        );
    ]

module Search = struct
  type event =
    | Input of string
    | Clear
    | Fire
    | Got of (string list, string) Remote.answer
    | Retry

  let show = function
    | Input text ->
        "Input " ^ text
    | Clear ->
        "Clear"
    | Fire ->
        "Fire"
    | Got (Remote.Ok _) ->
        "Got ok"
    | Got (Remote.Rejected _) ->
        "Got rejected"
    | Got (Remote.Failed _) ->
        "Got failed"
    | Retry ->
        "Retry"

  type state =
    | Idle
    | Typing of string
    | Searching of string
    | Shown of string * string list
    | Failed of string
  [@@deriving state_shape]

  let endpoint =
    Remote.declare ~path:"/api/search" ~input:Codec.string
      ~output:(Codec.list Codec.string) ~rejected:Codec.string

  let machine =
    Machine.make ~id:"search" ~init:Idle ~shape:state_shape
      ~owns:(function
        | Typing _ ->
            [ Source.after 300 Fire ]
        | Searching query ->
            [ Remote.call endpoint query (fun answer -> Got answer) ]
        | Idle | Shown _ | Failed _ ->
            []
        )
      (fun state event ->
        match (state, event) with
        | _, Input query ->
            Machine.Reenter (Typing query)
        | (Typing _ | Shown _ | Failed _), Clear ->
            Machine.Go Idle
        | Typing query, Fire ->
            Machine.Go (Searching query)
        | Searching query, Got (Remote.Ok hits) ->
            Machine.Go (Shown (query, hits))
        | Searching query, Got (Remote.Rejected _ | Remote.Failed _) ->
            Machine.Go (Failed query)
        | Failed query, Retry ->
            Machine.Go (Searching query)
        | _ ->
            Machine.Ignore
      )

  let btn_of label event =
    Node.element "button" [ Attr.on_press event ] [ Node.text label ]

  let view state =
    Node.list
      [
        Node.element "input"
          [
            Attr.string "aria-label" "query";
            Attr.on_input (fun text -> Input text);
          ]
          [];
        ( match state with
        | Idle | Typing _ | Searching _ ->
            Node.text "..."
        | Shown _ ->
            btn_of "clear" Clear
        | Failed _ ->
            Node.list [ btn_of "retry" Retry; btn_of "clear" Clear ]
        );
      ]

  let component = Component.make machine view

  let check () =
    Check.component
      ~inputs:(fun _ -> [ Input "a" ])
      ~samples:[ Check.answers endpoint [ [ "hit" ] ] ]
      ~label:(fun event ->
        match event with
        | Input _ ->
            "input"
        | Clear ->
            "clear"
        | Fire ->
            "fire"
        | Got _ ->
            "got"
        | Retry ->
            "retry"
      )
      component
end

let ok_of = function
  | Check.Ok { spec; edges; paths; _ } ->
      (spec, edges, paths)
  | report ->
      Alcotest.fail
        (Format.asprintf "%a"
           (Check.pp_report
              ~pp_event:
                (Check.pp_input (fun f e ->
                     Format.pp_print_string f (Search.show e)
                 )
                )
              ?pp_state:None
           )
           report
        )

let test_spec () =
  let spec, edges, _ = ok_of (Search.check ()) in
  Alcotest.(check string) "id" "search" spec.id;
  Alcotest.(check string) "init" "idle" spec.init;
  Alcotest.(check (list string))
    "states: the initial one, then by name"
    [ "idle"; "failed"; "searching"; "shown"; "typing" ]
    (List.map (fun (state : Spec.state) -> state.name) spec.states);
  let state name =
    List.find (fun (state : Spec.state) -> state.name = name) spec.states
  in
  Alcotest.(check (list int)) "timers" [ 300 ] (state "typing").timers;
  Alcotest.(check (list string))
    "sources" [ "/api/search" ] (state "searching").sources;
  Alcotest.(check (list (pair string string)))
    "the edges the run took"
    [
      ("failed", "idle");
      ("failed", "searching");
      ("failed", "typing");
      ("idle", "typing");
      ("searching", "failed");
      ("searching", "shown");
      ("searching", "typing");
      ("shown", "idle");
      ("shown", "typing");
      ("typing", "searching");
      ("typing", "typing");
    ]
    spec.edges;
  Alcotest.(check (list string))
    "lint" []
    (List.map Spec.issue_to_string (Spec.lint spec));
  Alcotest.(check bool)
    "a reenter is a self edge with its label" true
    (List.exists
       (fun (label : Spec.label) ->
         label.from = "typing" && label.target = "typing"
         && label.event = "input" && not label.kept
       )
       edges
    )

let test_paths () =
  let _, _, paths = ok_of (Search.check ()) in
  Alcotest.(check (list (pair string (list string))))
    "a shortest path to each state, in the order the search reached them"
    [
      ("idle", []);
      ("typing", [ "Input a" ]);
      ("searching", [ "Input a"; "Fire" ]);
      ("shown", [ "Input a"; "Fire"; "Got ok" ]);
      ("failed", [ "Input a"; "Fire"; "Got failed" ]);
    ]
    (List.map
       (fun (name, path) -> (name, List.map (show_input Search.show) path))
       paths
    )

type kept = Counting of int [@@deriving state_shape]

let test_kept_steps_are_labels_not_edges () =
  let machine =
    Machine.make ~id:"kept" ~init:(Counting 0) ~shape:kept_shape
      ~owns:(fun _ -> [ Source.after 5 (Dec : ev) ])
      (fun (Counting n) (event : ev) ->
        match event with
        | Inc ->
            Machine.Go (Counting (min 1 (n + 1)))
        | Dec ->
            Machine.Reenter (Counting 0)
        | Go ->
            Machine.Stay []
      )
  in
  let view _ = Node.list [ btn "+" Inc; btn "go" Go ] in
  let spec, edges =
    match Check.component ~label:show (Component.make machine view) with
    | Check.Ok { spec; edges; _ } ->
        (spec, edges)
    | _ ->
        Alcotest.fail "expected ok"
  in
  Alcotest.(check (list (pair string string)))
    "only the reenter is an edge"
    [ ("counting", "counting") ]
    spec.edges;
  Alcotest.(check (list (pair string bool)))
    "labels"
    [ ("Dec", false); ("Go", true); ("Inc", true) ]
    (List.sort_uniq compare
       (List.map (fun (label : Spec.label) -> (label.event, label.kept)) edges)
    )

type item = Shown_item | Loading_item of int [@@deriving state_shape]

let item_endpoint =
  Remote.declare ~path:"/api/item" ~input:Codec.int ~output:Codec.string
    ~rejected:Codec.string

let item_component =
  let machine =
    Machine.make_with_props ~id:"item" ~init:Shown_item ~shape:item_shape
      ~owns:(fun number -> function
        | Shown_item ->
            []
        | Loading_item _ ->
            [ Remote.call item_endpoint number (fun _ -> `Loaded) ]
        )
      ~on_props:(fun previous number state ->
        match (previous, state) with
        | None, _ ->
            Machine.Go (Loading_item number)
        | Some _, Loading_item _ ->
            Machine.Go (Loading_item number)
        | Some _, Shown_item ->
            Machine.Ignore
      )
      (fun _ state `Loaded ->
        match state with
        | Loading_item _ ->
            Machine.Go Shown_item
        | Shown_item ->
            Machine.Ignore
      )
  in
  Component.make_with_props machine (fun number state ->
      Node.text
        (Printf.sprintf "%d %s" number (Machine.shape_name item_shape state))
  )

let test_props_input () =
  let calls_follow_props =
    {
      Check.name = "the call has the input of the state";
      holds =
        (fun state _ ->
          match state with Loading_item n -> n >= 0 | Shown_item -> true
        );
    }
  in
  ( match
      Check.component ~props:[ 1; 2 ] ~invariants:[ calls_follow_props ]
        item_component
    with
  | Check.Ok { states; paths; _ } ->
      Alcotest.(check bool) "explores both props" true (states >= 4);
      Alcotest.(check (list string))
        "both states reached"
        [ "loading_item"; "shown_item" ]
        (List.sort compare (List.map fst paths))
  | Check.Violation v ->
      Alcotest.failf "violation %s" v.invariant
  | _ ->
      Alcotest.fail "expected ok"
  );
  let never_two =
    {
      Check.name = "never-two";
      holds = (fun state _ -> state <> Loading_item 2);
    }
  in
  match
    Check.component ~props:[ 1; 2 ] ~invariants:[ never_two ] item_component
  with
  | Check.Violation { invariant = "never-two"; trace; _ } ->
      Alcotest.(check (list string))
        "a start with the second props" []
        (List.map
           (fun (input : (int, _) Check.input) ->
             match input with
             | Check.Event _ ->
                 "event"
             | Check.Props n ->
                 "props " ^ string_of_int n
           )
           trace
        )
  | _ ->
      Alcotest.fail "expected never-two to fail"

let () =
  Alcotest.run "check"
    [
      ( "component",
        [
          Alcotest.test_case "correct component passes" `Quick test_ok;
          Alcotest.test_case "seeded bug, shortest trace" `Quick test_seeded_bug;
          Alcotest.test_case "missing accessible name" `Quick test_no_name;
          Alcotest.test_case "aria-label is a name" `Quick
            test_aria_label_is_a_name;
          Alcotest.test_case "focus to missing ref" `Quick
            test_missing_focus_ref;
          Alcotest.test_case "focus to present ref" `Quick
            test_present_focus_ref;
          Alcotest.test_case "dead end" `Quick test_dead_end;
          Alcotest.test_case "final state is not a dead end" `Quick
            test_dead_end_final_ok;
          Alcotest.test_case "duplicate refs" `Quick test_duplicate_refs;
          Alcotest.test_case "state limit" `Quick test_limit;
          Alcotest.test_case "functional value" `Quick test_functional_value;
          Alcotest.test_case "after events" `Quick test_after_is_enabled;
          Alcotest.test_case "timer stops on exit" `Quick
            test_timer_stops_on_exit;
          Alcotest.test_case "ignored event" `Quick test_ignored_event;
          Alcotest.test_case "ignored timer event" `Quick
            test_ignored_timer_event;
          Alcotest.test_case "an ignored held source event is allowed" `Quick
            test_ignored_held_event_is_allowed;
          Alcotest.test_case "inputs" `Quick test_inputs;
          Alcotest.test_case "pp_report" `Quick test_pp;
          Alcotest.test_case "Ok.spec" `Quick test_spec;
          Alcotest.test_case "Ok.paths" `Quick test_paths;
          Alcotest.test_case "kept steps are labels, not edges" `Quick
            test_kept_steps_are_labels_not_edges;
          Alcotest.test_case "props are an input" `Quick test_props_input;
        ]
      );
      ( "components",
        [
          Alcotest.test_case "child and island are listed, not run" `Quick
            test_component_node_not_explored;
          Alcotest.test_case "no child, nothing listed" `Quick
            test_no_children_nothing_listed;
          Alcotest.test_case "child bug: page check misses it" `Quick
            test_page_check_misses_child_bug;
          Alcotest.test_case "child bug: alone check finds it" `Quick
            test_child_alone_finds_bug;
        ]
      );
      ( "host v0.4",
        [
          Alcotest.test_case "sources give events" `Quick
            test_sources_give_events;
          Alcotest.test_case "Escape source is pressed" `Quick
            test_escape_source_is_pressed;
          Alcotest.test_case "outside press source is pressed" `Quick
            test_outside_press_source_is_pressed;
          Alcotest.test_case "modal without a way out is a dead end" `Quick
            test_modal_without_a_way_out_is_a_dead_end;
          Alcotest.test_case "element key handler is pressed" `Quick
            test_element_key_handler_is_pressed;
          Alcotest.test_case "portal handlers are explored" `Quick
            test_portal_handlers_are_explored;
          Alcotest.test_case "boundary needs a reset" `Quick
            test_boundary_needs_a_reset;
          Alcotest.test_case "action results arrive" `Quick
            test_action_results_arrive;
          Alcotest.test_case "scroll, visibility and online give events" `Quick
            test_new_sources_give_events;
          Alcotest.test_case "scroll_to a missing ref" `Quick
            test_scroll_to_missing_ref;
          Alcotest.test_case "live rejects an action with a result" `Quick
            test_live_rejects_result_action;
          Alcotest.test_case "live rejects navigation and page actions" `Quick
            test_live_rejects_page_actions;
          Alcotest.test_case "live rejects a client-only source" `Quick
            test_live_rejects_client_only_source;
        ]
      );
      ( "app",
        [
          Alcotest.test_case "history matches route" `Quick test_app_ok;
          Alcotest.test_case "back after navigate" `Quick test_back_returns;
          Alcotest.test_case "round trip: query printed wrong" `Quick
            test_round_trip_query_printed_wrong;
          Alcotest.test_case "round trip: parse ignores query" `Quick
            test_round_trip_parse_ignores_query;
          Alcotest.test_case "round trip: path data parsed wrong" `Quick
            test_round_trip_item_parsed_wrong;
          Alcotest.test_case "round trip: a route pair disagrees" `Quick
            test_round_trip_route_pair_disagrees;
          Alcotest.test_case "link is not canonical" `Quick
            test_link_not_canonical;
          Alcotest.test_case "link to the not-found route" `Quick
            test_link_to_not_found;
          Alcotest.test_case "not-found link needs the predicate" `Quick
            test_link_to_not_found_allowed_without_predicate;
        ]
      );
    ]
