open Silica

type ev = Inc | Dec | Go

let show = function Inc -> "Inc" | Dec -> "Dec" | Go -> "Go"

let show_input show_event : (_, _) Check.input -> string = function
  | Check.Event event ->
      show_event event
  | Check.Props _ ->
      "new props"

let print report = Format.asprintf "%a" Check.pp_report report

let contains text part =
  let length = String.length part in
  let rec go index =
    index + length <= String.length text
    && (String.sub text index length = part || go (index + 1))
  in
  go 0

let inputs (violation : (_, _) Check.violation) =
  List.map
    (fun (step : _ Check.step) -> step.input)
    (Check.steps violation.counterexample)

let first report =
  match report.Check.violations with
  | violation :: _ ->
      violation
  | [] ->
      Alcotest.fail ("expected a violation, got:\n" ^ print report)

let passes report =
  if not (Check.passed report) then
    Alcotest.fail ("expected the check to pass, got:\n" ^ print report)
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

let violation name trace check =
  let violation = first (Check.run check) in
  Alcotest.(check string) "rule" name violation.rule;
  Alcotest.(check (list string))
    "trace" trace
    (List.map (show_input show) (inputs violation))

let test_ok () =
  let report = Check.run (Check.component (component counter_view)) in
  passes report;
  Alcotest.(check int) "states" 3 report.states;
  Alcotest.(check int) "transitions" 4 report.transitions

let non_negative =
  Check.always ~name:"non-negative" (Check.state (fun n -> n >= 0))

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
    (Check.component ~properties:[ non_negative ] c)

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
  passes (Check.run (Check.component c))

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
            [
              Attr.ref_ (Ref.make "x");
              Attr.string "aria-label" "x";
              Attr.on_press Inc;
            ]
            []
      )
      ~inc:(fun _ -> Some (1, [ Action.Focus (Ref.make "x") ]))
  in
  let report = Check.run (Check.component c) in
  passes report;
  Alcotest.(check int) "states" 2 report.states

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
  let report = Check.run (Check.component (Component.make machine view)) in
  passes report;
  Alcotest.(check int) "states" 2 report.states

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
  let report =
    Check.run (Check.component ~max_states:5 (component (fun _ -> btn "+" Inc)))
  in
  Alcotest.(check bool) "incomplete" false report.complete;
  Alcotest.(check bool) "not passed" false (Check.passed report);
  Alcotest.(check int) "states" 5 report.states;
  let text = print report in
  Alcotest.(check bool)
    "the text gives the state-limit hint" true
    (contains text "Bound the search with ~within, or raise ~max_states.")

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
  match
    Check.run (Check.component (Component.make machine (fun _ -> btn "+" Inc)))
  with
  | exception Check.Invalid_check message ->
      Alcotest.(check bool)
        "the message names the machine" true
        (contains message "machine c")
  | report ->
      Alcotest.fail ("expected Invalid_check, got:\n" ^ print report)

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
          [ Attr.string "aria-label" "count"; Attr.on_input (fun _ -> Inc) ]
          [ Node.text (string_of_int n) ]
      )
      ~inc:(fun n -> Some (min 1 (n + 1), []))
  in
  let report = Check.run (Check.component ~inputs:(fun _ -> [ Inc ]) c) in
  passes report;
  Alcotest.(check int) "states" 2 report.states

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
  let report = Check.run (Check.component ~properties:[ non_negative ] c) in
  Alcotest.(check string)
    "report"
    "c: 4 states, 5 transitions\n\
     violation 1: non-negative\n\
    \  The property \"non-negative\" does not hold in state active.\n\
    \  hint: Change the update or the view so that \"non-negative\" holds in \
     state active, or correct the property.\n\
    \  1. active -> active: (unnamed event), by the view offers press on \
     button \"+\" at button\n\
    \  2. active -> active: (unnamed event), by the view offers press on \
     button \"+\" at button[1]\n\
    \  3. active -> active: (unnamed event), by the view offers press on \
     button \"-\" at button\n\
    \  state: active\n\
    \  view:\n\
    \    button \"+\" [press]"
    (print report)

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
  Check.always ~name:"child-non-negative" (Check.state (fun n -> n >= 0))

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
  let report = Check.run (Check.component (Component.make machine view)) in
  passes report;
  Alcotest.(check int) "states of the root only" 2 report.states;
  Alcotest.(check (list string))
    "not explored"
    [
      "component child";
      "component props-child";
      "component unnamed-child";
      "island chat";
    ]
    report.not_explored;
  Alcotest.(check (list int))
    "the props given to a child, without duplicates" [ 3; 5 ]
    (Check.props report.children props_child_component);
  Alcotest.(check (list unit))
    "a child with no props" [ () ]
    (Check.props report.children child_component)

let test_no_children_nothing_listed () =
  let report = Check.run (Check.component (component counter_view)) in
  passes report;
  Alcotest.(check (list string)) "not explored" [] report.not_explored

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

let show_app formatter : _ Check.app_event -> unit = function
  | Check.Event event ->
      show_router_event formatter event
  | Check.Link url ->
      Format.fprintf formatter "link %s" (Url.to_string url)
  | Check.Browser_back ->
      Format.pp_print_string formatter "back"
  | Check.Browser_forward ->
      Format.pp_print_string formatter "forward"

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

let app ?(extra = []) ?properties ?(parse = Router.parse routes)
    ?(print = Router.print routes) ?(routes_to_check = all_routes) ?is_not_found
    () =
  Check.run
    (Check.app ?properties ~routes:routes_to_check ~parse ~print ?is_not_found
       ~url_event:(fun url -> Router.Url_changed url)
       (app_root ~extra ())
    )

let app_violation name trace report =
  let violation = first report in
  Alcotest.(check string) "rule" name violation.rule;
  Alcotest.(check (list string))
    "trace" trace
    (List.map (Format.asprintf "%a" show_app) (inputs violation))

let test_app_ok () =
  let history_matches =
    Check.always ~name:"history entry matches route"
      (Check.state (fun (state : _ Check.app_state) ->
           let url = List.nth state.history state.index in
           state.router = Router.parse routes url
           && Url.to_string (Router.print routes state.router)
              = Url.to_string url
       )
      )
  in
  let report =
    app ~properties:[ history_matches ]
      ~is_not_found:(Router.is_not_found routes)
      ()
  in
  passes report;
  Alcotest.(check bool) "explores history" true (report.states > 5);
  Alcotest.(check (list string))
    "page is not explored" [ "component child" ] report.not_explored

let test_back_returns () =
  let index_in_history =
    Check.always ~name:"index in history"
      (Check.state (fun (state : _ Check.app_state) ->
           state.index < List.length state.history
       )
      )
  in
  passes (app ~properties:[ index_in_history ] ())

let test_page_check_misses_child_bug () =
  let report = app () in
  passes report;
  Alcotest.(check (list string))
    "child listed" [ "component child" ] report.not_explored

let test_child_alone_finds_bug () =
  let violation =
    first
      (Check.run
         (Check.component ~properties:[ child_non_negative ]
            ~inputs:(fun _ -> [ Set (-1) ])
            child_component
         )
      )
  in
  Alcotest.(check string) "rule" "child-non-negative" violation.rule;
  Alcotest.(check (list string))
    "trace" [ "Set -1" ]
    (List.map (show_input show_child) (inputs violation))

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
  let violation =
    first
      (Check.run
         (Check.app ~routes:[ Front ] ~parse:(Router.parse paged)
            ~print:(Router.print paged)
            ~inputs:(fun _ -> [ Router.Url_changed (Url.of_string "/page/1") ])
            ~url_event:(fun url -> Router.Url_changed url)
            (Router.make paged ~render:(fun _ -> Node.text "page"))
         )
      )
  in
  Alcotest.(check string) "rule" "route-round-trip" violation.rule;
  Alcotest.(check bool)
    "reached the bad route" true
    (violation.state.router = Page 1)

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
  passes (app ~extra:[ raw_link "/items/007" ] ())

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
    Check.always ~name:"never-late" (Check.state (fun s -> s <> Late))
  in
  let report =
    Check.run (Check.component ~properties:[ never_late ] stale_component)
  in
  Alcotest.(check (list string))
    "no violation" []
    (List.map
       (fun (violation : _ Check.violation) -> violation.rule)
       report.violations
    );
  Alcotest.(check (list string))
    "the timer of waiting never fires in left" [ "late" ] report.unreached;
  let text = print report in
  Alcotest.(check bool)
    "the text gives the unreached-state hint" true
    (contains text "Add samples, inputs or props that reach late");
  let report =
    Check.run
      (Check.component ~properties:[ never_late ]
         ~allow:
           [
             Check.allow ~rule:"unreached-state" ~state:"late"
               ~reason:"the timer of waiting stops when the machine leaves it"
               ();
           ]
         stale_component
      )
  in
  passes report;
  Alcotest.(check int) "states" 3 report.states;
  Alcotest.(check int) "transitions" 3 report.transitions;
  Alcotest.(check (list int))
    "the allowance applied once" [ 1 ]
    (List.map snd report.allowed)

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
  let violation =
    first
      (Check.run
         (Check.component (Component.make machine (fun state -> view state)))
      )
  in
  Alcotest.(check string) "rule" "ignored-event" violation.rule;
  Alcotest.(check (list string))
    "the trace ends with the ignored event" [ "Go" ]
    (List.map (show_input show) (inputs violation));
  Alcotest.(check string)
    "the state before it" "a"
    (Machine.shape_name broken_shape violation.state);
  Alcotest.(check (option string))
    "the element" (Some "button[2]") violation.element;
  ( match Check.steps violation.counterexample with
  | [ step ] ->
      Alcotest.(check string) "from" "a" step.from;
      Alcotest.(check string) "target: the state did not change" "a" step.target;
      Alcotest.(check bool)
        "the cause is the press on the go button" true
        (step.cause
        = Check.View
            {
              handler = "press";
              element = "button[2]";
              role = Some "button";
              name = "go";
            }
        )
  | _ ->
      Alcotest.fail "expected one step"
  );
  Alcotest.(check string)
    "the message names the element and the state"
    "In state a, the view offers press on button \"go\" at button[2] gives the \
     event (unnamed), and the update returns Ignore."
    violation.message;
  Alcotest.(check bool)
    "the hint says how to fix it" true
    (contains violation.hint "Handle (unnamed) in state a");
  Alcotest.(check string)
    "the view at the failing step"
    "button \"inc\" [press]\nbutton \"go\" [press]\n" violation.view;
  let report =
    Check.run
      (Check.component
         (Component.make machine (fun state -> view ~disabled:true state))
      )
  in
  passes report;
  Alcotest.(check int)
    "a disabled button offers nothing: a (its timer runs again after it \
     fires), b"
    2 report.states;
  let machine, view = broken_component ~go:(Machine.Stay []) in
  let report =
    Check.run
      (Check.component (Component.make machine (fun state -> view state)))
  in
  passes report;
  Alcotest.(check int) "states" 2 report.states;
  Alcotest.(check int) "inc, go and the timer" 3 report.transitions

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
  let report =
    Check.run
      (Check.component
         (Component.make machine (fun _ ->
              Node.element "div" [ Attr.ref_ panel ] [ btn "+" Inc ]
          )
         )
      )
  in
  passes report;
  Alcotest.(check int) "states" 2 report.states

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
  let report =
    Check.run
      (Check.component
         (Dialog.component ~escape:true ~outside:true ~observers:true)
      )
  in
  passes report;
  Alcotest.(check int) "states" 2 report.states;
  Alcotest.(check int)
    "open; then Escape and outside press (one event, Close), resize, visible, \
     hidden"
    5 report.transitions

let test_escape_source_is_pressed () =
  let report =
    Check.run
      (Check.component
         (Dialog.component ~escape:true ~outside:false ~observers:false)
      )
  in
  passes report;
  Alcotest.(check int) "states" 2 report.states;
  Alcotest.(check int) "open, then Escape" 2 report.transitions

let test_outside_press_source_is_pressed () =
  let report =
    Check.run
      (Check.component
         (Dialog.component ~escape:false ~outside:true ~observers:false)
      )
  in
  passes report;
  Alcotest.(check int) "states" 2 report.states;
  Alcotest.(check int) "open, then outside press" 2 report.transitions

let test_modal_without_a_way_out_is_a_dead_end () =
  let violation =
    first
      (Check.run
         (Check.component
            (Dialog.component ~escape:false ~outside:false ~observers:false)
         )
      )
  in
  Alcotest.(check string) "rule" "dead-end" violation.rule;
  Alcotest.(check (list string))
    "opened and stuck" [ "Open" ]
    (List.map (show_input Dialog.show) (inputs violation));
  Alcotest.(check bool)
    "the hint names the state" true
    (contains violation.hint "leaves opened")

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
       ~properties:
         [ Check.always ~name:"below-one" (Check.state (fun n -> n < 1)) ]
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
       ~properties:
         [ Check.always ~name:"below-one" (Check.state (fun n -> n < 1)) ]
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
  Alcotest.(check (list string))
    "a boundary with a reset passes" []
    (List.map
       (fun (violation : _ Check.violation) -> violation.rule)
       (Check.run (Check.component ~max_states:5 with_reset)).violations
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
  let violation = first (Check.run (Check.live live)) in
  Alcotest.(check string) "rule" "live-uses-client-source" violation.rule;
  Alcotest.(check string)
    "reached in the opened state" "opened"
    (Machine.shape_name Dialog.state_shape violation.state);
  Alcotest.(check int) "after one event" 1 (List.length (inputs violation));
  Alcotest.(check bool)
    "the hint names the source" true
    (contains violation.hint "focus_trap");
  let fine =
    Live.declare ~channel:"fine" ~state:int_codec
      ~event:
        (Codec.make
           ~encode:(fun _ -> Silica_json.Json.declassify (`Int 0))
           ~decode:(fun _ -> Ok Inc)
        )
      (component counter_view)
  in
  passes (Check.run (Check.live fine))

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
  let report = Check.run (Check.component Gadget.component) in
  passes report;
  Alcotest.(check int)
    "the start, then each subset of the three results after Ask" 9 report.states;
  let all_three =
    Check.always ~name:"not-all-three"
      (Check.state (fun (_, got) -> List.length got < 3))
  in
  let violation =
    first
      (Check.run (Check.component ~properties:[ all_three ] Gadget.component))
  in
  Alcotest.(check (list string))
    "a fixed rect, copy and load succeed"
    [ "Ask"; "Got copy:true"; "Got load:true"; "Got rect:0" ]
    (List.sort compare (List.map (show_input Gadget.show) (inputs violation)));
  Alcotest.(check (list string))
    "each result names its action"
    [ "Copy"; "Load_script"; "Measure" ]
    (List.sort compare
       (List.filter_map
          (fun (step : _ Check.step) ->
            match step.cause with
            | Check.Action_result action ->
                Some action
            | _ ->
                None
          )
          (Check.steps violation.counterexample)
       )
    )

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
    Check.always ~name:"not-all"
      (Check.state (fun (Watcher.Watching got) -> List.length got < 5))
  in
  let violation =
    first (Check.run (Check.component ~properties:[ all ] Watcher.component))
  in
  let (Watcher.Watching got) = violation.state in
  Alcotest.(check (list string))
    "scroll gives a zero position; visibility and online give both values"
    [
      "online:false"; "online:true"; "scroll:0"; "visible:false"; "visible:true";
    ]
    got;
  Alcotest.(check bool)
    "each step is caused by a held source" true
    (List.for_all
       (fun (step : _ Check.step) ->
         match step.cause with Check.Held _ -> true | _ -> false
       )
       (Check.steps violation.counterexample)
    )

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
  violation "live-uses-client-action" [ "Go" ] (Check.live ~max_states:100 live)

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
      ~events:
        (Machine.shape
           ~name:(function
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
           ()
        )
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
    Check.run
      (Check.component
         ~inputs:(fun _ -> [ Input "a" ])
         ~samples:[ Check.answers endpoint [ [ "hit" ] ] ]
         component
      )
end

let ok_of report =
  passes report;
  match report.Check.specs with
  | spec :: _ ->
      (spec, report.paths)
  | [] ->
      Alcotest.fail "expected a spec"

let test_spec () =
  let spec, _ = ok_of (Search.check ()) in
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
    (Spec.arrows spec);
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
       spec.edges
    )

let test_paths () =
  let _, paths = ok_of (Search.check ()) in
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
       (fun (name, path) ->
         ( name,
           List.map
             (fun (step : _ Check.step) -> show_input Search.show step.input)
             path
         )
       )
       paths
    )

type kept = Counting of int [@@deriving state_shape]

let test_kept_steps_are_labels_not_edges () =
  let machine =
    Machine.make ~id:"kept" ~init:(Counting 0) ~shape:kept_shape
      ~events:(Machine.shape ~name:show ())
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
  let spec, _ =
    ok_of (Check.run (Check.component (Component.make machine view)))
  in
  Alcotest.(check (list (pair string string)))
    "only the reenter is an arrow"
    [ ("counting", "counting") ]
    (Spec.arrows spec);
  Alcotest.(check (list (pair string bool)))
    "labels"
    [ ("Dec", false); ("Go", true); ("Inc", true) ]
    (List.sort_uniq compare
       (List.map
          (fun (label : Spec.label) -> (label.event, label.kept))
          spec.edges
       )
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
    Check.always ~name:"the call has the input of the state"
      (Check.state (function Loading_item n -> n >= 0 | Shown_item -> true))
  in
  let report =
    Check.run
      (Check.component ~props:[ 1; 2 ] ~properties:[ calls_follow_props ]
         item_component
      )
  in
  passes report;
  Alcotest.(check bool) "explores both props" true (report.states >= 4);
  Alcotest.(check (list string))
    "both states reached"
    [ "loading_item"; "shown_item" ]
    (List.sort compare (List.map fst report.paths));
  let never_two =
    Check.always ~name:"never-two"
      (Check.state (fun state -> state <> Loading_item 2))
  in
  let violation =
    first
      (Check.run
         (Check.component ~props:[ 1; 2 ] ~properties:[ never_two ]
            item_component
         )
      )
  in
  Alcotest.(check string) "rule" "never-two" violation.rule;
  Alcotest.(check (list string))
    "a start with the second props" []
    (List.map
       (fun (input : (int, _) Check.input) ->
         match input with
         | Check.Event _ ->
             "event"
         | Check.Props number ->
             "props " ^ string_of_int number
       )
       (inputs violation)
    )

type step_state = Zero | One | Two | Three [@@deriving state_shape]
type step_event = Next | Broken [@@deriving event_shape]

let three_breaches ?(update = fun state event -> (state, event)) () =
  let machine =
    Machine.make ~id:"three" ~init:Zero ~shape:step_state_shape
      ~events:step_event_shape (fun state event ->
        let state, event = update state event in
        match (state, event) with
        | Zero, Next ->
            Machine.Go One
        | One, Next ->
            Machine.Go Two
        | Two, (Next | Broken) ->
            Machine.Go Three
        | _ ->
            Machine.Ignore
    )
  in
  let view = function
    | Zero | One ->
        btn "next" Next
    | Two ->
        Node.list
          [ btn "next" Next; Node.element "button" [ Attr.on_press Broken ] [] ]
    | Three ->
        Node.text "stuck"
  in
  Component.make machine view

let not_one =
  Check.always ~name:"not-one" (Check.state (fun state -> state <> One))

let rules_of report =
  List.map
    (fun (violation : _ Check.violation) ->
      (violation.rule, violation.state_name)
    )
    report.Check.violations

let test_all_violations () =
  let report =
    Check.run (Check.component ~properties:[ not_one ] (three_breaches ()))
  in
  Alcotest.(check (list (pair string string)))
    "three breaches, in breadth-first order"
    [ ("not-one", "one"); ("accessible-name", "two"); ("dead-end", "three") ]
    (rules_of report);
  Alcotest.(check bool) "not truncated" false report.truncated;
  Alcotest.(check (list int))
    "each trace is a shortest path" [ 1; 2; 3 ]
    (List.map
       (fun (violation : _ Check.violation) ->
         List.length (Check.steps violation.counterexample)
       )
       report.violations
    );
  let report =
    Check.run
      (Check.component ~max_violations:2 ~properties:[ not_one ]
         (three_breaches ())
      )
  in
  Alcotest.(check (list (pair string string)))
    "max_violations keeps the first two"
    [ ("not-one", "one"); ("accessible-name", "two") ]
    (rules_of report);
  Alcotest.(check bool) "truncated" true report.truncated;
  Alcotest.(check bool)
    "the text says the search was truncated" true
    (contains (print report) "truncated: the search stopped after 2 violations")

let test_allowances () =
  let report =
    Check.run
      (Check.component ~properties:[ not_one ]
         ~allow:
           [
             Check.allow ~rule:"accessible-name" ~state:"two"
               ~element:"button[2]" ~reason:"a test of allowances" ();
             Check.allow ~rule:"not-one" ~reason:"a test of property allowances"
               ();
             Check.allow ~rule:"dead-end" ~state:"zero" ~reason:"never used" ();
           ]
         (three_breaches ())
      )
  in
  Alcotest.(check (list (pair string string)))
    "only the dead end is left"
    [ ("dead-end", "three") ]
    (rules_of report);
  Alcotest.(check (list (pair string int)))
    "each allowance with its count"
    [ ("accessible-name", 1); ("not-one", 1); ("dead-end", 0) ]
    (List.map
       (fun ((allowance : Check.allowance), count) -> (allowance.rule, count))
       report.allowed
    );
  Alcotest.(check bool)
    "the text gives each reason" true
    (contains (print report)
       "allowed accessible-name in two at button[2] (1): a test of allowances"
    );
  let invalid allow =
    match Check.run (Check.component ~allow (three_breaches ())) with
    | exception Check.Invalid_check message ->
        message
    | report ->
        Alcotest.fail ("expected Invalid_check, got:\n" ^ print report)
  in
  Alcotest.(check bool)
    "a blank reason" true
    (contains
       (invalid [ Check.allow ~rule:"dead-end" ~reason:" " () ])
       "has no reason"
    );
  Alcotest.(check bool)
    "a rule that does not exist" true
    (contains
       (invalid [ Check.allow ~rule:"dead-ends" ~reason:"typo" () ])
       "neither a built-in rule"
    )

let test_invalid_setup () =
  ( match Check.run (Check.component item_component) with
  | exception Check.Invalid_check message ->
      Alcotest.(check string)
        "props are needed" "the component item has props: give them with ~props"
        message
  | report ->
      Alcotest.fail ("expected Invalid_check, got:\n" ^ print report)
  );
  match
    Check.run
      (Check.app ~routes:[] ~parse:(Router.parse routes)
         ~print:(Router.print routes)
         ~url_event:(fun url -> Router.Url_changed url)
         (app_root ())
      )
  with
  | exception Check.Invalid_check message ->
      Alcotest.(check bool) "routes are needed" true (contains message "~routes")
  | report ->
      Alcotest.fail ("expected Invalid_check, got:\n" ^ print report)

let test_raised () =
  let update state event =
    match (state, event) with
    | One, Next ->
        failwith "boom"
    | _ ->
        (state, event)
  in
  let report = Check.run (Check.component (three_breaches ~update ())) in
  let violation = first report in
  Alcotest.(check string) "rule" "raised" violation.rule;
  Alcotest.(check string) "the state before the step" "one" violation.state_name;
  Alcotest.(check (list string))
    "the trace ends with the step that raised" [ "next"; "next" ]
    (List.map
       (fun (step : _ Check.step) -> step.event)
       (Check.steps violation.counterexample)
    );
  Alcotest.(check string)
    "message" "The update raised Failure(\"boom\") in state one."
    violation.message;
  Alcotest.(check bool)
    "the hint" true
    (contains violation.hint "Fix the exception in the update");
  Alcotest.(check (list string))
    "the search does not go on from a step that raised" [ "two"; "three" ]
    report.unreached;
  let machine =
    Machine.make ~id:"view-raises" ~init:Zero ~shape:step_state_shape
      ~events:step_event_shape (fun state event ->
        match (state, event) with
        | Zero, Next ->
            Machine.Go One
        | One, Next ->
            Machine.Go Zero
        | _ ->
            Machine.Ignore
    )
  in
  let view = function Zero -> btn "next" Next | _ -> invalid_arg "no view" in
  let violation =
    first (Check.run (Check.component (Component.make machine view)))
  in
  Alcotest.(check string) "rule" "raised" violation.rule;
  Alcotest.(check string)
    "the state the view failed in" "one" violation.state_name;
  Alcotest.(check string)
    "message" "The view raised Invalid_argument(\"no view\") in state one."
    violation.message;
  Alcotest.(check string) "no view text" "" violation.view

let test_views_per_state () =
  let report = Search.check () in
  Alcotest.(check (list string))
    "one view for each state, in the order of the paths"
    (List.map fst report.paths)
    (List.map fst report.views);
  Alcotest.(check string)
    "the view of the first snapshot of failed"
    "textbox \"query\" [input]\n\
     button \"retry\" [press]\n\
     button \"clear\" [press]\n"
    (List.assoc "failed" report.views)

let test_causes () =
  let report = Search.check () in
  let causes =
    List.map
      (fun (step : _ Check.step) ->
        (step.event, step.cause, step.from, step.target)
      )
      (List.assoc "shown" report.paths)
  in
  Alcotest.(check int) "three steps" 3 (List.length causes);
  match causes with
  | [
   ("input", Check.Input, "idle", "typing");
   ("fire", Check.Timer { ms = 300; id }, "typing", "searching");
   ( "got",
     Check.Answer { path = "/api/search"; id = answer },
     "searching",
     "shown"
   );
  ] ->
      Alcotest.(check string) "the timer id" "search/typing/after 300ms" id;
      Alcotest.(check string)
        "the call id" "search/searching//api/search \"a\"" answer
  | _ ->
      Alcotest.fail
        (String.concat "; "
           (List.map
              (fun (event, cause, from, target) ->
                Printf.sprintf "%s: %s -> %s by %s" event from target
                  (Check.describe_cause cause)
              )
              causes
           )
        )

let test_property_hint_and_event () =
  let violation =
    first
      (Check.run
         (Check.component
            ~inputs:(fun _ -> [ Search.Input "a" ])
            ~samples:[ Check.answers Search.endpoint [ [ "hit" ] ] ]
            ~properties:
              [
                Check.always ~name:"no-hit" ~hint:"Keep {state} away from hits."
                  (Check.not_
                     (Check.event (function
                       | Search.Got (Remote.Ok _) ->
                           true
                       | _ ->
                           false
                       ))
                  );
              ]
            Search.component
         )
      )
  in
  Alcotest.(check string)
    "the hint is filled" "Keep shown away from hits." violation.hint;
  Alcotest.(check (list string))
    "the first step whose event is a hit"
    [ "Input a"; "Fire"; "Got ok" ]
    (List.map (show_input Search.show) (inputs violation));
  let report =
    Check.run
      (Check.component
         ~properties:
           [
             Check.always ~name:"named and positive"
               Check.(
                 state_named "active"
                 &&& (state (fun n -> n >= 0) ||| view (fun _ -> false))
               );
           ]
         (component counter_view)
      )
  in
  passes report

let impure ~source =
  let starting = ref true in
  let machine =
    Machine.make ~id:"impure" ~init:0
      ~shape:(Machine.shape ~name:(fun _ -> "on") ())
      ~owns:(fun _ ->
        if !starting then
          [ source ]
        else
          []
      )
      (fun _ (_ : ev) -> Machine.Stay [])
  in
  Component.make machine (fun _ ->
      starting := false;
      btn "+" Inc
  )

let endpoint_for_owner =
  Remote.declare ~path:"/api/owner" ~input:Codec.int ~output:Codec.int
    ~rejected:Codec.string

let finding rule check =
  let report = Check.run check in
  match
    List.find_opt
      (fun (violation : _ Check.violation) -> violation.rule = rule)
      report.violations
  with
  | Some violation ->
      (violation.message, violation.hint)
  | None ->
      Alcotest.fail ("expected the rule " ^ rule ^ ", got:\n" ^ print report)

let text_finding rule report =
  let lines = String.split_on_char '\n' (print report) in
  let rec find = function
    | message :: hint :: _ when contains message (rule ^ ":") ->
        (message, hint)
    | _ :: rest ->
        find rest
    | [] ->
        Alcotest.fail
          ("expected the rule " ^ rule ^ " in the text, got:\n" ^ print report)
  in
  let message, hint = find lines in
  if not (contains hint "hint: ") then Alcotest.fail ("no hint after " ^ message);
  (message, hint)

let passes_view (view : ev Node.t) =
  passes
    (Check.run
       (Check.component
          (component
             (fun _ -> Node.list [ btn "+" Inc; view ])
             ~inc:(fun n -> Some (n, []))
          )
       )
    )

let test_accessible_views_pass () =
  let span text = Node.element "span" [] [ Node.text text ] in
  let link children =
    Node.element "a" [ Attr.string "href" "/issue/2" ] children
  in
  passes_view (link [ span "ENG-2"; Node.text " "; span "Second" ]);
  passes_view
    (link
       [
         Node.element "div" [] [ Node.text "ENG-2" ];
         Node.element "div" [] [ Node.text "Second" ];
       ]
    );
  passes_view
    (Node.element "a"
       [
         Attr.string "href" "/issue/2"; Attr.string "aria-label" "ENG-2 Second";
       ]
       [ span "ENG-2"; span "Second" ]
    );
  passes_view (link [ span "ENG-2"; span "(open)" ]);
  passes_view
    (Node.list
       [
         Node.element "label"
           [ Attr.string "for" "title" ]
           [ Node.text "Title" ];
         Node.element "input"
           [
             Attr.string "id" "title";
             Attr.string "aria-describedby" "title-help";
           ]
           [];
         Node.element "p"
           [ Attr.string "id" "title-help" ]
           [ Node.text "Short" ];
       ]
    );
  passes_view
    (Node.element "div"
       [
         Attr.string "role" "listbox";
         Attr.string "aria-label" "Fruit";
         Attr.string "tabindex" "0";
         Attr.string "aria-activedescendant" "apple";
       ]
       [
         Node.element "div"
           [
             Attr.string "role" "option";
             Attr.string "id" "apple";
             Attr.string "aria-selected" "true";
             Attr.on_press Inc;
           ]
           [ Node.text "Apple" ];
       ]
    );
  passes_view
    (Node.list
       [
         Node.element "h2" [] [ Node.text "Issues" ];
         Node.element "h3" [] [ Node.text "Open" ];
         Node.element "h2" [] [ Node.text "Closed" ];
       ]
    );
  passes_view
    (Node.element "img"
       [ Attr.string "src" "/dot.png"; Attr.string "alt" "" ]
       []
    )

let test_dialog_with_trap_passes () =
  let panel = Ref.make "panel" in
  let machine =
    Machine.make ~id:"trapped" ~init:0
      ~shape:(Machine.shape ~name:(fun _ -> "open") ())
      ~owns:(fun _ -> [ Source.focus_trap panel ])
      (fun _ (event : ev) ->
        match event with Inc -> Machine.Stay [] | _ -> Machine.Ignore
      )
  in
  passes
    (Check.run
       (Check.component
          (Component.make machine (fun _ ->
               Node.element "div"
                 [
                   Attr.ref_ panel;
                   Attr.string "role" "dialog";
                   Attr.string "aria-modal" "true";
                   Attr.string "aria-label" "Edit";
                 ]
                 [ btn "+" Inc ]
           )
          )
       )
    )

let view_breach rule (breach : ev Node.t) =
  finding rule
    (Check.component
       (component
          (fun _ -> Node.list [ btn "+" Inc; breach ])
          ~inc:(fun n -> Some (n, []))
          ~dec:(fun n -> Some (n, []))
       )
    )

let suite_finding rule result =
  let module Json = Silica_json.Json in
  let text json =
    match Json.classify json with `String text -> Some text | _ -> None
  in
  let rec find json =
    match Json.classify json with
    | `Assoc fields -> (
        match
          ( Option.bind (List.assoc_opt "rule" fields) text,
            Option.bind (List.assoc_opt "message" fields) text,
            Option.bind (List.assoc_opt "hint" fields) text
          )
        with
        | Some found, Some message, Some hint when found = rule ->
            Some (message, hint)
        | _ ->
            List.find_map (fun (_, value) -> find value) fields
      )
    | `List items ->
        List.find_map find items
    | _ ->
        None
  in
  match find (Check_suite.to_json result) with
  | Some texts ->
      texts
  | None ->
      Alcotest.fail
        ("expected the rule " ^ rule ^ " in the JSON, got:\n"
        ^ Silica_json.Json.to_string (Check_suite.to_json result)
        )

let test_every_rule () =
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
  let live machine =
    Live.declare ~channel:"rule" ~state:Codec.int
      ~event:
        (Codec.make
           ~encode:(fun _ -> Silica_json.Json.declassify (`Int 0))
           ~decode:(fun _ -> Ok Go)
        )
      (Component.make machine (fun _ -> btn "go" Go))
  in
  let fixtures =
    [
      ( "accessible-name",
        fun () ->
          finding "accessible-name"
            (Check.component
               (component
                  (fun _ -> Node.element "button" [ Attr.on_press Inc ] [])
                  ~inc:(fun n -> Some (n, []))
               )
            )
      );
      ( "field-label",
        fun () -> view_breach "field-label" (Node.element "input" [] [])
      );
      ( "img-alt",
        fun () ->
          view_breach "img-alt"
            (Node.element "img" [ Attr.string "src" "/logo.png" ] [])
      );
      ( "unique-ids",
        fun () ->
          view_breach "unique-ids"
            (Node.list
               [
                 Node.element "p" [ Attr.string "id" "note" ] [ Node.text "a" ];
                 Node.element "p" [ Attr.string "id" "note" ] [ Node.text "b" ];
               ]
            )
      );
      ( "aria-reference",
        fun () ->
          view_breach "aria-reference"
            (Node.element "p"
               [ Attr.string "aria-describedby" "missing" ]
               [ Node.text "a" ]
            )
      );
      ( "valid-role",
        fun () ->
          view_breach "valid-role"
            (Node.element "div" [ Attr.string "role" "buton" ] [ Node.text "a" ])
      );
      ( "required-children",
        fun () ->
          view_breach "required-children"
            (Node.element "ul"
               [
                 Attr.string "role" "listbox"; Attr.string "aria-label" "Fruit";
               ]
               [ Node.element "li" [] [ Node.text "Apple" ] ]
            )
      );
      ( "nested-interactive",
        fun () ->
          view_breach "nested-interactive"
            (Node.element "button"
               [ Attr.on_press (Dec : ev) ]
               [
                 Node.text "Open ";
                 Node.element "a"
                   [ Attr.string "href" "/help" ]
                   [ Node.text "help" ];
               ]
            )
      );
      ( "keyboard-access",
        fun () ->
          view_breach "keyboard-access"
            (Node.element "div"
               [ Attr.on_press (Dec : ev) ]
               [ Node.text "Remove" ]
            )
      );
      ( "dialog-name",
        fun () ->
          view_breach "dialog-name"
            (Node.element "div"
               [ Attr.string "role" "dialog" ]
               [ Node.text "a" ]
            )
      );
      ( "dialog-focus",
        fun () ->
          view_breach "dialog-focus"
            (Node.element "div"
               [
                 Attr.string "role" "dialog";
                 Attr.string "aria-modal" "true";
                 Attr.string "aria-label" "Edit";
               ]
               [ Node.text "a" ]
            )
      );
      ( "heading-order",
        fun () ->
          view_breach "heading-order"
            (Node.list
               [
                 Node.element "h1" [] [ Node.text "Issues" ];
                 Node.element "h3" [] [ Node.text "Open" ];
               ]
            )
      );
      ( "name-spacing",
        fun () ->
          view_breach "name-spacing"
            (Node.element "a"
               [ Attr.string "href" "/issue/2" ]
               [
                 Node.element "span" [] [ Node.text "ENG-2" ];
                 Node.element "span" [] [ Node.text "Second" ];
               ]
            )
      );
      ( "unique-refs",
        fun () ->
          let shared = Ref.make "dup" in
          finding "unique-refs"
            (Check.component
               (component
                  (fun _ ->
                    Node.list
                      [
                        Node.element "input" [ Attr.ref_ shared ] [];
                        Node.element "input" [ Attr.ref_ shared ] [];
                        btn "go" Inc;
                      ]
                  )
                  ~inc:(fun n -> Some (n, []))
               )
            )
      );
      ( "focus-target",
        fun () ->
          finding "focus-target"
            (Check.component
               (component
                  (fun n ->
                    if n = 0 then
                      btn "go" Inc
                    else
                      Node.text "done"
                  )
                  ~inc:(fun _ -> Some (1, [ Action.Focus (Ref.make "x") ]))
               )
            )
      );
      ( "boundary-reset",
        fun () ->
          finding "boundary-reset"
            (Check.component
               (component
                  (fun _ ->
                    Node.boundary
                      ~fallback:(fun _ ~reset:_ -> Node.text "broken")
                      (btn "+" Inc)
                  )
                  ~inc:(fun n -> Some (n, []))
               )
            )
      );
      ( "dead-end",
        fun () -> finding "dead-end" (Check.component (three_breaches ()))
      );
      ( "timer-owner",
        fun () ->
          finding "timer-owner"
            (Check.component (impure ~source:(Source.after 5 (Dec : ev))))
      );
      ( "call-owner",
        fun () ->
          finding "call-owner"
            (Check.component
               (impure
                  ~source:
                    (Remote.call endpoint_for_owner 1 (fun _ -> (Dec : ev)))
               )
            )
      );
      ( "ignored-event",
        fun () ->
          let machine, view = broken_component ~go:Machine.Ignore in
          finding "ignored-event" (Check.component (Component.make machine view))
      );
      ( "raised",
        fun () ->
          finding "raised"
            (Check.component
               (three_breaches
                  ~update:(fun state event ->
                    match state with
                    | One ->
                        failwith "boom"
                    | _ ->
                        (state, event)
                  )
                  ()
               )
            )
      );
      ( "unreached-state",
        fun () ->
          text_finding "unreached-state"
            (Check.run (Check.component stale_component))
      );
      ( "live-uses-client-source",
        fun () ->
          finding "live-uses-client-source"
            (Check.live
               (Live.declare ~channel:"dialog" ~state:Dialog.codec
                  ~event:
                    (Codec.make
                       ~encode:(fun _ -> Silica_json.Json.declassify (`Int 0))
                       ~decode:(fun _ -> Ok Dialog.Open)
                    )
                  (Dialog.component ~escape:true ~outside:true ~observers:true)
               )
            )
      );
      ( "live-uses-client-action",
        fun () ->
          finding "live-uses-client-action"
            (Check.live
               (live
                  (simple ~id:"m" ~init:0 (fun n -> function
                    | Go ->
                        Some (n, [ Action.Set_status 404 ])
                    | Inc | Dec ->
                        None
                    ))
               )
            )
      );
      ( "route-round-trip",
        fun () ->
          finding "route-round-trip"
            (Check.app ~routes:[ Front ] ~parse:(Router.parse paged)
               ~print:(Router.print paged)
               ~inputs:(fun _ ->
                 [ Router.Url_changed (Url.of_string "/page/1") ]
               )
               ~url_event:(fun url -> Router.Url_changed url)
               (Router.make paged ~render:(fun _ -> Node.text "page"))
            )
      );
      ( "link-parses",
        fun () ->
          finding "link-parses"
            (Check.app ~routes:all_routes ~parse:(Router.parse routes)
               ~print:(Router.print routes)
               ~url_event:(fun url -> Router.Url_changed url)
               (app_root ~extra:[ raw_link "/items?sort=name" ] ())
            )
      );
      ( "state-limit",
        fun () ->
          text_finding "state-limit"
            (Check.run
               (Check.component ~max_states:5 (component (fun _ -> btn "+" Inc)))
            )
      );
      ( "unchecked-component",
        fun () ->
          suite_finding "unchecked-component"
            (Check_suite.run
               [
                 Check_suite.entry
                   (Check.component
                      ~within:(fun n -> n <= 1)
                      (component (fun _ ->
                           Node.list
                             [ btn "+" Inc; Node.component child_component ]
                       )
                      )
                   );
               ]
            )
      );
      ( "golden-drift",
        fun () ->
          suite_finding "golden-drift"
            (Check_suite.golden "no-golden-files"
               (Check_suite.run
                  [
                    Check_suite.entry
                      (Check.component
                         ~within:(fun n -> n <= 1)
                         (component (fun _ -> btn "+" Inc))
                      );
                  ]
               )
            )
      );
    ]
  in
  Alcotest.(check (list string))
    "a fixture for every built-in rule"
    (List.sort compare
       (List.map (fun (rule : Check.rule) -> rule.id) Check.rules)
    )
    (List.sort compare (List.map fst fixtures));
  List.iter
    (fun (rule, fixture) ->
      let message, hint = fixture () in
      let row =
        List.find (fun (row : Check.rule) -> row.id = rule) Check.rules
      in
      Alcotest.(check bool)
        (rule ^ ": a message") true
        (String.trim row.message <> "");
      Alcotest.(check bool) (rule ^ ": a hint") true (String.trim row.hint <> "");
      Alcotest.(check bool)
        (rule ^ ": the violation has a hint")
        true
        (String.trim hint <> "");
      Alcotest.(check bool)
        (rule ^ ": every place is filled")
        false
        (contains message "{" || contains hint "{")
    )
    fixtures

let () =
  Alcotest.run "check"
    [
      ( "component",
        [
          Alcotest.test_case "correct component passes" `Quick test_ok;
          Alcotest.test_case "seeded bug, shortest trace" `Quick test_seeded_bug;
          Alcotest.test_case "missing accessible name" `Quick test_no_name;
          Alcotest.test_case "accessible views pass the rules" `Quick
            test_accessible_views_pass;
          Alcotest.test_case "a modal dialog with a focus trap" `Quick
            test_dialog_with_trap_passes;
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
      ( "report",
        [
          Alcotest.test_case "all violations, max_violations" `Quick
            test_all_violations;
          Alcotest.test_case "allowances" `Quick test_allowances;
          Alcotest.test_case "setup errors" `Quick test_invalid_setup;
          Alcotest.test_case "raised" `Quick test_raised;
          Alcotest.test_case "a view for each state" `Quick test_views_per_state;
          Alcotest.test_case "the cause of each step" `Quick test_causes;
          Alcotest.test_case "property hint and event condition" `Quick
            test_property_hint_and_event;
          Alcotest.test_case "every rule breaks, with a hint" `Quick
            test_every_rule;
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
