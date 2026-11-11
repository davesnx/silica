open Silica

type ev = Inc | Dec | Go

let show = function Inc -> "Inc" | Dec -> "Dec" | Go -> "Go"
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
  { Component.machine; view }

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
      Alcotest.(check (list string)) "trace" trace (List.map show v.trace)
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
      ~inc:(fun _ -> None)
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
            [ Attr.ref_ (Ref.make "x"); Attr.on_press Go ]
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
  violation "dead-end" [ "Go" ] (Check.component { c with machine })

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
  match Check.component { Component.machine; view } with
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
      ~inc:(fun _ -> None)
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
  match
    Check.component { Component.machine; view = (fun _ -> btn "+" Inc) }
  with
  | Check.Error m ->
      Alcotest.(check bool) "message" true (String.length m > 0)
  | _ ->
      Alcotest.fail "expected error"

module Wait = struct
  type _ t = Waiting : unit t | Over : unit t
end

module Waiter = Machine.Make (Wait)

let test_after_is_enabled () =
  let table =
    Waiter.make ~id:"c"
      ~init:(Waiter.at Wait.Waiting ())
      [
        Waiter.state Wait.Waiting "waiting"
          ~after:[ (100, Go) ]
          ~to_:[ Wait.Over ]
          (fun () -> function
            | Go -> Waiter.goto Wait.Over () | _ -> Waiter.unhandled
            );
        Waiter.state Wait.Over "over" Waiter.ignore;
      ]
  in
  let machine =
    Machine.map_snapshot table
      ~get:(fun (Waiter.At (state, _) : Waiter.snapshot) : int ->
        match state with Wait.Waiting -> 0 | Wait.Over -> 1
      )
      ~set:(function
        | 0 -> Waiter.at Wait.Waiting () | _ -> Waiter.at Wait.Over ()
        )
  in
  let view n = Node.text (string_of_int n) in
  violation "dead-end" [ "Go" ] (Check.component { Component.machine; view })

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
         ~pp_event:(fun f e -> Format.pp_print_string f (show e))
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
        Node.component ~props:(Set 1) child_component;
        Node.component unnamed_child_component;
        Node.element "div" [ Attr.string "data-silica-live" "chat" ] [];
        Node.element "div" [ Attr.string "data-silica-live" "chat" ] [];
      ]
  in
  match Check.component { Component.machine; view } with
  | Check.Ok { states; not_explored; _ } ->
      Alcotest.(check int) "states of the root only" 2 states;
      Alcotest.(check (list string))
        "not explored"
        [ "component child"; "component unnamed-child"; "island chat" ]
        not_explored
  | _ ->
      Alcotest.fail "expected ok"

let test_no_children_nothing_listed () =
  match Check.component (component counter_view) with
  | Check.Ok { not_explored; _ } ->
      Alcotest.(check (list string)) "not explored" [] not_explored
  | _ ->
      Alcotest.fail "expected ok"

module State = struct
  type _ t =
    | Items : string t
    | Item : int t
    | About : unit t
    | Not_found : string t

  let path : type data. data t -> data Route.t = function
    | Items ->
        Route.(s "items" +? string_query "sort" ~default:"name")
    | Item ->
        Route.(s "items" / int)
    | About ->
        Route.(s "about")
    | Not_found ->
        Route.rest
end

module Routes = Router.Make (State)

let routes =
  Routes.routes ~not_found:State.Not_found
    Routes.[ State.Items; State.Item; State.About; State.Not_found ]

let items_default = Routes.At (State.Items, "name")
let items_by_price = Routes.At (State.Items, "price")
let item_seven = Routes.At (State.Item, 7)
let about = Routes.At (State.About, ())
let missing = Routes.At (State.Not_found, "/missing")
let all_routes = [ items_default; items_by_price; item_seven; about; missing ]

let show_router_event f = function
  | Router.Url_changed url ->
      Format.fprintf f "url_changed %s" (Url.to_string url)
  | Router.Preload url ->
      Format.fprintf f "preload %s" (Url.to_string url)

let show_app = Check.pp_app_event show_router_event

let good_links =
  [
    Routes.link routes State.Items "name" [] [ Node.text "items" ];
    Routes.link routes State.Items "price" [] [ Node.text "by price" ];
    Routes.link routes State.Item 7 [] [ Node.text "item" ];
    Routes.link routes State.About () [] [ Node.text "about" ];
  ]

let raw_link href =
  Node.element "a"
    [ Attr.string "href" href; Attr.bool "data-silica-link" true ]
    [ Node.text "raw" ]

let app_root ?(extra = []) ?(props = Set 1) () url =
  let render _ =
    Node.list (good_links @ extra @ [ Node.component ~props child_component ])
  in
  Routes.make routes ~render url

let not_found_edges =
  List.concat_map
    (fun route -> [ ("/*", route); (route, "/*") ])
    [ "/about"; "/items/:int"; "/items?sort" ]

let app ?(extra = []) ?invariants ?(parse = Routes.parse routes)
    ?(print = Routes.print routes) ?(routes_to_check = all_routes) ?is_not_found
    () =
  Check.app ?invariants ~allow_unreached:not_found_edges ~routes:routes_to_check
    ~parse ~print ?is_not_found
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
          state.router = Routes.parse routes url
          && Url.to_string (Routes.print routes state.router)
             = Url.to_string url
        );
    }
  in
  match
    app ~invariants:[ history_matches ]
      ~is_not_found:(Routes.is_not_found routes)
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
        (List.map show_child v.trace)
  | _ ->
      Alcotest.fail "expected violation"

let test_round_trip_query_printed_wrong () =
  let print = function
    | Routes.At (State.Items, sort) ->
        Routes.print routes
          (Routes.At (State.Items, String.uppercase_ascii sort))
    | route ->
        Routes.print routes route
  in
  app_violation "route-round-trip" [] (app ~print ())

let test_round_trip_parse_ignores_query () =
  let parse url =
    match Routes.parse routes url with
    | Routes.At (State.Items, _) ->
        items_default
    | route ->
        route
  in
  app_violation "route-round-trip" [] (app ~parse ())

let test_round_trip_item_parsed_wrong () =
  let parse url =
    match Routes.parse routes url with
    | Routes.At (State.Item, _) ->
        Routes.At (State.Item, 0)
    | route ->
        route
  in
  app_violation "route-round-trip" [] (app ~parse ())

let test_link_not_canonical () =
  app_violation "link-parses" [] (app ~extra:[ raw_link "/items?sort=name" ] ())

let test_link_to_not_found () =
  app_violation "link-parses" []
    (app
       ~extra:[ raw_link "/items/007" ]
       ~is_not_found:(Routes.is_not_found routes)
       ()
    )

let test_link_to_not_found_allowed_without_predicate () =
  match app ~extra:[ raw_link "/items/007" ] () with
  | Check.Ok _ ->
      ()
  | report ->
      app_failure report

type stale_event = Leave | Back | Tick
type stale_state = Waiting | Left | Fired | Late

module Stale = struct
  type _ t = Waiting : unit t | Left : unit t | Fired : unit t | Late : unit t
end

module Timer = Machine.Make (Stale)

let timer_cases =
  [
    Timer.state Stale.Waiting "waiting"
      ~after:[ (10, Tick) ]
      ~to_:[ Stale.Left; Stale.Fired ]
      (fun () -> function
        | Leave ->
            Timer.goto Stale.Left ()
        | Tick ->
            Timer.goto Stale.Fired ()
        | Back ->
            Timer.unhandled
        );
    Timer.state Stale.Left "left" ~to_:[ Stale.Waiting; Stale.Late ]
      (fun () -> function
      | Back ->
          Timer.goto Stale.Waiting ()
      | Tick ->
          Timer.goto Stale.Late ()
      | Leave ->
          Timer.unhandled
      );
    Timer.state Stale.Fired "fired" ~final:true Timer.ignore;
    Timer.state Stale.Late "late" ~final:true Timer.ignore;
  ]

let stale_component =
  let table =
    Timer.make ~id:"timer" ~init:(Timer.at Stale.Waiting ()) timer_cases
  in
  let machine =
    Machine.map_snapshot table
      ~get:(fun (Timer.At (state, _) : Timer.snapshot) : stale_state ->
        match state with
        | Stale.Waiting ->
            Waiting
        | Stale.Left ->
            Left
        | Stale.Fired ->
            Fired
        | Stale.Late ->
            Late
      )
      ~set:(function
        | Waiting ->
            Timer.at Stale.Waiting ()
        | Left ->
            Timer.at Stale.Left ()
        | Fired ->
            Timer.at Stale.Fired ()
        | Late ->
            Timer.at Stale.Late ()
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
  { Component.machine; view }

let test_timer_stops_on_exit () =
  let never_late =
    { Check.name = "never-late"; holds = (fun s _ -> s <> Late) }
  in
  ( match Check.component ~invariants:[ never_late ] stale_component with
  | Check.Unreached { edges } ->
      Alcotest.(check (list (pair string string)))
        "the timer of waiting never fires in left"
        [ ("left", "late") ]
        edges
  | _ ->
      Alcotest.fail "expected the edge to late to be unreached"
  );
  match
    Check.component ~invariants:[ never_late ]
      ~allow_unreached:[ ("left", "late") ]
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

module Bad = struct
  type _ t = A : unit t | B : unit t | C : unit t
end

module Broken = Machine.Make (Bad)

let broken_component ~target =
  let table =
    Broken.make ~id:"broken" ~init:(Broken.at Bad.A ())
      [
        Broken.state Bad.A "a" ~to_:[ Bad.B ] (fun () -> function
          | Inc ->
              Broken.goto Bad.B ()
          | Go ->
              target ()
          | Dec ->
              Broken.unhandled
          );
        Broken.state Bad.B "b" ~final:true Broken.ignore;
      ]
  in
  let machine =
    Machine.map_snapshot table
      ~get:(fun (Broken.At (state, _) : Broken.snapshot) : int ->
        match state with Bad.A -> 0 | Bad.B -> 1 | Bad.C -> 2
      )
      ~set:(function
        | 0 ->
            Broken.at Bad.A ()
        | 1 ->
            Broken.at Bad.B ()
        | _ ->
            Broken.at Bad.C ()
        )
  in
  let view = function
    | 0 ->
        Node.list [ btn "inc" Inc; btn "go" Go ]
    | _ ->
        Node.text "done"
  in
  { Component.machine; view }

let test_undeclared_target () =
  violation "undeclared-target" [ "Go" ]
    (Check.component (broken_component ~target:(fun () -> Broken.goto Bad.C ())))

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

  module States = struct
    type _ t = Closed : unit t | Opened : unit t
  end

  module Dialog_machine = Machine.Make (States)

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
    Dialog_machine.make ~id:"dialog"
      ~init:(Dialog_machine.at States.Closed ())
      [
        Dialog_machine.state States.Closed "closed"
          ~to_:Dialog_machine.[ States.Opened ]
          (fun () -> function
            | Open ->
                Dialog_machine.goto States.Opened ()
            | _ ->
                Dialog_machine.unhandled
            );
        Dialog_machine.state States.Opened "opened"
          ~to_:Dialog_machine.[ States.Closed ]
          ~invoke:sources
          (fun () -> function
            | Close ->
                Dialog_machine.goto States.Closed ()
            | Resized | Seen _ ->
                Dialog_machine.stay []
            | Open ->
                Dialog_machine.unhandled
            );
      ]

  let view (Dialog_machine.At (state, _) : Dialog_machine.snapshot) :
      event Node.t =
    match state with
    | States.Closed ->
        Node.element "button" [ Attr.on_press Open ] [ Node.text "open" ]
    | States.Opened ->
        Node.element "div"
          [ Attr.ref_ panel ]
          [ Node.element "p" [] [ Node.text "dialog" ] ]

  let component ~escape ~outside ~observers =
    { Component.machine = machine ~escape ~outside ~observers; view }
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
        (List.map Dialog.show trace)
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
  let machine =
    Machine.map_snapshot
      (Dialog.machine ~escape:true ~outside:true ~observers:true)
      ~get:(fun (Dialog.Dialog_machine.At (state, _)) ->
        match state with Dialog.States.Closed -> 0 | Dialog.States.Opened -> 1
      )
      ~set:(fun n ->
        if n = 0 then
          Dialog.Dialog_machine.at Dialog.States.Closed ()
        else
          Dialog.Dialog_machine.at Dialog.States.Opened ()
      )
  in
  let live =
    Live.declare ~channel:"dialog" ~state:int_codec
      ~event:
        (Codec.make
           ~encode:(fun _ -> Silica_json.Json.declassify (`Int 0))
           ~decode:(fun _ -> Ok Dialog.Open)
        )
      {
        Component.machine;
        view =
          (fun n ->
            Dialog.view
              ( if n = 0 then
                  Dialog.Dialog_machine.at Dialog.States.Closed ()
                else
                  Dialog.Dialog_machine.at Dialog.States.Opened ()
              )
          );
      }
  in
  ( match Check.live live with
  | Check.Violation { invariant = "live-uses-client-source"; trace; state } ->
      Alcotest.(check int) "reached in the opened state" 1 state;
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
          None
      | Got line ->
          Some ((asked, List.sort_uniq compare (line :: got)), [])
      )

  let view _ =
    Node.element "div"
      [ Attr.ref_ (Ref.make "box") ]
      [ Node.element "button" [ Attr.on_press Ask ] [ Node.text "ask" ] ]

  let component = { Component.machine; view }
end

let test_action_results_arrive () =
  ( match Check.component Gadget.component with
  | Check.Ok { states; _ } ->
      Alcotest.(check int)
        "the start, then each subset of the three results after Ask" 9 states
  | _ ->
      Alcotest.fail "expected ok"
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
        (List.sort compare (List.map Gadget.show v.trace))
  | _ ->
      Alcotest.fail "expected the three results to arrive"

module Watcher = struct
  type event = Got of string

  module States = struct
    type _ t = Watching : string list t
  end

  module Watcher_machine = Machine.Make (States)

  let machine =
    Watcher_machine.make ~id:"watcher"
      ~init:(Watcher_machine.at States.Watching [])
      [
        Watcher_machine.state States.Watching "watching"
          ~invoke:
            [
              Source.scroll (fun (s : Scroll.t) ->
                  Got (Printf.sprintf "scroll:%.0f" s.top)
              );
              Source.visibility (fun v -> Got (Printf.sprintf "visible:%b" v));
              Source.online (fun v -> Got (Printf.sprintf "online:%b" v));
            ]
          (fun got (Got line) ->
            Watcher_machine.goto States.Watching
              (List.sort_uniq compare (line :: got))
          );
      ]

  let view _ = Node.text "watching"
  let component = { Component.machine; view }
end

let test_new_sources_give_events () =
  let all =
    {
      Check.name = "not-all";
      holds =
        (fun (Watcher.Watcher_machine.At (Watcher.States.Watching, got)) _ ->
          List.length got < 5
        );
    }
  in
  match Check.component ~invariants:[ all ] Watcher.component with
  | Check.Violation
      { state = Watcher.Watcher_machine.At (Watcher.States.Watching, got); _ }
    ->
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
    (Check.component { Component.machine; view = (fun () -> btn "go" Go) })

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
      { Component.machine; view = (fun _ -> btn "go" Go) }
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
          { Component.machine; view = (fun _ -> btn "go" Go) }
      in
      violation "live-uses-client-action" [ "Go" ] (Check.live live)
    )
    [
      Action.Set_status 404;
      Action.Revert_navigation;
      Action.Resume (Nav.Push (Url.of_string "/"));
      Action.Preload
        (Remote.request
           (Remote.declare ~path:"/p" ~input:Codec.int ~output:Codec.int)
           1
        );
    ]

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
          Alcotest.test_case "undeclared target" `Quick test_undeclared_target;
          Alcotest.test_case "inputs" `Quick test_inputs;
          Alcotest.test_case "pp_report" `Quick test_pp;
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
          Alcotest.test_case "link is not canonical" `Quick
            test_link_not_canonical;
          Alcotest.test_case "link to the not-found route" `Quick
            test_link_to_not_found;
          Alcotest.test_case "not-found link needs the predicate" `Quick
            test_link_to_not_found_allowed_without_predicate;
        ]
      );
    ]
