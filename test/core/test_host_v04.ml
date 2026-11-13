open Silica_test_support
open Silica

let strings = Alcotest.(list string)

let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec go i =
    i + n <= h && (String.sub haystack i n = needle || go (i + 1))
  in
  go 0

let check_has message haystack needle =
  if not (contains haystack needle) then
    Alcotest.failf "%s: %S not in %s" message needle haystack

let check_lacks message haystack needle =
  if contains haystack needle then
    Alcotest.failf "%s: %S in %s" message needle haystack

module Clock = struct
  type t = {
    mutable now : int;
    mutable tasks : (int * int * (unit -> unit)) list;
    mutable next : int;
  }

  let make () = { now = 0; tasks = []; next = 0 }

  let schedule t ms callback =
    let id = t.next in
    t.next <- t.next + 1;
    t.tasks <- t.tasks @ [ (id, t.now + ms, callback) ];
    fun () -> t.tasks <- List.filter (fun (i, _, _) -> i <> id) t.tasks

  let advance t ms =
    let target = t.now + ms in
    let rec go () =
      match
        List.sort
          (fun (_, a, _) (_, b, _) -> compare a b)
          (List.filter (fun (_, at, _) -> at <= target) t.tasks)
      with
      | [] ->
          t.now <- target
      | (id, at, callback) :: _ ->
          t.now <- at;
          t.tasks <- List.filter (fun (i, _, _) -> i <> id) t.tasks;
          callback ();
          go ()
    in
    go ()
end

let test_throttle () =
  let clock = Clock.make () in
  let seen = ref [] in
  let send =
    Rate.limiter ~schedule:(Clock.schedule clock) (Rate.Throttle 100) (fun v ->
        seen := !seen @ [ v ]
    )
  in
  send 1;
  Alcotest.(check (list int)) "first value goes through at once" [ 1 ] !seen;
  send 2;
  send 3;
  Alcotest.(check (list int)) "values in the window are held" [ 1 ] !seen;
  Clock.advance clock 99;
  Alcotest.(check (list int)) "nothing before the window ends" [ 1 ] !seen;
  Clock.advance clock 1;
  Alcotest.(check (list int)) "the last held value wins" [ 1; 3 ] !seen;
  Clock.advance clock 100;
  send 4;
  Alcotest.(check (list int))
    "a quiet window closes, the next value goes through at once" [ 1; 3; 4 ]
    !seen

let test_throttle_trailing_window () =
  let clock = Clock.make () in
  let seen = ref [] in
  let send =
    Rate.limiter ~schedule:(Clock.schedule clock) (Rate.Throttle 100) (fun v ->
        seen := !seen @ [ v ]
    )
  in
  send 1;
  Clock.advance clock 50;
  send 2;
  Clock.advance clock 50;
  Alcotest.(check (list int)) "trailing value" [ 1; 2 ] !seen;
  send 3;
  Alcotest.(check (list int))
    "the trailing delivery opened a new window" [ 1; 2 ] !seen;
  Clock.advance clock 100;
  Alcotest.(check (list int)) "held value of the new window" [ 1; 2; 3 ] !seen

let test_debounce () =
  let clock = Clock.make () in
  let seen = ref [] in
  let send =
    Rate.limiter ~schedule:(Clock.schedule clock) (Rate.Debounce 50) (fun v ->
        seen := !seen @ [ v ]
    )
  in
  send 1;
  Clock.advance clock 30;
  send 2;
  Clock.advance clock 30;
  Alcotest.(check (list int)) "a new value restarts the wait" [] !seen;
  Clock.advance clock 20;
  Alcotest.(check (list int)) "the last value is delivered once" [ 2 ] !seen;
  Clock.advance clock 500;
  Alcotest.(check (list int)) "and not again" [ 2 ] !seen

let test_answers_round_trip () =
  let nasty = "</script><!-- \"quoted\" \\ & <b>" in
  let table =
    Answers.add Answers.empty ~path:"/api/a" ~input:"1" ~output:nasty
  in
  let table = Answers.add table ~path:"/api/b" ~input:"2" ~output:"3" in
  let script = Answers.to_script ~nonce:"n0nce" table in
  check_has "element" script
    "<script type=\"application/json\" id=\"silica-answers\" nonce=\"n0nce\">";
  let inner =
    let start = String.index script '>' + 1 in
    let stop = String.rindex script '<' in
    String.sub script start (stop - start)
  in
  check_lacks "no tag can start inside the JSON" inner "<";
  ( match Answers.of_string inner with
  | Ok back ->
      Alcotest.(check (option string))
        "nasty text survives" (Some nasty)
        (Answers.find back ~path:"/api/a" ~input:"1");
      Alcotest.(check (option string))
        "second entry" (Some "3")
        (Answers.find back ~path:"/api/b" ~input:"2");
      Alcotest.(check (option string))
        "another input is not answered" None
        (Answers.find back ~path:"/api/b" ~input:"9");
      Alcotest.(check (option string))
        "an answer is used once" None
        (Answers.find
           (Answers.remove back ~path:"/api/b" ~input:"2")
           ~path:"/api/b" ~input:"2"
        )
  | Error message ->
      Alcotest.fail message
  );
  Alcotest.(check bool) "empty" true (Answers.is_empty Answers.empty);
  match Answers.of_string "[1]" with
  | Error _ ->
      ()
  | Ok _ ->
      Alcotest.fail "a list is not a table"

let test_render_markers () =
  let text = Node.text and el = Node.element in
  let render node = Render.to_string node in
  Alcotest.(check string)
    "adjacent texts" "<p>a<!-- -->b<!-- -->c</p>"
    (render (el "p" [] [ text "a"; text "b"; text "c" ]));
  Alcotest.(check string)
    "texts in lists and keyed nodes count as adjacent"
    "<p>a<!-- -->b<!-- -->c</p>"
    (render
       (el "p" []
          [ text "a"; Node.list [ text "b"; Node.keyed "k" (text "c") ] ]
       )
    );
  Alcotest.(check string)
    "an element between texts needs no marker" "<p>a<b></b>c</p>"
    (render (el "p" [] [ text "a"; el "b" [] []; text "c" ]));
  Alcotest.(check string)
    "empty text prints nothing and is not a neighbour" "<p>a<!-- -->b</p>"
    (render (el "p" [] [ text "a"; text ""; text "b" ]));
  Alcotest.(check string)
    "the first text of an element follows no text before it" "<p>a</p><p>b</p>"
    (render (Node.list [ el "p" [] [ text "a" ]; el "p" [] [ text "b" ] ]));
  Alcotest.(check string)
    "a portal prints nothing, so the texts around it meet" "<p>a<!-- -->b</p>"
    (render (el "p" [] [ text "a"; Node.portal [ text "gone" ]; text "b" ]))

let test_render_svg () =
  let el = Node.element and text = Node.text in
  Alcotest.(check string)
    "SVG elements print with their attribute names as given"
    "<svg viewBox=\"0 0 1 1\"><use xlink:href=\"#a\"></use><path d=\"M0 \
     0\"></path><foreignObject><p>x</p></foreignObject></svg>"
    (Render.to_string
       (el "svg"
          [ Attr.string "viewBox" "0 0 1 1" ]
          [
            el "use" [ Attr.string "xlink:href" "#a" ] [];
            el "path" [ Attr.string "d" "M0 0" ] [];
            el "foreignObject" [] [ el "p" [] [ text "x" ] ];
          ]
       )
    )

let test_node_map_new_nodes () =
  let key_handler = Attr.on_key_down ~prevent:true (fun _ -> Some 1) in
  let rated = Attr.on_input ~rate:(Rate.Debounce 5) String.length in
  let node =
    Node.element "div"
      [ key_handler; rated; Attr.on_reset ]
      [
        Node.portal ~target:"t" [ Node.text "p" ];
        Node.boundary
          ~fallback:(fun _ ~reset ->
            Node.element "b" [ reset; Attr.on_press 7 ] []
          )
          (Node.element "i" [ Attr.on_press 3 ] []);
      ]
  in
  match Node.view (Node.map (fun n -> n * 10) node) with
  | Element
      ( "div",
        [
          On_key_down { run; prevent = true };
          Rated (Debounce 5, On_input f);
          On_reset;
        ],
        [ portal; guard ]
      ) -> (
      Alcotest.(check (option int))
        "key handler mapped" (Some 10) (run Key.escape);
      Alcotest.(check int) "rated input mapped" 30 (f "abc");
      ( match Node.view portal with
      | Portal (Some "t", [ _ ]) ->
          ()
      | _ ->
          Alcotest.fail "portal kept its target and children"
      );
      match Node.view guard with
      | Boundary { fallback; body } -> (
          ( match Node.view body with
          | Element ("i", [ On_press 30 ], []) ->
              ()
          | _ ->
              Alcotest.fail "body mapped"
          );
          match
            Node.view
              (fallback
                 { Node.id = "x"; message = "m"; detail = None }
                 ~reset:Node.On_reset
              )
          with
          | Element ("b", [ On_reset; On_press 70 ], []) ->
              ()
          | _ ->
              Alcotest.fail "fallback mapped, reset kept"
        )
      | _ ->
          Alcotest.fail "boundary kept"
    )
  | _ ->
      Alcotest.fail "shape changed"

module Held = struct
  type event =
    | Open
    | Close
    | Escape
    | Outside
    | Resized of Rect.t
    | Seen of bool

  type data = { opened : bool; log : string list }
  type state = Shut of data | Popup of data

  let name = function Shut _ -> "shut" | Popup _ -> "popup"

  let states = [ "shut"; "popup" ]

  let panel = Ref.make "panel"
  let box = Ref.make "box"

  let sources =
    [
      Host.focus_trap panel;
      Host.scroll_lock;
      Host.outside_press panel Outside;
      Host.keys ~prevent:true
        [ (Key.escape, Escape); (Key.make ~ctrl:true "k", Close) ];
      Host.resize box (fun rect -> Resized rect);
      Host.intersection box (fun visible -> Seen visible);
    ]

  let machine =
    let note data text = { data with log = text :: data.log } in
    Machine.make ~id:"held"
      ~init:(Shut { opened = false; log = [] })
      ~name ~states
      ~during:(function Shut _ -> [] | Popup _ -> sources)
      (fun state event ->
        match (state, event) with
        | Shut data, Open ->
            Machine.Go (Popup { data with opened = true })
        | Popup data, (Close | Escape | Outside) ->
            Machine.Go (Shut { data with opened = false })
        | Popup data, Resized rect ->
            Machine.Go
              (Popup (note data (Printf.sprintf "resized %.0f" rect.width)))
        | Popup data, Seen visible ->
            Machine.Go
              (Popup
                 (note data
                    ( if visible then
                        "seen"
                      else
                        "hidden"
                    )
                 )
              )
        | Shut _, _ | Popup _, Open ->
            Machine.Ignore
      )

  let view : state -> event Node.t = function
    | Shut _ ->
        Node.element "button" [ Attr.on_press Open ] [ Node.text "open" ]
    | Popup _ ->
        Node.element "div"
          [ Attr.ref_ panel ]
          [ Node.element "span" [ Attr.ref_ box ] [ Node.text "popup" ] ]

  let component = Component.make machine view
  let data = function Shut data | Popup data -> data
  let opened sim = (data (Sim.snapshot sim)).opened
end

let test_machine_sources () =
  let machine = Held.machine in
  let state, actions = Machine.start machine in
  Alcotest.(check int) "no source in the first state" 0 (List.length actions);
  Alcotest.(check int)
    "no held resource" 0
    (List.length (Machine.held machine state));
  let state, actions, _ = Machine.step machine state Held.Open in
  let started =
    List.filter_map
      (function
        | Action.Start_held { id; held } ->
            Some (id ^ " " ^ Action.held_name held)
        | _ ->
            None
        )
      actions
  in
  Alcotest.check strings "sources start on entry, keyed by kind and refs"
    [
      "held/popup/focus_trap panel focus_trap";
      "held/popup/scroll_lock scroll_lock";
      "held/popup/outside_press panel outside_press";
      "held/popup/keys keys";
      "held/popup/resize box resize";
      "held/popup/intersection box intersection";
    ]
    started;
  Alcotest.(check int)
    "held lists them" 6
    (List.length (Machine.held machine state));
  Alcotest.check strings "active names the held resources"
    [
      "focus_trap";
      "scroll_lock";
      "outside_press";
      "keys";
      "resize";
      "intersection";
    ]
    (List.map
       (function
         | Machine.Held { name; _ } ->
             name
         | Machine.Timer _ | Machine.Call _ | Machine.Subscription _ ->
             "not held"
         )
       (Machine.active machine state)
    );
  Alcotest.(check bool)
    "every held resource is held" true
    (List.for_all Host.is_held Held.sources);
  let state, actions, how = Machine.step machine state (Held.Seen true) in
  Alcotest.(check bool)
    "a step in the state keeps the sources" true
    (how = Machine.Kept && actions = []);
  let _, actions, _ = Machine.step machine state Held.Escape in
  let stopped =
    List.filter_map (function Action.Stop id -> Some id | _ -> None) actions
  in
  Alcotest.(check int) "every source stops on exit" 6 (List.length stopped)

let test_sim_sources () =
  let sim = Sim.mount Held.component in
  Alcotest.check strings "nothing runs while shut" [] (Sim.running sim);
  Sim.press sim "open";
  Alcotest.(check int) "sources run" 6 (List.length (Sim.held sim));
  Alcotest.(check bool)
    "ids are qualified by instance and entry" true
    (List.for_all
       (fun (id, _) -> String.starts_with ~prefix:"1.1/held/popup/" id)
       (Sim.held sim)
    );
  Alcotest.(check bool)
    "an unbound key reaches nobody" false
    (Sim.press_key sim (Key.make "x"));
  Alcotest.(check bool) "still open" true (Held.opened sim);
  Alcotest.(check bool)
    "a bound key reaches the machine" true
    (Sim.press_key sim Key.escape);
  Alcotest.(check bool) "closed by Escape" false (Held.opened sim);
  Alcotest.check strings "sources stop with the state" [] (Sim.running sim);
  Sim.press sim "open";
  Sim.outside_press sim;
  Alcotest.(check bool) "closed by outside press" false (Held.opened sim);
  Alcotest.check strings "sources stopped again" [] (Sim.running sim)

module Bomb = struct
  type event = Inc | Boom | Break_view | Tick
  type state = Live of { count : int; ticks : int; broken : bool }

  let name = function Live _ -> "live"

  let states = [ "live" ]

  let machine =
    Machine.make ~id:"bomb"
      ~init:(Live { count = 0; ticks = 0; broken = false })
      ~name ~states
      ~during:(fun _ -> [ Host.after 10 Tick ])
      (fun (Live data) -> function
        | Inc ->
            Machine.Go (Live { data with count = data.count + 1 })
        | Tick ->
            Machine.Reenter (Live { data with ticks = data.ticks + 1 })
        | Boom ->
            failwith "boom"
        | Break_view ->
            Machine.Go (Live { data with broken = true })
        )

  let view (Live data) : event Node.t =
    if data.broken then failwith "view broke";
    Node.element "div" []
      [
        Node.element "button"
          [ Attr.on_press Inc ]
          [ Node.text (Printf.sprintf "count %d" data.count) ];
        Node.element "button" [ Attr.on_press Boom ] [ Node.text "explode" ];
        Node.element "button"
          [ Attr.on_press Break_view ]
          [ Node.text "break view" ];
      ]

  let component = Component.make machine view
end

module Shell = struct
  type event = Bump | Child of Bomb.event
  type state = Shell of { bumps : int }

  let name = function Shell _ -> "shell"

  let states = [ "shell" ]

  let machine =
    Machine.make ~id:"shell"
      ~init:(Shell { bumps = 0 })
      ~name ~states
      (fun (Shell data) -> function
        | Bump ->
            Machine.Go (Shell { bumps = data.bumps + 1 })
        | Child _ ->
            Machine.Ignore
        )

  let fallback (error : Node.error) ~reset : event Node.t =
    Node.element "section" []
      [
        Node.element "p" []
          [
            Node.text
              ("fallback: " ^ error.message ^ " | "
              ^ Option.value error.detail ~default:"no detail"
              );
          ];
        Node.element "button" [ reset ] [ Node.text "retry" ];
      ]

  let bomb () =
    Component.node ~output:(fun event _ -> [ Child event ]) Bomb.component
    |> Node.map (fun event -> event)

  let guarded : (unit, state, event) Component.t =
    Component.make machine (fun (Shell data) ->
        Node.element "main" []
          [
            Node.element "button"
              [ Attr.on_press Bump ]
              [ Node.text (Printf.sprintf "bumps %d" data.bumps) ];
            Node.boundary ~fallback (bomb ());
          ]
    )

  let unguarded : (unit, state, event) Component.t =
    Component.make machine (fun _ -> Node.element "main" [] [ bomb () ])

  let bumps sim = match Sim.snapshot sim with Shell data -> data.bumps
end

let test_boundary_catches_step_exception () =
  let sim = Sim.mount Shell.guarded in
  Sim.press sim "bumps 0";
  Sim.press sim "count 0";
  Sim.press sim "count 1";
  check_has "body shows" (Sim.html sim) "count 2";
  Alcotest.(check int) "child and shell run" 2 (Sim.count sim);
  Sim.press sim "explode";
  let html = Sim.html sim in
  check_has "fallback shows" html "fallback: Something went wrong.";
  check_has "prod: no detail" html "no detail";
  check_lacks "body is gone" html "count 2";
  check_lacks "exception text stays on the server" html "boom";
  Alcotest.(check int) "the child ended" 1 (Sim.count sim);
  Alcotest.(check int) "the parent kept its state" 1 (Shell.bumps sim);
  Alcotest.check strings "the child's timer stopped" [] (Sim.running sim)

let test_boundary_catches_view_exception () =
  let sim = Sim.mount Shell.guarded in
  Sim.press sim "break view";
  let html = Sim.html sim in
  check_has "fallback shows" html "fallback: Something went wrong.";
  Alcotest.(check int) "the child ended" 1 (Sim.count sim);
  Alcotest.check strings "no timer left" [] (Sim.running sim);
  Sim.press sim "bumps 0";
  Alcotest.(check int) "the parent still works" 1 (Shell.bumps sim)

let test_boundary_reset_remounts () =
  let sim = Sim.mount Shell.guarded in
  Sim.press sim "count 0";
  Sim.press sim "count 1";
  check_has "state before" (Sim.html sim) "count 2";
  Sim.press sim "explode";
  Sim.press sim "bumps 0";
  Sim.press sim "retry";
  let html = Sim.html sim in
  check_has "body again" html "count 0";
  check_lacks "fallback gone" html "fallback";
  Alcotest.(check int) "child runs again" 2 (Sim.count sim);
  Alcotest.(check int) "the parent kept its state" 1 (Shell.bumps sim);
  Alcotest.(check int)
    "the timer started again" 1
    (List.length (Sim.running sim));
  Sim.press sim "explode";
  check_has "can fail again" (Sim.html sim) "fallback";
  Sim.press sim "retry";
  check_has "and reset again" (Sim.html sim) "count 0"

let test_boundary_dev_detail () =
  let sim = Sim.mount ~dev:true Shell.guarded in
  Sim.press sim "explode";
  let html = Sim.html sim in
  check_has "exception" html "Failure(&quot;boom&quot;)";
  check_has "machine" html "machine: bomb";
  check_has "state" html "state: live"

let test_boundary_hook_gets_exception () =
  let seen = ref [] in
  let tree =
    Instance.mount
      ~on_error:(fun exn (error : Node.error) ->
        seen := (Printexc.to_string exn, String.length error.id) :: !seen
      )
      Shell.guarded
  in
  let node, _ = Instance.update tree None in
  ignore node;
  let press text =
    let node, _ = Instance.update tree None in
    let rec find n =
      match Node.view n with
      | Node.Element (_, attrs, children) as shape -> (
          let here =
            List.find_map
              (function Node.On_press m -> Some m | _ -> None)
              attrs
          in
          match (here, shape) with
          | Some m, Node.Element (_, _, [ child ])
            when match Node.view child with
                 | Node.Text t ->
                     t = text
                 | _ ->
                     false ->
              Some m
          | _ ->
              List.find_map find children
        )
      | Node.List children ->
          List.find_map find children
      | Node.Keyed (_, child) ->
          find child
      | _ ->
          None
    in
    match find node with
    | Some m ->
        ignore (Instance.update tree (Some m))
    | None ->
        Alcotest.failf "no button %s" text
  in
  press "explode";
  match !seen with
  | [ (text, id_length) ] ->
      Alcotest.(check string) "exception" "Failure(\"boom\")" text;
      Alcotest.(check int) "id" 6 id_length
  | other ->
      Alcotest.failf "hook ran %d times" (List.length other)

let test_no_boundary_exception_escapes () =
  let sim = Sim.mount Shell.unguarded in
  match Sim.press sim "explode" with
  | () ->
      Alcotest.fail "the exception must leave the host step"
  | exception Failure message ->
      Alcotest.(check string) "the exception itself" "boom" message

module Nested = struct
  type event = Nothing
  type state = Only

  let name = function Only -> "only"

  let states = [ "only" ]

  let machine =
    Machine.make ~id:"nested" ~init:Only ~name ~states (fun Only Nothing ->
        Machine.Ignore
    )

  let bad_fallback _ ~reset:_ : event Node.t = failwith "fallback broke"

  let ok_fallback _ ~reset : event Node.t =
    Node.element "p" [ reset ] [ Node.text "outer fallback" ]

  let component : (unit, state, event) Component.t =
    Component.make machine (fun _ ->
        Node.boundary ~fallback:ok_fallback
          (Node.element "div" []
             [
               Node.boundary ~fallback:bad_fallback
                 (Component.node Bomb.component
                 |> Node.map (fun (_ : Bomb.event) -> Nothing)
                 );
             ]
          )
    )
end

let test_fallback_exception_goes_to_the_next_boundary () =
  let sim = Sim.mount Nested.component in
  Sim.press sim "explode";
  let html = Sim.html sim in
  check_has "outer fallback shows" html "outer fallback";
  check_lacks "inner fallback never shows" html "fallback broke"

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "silica host v0.4"
    [
      ( "rate",
        [
          t "throttle: first at once, last wins" test_throttle;
          t "throttle: trailing delivery opens a window"
            test_throttle_trailing_window;
          t "debounce: last value after a quiet time" test_debounce;
        ]
      );
      ("answers", [ t "round trip and escaping" test_answers_round_trip ]);
      ( "render",
        [
          t "markers between adjacent texts" test_render_markers;
          t "svg prints names as given" test_render_svg;
        ]
      );
      ("node", [ t "map over the new nodes" test_node_map_new_nodes ]);
      ( "sources",
        [
          t "machine starts and stops sources" test_machine_sources;
          t "host runs sources by state" test_sim_sources;
        ]
      );
      ( "boundary",
        [
          t "catches a step exception" test_boundary_catches_step_exception;
          t "catches a view exception" test_boundary_catches_view_exception;
          t "reset remounts" test_boundary_reset_remounts;
          t "dev shows details" test_boundary_dev_detail;
          t "on_error gets the exception" test_boundary_hook_gets_exception;
          t "no boundary: the exception escapes"
            test_no_boundary_exception_escapes;
          t "fallback exception goes to the next boundary"
            test_fallback_exception_goes_to_the_next_boundary;
        ]
      );
    ]
