open Silica_test_support
open Silica

let strings = Alcotest.(list string)
let echo =
  Remote.declare ~path:"/api/echo" ~input:Codec.int ~output:Codec.int
    ~rejected:Codec.string

module Ticker = struct
  type event = Tick | Bump | Settled of int
  type state = Running of { ticks : int; bumps : int; last : int option }

  let name = function Running _ -> "running"

  let states = [ "running" ]

  let machine =
    Machine.make_with_props ~id:"ticker"
      ~init:(Running { ticks = 0; bumps = 0; last = None })
      ~name ~states
      ~during:(fun _label (Running data) ->
        [
          Host.after 10 Tick;
          Remote.call echo data.bumps (function
            | Remote.Ok n ->
                Settled n
            | Remote.Rejected _ | Remote.Failed _ ->
                Settled (-1)
            );
        ]
      )
      (fun _label (Running data) -> function
        | Tick ->
            Machine.Reenter (Running { data with ticks = data.ticks + 1 })
        | Bump ->
            Machine.Go (Running { data with bumps = data.bumps + 1 })
        | Settled n ->
            Machine.Go (Running { data with last = Some n })
        )

  let view label (Running data) : event Node.t =
    Node.element "button"
      [ Attr.on_press Bump ]
      [
        Node.text
          (Printf.sprintf "%s:%d:%d%s" label data.ticks data.bumps
             ( match data.last with
             | Some n ->
                 "=" ^ string_of_int n
             | None ->
                 ""
             )
          );
      ]

  let component = Component.make_with_props machine view

  let make ?key ~label ~on_bump () =
    Component.node_with_props ?key ~props:label
      ~output:(fun event (Running data) ->
        match event with Bump -> [ on_bump data.bumps ] | _ -> []
      )
      component
end

let running_ids instance bumps =
  [
    Printf.sprintf "%s/ticker/running//api/echo %d" instance bumps;
    instance ^ "/ticker/running/after 10ms";
  ]

module Parent = struct
  type event = Show of string list | Bumped of string * int | Label of string

  type state =
    | Parent of {
        keys : string list;
        heard : (string * int) list;
        label : string;
      }

  let name = function Parent _ -> "parent"

  let states = [ "parent" ]

  let machine =
    Machine.make ~id:"parent"
      ~init:(Parent { keys = []; heard = []; label = "x" })
      ~name ~states
      (fun (Parent data) -> function
        | Show keys ->
            Machine.Go (Parent { data with keys })
        | Bumped (key, n) ->
            Machine.Go (Parent { data with heard = (key, n) :: data.heard })
        | Label label ->
            Machine.Go (Parent { data with label })
        )

  let view (Parent data) : event Node.t =
    Node.element "div" []
      (List.map
         (fun key ->
           Ticker.make ~key ~label:(key ^ data.label)
             ~on_bump:(fun n -> Bumped (key, n))
             ()
         )
         data.keys
      )

  let component = Component.make machine view
  let heard sim = match Sim.snapshot sim with Parent data -> data.heard
end

module Unkeyed = struct
  type event = Nothing
  type state = Two

  let name = function Two -> "two"

  let states = [ "two" ]

  let machine =
    Machine.make ~id:"two" ~init:Two ~name ~states (fun Two Nothing ->
        Machine.Ignore
    )

  let view Two : event Node.t =
    Node.element "div" []
      [
        Ticker.make ~label:"a" ~on_bump:(fun _ -> Nothing) ();
        Ticker.make ~label:"b" ~on_bump:(fun _ -> Nothing) ();
      ]

  let component = Component.make machine view
end

let contains text needle =
  let n = String.length needle and h = String.length text in
  let rec go i = i + n <= h && (String.sub text i n = needle || go (i + 1)) in
  go 0

let has sim needle =
  let html = Sim.html sim in
  let n = String.length needle and h = String.length html in
  let rec go i = i + n <= h && (String.sub html i n = needle || go (i + 1)) in
  if not (go 0) then Alcotest.failf "%S not in %s" needle html

let lacks sim needle =
  let html = Sim.html sim in
  let n = String.length needle and h = String.length html in
  let rec go i = i + n <= h && (String.sub html i n = needle || go (i + 1)) in
  if go 0 then Alcotest.failf "%S in %s" needle html

module Task = struct
  type event = Finish
  type state = Working | Done

  let name = function Working -> "working" | Done -> "done"

  let states = [ "working"; "done" ]

  let machine =
    Machine.make_with_props ~id:"task" ~init:Working ~name ~states
      ~final:(function Done -> true | Working -> false)
      ~on_props:(fun _ start_done state ->
        match (start_done, state) with
        | true, Working ->
            Machine.Go Done
        | _ ->
            Machine.Ignore
      )
      (fun _ state Finish ->
        match state with Working -> Machine.Go Done | Done -> Machine.Ignore
      )

  let view : state -> event Node.t = function
    | Working ->
        Node.element "button" [ Attr.on_press Finish ] [ Node.text "finish" ]
    | Done ->
        Node.text "finished"

  let component = Component.make machine view

  let make ?key ~start_done ~on_done () =
    Component.node_with_props ?key ~props:start_done
      ~on_final:(fun _ -> [ on_done ])
      component
end

module Boss = struct
  type event = Add of int | Finished of int
  type state = Boss of { tasks : int list; finished : int list }

  let name = function Boss _ -> "boss"

  let states = [ "boss" ]

  let machine =
    Machine.make ~id:"boss"
      ~init:(Boss { tasks = []; finished = [] })
      ~name ~states
      (fun (Boss data) -> function
        | Add id ->
            Machine.Go (Boss { data with tasks = data.tasks @ [ id ] })
        | Finished id ->
            Machine.Go (Boss { data with finished = data.finished @ [ id ] })
        )

  let view (Boss data) : event Node.t =
    Node.element "div" []
      (List.map
         (fun id ->
           Task.make ~key:(string_of_int id) ~start_done:(id < 0)
             ~on_done:(Finished id) ()
         )
         data.tasks
      )

  let component = Component.make machine view
  let finished sim = match Sim.snapshot sim with Boss data -> data.finished
end

module Patient = struct
  type event = Settled of (int, string) Remote.answer
  type state = Asking | Answered of int | Gave_up of string

  let name = function
    | Asking ->
        "asking"
    | Answered _ ->
        "answered"
    | Gave_up _ ->
        "gave_up"

  let states = [ "asking"; "answered"; "gave_up" ]

  let machine =
    Machine.make ~id:"patient" ~init:Asking ~name ~states
      ~during:(function
        | Asking ->
            [ Remote.call ~timeout_ms:30 echo 1 (fun r -> Settled r) ]
        | Answered _ | Gave_up _ ->
            []
        )
      (fun state event ->
        match (state, event) with
        | Asking, Settled (Remote.Ok n) ->
            Machine.Go (Answered n)
        | Asking, Settled (Remote.Failed (Remote.Network why)) ->
            Machine.Go (Gave_up why)
        | Asking, Settled (Remote.Failed _ | Remote.Rejected _) ->
            Machine.Go (Gave_up "other")
        | (Answered _ | Gave_up _), Settled _ ->
            Machine.Ignore
      )

  let view : state -> event Node.t = function
    | Asking ->
        Node.text "asking"
    | Answered n ->
        Node.text ("answered " ^ string_of_int n)
    | Gave_up why ->
        Node.text ("gave up: " ^ why)

  let component = Component.make machine view
end

let test_call_timeout_reaches_the_machine () =
  let sim = Sim.mount Patient.component in
  Alcotest.(check (list (option int)))
    "the host is told the timeout" [ Some 30 ] (Sim.call_timeouts sim);
  Sim.expire_call sim echo;
  has sim "gave up: timeout";
  Alcotest.check strings "the call is stopped" [] (Sim.running sim)

let test_call_answer_before_timeout () =
  let sim = Sim.mount Patient.component in
  Sim.answer sim echo 4;
  has sim "answered 4"

let test_final_child_reaches_parent () =
  let sim = Sim.mount Boss.component in
  Sim.send sim (Boss.Add 1);
  Sim.send sim (Boss.Add 2);
  Alcotest.(check (list int)) "nobody finished" [] (Boss.finished sim);
  Sim.press sim "finish";
  Alcotest.(check (list int))
    "parent heard the first task" [ 1 ] (Boss.finished sim);
  has sim "finished";
  Sim.send sim (Boss.Add 3);
  Alcotest.(check (list int))
    "a render does not repeat the output" [ 1 ] (Boss.finished sim)

let test_child_final_at_start_reaches_parent () =
  let sim = Sim.mount Boss.component in
  Sim.send sim (Boss.Add (-5));
  Alcotest.(check (list int)) "heard once" [ -5 ] (Boss.finished sim);
  Sim.send sim (Boss.Add 1);
  Alcotest.(check (list int)) "still once" [ -5 ] (Boss.finished sim)

let test_keyed_child_is_a_keyed_node () =
  let sim = Sim.mount Parent.component in
  Sim.send sim (Parent.Show [ "a"; "b" ]);
  let keys node =
    match Node.view node with
    | Node.Element (_, _, children) ->
        List.map
          (fun child ->
            match Node.view child with
            | Node.Keyed (key, _) ->
                key
            | _ ->
                "unkeyed"
          )
          children
    | _ ->
        []
  in
  Alcotest.(check (list string)) "keys" [ "a"; "b" ] (keys (Sim.node sim));
  Sim.send sim (Parent.Show [ "b"; "a" ]);
  Alcotest.(check (list string))
    "keys follow the order" [ "b"; "a" ]
    (keys (Sim.node sim))

let test_mount_starts_children () =
  let sim = Sim.mount Parent.component in
  Alcotest.(check int) "only the root" 1 (Sim.count sim);
  Sim.send sim (Parent.Show [ "a"; "b" ]);
  Alcotest.(check int) "root and two children" 3 (Sim.count sim);
  has sim "ax:0:0";
  has sim "bx:0:0";
  Alcotest.check strings "each child started a timer and a call"
    (running_ids "2.0" 0 @ running_ids "3.0" 0)
    (Sim.running sim)

let test_unmount_stops_children_first () =
  let sim = Sim.mount Parent.component in
  Sim.send sim (Parent.Show [ "a"; "b" ]);
  Sim.clear_log sim;
  Sim.unmount sim;
  Alcotest.check strings "stops"
    [
      "stop 2.0/ticker/running/after 10ms";
      "stop 2.0/ticker/running//api/echo 0";
      "stop 3.0/ticker/running/after 10ms";
      "stop 3.0/ticker/running//api/echo 0";
    ]
    (Sim.log sim);
  Alcotest.check strings "nothing runs" [] (Sim.running sim)

let test_unmount_orders_children_before_parent () =
  let module Owner = struct
    type event = Tick
    type state = Owner

    let name = function Owner -> "owner"

    let states = [ "owner" ]

    let machine =
      Machine.make ~id:"owner" ~init:Owner ~name ~states
        ~during:(fun Owner -> [ Host.after 5 Tick ])
        (fun Owner Tick -> Machine.Stay [])

    let view Owner : event Node.t =
      Node.element "div" []
        [ Ticker.make ~label:"k" ~on_bump:(fun _ -> Tick) () ]
  end in
  let sim = Sim.mount (Component.make Owner.machine Owner.view) in
  Sim.clear_log sim;
  Sim.unmount sim;
  let stops = Sim.log sim in
  let position id =
    let rec go i = function
      | [] ->
          Alcotest.failf "no %s in %s" id (String.concat "," stops)
      | x :: rest ->
          if x = id then
            i
          else
            go (i + 1) rest
    in
    go 0 stops
  in
  Alcotest.(check bool)
    "the child stops before its parent" true
    (position "stop 2.0/ticker/running/after 10ms"
    < position "stop 1.0/owner/owner/after 5ms"
    )

let test_removed_child_stops () =
  let sim = Sim.mount Parent.component in
  Sim.send sim (Parent.Show [ "a"; "b" ]);
  Sim.clear_log sim;
  Sim.send sim (Parent.Show [ "b" ]);
  Alcotest.check strings "only the removed child stops"
    [
      "stop 2.0/ticker/running/after 10ms";
      "stop 2.0/ticker/running//api/echo 0";
    ]
    (Sim.log sim);
  Alcotest.(check int) "root and one child" 2 (Sim.count sim);
  lacks sim "ax:"

let test_keyed_reorder_keeps_state () =
  let sim = Sim.mount Parent.component in
  Sim.send sim (Parent.Show [ "a"; "b" ]);
  Sim.press sim "ax";
  Sim.press sim "ax";
  Sim.press sim "bx";
  has sim "ax:0:2";
  has sim "bx:0:1";
  Sim.clear_log sim;
  Sim.send sim (Parent.Show [ "b"; "a" ]);
  has sim "<button>bx:0:1</button><button>ax:0:2</button>";
  Alcotest.check strings "no stop and no start" [] (Sim.log sim);
  Alcotest.(check int) "same instances" 3 (Sim.count sim)

let test_removed_key_ends_and_returns_fresh () =
  let sim = Sim.mount Parent.component in
  Sim.send sim (Parent.Show [ "a"; "b" ]);
  Sim.press sim "ax";
  has sim "ax:0:1";
  Sim.send sim (Parent.Show [ "b" ]);
  lacks sim "ax:";
  Sim.send sim (Parent.Show [ "a"; "b" ]);
  has sim "ax:0:0";
  has sim "bx:0:0"

let test_props_reach_child () =
  let sim = Sim.mount Parent.component in
  Sim.send sim (Parent.Show [ "a" ]);
  Sim.press sim "ax";
  has sim "ax:0:1";
  Sim.clear_log sim;
  Sim.send sim (Parent.Label "y");
  has sim "ay:0:1";
  Alcotest.check strings "a props change does not restart the child" []
    (Sim.log sim);
  Sim.send sim (Parent.Label "y");
  has sim "ay:0:1"

let test_output_reaches_parent () =
  let sim = Sim.mount Parent.component in
  Sim.send sim (Parent.Show [ "a"; "b" ]);
  Sim.press sim "bx";
  Sim.press sim "bx";
  Sim.press sim "ax";
  Alcotest.(check (list (pair string int)))
    "parent heard the children"
    [ ("a", 1); ("b", 2); ("b", 1) ]
    (Parent.heard sim)

let test_instances_do_not_clash () =
  let sim = Sim.mount Unkeyed.component in
  let ids = Sim.running sim in
  Alcotest.(check int)
    "four distinct resources" 4
    (List.length (List.sort_uniq compare ids));
  Sim.fire sim "2.0/ticker/running/after 10ms";
  has sim "a:1:0";
  has sim "b:0:0";
  Alcotest.(check bool)
    "the other instance still has its timer" true
    (List.mem "3.0/ticker/running/after 10ms" (Sim.running sim))

let test_timer_restarts_with_new_entry () =
  let sim = Sim.mount Parent.component in
  Sim.send sim (Parent.Show [ "a" ]);
  Sim.fire sim "2.0/ticker/running/after 10ms";
  has sim "ax:1:0";
  Alcotest.check strings "the restart uses a new entry number"
    (running_ids "2.1" 0) (Sim.running sim)

let test_late_result_of_old_entry_is_dropped () =
  let sim = Sim.mount Parent.component in
  Sim.send sim (Parent.Show [ "a" ]);
  Sim.fire sim "2.0/ticker/running/after 10ms";
  has sim "ax:1:0";
  Sim.fire_after_stop sim "2.0/ticker/running//api/echo 0";
  has sim "ax:1:0";
  Sim.fire_after_stop sim "2.0/ticker/running/after 10ms";
  has sim "ax:1:0";
  Sim.answer sim echo 9;
  has sim "ax:1:0=9"

let test_answer_of_current_entry_arrives () =
  let sim = Sim.mount Parent.component in
  Sim.send sim (Parent.Show [ "a" ]);
  Sim.answer sim echo 5;
  has sim "ax:0:0=5"

let test_message_after_unmount_is_dropped () =
  let sim = Sim.mount Parent.component in
  Sim.send sim (Parent.Show [ "a" ]);
  Sim.unmount sim;
  let before = Sim.log sim in
  Sim.fire_after_stop sim "2.0/ticker/running/after 10ms";
  Alcotest.check strings "no action after unmount" before (Sim.log sim)

let test_duplicate_key_is_an_error () =
  let sim = Sim.mount Parent.component in
  match Sim.send sim (Parent.Show [ "a"; "a" ]) with
  | () ->
      Alcotest.fail "expected Invalid_argument"
  | exception Invalid_argument _ ->
      ()

module Clip = struct
  type event = Copied of bool
  type state = Copying | Elsewhere of bool option

  let name = function Copying -> "copying" | Elsewhere _ -> "elsewhere"

  let states = [ "copying"; "elsewhere" ]

  let machine =
    Machine.make_with_props ~id:"clip" ~init:Copying ~name ~states
      ~entry:(fun () -> function
        | Copying ->
            [
              Action.Copy
                { text = "hi"; on_result = Some (fun ok -> Copied ok) };
            ]
        | Elsewhere _ ->
            []
        )
      ~on_props:(fun previous () state ->
        match (previous, state) with
        | None, Copying ->
            Machine.Go (Elsewhere None)
        | _ ->
            Machine.Ignore
      )
      (fun () _ (Copied ok) -> Machine.Go (Elsewhere (Some ok)))

  let view : state -> event Node.t = function
    | Copying ->
        Node.text "copying"
    | Elsewhere None ->
        Node.text "waiting"
    | Elsewhere (Some ok) ->
        Node.text (Printf.sprintf "copied=%b" ok)

  let component = Component.make machine view
end

module Clip_host = struct
  type event = Show of bool
  type state = Host of bool

  let name = function Host _ -> "host"

  let states = [ "host" ]

  let machine =
    Machine.make ~id:"clip-host" ~init:(Host true) ~name ~states
      (fun _ (Show shown) -> Machine.Go (Host shown)
    )

  let view (Host shown) : event Node.t =
    if shown then
      Component.node Clip.component
    else
      Node.text "none"

  let component = Component.make machine view
end

let copy_result actions =
  match
    List.find_map
      (function
        | Action.Run (Action.Copy { on_result = Some k; _ }) ->
            Some k
        | _ ->
            None
        )
      actions
  with
  | Some k ->
      k
  | None ->
      Alcotest.fail "no Copy action with a result"

let text_of node = Render.to_string node

let test_action_result_reaches_its_instance () =
  let tree = Instance.mount Clip_host.component in
  let node, actions = Instance.update tree None in
  Alcotest.(check string)
    "the child left the copying state" "waiting" (text_of node);
  let k = copy_result actions in
  let node, _ = Instance.update tree (Some (k true)) in
  Alcotest.(check string)
    "the result reaches the child in its new state" "copied=true" (text_of node)

let test_action_result_after_end_is_dropped () =
  let tree = Instance.mount Clip_host.component in
  let _, actions = Instance.update tree None in
  let k = copy_result actions in
  let _ = Instance.update tree (Some (Instance.message tree (Show false))) in
  Alcotest.(check int) "the child ended" 1 (Instance.count tree);
  let node, actions = Instance.update tree (Some (k true)) in
  Alcotest.(check string) "the view does not change" "none" (text_of node);
  Alcotest.(check int) "no actions" 0 (List.length actions);
  Alcotest.(check int) "no child starts again" 1 (Instance.count tree)

module Same_id_int = struct
  type state = Number of int

  let name = function Number _ -> "number"

  let states = [ "number" ]

  let machine =
    Machine.make ~id:"same" ~init:(Number 0) ~name ~states
      ~during:(fun _ -> [ Host.after 1 `Up ])
      (fun (Number n) `Up -> Machine.Go (Number (n + 1)))

  let component =
    Component.make machine (fun (Number n) ->
        Node.element "button"
          [ Attr.on_press `Up ]
          [ Node.text (Printf.sprintf "int %d" n) ]
    )
end

module Same_id_text = struct
  type state = Text of string

  let name = function Text _ -> "text"

  let states = [ "text" ]

  let machine =
    Machine.make ~id:"same" ~init:(Text "") ~name ~states
      ~during:(fun _ -> [ Host.after 1 `Up ])
      (fun (Text s) `Up -> Machine.Go (Text (s ^ "!")))

  let component =
    Component.make machine (fun (Text s) ->
        Node.element "button" [ Attr.on_press `Up ] [ Node.text ("text " ^ s) ]
    )
end

module Other_id = struct
  type state = Number of int

  let name = function Number _ -> "number"

  let states = [ "number" ]

  let machine =
    Machine.make ~id:"other" ~init:(Number 0) ~name ~states
      ~during:(fun _ -> [ Host.after 1 `Up ])
      (fun (Number n) `Up -> Machine.Go (Number (n + 1)))

  let component =
    Component.make machine (fun (Number n) ->
        Node.element "button"
          [ Attr.on_press `Up ]
          [ Node.text (Printf.sprintf "int %d" n) ]
    )
end

module Switcher = struct
  type shown = Int | Text | Other
  type event = Show of shown | Redraw
  type state = Showing of shown * int

  let name = function Showing _ -> "showing"

  let states = [ "showing" ]

  let machine =
    Machine.make ~id:"switcher"
      ~init:(Showing (Int, 0))
      ~name ~states
      (fun (Showing (shown, draws)) -> function
        | Show next ->
            Machine.Go (Showing (next, draws))
        | Redraw ->
            Machine.Go (Showing (shown, draws + 1))
        )

  let view (Showing (shown, _)) : event Node.t =
    Node.element "div" []
      [
        ( match shown with
        | Int ->
            Component.node Same_id_int.component
        | Text ->
            Component.node Same_id_text.component
        | Other ->
            Component.node Other_id.component
        );
      ]

  let component = Component.make machine view
end

let test_same_id_keeps_the_child () =
  let sim = Sim.mount Switcher.component in
  Sim.press sim "int";
  has sim "int 1";
  Sim.clear_log sim;
  Sim.send sim Switcher.Redraw;
  has sim "int 1";
  Alcotest.check strings "no stop and no start" [] (Sim.log sim);
  Sim.send sim (Switcher.Show Switcher.Int);
  has sim "int 1"

let raises_two_machines run =
  match run () with
  | () ->
      Alcotest.fail "expected Invalid_argument for two machines under one id"
  | exception Invalid_argument message ->
      Alcotest.(check bool)
        "the message names the id" true
        (contains message "\"same\"")

let test_same_id_other_types_is_an_error () =
  let sim = Sim.mount Switcher.component in
  Sim.press sim "int";
  has sim "int 1";
  raises_two_machines (fun () -> Sim.send sim (Switcher.Show Switcher.Text))

module Rebuilt = struct
  type event = Redraw
  type state = Drawn of int

  let name = function Drawn _ -> "drawn"

  let states = [ "drawn" ]

  let child () =
    Component.make
      (Machine.make ~id:"same" ~init:0
         ~name:(fun _ -> "counting")
         (fun count () -> Machine.Go (count + 1))
      )
      (fun count ->
        Node.element "button"
          [ Attr.on_press () ]
          [ Node.text ("rebuilt " ^ string_of_int count) ]
      )

  let component =
    Component.make
      (Machine.make ~id:"rebuilt" ~init:(Drawn 0) ~name ~states
         (fun (Drawn draws) Redraw -> Machine.Go (Drawn (draws + 1))
       )
      )
      (fun _ -> Component.node (child ()))
end

let test_machine_built_during_render_is_an_error () =
  let sim = Sim.mount Rebuilt.component in
  has sim "rebuilt 0";
  raises_two_machines (fun () -> Sim.send sim Rebuilt.Redraw)

let test_other_id_is_a_new_child () =
  let sim = Sim.mount Switcher.component in
  Sim.press sim "int";
  has sim "int 1";
  Sim.clear_log sim;
  Sim.send sim (Switcher.Show Switcher.Other);
  has sim "int 0";
  Alcotest.check strings "the old child ends, a new one starts"
    [ "start 3.0/other/number/after 1ms"; "stop 2.0/same/number/after 1ms" ]
    (List.sort compare (Sim.log sim))

let test_changed_call_input_restarts_the_call () =
  let sim = Sim.mount Parent.component in
  Sim.send sim (Parent.Show [ "a" ]);
  Sim.clear_log sim;
  Sim.press sim "ax";
  Alcotest.check strings "a new input stops the old call and starts a new one"
    [
      "stop 2.0/ticker/running//api/echo 0";
      "start 2.0/ticker/running//api/echo 1";
    ]
    (Sim.log sim);
  Alcotest.check strings "the timer keeps running" (running_ids "2.0" 1)
    (Sim.running sim);
  Sim.fire_after_stop sim "2.0/ticker/running//api/echo 0";
  has sim "ax:0:1";
  Sim.answer sim echo 7;
  has sim "ax:0:1=7"

let () =
  let t n f = Alcotest.test_case n `Quick f in
  Alcotest.run "silica instance"
    [
      ( "instance",
        [
          t "mount starts children" test_mount_starts_children;
          t "unmount stops children" test_unmount_stops_children_first;
          t "unmount: children before parent"
            test_unmount_orders_children_before_parent;
          t "removed child stops" test_removed_child_stops;
          t "keyed reorder keeps state" test_keyed_reorder_keeps_state;
          t "removed key ends, returning key is fresh"
            test_removed_key_ends_and_returns_fresh;
          t "props reach the child" test_props_reach_child;
          t "output reaches the parent" test_output_reaches_parent;
          t "instances do not clash" test_instances_do_not_clash;
          t "new entry, new id" test_timer_restarts_with_new_entry;
          t "late result of an old entry is dropped"
            test_late_result_of_old_entry_is_dropped;
          t "answer of the current entry arrives"
            test_answer_of_current_entry_arrives;
          t "message after unmount is dropped"
            test_message_after_unmount_is_dropped;
          t "duplicate key" test_duplicate_key_is_an_error;
          t "final child output reaches the parent"
            test_final_child_reaches_parent;
          t "child final at start reaches the parent"
            test_child_final_at_start_reaches_parent;
          t "call timeout reaches the machine"
            test_call_timeout_reaches_the_machine;
          t "answer before the timeout" test_call_answer_before_timeout;
          t "keyed child is a keyed node" test_keyed_child_is_a_keyed_node;
          t "action result reaches its instance"
            test_action_result_reaches_its_instance;
          t "action result after the end is dropped"
            test_action_result_after_end_is_dropped;
          t "same id keeps the child" test_same_id_keeps_the_child;
          t "same id, other types: an error"
            test_same_id_other_types_is_an_error;
          t "a machine built during render: an error"
            test_machine_built_during_render_is_an_error;
          t "other id: new child" test_other_id_is_a_new_child;
          t "changed call input restarts the call"
            test_changed_call_input_restarts_the_call;
        ]
      );
    ]
