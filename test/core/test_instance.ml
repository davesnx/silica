open Silica_test_support
open Silica

let strings = Alcotest.(list string)
let echo = Remote.declare ~path:"/api/echo" ~input:Codec.int ~output:Codec.int

module Ticker = struct
  type event = Props of string | Tick | Bump | Settled of int

  type data = { label : string; ticks : int; bumps : int; last : int option }

  module States = struct
    type _ t = Running : data t
  end

  module Ticker_machine = Machine.Make (States)

  let machine =
    Ticker_machine.make ~id:"ticker"
      ~init:
        (Ticker_machine.at Running
           { label = ""; ticks = 0; bumps = 0; last = None }
        )
      [
        Ticker_machine.state Running "running"
          ~after:[ (10, Tick) ]
          ~invoke:
            [
              Source.call echo
                (fun data -> data.bumps)
                (fun r ->
                  match r with Ok n -> Settled n | Error _ -> Settled (-1)
                );
            ]
          (fun data -> function
            | Props label ->
                Ticker_machine.goto Running { data with label }
            | Tick ->
                Ticker_machine.reenter Running
                  { data with ticks = data.ticks + 1 }
            | Bump ->
                Ticker_machine.goto Running { data with bumps = data.bumps + 1 }
            | Settled n ->
                Ticker_machine.goto Running { data with last = Some n }
            );
      ]

  let view (Ticker_machine.At (States.Running, data) : Ticker_machine.snapshot)
      : event Node.t =
    Node.element "button"
      [ Attr.on_press Bump ]
      [
        Node.text
          (Printf.sprintf "%s:%d:%d%s" data.label data.ticks data.bumps
             ( match data.last with
             | Some n ->
                 "=" ^ string_of_int n
             | None ->
                 ""
             )
          );
      ]

  let component = Component.make machine view

  let make ?key ~label ~on_bump () =
    Component.node ?key ~props:(Props label)
      ~output:(fun event (Ticker_machine.At (States.Running, data)) ->
        match event with Bump -> [ on_bump data.bumps ] | _ -> []
      )
      component
end

module Parent = struct
  type event = Show of string list | Bumped of string * int | Label of string

  type data = {
    keys : string list;
    heard : (string * int) list;
    label : string;
  }

  module States = struct
    type _ t = Parent : data t
  end

  module Parent_machine = Machine.Make (States)

  let machine =
    Parent_machine.make ~id:"parent"
      ~init:(Parent_machine.at Parent { keys = []; heard = []; label = "x" })
      [
        Parent_machine.state Parent "parent" (fun data -> function
          | Show keys ->
              Parent_machine.goto Parent { data with keys }
          | Bumped (key, n) ->
              Parent_machine.goto Parent
                { data with heard = (key, n) :: data.heard }
          | Label label ->
              Parent_machine.goto Parent { data with label }
          );
      ]

  let view (Parent_machine.At (States.Parent, data) : Parent_machine.snapshot) :
      event Node.t =
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
  let heard sim =
    (fun (Parent_machine.At (States.Parent, data)) -> data.heard)
      (Sim.snapshot sim)
end

module Unkeyed = struct
  type event = Nothing

  module States = struct
    type _ t = Two : unit t
  end

  module Unkeyed_machine = Machine.Make (States)

  let machine =
    Unkeyed_machine.make ~id:"two"
      ~init:(Unkeyed_machine.at Two ())
      [ Unkeyed_machine.state Two "two" Unkeyed_machine.ignore ]

  let view (_ : Unkeyed_machine.snapshot) : event Node.t =
    Node.element "div" []
      [
        Ticker.make ~label:"a" ~on_bump:(fun _ -> Nothing) ();
        Ticker.make ~label:"b" ~on_bump:(fun _ -> Nothing) ();
      ]

  let component = Component.make machine view
end

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
  type event = Props of bool | Finish

  module States = struct
    type _ t = Working : unit t | Done : unit t
  end

  module Task_machine = Machine.Make (States)

  let machine =
    Task_machine.make ~id:"task"
      ~init:(Task_machine.at Working ())
      [
        Task_machine.state Working "working" ~to_:[ Done ] (fun () -> function
          | Props true | Finish ->
              Task_machine.goto Done ()
          | Props false ->
              Task_machine.unhandled
          );
        Task_machine.state Done "done" ~final:true Task_machine.ignore;
      ]

  let view : Task_machine.snapshot -> event Node.t = function
    | Task_machine.At (States.Working, ()) ->
        Node.element "button" [ Attr.on_press Finish ] [ Node.text "finish" ]
    | Task_machine.At (States.Done, ()) ->
        Node.text "finished"

  let component = Component.make machine view

  let make ?key ~start_done ~on_done () =
    Component.node ?key ~props:(Props start_done)
      ~on_final:(fun _ -> [ on_done ])
      component
end

module Boss = struct
  type event = Add of int | Finished of int

  type data = { tasks : int list; finished : int list }

  module States = struct
    type _ t = Boss : data t
  end

  module Boss_machine = Machine.Make (States)

  let machine =
    Boss_machine.make ~id:"boss"
      ~init:(Boss_machine.at Boss { tasks = []; finished = [] })
      [
        Boss_machine.state Boss "boss" (fun data -> function
          | Add id ->
              Boss_machine.goto Boss { data with tasks = data.tasks @ [ id ] }
          | Finished id ->
              Boss_machine.goto Boss
                { data with finished = data.finished @ [ id ] }
          );
      ]

  let view (Boss_machine.At (States.Boss, data) : Boss_machine.snapshot) :
      event Node.t =
    Node.element "div" []
      (List.map
         (fun id ->
           Task.make ~key:(string_of_int id) ~start_done:(id < 0)
             ~on_done:(Finished id) ()
         )
         data.tasks
      )

  let component = Component.make machine view
  let finished sim =
    (fun (Boss_machine.At (States.Boss, data)) -> data.finished)
      (Sim.snapshot sim)
end

module Patient = struct
  type event = Settled of (int, Remote.error) result

  module States = struct
    type _ t = Asking : unit t | Answered : int t | Gave_up : string t
  end

  module Patient_machine = Machine.Make (States)

  let machine =
    Patient_machine.make ~id:"patient"
      ~init:(Patient_machine.at Asking ())
      [
        Patient_machine.state Asking "asking" ~to_:[ Answered; Gave_up ]
          ~invoke:
            [
              Source.call ~timeout_ms:30 echo (fun () -> 1) (fun r -> Settled r);
            ]
          (fun () -> function
            | Settled (Ok n) ->
                Patient_machine.goto Answered n
            | Settled (Error (Remote.Network why)) ->
                Patient_machine.goto Gave_up why
            | Settled (Error _) ->
                Patient_machine.goto Gave_up "other"
            );
        Patient_machine.state Answered "answered" Patient_machine.ignore;
        Patient_machine.state Gave_up "gave_up" Patient_machine.ignore;
      ]

  let view : Patient_machine.snapshot -> event Node.t = function
    | Patient_machine.At (States.Asking, ()) ->
        Node.text "asking"
    | Patient_machine.At (States.Answered, n) ->
        Node.text ("answered " ^ string_of_int n)
    | Patient_machine.At (States.Gave_up, why) ->
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
    [
      "2.0/ticker/running/after0";
      "2.0/ticker/running/invoke0";
      "3.0/ticker/running/after0";
      "3.0/ticker/running/invoke0";
    ]
    (Sim.running sim)

let test_unmount_stops_children_first () =
  let sim = Sim.mount Parent.component in
  Sim.send sim (Parent.Show [ "a"; "b" ]);
  Sim.clear_log sim;
  Sim.unmount sim;
  Alcotest.check strings "stops"
    [
      "stop 2.0/ticker/running/after0";
      "stop 2.0/ticker/running/invoke0";
      "stop 3.0/ticker/running/after0";
      "stop 3.0/ticker/running/invoke0";
    ]
    (Sim.log sim);
  Alcotest.check strings "nothing runs" [] (Sim.running sim)

let test_unmount_orders_children_before_parent () =
  let module Owner = struct
    type event = Tick

    module States = struct
      type _ t = Owner : unit t
    end

    module Owner_machine = Machine.Make (States)

    let machine =
      Owner_machine.make ~id:"owner"
        ~init:(Owner_machine.at Owner ())
        [
          Owner_machine.state Owner "owner"
            ~after:[ (5, Tick) ]
            Owner_machine.ignore;
        ]

    let view (_ : Owner_machine.snapshot) : event Node.t =
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
    (position "stop 2.0/ticker/running/after0"
    < position "stop 1.0/owner/owner/after0"
    )

let test_removed_child_stops () =
  let sim = Sim.mount Parent.component in
  Sim.send sim (Parent.Show [ "a"; "b" ]);
  Sim.clear_log sim;
  Sim.send sim (Parent.Show [ "b" ]);
  Alcotest.check strings "only the removed child stops"
    [ "stop 2.0/ticker/running/after0"; "stop 2.0/ticker/running/invoke0" ]
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
  Alcotest.check strings "a props change is an event, not a restart" []
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
  Sim.fire sim "2.0/ticker/running/after0";
  has sim "a:1:0";
  has sim "b:0:0";
  Alcotest.(check bool)
    "the other instance still has its timer" true
    (List.mem "3.0/ticker/running/after0" (Sim.running sim))

let test_timer_restarts_with_new_entry () =
  let sim = Sim.mount Parent.component in
  Sim.send sim (Parent.Show [ "a" ]);
  Sim.fire sim "2.0/ticker/running/after0";
  has sim "ax:1:0";
  Alcotest.check strings "the restart uses a new entry number"
    [ "2.1/ticker/running/after0"; "2.1/ticker/running/invoke0" ]
    (Sim.running sim)

let test_late_result_of_old_entry_is_dropped () =
  let sim = Sim.mount Parent.component in
  Sim.send sim (Parent.Show [ "a" ]);
  Sim.fire sim "2.0/ticker/running/after0";
  has sim "ax:1:0";
  Sim.fire_after_stop sim "2.0/ticker/running/invoke0";
  has sim "ax:1:0";
  Sim.fire_after_stop sim "2.0/ticker/running/after0";
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
  Sim.fire_after_stop sim "2.0/ticker/running/after0";
  Alcotest.check strings "no action after unmount" before (Sim.log sim)

let test_duplicate_key_is_an_error () =
  let sim = Sim.mount Parent.component in
  match Sim.send sim (Parent.Show [ "a"; "a" ]) with
  | () ->
      Alcotest.fail "expected Invalid_argument"
  | exception Invalid_argument _ ->
      ()

module Clip = struct
  type event = Copied of bool | Moved

  module States = struct
    type _ t = Copying : unit t | Elsewhere : bool option t
  end

  module Clip_machine = Machine.Make (States)

  let machine =
    Clip_machine.make ~id:"clip"
      ~init:(Clip_machine.at Copying ())
      [
        Clip_machine.state Copying "copying" ~to_:[ Elsewhere ]
          ~entry:(fun () ->
            [
              Action.Copy
                { text = "hi"; on_result = Some (fun ok -> Copied ok) };
            ]
          )
          (fun () -> function
            | Moved ->
                Clip_machine.goto Elsewhere None
            | Copied ok ->
                Clip_machine.goto Elsewhere (Some ok)
            );
        Clip_machine.state Elsewhere "elsewhere" (fun _ -> function
          | Copied ok ->
              Clip_machine.goto Elsewhere (Some ok)
          | Moved ->
              Clip_machine.unhandled
          );
      ]

  let view : Clip_machine.snapshot -> event Node.t = function
    | Clip_machine.At (States.Copying, ()) ->
        Node.text "copying"
    | Clip_machine.At (States.Elsewhere, None) ->
        Node.text "waiting"
    | Clip_machine.At (States.Elsewhere, Some ok) ->
        Node.text (Printf.sprintf "copied=%b" ok)

  let component = Component.make machine view
end

module Clip_host = struct
  type event = Show of bool

  module States = struct
    type _ t = Host : bool t
  end

  module Clip_host_machine = Machine.Make (States)

  let machine =
    Clip_host_machine.make ~id:"clip-host"
      ~init:(Clip_host_machine.at Host true)
      [
        Clip_host_machine.state Host "host" (fun _ -> function
          | Show shown ->
              Clip_host_machine.goto Host shown
          );
      ]

  let view
      (Clip_host_machine.At (States.Host, shown) : Clip_host_machine.snapshot) :
      event Node.t =
    if shown then
      Component.node ~props:Clip.Moved Clip.component
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
        ]
      );
    ]
