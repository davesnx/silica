open Silica
module Sim = Silica_test_support.Sim

let strings = Alcotest.(list string)

type poll = Polling of int

let poll_name = function Polling _ -> "polling"

let poll_states = [ "polling" ]

type tick = Tick

let poller =
  Component.make
    (Machine.make ~id:"poll" ~init:(Polling 0) ~name:poll_name
       ~states:poll_states
       ~during:(fun _ -> [ Source.after 1000 Tick ])
       (fun (Polling n) Tick -> Go (Polling (n + 1)))
    )
    (fun (Polling n) -> Node.text (string_of_int n))

let test_fired_timer_starts_again () =
  let sim = Sim.mount poller in
  let timer = List.hd (Sim.running sim) in
  Sim.fire sim timer;
  Alcotest.(check bool) "one tick" true (Sim.snapshot sim = Polling 1);
  Alcotest.check strings "the timer runs again" [ timer ] (Sim.running sim);
  Sim.fire sim timer;
  Alcotest.(check bool) "two ticks" true (Sim.snapshot sim = Polling 2)

let fetch =
  Remote.declare ~path:"/api/fetch" ~input:Codec.unit ~output:Codec.int
    ~rejected:Codec.string

type watch = Watching of int

let watch_name = function Watching _ -> "watching"

let watch_states = [ "watching" ]

type answered = Answered of (int, string) Remote.answer

let watcher =
  Component.make
    (Machine.make ~id:"watch" ~init:(Watching 0) ~name:watch_name
       ~states:watch_states
       ~during:(fun _ ->
         [ Remote.call fetch () (fun answer -> Answered answer) ]
       )
       (fun (Watching n) (Answered _) -> Go (Watching (n + 1)))
    )
    (fun (Watching n) -> Node.text (string_of_int n))

let test_answered_call_starts_again () =
  let sim = Sim.mount watcher in
  Sim.answer sim fetch 1;
  Alcotest.(check bool) "one answer" true (Sim.snapshot sim = Watching 1);
  Alcotest.(check int) "the call runs again" 1 (List.length (Sim.running sim));
  Sim.answer sim fetch 2;
  Alcotest.(check bool) "two answers" true (Sim.snapshot sim = Watching 2)

type pair_event = A | B | Drop
type pair = Timers of pair_event list * pair_event option

let pair_name = function Timers _ -> "timers"

let pair_states = [ "timers" ]

let pair =
  Component.make
    (Machine.make ~id:"pair"
       ~init:(Timers ([ A; B ], None))
       ~name:pair_name ~states:pair_states
       ~during:(fun (Timers (events, _)) ->
         List.map (fun event -> Source.after 1000 event) events
       )
       (fun (Timers (events, last)) -> function
         | Drop ->
             Go (Timers ([ B ], last))
         | (A | B) as event ->
             Go (Timers (events, Some event))
         )
    )
    (fun _ -> Node.text "pair")

type table = Table of pair_event

let table_name = function Table _ -> "table"

let table_states = [ "table" ]

type table_event = Pressed of pair_event | Switch

let table =
  Component.make
    (Machine.make ~id:"table" ~init:(Table A) ~name:table_name
       ~states:table_states
       ~during:(fun (Table event) ->
         [ Source.keys [ (Key.enter, Pressed event) ] ]
       )
       (fun (Table _) -> function
         | Switch -> Go (Table B) | Pressed event -> Go (Table event)
         )
    )
    (fun (Table event) ->
      Node.text (match event with A -> "a" | B -> "b" | Drop -> "drop")
    )

type kept = Keep of string

let kept_name = function Keep _ -> "keep"

let kept_states = [ "keep" ]

type kept_event = Fired of string | Rename of string

let renamer =
  Component.make
    (Machine.make ~id:"rename" ~init:(Keep "first") ~name:kept_name
       ~states:kept_states
       ~during:(fun (Keep label) -> [ Source.after 1000 (Fired label) ])
       (fun (Keep _) -> function
         | Rename label ->
             Go (Keep label)
         | Fired label ->
             Go (Keep ("fired " ^ label))
         )
    )
    (fun (Keep label) -> Node.text label)

let test_kept_timer_raises_its_current_event () =
  let sim = Sim.mount renamer in
  Sim.send sim (Rename "second");
  let timer = List.hd (Sim.running sim) in
  Sim.fire sim timer;
  Alcotest.(check bool)
    "the event of the current owns" true
    (Sim.snapshot sim = Keep "fired second")

let test_two_timers_keep_their_events () =
  let sim = Sim.mount pair in
  Sim.send sim Drop;
  Alcotest.(check int) "one timer runs" 1 (List.length (Sim.running sim));
  Sim.fire sim (List.hd (Sim.running sim));
  Alcotest.(check bool)
    "the timer that runs raises B" true
    (Sim.snapshot sim = Timers ([ B ], Some B))

let test_a_changed_key_table_restarts () =
  let sim = Sim.mount table in
  Sim.send sim Switch;
  ignore (Sim.press_key sim Key.enter);
  Alcotest.(check bool)
    "the table of the current state" true
    (Sim.snapshot sim = Table B)

let () =
  Alcotest.run "silica sources"
    [
      ( "delivered",
        [
          Alcotest.test_case "a fired timer starts again" `Quick
            test_fired_timer_starts_again;
          Alcotest.test_case "an answered call starts again" `Quick
            test_answered_call_starts_again;
        ]
      );
      ( "keys",
        [
          Alcotest.test_case "two timers keep their events" `Quick
            test_two_timers_keep_their_events;
          Alcotest.test_case "a changed key table restarts" `Quick
            test_a_changed_key_table_restarts;
          Alcotest.test_case "a kept timer raises its current event" `Quick
            test_kept_timer_raises_its_current_event;
        ]
      );
    ]
