open Silica

let int_option = Alcotest.(option int)

let status_tests =
  let open Status in
  [
    Alcotest.test_case "data of each status" `Quick (fun () ->
        Alcotest.check int_option "idle" None (data (Idle : int t));
        Alcotest.check int_option "loading none" None (data (Loading None));
        Alcotest.check int_option "loading keeps old" (Some 1)
          (data (Loading (Some 1)));
        Alcotest.check int_option "ready" (Some 2)
          (data (Ready { data = 2; stale = false }));
        Alcotest.check int_option "failed keeps last" (Some 3)
          (data (Failed { error = Remote.Http 500; last = Some 3 }));
        Alcotest.check int_option "failed without last" None
          (data (Failed { error = Remote.Http 500; last = None }))
    );
    Alcotest.test_case "is_loading and is_stale" `Quick (fun () ->
        Alcotest.(check bool) "loading" true (is_loading (Loading (Some 1)));
        Alcotest.(check bool)
          "ready is not loading" false
          (is_loading (Ready { data = 1; stale = false }));
        Alcotest.(check bool)
          "stale" true
          (is_stale (Ready { data = 1; stale = true }));
        Alcotest.(check bool)
          "fresh" false
          (is_stale (Ready { data = 1; stale = false }));
        Alcotest.(check bool) "idle is not stale" false (is_stale (Idle : int t))
    );
    Alcotest.test_case "map changes the data and keeps the shape" `Quick
      (fun () ->
        let double = ( * ) 2 in
        Alcotest.(check bool)
          "ready, stale kept" true
          (map double (Ready { data = 2; stale = true })
          = Ready { data = 4; stale = true }
          );
        Alcotest.(check bool)
          "loading" true
          (map double (Loading (Some 2)) = Loading (Some 4));
        Alcotest.(check bool)
          "failed" true
          (map double (Failed { error = Remote.Http 1; last = Some 2 })
          = Failed { error = Remote.Http 1; last = Some 4 }
          );
        Alcotest.(check bool) "idle" true (map double Idle = Idle)
    );
  ]

module Int_list = Optimistic.Make (struct
  type value = int list
  type change = int

  let apply xs x = xs @ [ x ]
end)

let add_list = Int_list.make []
let ints = Alcotest.(list int)

let optimistic_tests =
  let open Int_list in
  [
    Alcotest.test_case "value shows pending changes on the confirmed value"
      `Quick (fun () ->
        let t = add_list |> apply ~id:1 10 |> apply ~id:2 20 in
        Alcotest.check ints "value" [ 10; 20 ] (value t);
        Alcotest.check ints "confirmed" [] (confirmed t);
        Alcotest.(check int) "pending" 2 (pending t)
    );
    Alcotest.test_case "settle without a value folds the change in" `Quick
      (fun () ->
        let t = add_list |> apply ~id:1 10 |> apply ~id:2 20 in
        let t = settle ~id:1 t in
        Alcotest.check ints "value unchanged" [ 10; 20 ] (value t);
        Alcotest.check ints "confirmed has 10" [ 10 ] (confirmed t);
        Alcotest.(check int) "one pending" 1 (pending t)
    );
    Alcotest.test_case "settle with a confirmed value puts the rest on top"
      `Quick (fun () ->
        let t = add_list |> apply ~id:1 10 |> apply ~id:2 20 in
        let t = settle ~confirmed:[ 99; 10 ] ~id:1 t in
        Alcotest.check ints "value" [ 99; 10; 20 ] (value t);
        Alcotest.check ints "confirmed" [ 99; 10 ] (confirmed t);
        Alcotest.(check int) "pending" 1 (pending t)
    );
    Alcotest.test_case "drop removes the change and the value goes back" `Quick
      (fun () ->
        let t = add_list |> apply ~id:1 10 |> apply ~id:2 20 in
        let t = drop ~id:1 t in
        Alcotest.check ints "value" [ 20 ] (value t);
        Alcotest.check ints "confirmed" [] (confirmed t)
    );
    Alcotest.test_case "unknown ids change nothing" `Quick (fun () ->
        let t = add_list |> apply ~id:1 10 in
        Alcotest.check ints "settle" [ 10 ] (value (settle ~id:7 t));
        Alcotest.check ints "drop" [ 10 ] (value (drop ~id:7 t));
        Alcotest.(check int) "pending after settle" 1 (pending (settle ~id:7 t))
    );
    Alcotest.test_case "same id replaces the pending change" `Quick (fun () ->
        let t = add_list |> apply ~id:1 10 |> apply ~id:1 11 in
        Alcotest.check ints "value" [ 11 ] (value t);
        Alcotest.(check int) "pending" 1 (pending t)
    );
    Alcotest.test_case "a value is first-order: snapshots compare it" `Quick
      (fun () ->
        let t = add_list |> apply ~id:1 10 in
        Alcotest.(check bool) "equal to itself" true (t = t);
        Alcotest.(check bool) "not equal to another" false (t = apply ~id:2 20 t);
        Alcotest.(check bool) "pending" true (is_pending ~id:1 t);
        Alcotest.(check bool) "not pending" false (is_pending ~id:2 t)
    );
    Alcotest.test_case "rebase keeps the pending changes" `Quick (fun () ->
        let t = add_list |> apply ~id:1 10 in
        let t = rebase [ 5 ] t in
        Alcotest.check ints "value" [ 5; 10 ] (value t);
        Alcotest.check ints "confirmed" [ 5 ] (confirmed t)
    );
    Alcotest.test_case "settle keeps the order of the pending changes" `Quick
      (fun () ->
        let t =
          add_list |> apply ~id:1 1 |> apply ~id:2 2 |> apply ~id:3 3
          |> settle ~id:2
        in
        Alcotest.check ints "value" [ 2; 1; 3 ] (value t);
        Alcotest.check ints "confirmed" [ 2 ] (confirmed t);
        Alcotest.(check int) "pending" 2 (pending t)
    );
  ]

let backoff_tests =
  [
    Alcotest.test_case "first tries are fast, then the delay doubles" `Quick
      (fun () ->
        let d attempt = Backoff.delay_ms ~attempt ~jitter:0. in
        Alcotest.(check int) "0" 100 (d 0);
        Alcotest.(check int) "1" 200 (d 1);
        Alcotest.(check int) "2" 400 (d 2);
        Alcotest.(check int) "5" 3200 (d 5)
    );
    Alcotest.test_case "the delay is capped at 5 s" `Quick (fun () ->
        let d attempt = Backoff.delay_ms ~attempt ~jitter:0. in
        Alcotest.(check int) "6" 5000 (d 6);
        Alcotest.(check int) "100" 5000 (d 100);
        Alcotest.(check int) "cap" 5000 Backoff.cap_ms
    );
    Alcotest.test_case "jitter adds up to a quarter" `Quick (fun () ->
        Alcotest.(check int) "full" 125 (Backoff.delay_ms ~attempt:0 ~jitter:1.);
        Alcotest.(check int)
          "full at cap" 6250
          (Backoff.delay_ms ~attempt:50 ~jitter:1.);
        Alcotest.(check int) "half" 112 (Backoff.delay_ms ~attempt:0 ~jitter:0.5)
    );
    Alcotest.test_case "a negative attempt is the first one" `Quick (fun () ->
        Alcotest.(check int)
          "neg" 100
          (Backoff.delay_ms ~attempt:(-3) ~jitter:0.)
    );
  ]

let topic_tests =
  [
    Alcotest.test_case "a topic has a name and a codec" `Quick (fun () ->
        let topic = Topic.declare ~name:"issues" ~state:Codec.int in
        Alcotest.(check string) "name" "issues" (Topic.name topic);
        Alcotest.(check string)
          "codec" "7"
          (Codec.to_string (Topic.state topic) 7)
    );
  ]

module Watch = struct
  type event = Watch of string | Stop | Update of int Topic.update

  module States = struct
    type _ t = Idle : unit t | Watching : (string * int Status.t) t
  end

  module Watch_machine = Machine.Make (States)

  let topic = Topic.declare ~name:"counts" ~state:Codec.int

  let machine =
    Watch_machine.make ~id:"watch" ~init:(Watch_machine.at Idle ())
      [
        Watch_machine.always ~to_:[ Watching; Idle ] (function
          | Watch key ->
              Watch_machine.reenter Watching (key, Status.Idle)
          | Stop ->
              Watch_machine.goto Idle ()
          | Update _ ->
              Watch_machine.unhandled
          );
        Watch_machine.state Idle "idle" Watch_machine.ignore;
        Watch_machine.state Watching "watching"
          ~invoke:[ Source.live topic fst (fun update -> Update update) ]
          (fun (key, _) -> function
            | Update { status; _ } ->
                Watch_machine.goto Watching (key, status)
            | _ ->
                Watch_machine.unhandled
            );
      ]

  let view = function
    | Watch_machine.At (Idle, ()) ->
        Node.text "idle"
    | Watch_machine.At (Watching, (key, status)) ->
        Node.text
          (key ^ ":"
          ^
          match status with
          | Status.Ready { data; stale } ->
              string_of_int data
              ^
              if stale then
                "?"
              else
                ""
          | Status.Loading _ ->
              "loading"
          | _ ->
              "other"
          )

  let component =
    Component.make (Machine.map_snapshot machine ~get:Fun.id ~set:Fun.id) view
end

let pairs = Alcotest.(list (pair string string))
let ready ?(stale = false) data = Status.Ready { data; stale }

let source_tests =
  [
    Alcotest.test_case "entering the state subscribes under the key of its data"
      `Quick (fun () ->
        let sim = Silica_test_support.Sim.mount Watch.component in
        Alcotest.check pairs "none while idle" []
          (Silica_test_support.Sim.subscriptions sim);
        Silica_test_support.Sim.send sim (Watch.Watch "a");
        Alcotest.check pairs "one"
          [ ("counts", "a") ]
          (Silica_test_support.Sim.subscriptions sim)
    );
    Alcotest.test_case "an update reaches the state" `Quick (fun () ->
        let module Sim = Silica_test_support.Sim in
        let sim = Sim.mount Watch.component in
        Sim.send sim (Watch.Watch "a");
        Sim.publish sim Watch.topic ~key:"a"
          { Topic.seq = 4; status = ready 10 };
        Alcotest.(check string) "html" "a:10" (Sim.html sim);
        Sim.publish sim Watch.topic ~key:"a"
          { Topic.seq = 4; status = ready ~stale:true 10 };
        Alcotest.(check string) "stale" "a:10?" (Sim.html sim)
    );
    Alcotest.test_case "an update for another key does not arrive" `Quick
      (fun () ->
        let module Sim = Silica_test_support.Sim in
        let sim = Sim.mount Watch.component in
        Sim.send sim (Watch.Watch "a");
        Sim.publish sim Watch.topic ~key:"b" { Topic.seq = 1; status = ready 7 };
        Alcotest.(check string) "html" "a:other" (Sim.html sim) |> ignore
    );
    Alcotest.test_case "leaving the state stops the subscription" `Quick
      (fun () ->
        let module Sim = Silica_test_support.Sim in
        let sim = Sim.mount Watch.component in
        Sim.send sim (Watch.Watch "a");
        Sim.clear_log sim;
        Sim.send sim Watch.Stop;
        Alcotest.check pairs "none" [] (Sim.subscriptions sim);
        Alcotest.(check int)
          "one stop in the log" 1
          (List.length
             (List.filter
                (fun line ->
                  String.length line > 4 && String.sub line 0 4 = "stop"
                )
                (Sim.log sim)
             )
          )
    );
    Alcotest.test_case "the key changes with a reenter" `Quick (fun () ->
        let module Sim = Silica_test_support.Sim in
        let sim = Sim.mount Watch.component in
        Sim.send sim (Watch.Watch "a");
        Sim.send sim (Watch.Watch "b");
        Alcotest.check pairs "only the new key"
          [ ("counts", "b") ]
          (Sim.subscriptions sim)
    );
    Alcotest.test_case "the spec and the active resources name the topic" `Quick
      (fun () ->
        let spec = Machine.spec Watch.machine in
        let watching =
          List.find (fun s -> s.Spec.name = "watching") spec.Spec.states
        in
        Alcotest.(check (list string))
          "sources" [ "live:counts" ] watching.Spec.sources;
        match
          Machine.active Watch.machine
            (Watch.Watch_machine.at Watching ("k", Status.Idle))
        with
        | [ Machine.Subscription { topic = "counts"; _ } ] ->
            ()
        | _ ->
            Alcotest.fail "expected one Subscription resource"
    );
  ]

let () =
  Alcotest.run "data"
    [
      ("status", status_tests);
      ("optimistic", optimistic_tests);
      ("backoff", backoff_tests);
      ("topic", topic_tests);
      ("source", source_tests);
    ]
