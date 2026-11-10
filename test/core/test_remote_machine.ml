open Silica
module Sim = Silica_test_support.Sim

let endpoint = Remote.declare ~path:"/api/n" ~input:Codec.int ~output:Codec.int

let status_text = function
  | Status.Idle ->
      "idle"
  | Status.Loading None ->
      "loading"
  | Status.Loading (Some old) ->
      Printf.sprintf "loading(%d)" old
  | Status.Ready { data; stale } ->
      Printf.sprintf "ready(%d%s)" data
        ( if stale then
            ",stale"
          else
            ""
        )
  | Status.Failed { last = None; _ } ->
      "failed"
  | Status.Failed { last = Some old; _ } ->
      Printf.sprintf "failed(%d)" old

let component ?timeout_ms () =
  Component.make (Remote_machine.make ?timeout_ms ~id:"n" endpoint)
    (fun (state : (int, int) Remote_machine.state) ->
      Node.text (status_text state.status)
  )

let strings = Alcotest.(list string)

let tests =
  [
    Alcotest.test_case "starts idle with no call" `Quick (fun () ->
        let sim = Sim.mount (component ()) in
        Alcotest.(check string) "html" "idle" (Sim.html sim);
        Alcotest.check strings "no call" [] (Sim.call_inputs sim)
    );
    Alcotest.test_case "load calls the endpoint and the answer is ready" `Quick
      (fun () ->
        let sim = Sim.mount (component ()) in
        Sim.send sim (Remote_machine.Load 3);
        Alcotest.(check string) "loading" "loading" (Sim.html sim);
        Alcotest.check strings "input" [ "3" ] (Sim.call_inputs sim);
        Sim.answer sim endpoint 9;
        Alcotest.(check string) "ready" "ready(9)" (Sim.html sim);
        Alcotest.check strings "call done" [] (Sim.call_inputs sim)
    );
    Alcotest.test_case "a failure keeps the input and retry calls again" `Quick
      (fun () ->
        let sim = Sim.mount (component ()) in
        Sim.send sim (Remote_machine.Load 3);
        Sim.answer_error sim endpoint (Remote.Http 500);
        Alcotest.(check string) "failed" "failed" (Sim.html sim);
        Alcotest.(check (option int))
          "input kept" (Some 3) (Sim.snapshot sim).input;
        Sim.send sim Remote_machine.Retry;
        Alcotest.(check string) "loading again" "loading" (Sim.html sim);
        Alcotest.check strings "same input" [ "3" ] (Sim.call_inputs sim);
        Sim.answer sim endpoint 4;
        Alcotest.(check string) "ready" "ready(4)" (Sim.html sim)
    );
    Alcotest.test_case "reload keeps the old data while loading" `Quick
      (fun () ->
        let sim = Sim.mount (component ()) in
        Sim.send sim (Remote_machine.Load 1);
        Sim.answer sim endpoint 10;
        Sim.send sim (Remote_machine.Load 2);
        Alcotest.(check string) "old data" "loading(10)" (Sim.html sim);
        Sim.answer_error sim endpoint (Remote.Network "x");
        Alcotest.(check string) "failed keeps last" "failed(10)" (Sim.html sim);
        Sim.send sim Remote_machine.Retry;
        Alcotest.(check string) "retry keeps last" "loading(10)" (Sim.html sim);
        Alcotest.check strings "retry input" [ "2" ] (Sim.call_inputs sim)
    );
    Alcotest.test_case "retry is unhandled outside failed" `Quick (fun () ->
        let sim = Sim.mount (component ()) in
        Sim.send sim Remote_machine.Retry;
        Alcotest.(check string) "idle" "idle" (Sim.html sim);
        Sim.send sim (Remote_machine.Load 1);
        Sim.send sim Remote_machine.Retry;
        Alcotest.check strings "still one call" [ "1" ] (Sim.call_inputs sim)
    );
    Alcotest.test_case "a new load stops the old call" `Quick (fun () ->
        let sim = Sim.mount (component ()) in
        Sim.send sim (Remote_machine.Load 1);
        Sim.send sim (Remote_machine.Load 2);
        Alcotest.check strings "only the new call" [ "2" ] (Sim.call_inputs sim)
    );
    Alcotest.test_case "reset drops the data and stops the call" `Quick
      (fun () ->
        let sim = Sim.mount (component ()) in
        Sim.send sim (Remote_machine.Load 1);
        Sim.answer sim endpoint 10;
        Sim.send sim Remote_machine.Reset;
        Alcotest.(check string) "idle" "idle" (Sim.html sim);
        Sim.send sim (Remote_machine.Load 1);
        Sim.send sim Remote_machine.Reset;
        Alcotest.check strings "no call" [] (Sim.call_inputs sim)
    );
    Alcotest.test_case "timeout_ms makes a timeout error" `Quick (fun () ->
        let sim = Sim.mount (component ~timeout_ms:50 ()) in
        Sim.send sim (Remote_machine.Load 1);
        Alcotest.(check (list (option int)))
          "timeout given to the host" [ Some 50 ] (Sim.call_timeouts sim);
        Sim.expire_call sim endpoint;
        Alcotest.(check string) "failed" "failed" (Sim.html sim);
        match (Sim.snapshot sim).status with
        | Status.Failed { error = Remote.Network "timeout"; _ } ->
            ()
        | _ ->
            Alcotest.fail "expected Network timeout"
    );
    Alcotest.test_case "no timeout by default" `Quick (fun () ->
        let sim = Sim.mount (component ()) in
        Sim.send sim (Remote_machine.Load 1);
        Alcotest.(check (list (option int)))
          "none" [ None ] (Sim.call_timeouts sim)
    );
  ]

let () = Alcotest.run "remote_machine" [ ("remote_machine", tests) ]
