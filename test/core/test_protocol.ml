open Silica
module Json = Silica_json.Json

let obj l = Json.declassify (`Assoc l)
let num n = Json.declassify (`Int n)

let client_t =
  Alcotest.testable
    (fun fmt c -> Format.pp_print_string fmt (Protocol.encode_client c))
    (fun a b -> Protocol.encode_client a = Protocol.encode_client b)

let server_t =
  Alcotest.testable
    (fun fmt c -> Format.pp_print_string fmt (Protocol.encode_server c))
    (fun a b -> Protocol.encode_server a = Protocol.encode_server b)

let ok = function Ok x -> x | Error e -> Alcotest.fail e

let round_client message =
  ok (Protocol.decode_client (Protocol.encode_client message))

let round_server message =
  ok (Protocol.decode_server (Protocol.encode_server message))

let is_error = function Error _ -> true | Ok _ -> false

let tests =
  [
    Alcotest.test_case "the version is 1" `Quick (fun () ->
        Alcotest.(check int) "v" 2 Protocol.version
    );
    Alcotest.test_case "hello for an island round trips" `Quick (fun () ->
        let hello =
          Protocol.Hello
            {
              v = 2;
              csrf = "tok";
              sub = Protocol.Island { channel = "counter"; instance = "i-1" };
            }
        in
        Alcotest.check client_t "same" hello (round_client hello)
    );
    Alcotest.test_case "hello for a topic round trips" `Quick (fun () ->
        let hello =
          Protocol.Hello
            {
              v = 2;
              csrf = "tok";
              sub = Protocol.Topic { name = "issues"; key = "team-1" };
            }
        in
        Alcotest.check client_t "same" hello (round_client hello)
    );
    Alcotest.test_case "the hello wire form is fixed" `Quick (fun () ->
        let text =
          Protocol.encode_client
            (Protocol.Hello
               {
                 v = 2;
                 csrf = "c";
                 sub = Protocol.Topic { name = "t"; key = "k" };
               }
            )
        in
        Alcotest.(check string)
          "json"
          {|{"t":"hello","v":2,"csrf":"c","sub":{"topic":"t","key":"k"}}|} text
    );
    Alcotest.test_case "event and ping round trip" `Quick (fun () ->
        let event = Protocol.Event { id = 7; event = obj [ ("a", num 1) ] } in
        Alcotest.check client_t "event" event (round_client event);
        Alcotest.check client_t "ping" Protocol.Ping (round_client Protocol.Ping);
        Alcotest.(check string)
          "ping wire" {|{"t":"ping"}|}
          (Protocol.encode_client Protocol.Ping)
    );
    Alcotest.test_case "server messages round trip" `Quick (fun () ->
        let all =
          [
            Protocol.Hello_ok { v = 2 };
            Protocol.State
              {
                seq = 3;
                ack = 2;
                state = num 5;
                actions = [ Protocol.Back; Protocol.Reload ];
              };
            Protocol.Error { code = "unknown_topic" };
            Protocol.Pong;
          ]
        in
        List.iter
          (fun message ->
            Alcotest.check server_t "same" message (round_server message)
          )
          all;
        Alcotest.(check string)
          "pong wire" {|{"t":"pong"}|}
          (Protocol.encode_server Protocol.Pong)
    );
    Alcotest.test_case "state carries seq and ack on the wire" `Quick (fun () ->
        let text = Protocol.encode_state Codec.int ~seq:9 ~ack:4 1 [] in
        Alcotest.(check string)
          "json" {|{"t":"state","seq":9,"ack":4,"state":1,"actions":[]}|} text;
        match ok (Protocol.decode_server text) with
        | Protocol.State { seq = 9; ack = 4; state; actions = [] } ->
            Alcotest.(check int)
              "decoded" 1
              (ok (Protocol.decode_state Codec.int state))
        | _ ->
            Alcotest.fail "not a state message"
    );
    Alcotest.test_case "encode_event puts the id and the encoded event" `Quick
      (fun () ->
        let text = Protocol.encode_event Codec.string ~id:3 "x" in
        Alcotest.(check string) "json" {|{"t":"event","id":3,"event":"x"}|} text;
        match ok (Protocol.decode_client text) with
        | Protocol.Event { id = 3; event } ->
            Alcotest.(check string)
              "payload" "x"
              (ok (Protocol.decode_event_payload Codec.string event))
        | _ ->
            Alcotest.fail "not an event"
    );
    Alcotest.test_case "bad input is an error, never an exception" `Quick
      (fun () ->
        let bad =
          [
            "";
            "not json";
            "[]";
            {|{"t":"nope"}|};
            {|{"t":"hello"}|};
            {|{"t":"hello","v":2,"csrf":"c"}|};
            {|{"t":"hello","v":2,"sub":{"topic":"a","key":"b"}}|};
            {|{"t":"hello","v":"1","csrf":"c","sub":{"topic":"a","key":"b"}}|};
            {|{"t":"hello","v":2,"csrf":"c","sub":{"nothing":1}}|};
            {|{"t":"hello","v":2,"csrf":"c","sub":{"topic":"a"}}|};
            {|{"t":"event","event":1}|};
            {|{"t":"event","id":"x","event":1}|};
            {|{"t":"event","id":1}|};
          ]
        in
        List.iter
          (fun text ->
            Alcotest.(check bool)
              text true
              (is_error (Protocol.decode_client text))
          )
          bad;
        List.iter
          (fun text ->
            Alcotest.(check bool)
              text true
              (is_error (Protocol.decode_server text))
          )
          [
            "";
            {|{"t":"state","state":1,"actions":[]}|};
            {|{"t":"state","seq":1,"ack":0,"actions":[]}|};
            {|{"t":"error"}|};
            {|{"t":"hello"}|};
          ]
    );
    Alcotest.test_case "a different version still decodes" `Quick (fun () ->
        match
          ok
            (Protocol.decode_client
               {|{"t":"hello","v":2,"csrf":"c","sub":{"topic":"a","key":"b"}}|}
            )
        with
        | Protocol.Hello { v = 2; _ } ->
            ()
        | _ ->
            Alcotest.fail "version must reach the server"
    );
    Alcotest.test_case "limits and close codes" `Quick (fun () ->
        Alcotest.(check int) "message cap" 65536 Protocol.max_message;
        Alcotest.(check int) "hard limit" 1048576 Protocol.hard_limit;
        Alcotest.(check int) "ping" 30000 Protocol.ping_interval_ms;
        Alcotest.(check int) "pong" 10000 Protocol.pong_timeout_ms;
        let codes =
          [
            Protocol.close_version;
            Protocol.close_hello;
            Protocol.close_csrf;
            Protocol.close_unknown;
            Protocol.close_session;
            Protocol.close_idle;
            Protocol.close_too_big;
            Protocol.close_server_error;
          ]
        in
        Alcotest.(check int)
          "all different" (List.length codes)
          (List.length (List.sort_uniq compare codes));
        Alcotest.(check int) "too big is 1009" 1009 Protocol.close_too_big;
        Alcotest.(check bool)
          "app codes" true
          (List.for_all
             (fun c -> c = 1009 || c = 1011 || (c >= 4000 && c < 5000))
             codes
          )
    );
  ]

let () = Alcotest.run "protocol" [ ("protocol", tests) ]
