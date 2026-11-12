open Silica

let contains text needle =
  let size = String.length needle and length = String.length text in
  let rec look index =
    index + size <= length
    && (String.sub text index size = needle || look (index + 1))
  in
  look 0

let typed = [ Record.title; Record.body; "zebra"; "elephant" ]

let visit = lazy (Record.visit ~create:true ())

let absent label text =
  List.iter
    (fun needle ->
      if contains text needle then
        Alcotest.failf "%s holds %S:\n%s" label needle text
    )
    typed

let full_trace_holds_the_text () =
  let full = Trace.to_jsonl (Lazy.force visit) in
  List.iter
    (fun needle ->
      Alcotest.(check bool)
        ("the trace with data holds " ^ needle)
        true (contains full needle)
    )
    typed

let names_only_holds_no_typed_text () =
  let names = Trace.names_only (Lazy.force visit) in
  absent "the names-only trace file" (Trace.to_jsonl names);
  absent "the names-only trace text" (Trace.to_text names)

let recorder_keeps_names_only () =
  let trace = Lazy.force visit in
  let recorder = Trace.Recorder.create ~build:"b" () in
  List.iter (Trace.Recorder.record recorder) trace.transitions;
  let recorded = Trace.Recorder.trace recorder ~url:(Some "/?q=zebra") in
  absent "the recorder trace" (Trace.to_jsonl recorded);
  Alcotest.(check bool)
    "the recorder gives the names-only form" true
    (recorded.transitions = (Trace.names_only trace).transitions);
  Alcotest.(check (option string))
    "the URL of the header loses its query" (Some "/") recorded.header.url

let after prefix text =
  if String.starts_with ~prefix text then
    Some
      (String.sub text (String.length prefix)
         (String.length text - String.length prefix)
      )
  else
    None

let without_last text =
  match String.rindex_opt text ' ' with
  | Some space ->
      String.sub text 0 space
  | None ->
      text

let ids_still_match () =
  let names = Trace.names_only (Lazy.force visit) in
  let actions =
    List.concat_map
      (fun (record : Transition.t) -> record.actions)
      names.transitions
  in
  let started =
    List.filter_map
      (fun action -> Option.map without_last (after "start call " action))
      actions
  in
  let stopped = List.filter_map (after "stop ") actions in
  let answered =
    List.filter_map
      (fun (record : Transition.t) ->
        match[@warning "-4"] record.cause with
        | Answer { id; _ } ->
            Some id
        | _ ->
            None
      )
      names.transitions
  in
  Alcotest.(check bool) "calls were started" true (List.length started >= 3);
  List.iter
    (fun id ->
      if not (List.mem id started) then
        Alcotest.failf "the answer %s has no start among %s" id
          (String.concat ", " started);
      if not (contains id "/#") then
        Alcotest.failf "the id %s is not a number" id
    )
    answered;
  List.iter
    (fun id ->
      if contains id "/api/" then Alcotest.failf "the stop %s keeps its path" id
    )
    stopped

let live_keys_with_spaces () =
  let record actions cause : Transition.t =
    {
      seq = 1;
      time_ms = None;
      instance = "app";
      number = 3;
      entry = 1;
      machine = "m";
      from = "a";
      target = "b";
      change = Entered;
      event = "go";
      data = None;
      props = None;
      snapshot = None;
      actions;
      cause;
    }
  in
  let id = "3.1/m/state with space/live:topic secret key" in
  let records =
    Transition.names_only
      [
        record
          [ "start live " ^ id ^ " topic secret key"; "set_title secret" ]
          (Element
             {
               handler = "press";
               role = Some "link";
               name = "secret";
               element = "a";
               value = None;
             }
          );
        record
          [ "stop " ^ id; "navigate /x?q=secret#secret" ]
          (Live { id; topic = "topic"; update = None });
        record [] (Url "/y?secret");
      ]
  in
  let text =
    String.concat "\n" (List.map (Codec.to_string Transition.codec) records)
  in
  if contains text "secret" then
    Alcotest.failf "names only keeps a secret:\n%s" text;
  match records with
  | [ first; second; third ] ->
      Alcotest.(check (list string))
        "the live start keeps its topic, the title goes"
        [ "start live 3.1/m/#1 topic"; "set_title" ]
        first.actions;
      Alcotest.(check (list string))
        "the stop has the same id, the URL its path"
        [ "stop 3.1/m/#1"; "navigate /x" ]
        second.actions;
      Alcotest.(check bool)
        "the cause has the same id" true
        (second.cause = Live { id = "3.1/m/#1"; topic = "topic"; update = None });
      Alcotest.(check bool)
        "the URL cause has its path" true (third.cause = Url "/y")
  | _ ->
      Alcotest.fail "three records"

let spec =
  match
    Spec.of_text
      "machine m\n\
       initial idle\n\
       state idle\n\
       state busy\n\
       edge idle -> busy on go\n\
       stay busy on props\n"
  with
  | Ok spec ->
      spec
  | Error message ->
      failwith message

let spec_conformance () =
  let record ?(machine = "m") ?(cause = Transition.Message) ~change ~from
      ~target ~event seq : Transition.t =
    {
      seq;
      time_ms = None;
      instance = "app";
      number = 1;
      entry = 0;
      machine;
      from;
      target;
      change;
      event;
      data = None;
      props = None;
      snapshot = None;
      actions = [];
      cause;
    }
  in
  let trace : Trace.t =
    {
      header =
        {
          root = "m";
          url = None;
          build = "b";
          origin = Runtime { platform = "memory" };
          dropped = 0;
          loop_from = None;
        };
      transitions =
        [
          record ~change:Started ~from:"" ~target:"idle" ~event:"" 1;
          record ~change:Entered ~from:"idle" ~target:"busy" ~event:"go" 2;
          record ~cause:Props ~change:Kept ~from:"busy" ~target:"busy" ~event:""
            3;
          record ~change:Unhandled ~from:"busy" ~target:"busy" ~event:"go" 4;
          record ~change:Entered ~from:"busy" ~target:"idle" ~event:"stop" 5;
          record ~change:Entered ~from:"busy" ~target:"lost" ~event:"go" 6;
          record ~machine:"other" ~change:Started ~from:"" ~target:"x" ~event:""
            7;
        ];
    }
  in
  Alcotest.(check (list string))
    "the steps that no spec has"
    [
      "transition 5, machine m, instance app: no spec has the edge busy -> \
       idle on stop";
      "transition 6, machine m, instance app: no spec has the state lost";
      "transition 7, machine other, instance app: no spec of the machine";
    ]
    (List.map Spec.deviation_to_string (Spec.conforms [ spec ] trace));
  let joint =
    {
      spec with
      edges =
        [ { from = "busy"; target = "idle"; event = "stop"; kept = false } ];
    }
  in
  Alcotest.(check int)
    "a second spec of the machine allows its edges" 2
    (List.length (Spec.conforms [ spec; joint ] trace))

let property_and_allowance () =
  Linear_codecs.register ();
  let blank =
    Check.always ~name:"no blank comment is sent"
      (Check.state (function
        | Linear_composer.Sending body ->
            String.trim body <> ""
        | Empty | Drafting _ | Failed _ ->
            true
        ))
  in
  let check allow =
    Check.component ~props:[ 2 ] ~properties:[ blank ] ~allow
      Linear_composer.component
  in
  let resumed snapshot : Transition.t =
    {
      seq = 1;
      time_ms = None;
      instance = "comment-composer";
      number = 1;
      entry = 0;
      machine = "comment-composer";
      from = "";
      target = "sending";
      change = Resumed;
      event = "";
      data = None;
      props = Some (Codec.int.encode 2);
      snapshot =
        Some
          (Codec.to_string Codec.string snapshot
          |> Printf.sprintf "[\"Sending\",%s]"
          |> Silica_json.Json.of_string |> Result.get_ok
          );
      actions = [];
      cause = Start;
    }
  in
  let trace snapshot : Trace.t =
    {
      header =
        {
          root = "comment-composer";
          url = None;
          build = "b";
          origin = Runtime { platform = "memory" };
          dropped = 0;
          loop_from = None;
        };
      transitions = [ resumed snapshot ];
    }
  in
  let failing allow snapshot =
    List.map
      (fun (deviation : Check.deviation) ->
        Check.difference_to_string deviation.difference
      )
      (Check.conforms (check allow) (trace snapshot)).deviations
  in
  Alcotest.(check (list string))
    "a resumed blank comment breaks the property"
    [
      "The property \"no blank comment is sent\" does not hold in state \
       sending.";
    ]
    (failing [] "   ");
  Alcotest.(check (list string)) "a comment with text holds" [] (failing [] "ok");
  Alcotest.(check (list string))
    "an allowance turns it off" []
    (failing
       [ Check.allow ~rule:"no blank comment is sent" ~reason:"a test" () ]
       "   "
    )

let () =
  Alcotest.run "conform"
    [
      ( "names only",
        [
          Alcotest.test_case "the trace with data holds the typed text" `Quick
            full_trace_holds_the_text;
          Alcotest.test_case
            "a names-only trace of mini Linear holds no typed text" `Quick
            names_only_holds_no_typed_text;
          Alcotest.test_case "a recorder without data keeps names only" `Quick
            recorder_keeps_names_only;
          Alcotest.test_case "ids are numbers that still match" `Quick
            ids_still_match;
          Alcotest.test_case "a live key with spaces goes" `Quick
            live_keys_with_spaces;
        ]
      );
      ( "spec",
        [ Alcotest.test_case "conformance by names" `Quick spec_conformance ]
      );
      ( "check",
        [
          Alcotest.test_case "properties on a recorded snapshot, allowances"
            `Quick property_and_allowance;
        ]
      );
    ]
