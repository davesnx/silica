open Silica
module Memory = Silica_memory

type light = Off | On

type light_event = Toggle

let light =
  Component.make_with_props
    (Machine.make_with_props ~id:"light" ~init:Off
       ~name:(function Off -> "off" | On -> "on")
       ~states:[ "off"; "on" ]
       ~event_name:(fun Toggle -> "toggle")
       (fun _ state Toggle ->
         Machine.Go (match state with Off -> On | On -> Off)
       )
    )
    (fun label _ ->
      Node.element "button"
        [ Attr.on_press Toggle ]
        [ Node.text ("Toggle " ^ label) ]
    )

type phase = Idle | Waiting | Loaded of string

type state = { phase : phase; shown : bool; label : string }

type event =
  | Load
  | Answered of (string, string) Remote.answer
  | Tick
  | Nothing
  | Rename of string
  | Toggled
  | Hide

let endpoint =
  Remote.declare ~path:"/api/load" ~input:Codec.unit ~output:Codec.string
    ~rejected:Codec.string

let phase_name = function
  | Idle ->
      "idle"
  | Waiting ->
      "waiting"
  | Loaded _ ->
      "loaded"

let event_name = function
  | Load ->
      "load"
  | Answered _ ->
      "answered"
  | Tick ->
      "tick"
  | Nothing ->
      "nothing"
  | Rename _ ->
      "rename"
  | Toggled ->
      "toggled"
  | Hide ->
      "hide"

let panel_machine =
  Machine.make ~id:"panel"
    ~init:{ phase = Idle; shown = true; label = "a" }
    ~name:(fun state -> phase_name state.phase)
    ~event_name
    ~during:(fun state ->
      match state.phase with
      | Waiting ->
          [ Remote.call endpoint () (fun answer -> Answered answer) ]
      | Loaded _ ->
          [ Source.after 500 Tick ]
      | Idle ->
          []
    )
    (fun state event ->
      match (state.phase, event) with
      | Idle, Load ->
          Machine.Go { state with phase = Waiting }
      | Waiting, Answered (Remote.Ok text) ->
          Machine.Go { state with phase = Loaded text }
      | Loaded _, Tick ->
          Machine.Go { state with phase = Idle }
      | _, Rename label ->
          Machine.Go { state with label }
      | _, Toggled ->
          Machine.Stay []
      | _, Hide ->
          Machine.Go { state with shown = false }
      | _, (Load | Answered _ | Tick | Nothing) ->
          Machine.Ignore
    )

let panel =
  Component.make panel_machine (fun state ->
      Node.element "main" []
        [
          Node.element "button" [ Attr.on_press Load ] [ Node.text "Load" ];
          Node.element "button"
            [ Attr.on_press Nothing ]
            [ Node.text "Nothing" ];
          Node.element "button" [ Attr.on_press Hide ] [ Node.text "Hide" ];
          Node.element "input"
            [
              Attr.string "aria-label" "Label";
              Attr.on_input (fun text -> Rename text);
            ]
            [];
          ( if state.shown then
              Component.node_with_props ~key:"a/b.c" ~props:state.label
                ~output:(fun Toggle _ -> [ Toggled ])
                light
            else
              Node.list []
          );
        ]
  )

let register () =
  let constructor name = Codec.constructor name [] in
  Machine.register_codecs panel_machine
    ~state:
      (Codec.make
         ~encode:(fun state ->
           Codec.string.encode (phase_name state.phase ^ " " ^ state.label)
         )
         ~decode:(fun _ -> Error "not read in this test")
      )
    ~event:
      (Codec.make
         ~encode:(fun event -> constructor (event_name event))
         ~decode:(fun _ -> Error "not read in this test")
      )
    ();
  Machine.register_codecs light.Component.machine ~props:Codec.string ()

let cause_text (cause : Transition.cause) =
  match cause with
  | Start ->
      "start"
  | Element { handler; role; name; element; value } ->
      handler ^ " "
      ^ Option.value role ~default:"-"
      ^ " " ^ name ^ " (" ^ element ^ ")"
      ^ Option.fold value ~none:"" ~some:(fun value -> " " ^ value)
  | Timer { ms; _ } ->
      "timer " ^ string_of_int ms
  | Answer { path; answer; _ } ->
      "answer " ^ path ^ " "
      ^ Option.fold answer ~none:"-" ~some:Silica_json.Json.to_string
  | Live { topic; _ } ->
      "live " ^ topic
  | Held { source; _ } ->
      "held " ^ source
  | Action_result { action } ->
      "result " ^ action
  | Props ->
      "props"
  | Child { instance } ->
      "child " ^ instance
  | Url url ->
      "url " ^ url
  | Preload url ->
      "preload " ^ url
  | Message ->
      "message"
  | Ended ->
      "end"

let change_text : Transition.change -> string = function
  | Started ->
      "started"
  | Resumed ->
      "resumed"
  | Unhandled ->
      "unhandled"
  | Kept ->
      "kept"
  | Entered ->
      "entered"
  | Ended ->
      "ended"

let line (record : Transition.t) =
  Printf.sprintf "%d %s %s->%s %s [%s] <- %s" record.seq record.instance
    record.from record.target
    (change_text record.change)
    record.event (cause_text record.cause)

let json_text = Option.map Silica_json.Json.to_string

let memory_run () =
  register ();
  let memory = Memory.mount panel in
  let press name =
    Memory.press (Memory.by_role ~name memory Accessibility.Button)
  in
  press "Load";
  ( match Memory.pending memory with
  | [ request ] ->
      Memory.reply request {|"hi"|}
  | _ ->
      Alcotest.fail "one call"
  );
  Memory.advance memory 500;
  press "Nothing";
  Memory.input (Memory.by_role ~name:"Label" memory Accessibility.Textbox) "b";
  press "Toggle b";
  press "Hide";
  memory

let records_of_a_run () =
  let trace = Memory.trace (memory_run ()) in
  Alcotest.(check (list string))
    "every start, step, ignored event, props change and end, with its cause"
    [
      "1 panel ->idle started [] <- start";
      "2 panel/#a%2Fb%2Ec:light ->off started [] <- start";
      "3 panel idle->waiting entered [load] <- press button Load (main > \
       button[1])";
      "4 panel waiting->loaded entered [answered] <- answer /api/load \
       [\"Ok\",\"hi\"]";
      "5 panel loaded->idle entered [tick] <- timer 500";
      "6 panel idle->idle unhandled [nothing] <- press button Nothing (main > \
       button[2])";
      "7 panel idle->idle kept [rename] <- input textbox Label (main > input) b";
      "8 panel/#a%2Fb%2Ec:light off->off unhandled [] <- props";
      "9 panel/#a%2Fb%2Ec:light off->on entered [toggle] <- press button \
       Toggle b (main > button[4])";
      "10 panel idle->idle kept [toggled] <- child panel/#a%2Fb%2Ec:light";
      "11 panel idle->idle kept [hide] <- press button Hide (main > button[3])";
      "12 panel/#a%2Fb%2Ec:light on->on ended [] <- end";
    ]
    (List.map line trace.transitions);
  let find seq = List.nth trace.transitions (seq - 1) in
  Alcotest.(check (option string))
    "the snapshot after a step, by the registered codec" (Some {|"waiting a"|})
    (json_text (find 3).snapshot);
  Alcotest.(check (option string))
    "the event data, by the registered codec" (Some {|["load"]|})
    (json_text (find 3).data);
  Alcotest.(check (option string))
    "the props at the start of a child" (Some {|"a"|}) (json_text (find 2).props);
  Alcotest.(check (option string))
    "the new props of a child" (Some {|"b"|}) (json_text (find 8).props);
  Alcotest.(check (option string))
    "no event data with no event codec" None (json_text (find 9).data);
  Alcotest.(check (option string))
    "no snapshot with no state codec" None
    (json_text (find 9).snapshot);
  Alcotest.(check (list string))
    "the actions of a step, with the ids the host sees"
    [ "start call 1.1/panel/waiting//api/load null /api/load" ]
    (find 3).actions;
  Alcotest.(check (list (option (float 0.))))
    "the time of each record, by the memory clock"
    [ Some 0.; Some 0.; Some 500.; Some 500. ]
    (List.map (fun seq -> (find seq).time_ms) [ 3; 4; 5; 6 ]);
  Alcotest.(check (pair int int))
    "instance and entry numbers" (2, 1)
    ((find 9).number, (find 9).entry);
  Alcotest.(check string) "the header names the root" "panel" trace.header.root;
  Alcotest.(check (option string))
    "the header has the first URL" (Some "/") trace.header.url

let text_form () =
  let text = Trace.to_text (Memory.trace (memory_run ())) in
  let lines = String.split_on_char '\n' text in
  Alcotest.(check string)
    "the header line" "trace panel / build memory from memory" (List.hd lines);
  Alcotest.(check string)
    "a step with its time, cause and actions"
    "3 +0ms panel idle -> waiting (entered) on load <- press button \"Load\" \
     (main > button[1]) => start call 1.1/panel/waiting//api/load null \
     /api/load"
    (List.nth lines 3);
  Alcotest.(check string)
    "a timer step"
    "5 +500ms panel loaded -> idle (entered) on tick <- timer 500ms \
     1.2/panel/loaded/after 500ms => stop 1.2/panel/loaded/after 500ms"
    (List.nth lines 5)

let hook_without_codecs () =
  let records = ref [] in
  let tree =
    Instance.mount_with_props
      ~observe:
        (Transition.observer (fun record ->
             records := Transition.resolve record :: !records
         )
        )
      ~props:"x" light
  in
  let node, _ = Instance.update tree None in
  ignore node;
  Instance.set_props tree "x";
  Instance.set_props tree "y";
  ignore (Instance.update tree (Some (Instance.message tree Toggle)));
  ignore (Instance.unmount tree);
  Alcotest.(check (list string))
    "a tree with a hook and no host: equal props make no record"
    [
      "1 light ->off started [] <- start";
      "2 light off->off unhandled [] <- props";
      "3 light off->on entered [toggle] <- message";
      "4 light on->on ended [] <- end";
    ]
    (List.rev_map line !records)

let no_hook_no_record () =
  let observed = ref 0 in
  let tree = Instance.mount panel in
  ignore (Instance.update tree (Some (Instance.message tree Load)));
  let tree_with_hook =
    Instance.mount ~observe:(Transition.observer (fun _ -> incr observed)) panel
  in
  ignore
    (Instance.update tree_with_hook
       (Some (Instance.message tree_with_hook Load))
    );
  Alcotest.(check int) "only the tree with a hook calls it" 3 !observed;
  ignore tree

let recorder_ring () =
  let recorder = Trace.Recorder.create ~capacity:2 ~build:"b1" () in
  List.iter (Trace.Recorder.record recorder) Trace_cases.samples;
  let trace = Trace.Recorder.trace recorder ~url:(Some "/x") in
  Alcotest.(check (list int))
    "the last records are kept" [ 15; 16 ]
    (List.map (fun (record : Transition.t) -> record.seq) trace.transitions);
  Alcotest.(check int)
    "the dropped records are counted"
    (List.length Trace_cases.samples - 2)
    trace.header.dropped;
  Alcotest.(check string) "the build" "b1" trace.header.build;
  Trace.Recorder.clear recorder;
  let cleared = Trace.Recorder.trace recorder ~url:None in
  Alcotest.(check (pair int int))
    "clear empties the ring" (0, 0)
    (List.length cleared.transitions, cleared.header.dropped)

let recorder_names_only () =
  register ();
  let recorder = Trace.Recorder.create ~build:"b" () in
  let memory_like =
    Instance.mount ~observe:(Trace.Recorder.observer recorder) panel
  in
  ignore (Instance.update memory_like None);
  ignore
    (Instance.update memory_like
       (Some (Instance.message memory_like (Rename "secret")))
    );
  let records = Trace.Recorder.transitions recorder in
  Alcotest.(check bool)
    "names are kept" true
    (List.exists
       (fun (record : Transition.t) -> record.event = "rename")
       records
    );
  Alcotest.(check bool)
    "data, props and snapshots are not kept" true
    (List.for_all
       (fun (record : Transition.t) ->
         record.data = None && record.snapshot = None && record.props = None
       )
       records
    );
  let stripped =
    List.map Transition.strip Trace_cases.samples
    |> List.filter_map (fun (record : Transition.t) ->
        match[@warning "-4"] record.cause with
        | Answer { answer = Some _; _ } | Live { update = Some _; _ } ->
            Some "answer or update"
        | Element { handler = "input"; value = Some _; _ } ->
            Some "input text"
        | _ ->
            None
    )
  in
  Alcotest.(check (list string))
    "strip drops answers, updates and input text" [] stripped;
  let data_recorder = Trace.Recorder.create ~data:true ~build:"b" () in
  let tree =
    Instance.mount ~observe:(Trace.Recorder.observer data_recorder) panel
  in
  ignore (Instance.update tree None);
  Alcotest.(check bool)
    "with data the snapshot is kept" true
    (List.exists
       (fun (record : Transition.t) -> record.snapshot <> None)
       (Trace.Recorder.transitions data_recorder)
    )

type probe = Probe

let probe =
  let el = Node.element and text = Node.text in
  let press = Attr.on_press Probe in
  let typing = Attr.on_input (fun _ -> Probe) in
  Component.make
    (Machine.make ~id:"probe" ~init:()
       ~name:(fun () -> "only")
       (fun () Probe -> Machine.Ignore)
    )
    (fun () ->
      el "main" []
        [
          el "header" []
            [ el "nav" [] [ el "button" [ press ] [ text "Home" ] ] ];
          el "ul" []
            (List.init 3 (fun index ->
                 el "li" []
                   [
                     el "button" [ press ]
                       [ text ("Row " ^ string_of_int index) ];
                   ]
             )
            );
          el "section"
            [ Attr.string "aria-label" "Box" ]
            [
              el "button" [ Attr.string "aria-labelledby" "named"; press ] [];
              el "span" [ Attr.string "id" "named" ] [ text "Labelled" ];
            ];
          el "label" [] [ text "Name "; el "input" [ typing ] [] ];
          el "label" [ Attr.string "for" "mail" ] [ text "Mail" ];
          el "input" [ Attr.string "id" "mail"; typing ] [];
          el "input" [ Attr.string "aria-label" "Plain"; typing ] [];
          el "div" [ Attr.string "role" "button"; press ] [ text "Div" ];
          Node.portal
            [
              el "div" [] [ el "button" [ press ] [ text "In portal" ] ];
              el "button" [ press ] [ text "Second" ];
            ];
        ]
    )

let elements_found_later () =
  let recorder = Trace.Recorder.create ~data:true ~build:"t" () in
  let tree = Instance.mount ~observe:(Trace.Recorder.observer recorder) probe in
  let view, _ = Instance.update tree None in
  let expected = ref [] in
  List.iter
    (fun element ->
      List.iter
        (fun attr ->
          let delivered =
            match[@warning "-4"] attr with
            | Node.On_press message ->
                Some ("press", None, message)
            | Node.On_input make ->
                Some ("input", Some "typed", make "typed")
            | _ ->
                None
          in
          Option.iter
            (fun (handler, value, message) ->
              ignore (Instance.update tree (Some message));
              expected :=
                Transition.Element
                  {
                    handler;
                    role =
                      Option.map Accessibility.role_to_string
                        (Accessibility.role element);
                    name = Accessibility.name element;
                    element = Accessibility.path element;
                    value;
                  }
                :: !expected
            )
            delivered
        )
        (Accessibility.attributes element)
    )
    (Accessibility.elements view);
  let got =
    List.filter_map
      (fun (record : Transition.t) ->
        match record.change with
        | Transition.Unhandled ->
            Some record.cause
        | Transition.Started | Resumed | Kept | Entered | Ended ->
            None
      )
      (Trace.Recorder.transitions recorder)
  in
  Alcotest.(check int) "one record for each handler" 11 (List.length got);
  List.iter2
    (fun expected got ->
      if expected <> got then
        Alcotest.failf
          "the element found later is not the element of the view:\n%s"
          (Trace.to_text
             {
               header = (Trace.Recorder.trace recorder ~url:None).header;
               transitions =
                 [
                   {
                     (List.hd (Trace.Recorder.transitions recorder)) with
                     cause = expected;
                   };
                   {
                     (List.hd (Trace.Recorder.transitions recorder)) with
                     cause = got;
                   };
                 ];
             }
          )
    )
    (List.rev !expected) got

let codec_round_trip () =
  Alcotest.(check int) "every sample reads back" 16 (Trace_cases.run ())

let answer outcome = (Remote.answer_codec Codec.int Codec.string).encode outcome

let answer_cause answer : Transition.cause =
  Answer { id = "1.1/save/saving//api/save null"; path = "/api/save"; answer }

let element ?role ?value handler name : Transition.cause =
  Element { handler; role; name; element = "main > button"; value }

let cause_labels () =
  List.iter
    (fun (expected, cause) ->
      Alcotest.(check string) expected expected (Transition.cause_label cause)
    )
    [
      ("press button \"Save\"", element ~role:"button" "press" "Save");
      ( "key_down Escape textbox \"Search\"",
        element ~role:"textbox" ~value:"Escape" "key_down" "Search"
      );
      ("press", element "press" "");
      ("after 2000ms", Timer { id = "1.1/save/saved/after 2000ms"; ms = 2000 });
      ("ok /api/save", answer_cause (Some (answer (Remote.Ok 1))));
      ("rejected /api/save", answer_cause (Some (answer (Remote.Rejected "no"))));
      ( "failed /api/save",
        answer_cause (Some (answer (Remote.Failed (Remote.Http 500))))
      );
      ("answer /api/save", answer_cause None);
    ]

let labels () =
  let record event change cause : Transition.t =
    {
      seq = 1;
      time_ms = None;
      instance = "save";
      number = 1;
      entry = 1;
      machine = "save";
      from = "idle";
      target = "saving";
      change;
      event;
      data = None;
      props = None;
      snapshot = None;
      actions = [];
      cause;
    }
  in
  let press = element ~role:"button" "press" "Save" in
  Alcotest.(check string)
    "the event name when the machine names it" "save"
    (Transition.label (record "save" Transition.Entered press));
  Alcotest.(check string)
    "the cause when it does not" "press button \"Save\""
    (Transition.label (record "" Transition.Entered press));
  Alcotest.(check string)
    "an unhandled event too" "after 300ms"
    (Transition.label
       (record "" Transition.Unhandled (Timer { id = "t"; ms = 300 }))
    );
  Alcotest.(check string)
    "no label for a start" ""
    (Transition.label (record "" Transition.Started Start))

let () =
  let case name run = Alcotest.test_case name `Quick run in
  Alcotest.run "trace"
    [
      ( "log",
        [
          case "the records of a run in memory" records_of_a_run;
          case "the text form" text_form;
          case "a hook on a tree with no host and no codecs" hook_without_codecs;
          case "a tree with no hook observes nothing" no_hook_no_record;
          case "the element of a handler, found when the trace is read"
            elements_found_later;
        ]
      );
      ( "labels",
        [
          case "the label of a cause" cause_labels;
          case "the event name, or the label of the cause" labels;
        ]
      );
      ( "file",
        [
          case "the codec and the file read back" codec_round_trip;
          case "the recorder is a ring" recorder_ring;
          case "the recorder keeps names only by default" recorder_names_only;
        ]
      );
    ]
