open Silica
module Json = Silica_json.Json

let json text =
  match Json.of_string text with
  | Ok json ->
      json
  | Error message ->
      failwith message

let record ?(change = Transition.Kept) ?data ?props ?snapshot ?(actions = [])
    ?time_ms seq cause : Transition.t =
  {
    seq;
    time_ms;
    instance = "app/0.1:child";
    number = 2;
    entry = 3;
    machine = "child";
    from = "idle";
    target = "busy";
    change;
    event = "go";
    data;
    props;
    snapshot;
    actions;
    cause;
  }

let samples : Transition.t list =
  [
    record ~change:Started ~props:(json {|{"label":"a"}|})
      ~snapshot:(json {|["Idle"]|}) 1 Start;
    record ~time_ms:12.5
      ~data:(json {|["Go",1,[true,null]]|})
      ~actions:[ "start timer 2.3/child/busy/after 500ms"; "focus title" ]
      2
      (Element
         {
           handler = "input";
           role = Some "textbox";
           name = "Title";
           element = "main > form > input";
           value = Some "line\none \"quoted\" é";
         }
      );
    record 3
      (Element
         {
           handler = "press";
           role = None;
           name = "";
           element = "div";
           value = None;
         }
      );
    record ~change:Entered 4 (Timer { id = "2.3/child/busy/after"; ms = 500 });
    record ~change:Unhandled 5
      (Answer
         {
           id = "2.3/child/busy/call";
           path = "/api/save";
           answer = Some (json {|["Ok",{"n":1}]|});
         }
      );
    record 6
      (Answer { id = "2.3/child/busy/call"; path = "/api/save"; answer = None });
    record 7
      (Live
         {
           id = "2.3/child/busy/live";
           topic = "issues";
           update = Some (json {|{"seq":2,"status":["Connecting"]}|});
         }
      );
    record 8 (Live { id = "x"; topic = "issues"; update = None });
    record 9 (Held { id = "2.3/child/busy/keys"; source = "keys" });
    record 10 (Action_result { action = "measure title" });
    record ~props:(json {|"b"|}) 11 Props;
    record 12 (Child { instance = "app/0.1:child/#a%2Fb:grandchild" });
    record 13 (Url "/issues?x=1");
    record 14 (Preload "/issue/2");
    record 15 Message;
    record ~change:Ended 16 Ended;
  ]

let header : Trace.header =
  {
    root = "app";
    url = Some "/";
    build = "abc123";
    origin = Runtime { platform = "memory" };
    dropped = 2;
    loop_from = None;
  }

let check name condition = if not condition then failwith name

let starts_with ~prefix text =
  String.length text >= String.length prefix
  && String.sub text 0 (String.length prefix) = prefix

let run () =
  List.iter
    (fun (sample : Transition.t) ->
      let text = Codec.to_string Transition.codec sample in
      match Codec.of_string Transition.codec text with
      | Ok decoded ->
          check
            ("record " ^ text ^ " reads back as another record")
            (decoded = sample)
      | Error message ->
          failwith ("record " ^ text ^ " does not decode: " ^ message)
    )
    samples;
  let trace : Trace.t = { header; transitions = samples } in
  let checker : Trace.t =
    {
      header =
        {
          header with
          origin =
            Checker { rule = "press-ignored"; start = 0; choices = [ 0; 2 ] };
          url = None;
          loop_from = Some 3;
        };
      transitions = samples;
    }
  in
  List.iter
    (fun trace ->
      match Trace.of_jsonl (Trace.to_jsonl trace) with
      | Ok again ->
          check "the file reads back as the same trace" (again = trace)
      | Error message ->
          failwith ("the file does not read back: " ^ message)
    )
    [ trace; checker ];
  let lines = String.split_on_char '\n' (Trace.to_jsonl trace) in
  check "one header line, one line per record, a final newline"
    (List.length lines = List.length samples + 2);
  let cut =
    String.concat "\n" (List.filteri (fun index _ -> index < 4) lines)
  in
  ( match Trace.of_jsonl cut with
  | Ok { transitions; _ } ->
      check "a cut file keeps the records before the cut"
        (List.length transitions = 3)
  | Error message ->
      failwith ("a cut file does not read: " ^ message)
  );
  let broken =
    String.concat "\n"
      (List.mapi
         (fun index line ->
           if index = 2 then
             {|{"seq":"two"}|}
           else
             line
         )
         lines
      )
  in
  ( match Trace.of_jsonl broken with
  | Ok _ ->
      failwith "a bad record reads"
  | Error message ->
      check
        ("the error names the line: " ^ message)
        (message = {|line 3: field "seq": expected an integer|})
  );
  ( match Trace.of_jsonl "{\"root\":1}" with
  | Ok _ ->
      failwith "a bad header reads"
  | Error message ->
      check
        ("the error names the header line: " ^ message)
        (starts_with ~prefix:"line 1: " message)
  );
  List.length samples
