open Silica

let todo (record : Transition.t) = record.machine = "todo"
let item (record : Transition.t) = record.machine = "item"

let state_codec =
  Silica_jsonkit.codec ~to_json:Example_todo.state_to_json
    ~of_json:Example_todo.state_of_json

let event_codec =
  Silica_jsonkit.codec ~to_json:Example_todo.event_to_json
    ~of_json:Example_todo.event_of_json

let snapshot (state : Example_todo.state) = Some (state_codec.encode state)

let change_record found change (trace : Trace.t) =
  let changed = ref false in
  let transitions =
    List.map
      (fun (record : Transition.t) ->
        if (not !changed) && found record then (
          changed := true;
          change record
        ) else
          record
      )
      trace.transitions
  in
  if not !changed then failwith "fixtures: no record to change";
  { trace with transitions }

let with_build build (trace : Trace.t) =
  { trace with header = { trace.header with build } }

let missing_edge trace =
  Trace.names_only trace
  |> change_record
       (fun record -> item record && record.event = "settled ok")
       (fun record -> { record with from = "missing" })

let padded = "  " ^ Record.body ^ "  "

let old_build trace =
  with_build "2026-10-01" trace
  |> change_record
       (fun record -> todo record && record.event = "draft")
       (fun record ->
         {
           record with
           data = Some (event_codec.encode (Example_todo.Draft padded));
           snapshot = snapshot { Example_todo.init with draft = padded };
         }
       )
  |> change_record
       (fun record -> todo record && record.event = "add")
       (fun record ->
         {
           record with
           snapshot =
             snapshot
               {
                 todos = [ { id = 1; text = padded; completed = false } ];
                 draft = "";
                 next_id = 2;
               };
         }
       )

let resumed_twice (trace : Trace.t) =
  let start =
    List.find
      (fun (record : Transition.t) ->
        todo record && record.change = Transition.Started
      )
      trace.transitions
  in
  let added =
    List.find
      (fun (record : Transition.t) -> todo record && record.event = "add")
      trace.transitions
  in
  {
    trace with
    transitions =
      [
        {
          start with
          seq = 1;
          change = Transition.Resumed;
          snapshot =
            snapshot
              {
                todos =
                  [
                    { id = 1; text = "first"; completed = false };
                    { id = 1; text = "second"; completed = false };
                  ];
                draft = "";
                next_id = 2;
              };
        };
        {
          added with
          seq = 2;
          event = "delete";
          data = Some (event_codec.encode (Example_todo.Delete 1));
          snapshot = snapshot { Example_todo.init with next_id = 2 };
          actions = [];
        };
      ];
  }

let write dir name trace =
  Out_channel.with_open_bin (Filename.concat dir name) (fun channel ->
      output_string channel (Trace.to_jsonl trace)
  )

let () =
  let dir = Sys.argv.(1) in
  if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
  let trace = with_build "dev" (Record.visit ~edit:false ()) in
  write dir "conforms.jsonl" trace;
  write dir "missing-edge.jsonl" (missing_edge trace);
  write dir "old-build.jsonl" (old_build trace);
  write dir "breaks-property.jsonl" (resumed_twice trace);
  write dir "edits.jsonl" (with_build "dev" (Record.visit ~edit:true ()))
