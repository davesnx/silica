open Silica

let json text =
  match Silica_json.Json.of_string text with
  | Ok json ->
      json
  | Error message ->
      failwith message

let composer (record : Transition.t) = record.machine = "comment-composer"

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
       (fun record -> composer record && record.event = "send")
       (fun record -> { record with from = "empty" })

let padded = "  " ^ Record.body ^ "  "

let old_build trace =
  with_build "2026-10-01" trace
  |> change_record
       (fun record -> composer record && record.event = "typed")
       (fun record ->
         {
           record with
           data = Some (json (Printf.sprintf "[\"Typed\",%S]" padded));
           snapshot = Some (json (Printf.sprintf "[\"Drafting\",%S]" padded));
         }
       )
  |> change_record
       (fun record -> composer record && record.event = "send")
       (fun record ->
         {
           record with
           snapshot = Some (json (Printf.sprintf "[\"Sending\",%S]" padded));
         }
       )

let resumed_blank (trace : Trace.t) =
  let start =
    List.find
      (fun (record : Transition.t) ->
        composer record && record.change = Transition.Started
      )
      trace.transitions
  in
  let answered =
    List.find
      (fun (record : Transition.t) ->
        composer record && record.event = "sent ok"
      )
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
          target = "sending";
          snapshot = Some (json "[\"Sending\",\"   \"]");
        };
        { answered with seq = 2 };
      ];
  }

let write dir name trace =
  Out_channel.with_open_bin (Filename.concat dir name) (fun channel ->
      output_string channel (Trace.to_jsonl trace)
  )

let () =
  let dir = Sys.argv.(1) in
  if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
  let trace = with_build "dev" (Record.visit ~create:false ()) in
  write dir "conforms.jsonl" trace;
  write dir "missing-edge.jsonl" (missing_edge trace);
  write dir "old-build.jsonl" (old_build trace);
  write dir "breaks-property.jsonl" (resumed_blank trace)
