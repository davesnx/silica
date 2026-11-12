type entry =
  | Entry : {
      check : ('s, 'ev) Check.t;
      dependents : (('s, 'ev) Check.report -> entry list) option;
    }
      -> entry
  | Elsewhere of { machine : string; reason : string }

let entry ?dependents check = Entry { check; dependents }

let elsewhere ~machine ~reason =
  if String.trim reason = "" then
    invalid_arg
      ("Check_suite.elsewhere: the component " ^ machine
     ^ " has no reason: say which suite checks it"
      );
  Elsewhere { machine; reason }

type ran =
  | Ran : { check : ('s, 'ev) Check.t; report : ('s, 'ev) Check.report } -> ran

type error = { id : string option; message : string }
type file_change = Missing | Changed of string list | Stale

type golden = {
  dir : string;
  written : bool;
  command : string;
  changes : (string * file_change) list;
}

type result = {
  command : string;
  ran : ran list;
  trace_files : (string * string list) list;
  unchecked : string list option;
  elsewhere : (string * string) list;
  errors : error list;
  golden : golden option;
}

let safe_id check =
  match Check.id check with
  | id ->
      Ok id
  | exception Check.Invalid_check message ->
      Error message

let component_of entry =
  match String.split_on_char ' ' entry with
  | "component" :: machine :: _ ->
      Some machine
  | _ ->
      None

let run ?only entries =
  let ran = ref [] in
  let errors = ref [] in
  let elsewhere = ref [] in
  let seen = ref [] in
  let selected id =
    match only with None -> true | Some ids -> List.mem id ids
  in
  let unseen () =
    match only with
    | None ->
        false
    | Some ids ->
        List.exists (fun id -> not (List.mem id !seen)) ids
  in
  let error ?id message = errors := { id; message } :: !errors in
  let rec go entries =
    List.iter
      (function
        | Elsewhere { machine; reason } ->
            elsewhere := (machine, reason) :: !elsewhere
        | Entry { check; dependents } -> (
            match safe_id check with
            | Error message ->
                error message
            | Ok id -> (
                let wanted = selected id in
                if wanted then seen := id :: !seen;
                if wanted || (Option.is_some dependents && unseen ()) then
                  match Check.run check with
                  | report -> (
                      if wanted then ran := Ran { check; report } :: !ran;
                      match
                        Option.fold ~none:[]
                          ~some:(fun make -> make report)
                          dependents
                      with
                      | children ->
                          go children
                      | exception exn ->
                          error ~id
                            ("the dependents of " ^ id ^ " raised "
                           ^ Printexc.to_string exn
                            )
                    )
                  | exception Check.Invalid_check message ->
                      error ~id message
                  | exception exn ->
                      error ~id ("the check raised " ^ Printexc.to_string exn)
              )
          )
        )
      entries
  in
  go entries;
  ( match only with
  | Some ids ->
      List.iter
        (fun id ->
          if not (List.mem id !seen) then
            error ("no entry checks the machine " ^ id)
        )
        ids
  | None ->
      ()
  );
  let ran = List.rev !ran in
  let elsewhere = List.rev !elsewhere in
  let unchecked =
    match only with
    | Some _ ->
        None
    | None ->
        let covered =
          List.concat_map
            (fun (Ran { report; _ }) ->
              report.machine
              :: List.map (fun (spec : Silica.Spec.t) -> spec.id) report.specs
            )
            ran
          @ List.map fst elsewhere
        in
        Some
          (List.sort_uniq compare
             (List.concat_map
                (fun (Ran { report; _ }) ->
                  List.filter_map
                    (fun entry ->
                      match component_of entry with
                      | Some machine when not (List.mem machine covered) ->
                          Some machine
                      | _ ->
                          None
                    )
                    report.not_explored
                )
                ran
             )
          )
  in
  {
    command = "check";
    ran;
    trace_files = [];
    unchecked;
    elsewhere;
    errors = List.rev !errors;
    golden = None;
  }

let step_line (step : _ Check.step) =
  Printf.sprintf "  %s -> %s on %s, by %s\n" step.from step.target
    (match step.event with "" -> "(unnamed event)" | event -> event)
    (Check.describe_cause step.cause)

let indented text =
  String.concat ""
    (List.filter_map
       (fun line ->
         if line = "" then
           None
         else
           Some ("  " ^ line ^ "\n")
       )
       (String.split_on_char '\n' text)
    )

let views_text (report : (_, _) Check.report) =
  String.concat "\n"
    (List.map
       (fun (name, view) ->
         let path =
           match List.assoc_opt name report.paths with
           | None | Some [] ->
               "  (start)\n"
           | Some steps ->
               String.concat "" (List.map step_line steps)
         in
         "state " ^ name ^ "\npath\n" ^ path ^ "view\n" ^ indented view
       )
       report.views
    )

let golden_files (Ran { check; report }) =
  let id = Check.id check in
  let prefix =
    if id = report.machine then
      ""
    else
      id ^ "."
  in
  List.concat_map
    (fun (spec : Silica.Spec.t) ->
      [
        (prefix ^ spec.id ^ ".spec.txt", Silica.Spec.to_text spec);
        (prefix ^ spec.id ^ ".qnt", Silica.Spec.to_quint spec);
        (prefix ^ spec.id ^ ".dot", Silica.Spec.to_dot spec);
      ]
    )
    report.specs
  @ [ (id ^ ".views.txt", views_text report) ]

let suffixes = [ ".spec.txt"; ".views.txt"; ".qnt"; ".dot" ]

let is_golden name =
  List.exists (fun suffix -> String.ends_with ~suffix name) suffixes

let read_file path = In_channel.with_open_bin path In_channel.input_all

let write_file path text =
  Out_channel.with_open_bin path (fun channel ->
      Out_channel.output_string channel text
  )

let lines text =
  match String.split_on_char '\n' text with
  | lines when String.ends_with ~suffix:"\n" text ->
      List.rev (List.tl (List.rev lines))
  | lines ->
      lines

let line_diff before after =
  let before = Array.of_list (lines before) in
  let after = Array.of_list (lines after) in
  let rows = Array.length before and columns = Array.length after in
  let common = Array.make_matrix (rows + 1) (columns + 1) 0 in
  for row = rows - 1 downto 0 do
    for column = columns - 1 downto 0 do
      common.(row).(column) <-
        ( if before.(row) = after.(column) then
            common.(row + 1).(column + 1) + 1
          else
            max common.(row + 1).(column) common.(row).(column + 1)
        )
    done
  done;
  let rec walk row column acc =
    if row < rows && column < columns && before.(row) = after.(column) then
      walk (row + 1) (column + 1) acc
    else if
      column < columns
      && (row = rows || common.(row).(column + 1) >= common.(row + 1).(column))
    then
      walk row (column + 1) (("+ " ^ after.(column)) :: acc)
    else if row < rows then
      walk (row + 1) column (("- " ^ before.(row)) :: acc)
    else
      List.rev acc
  in
  walk 0 0 []

let spec_lines ~before ~after =
  match (Silica.Spec.of_text before, Silica.Spec.of_text after) with
  | Ok old_spec, Ok new_spec -> (
      match Silica.Spec.diff old_spec new_spec with
      | [] ->
          line_diff before after
      | changes ->
          List.map Silica.Spec.change_to_string changes
    )
  | Error message, _ ->
      ("the golden file does not read: " ^ message) :: line_diff before after
  | _, Error message ->
      [ "the new spec does not read: " ^ message ]

let write_command ~program dir =
  let executable = Filename.basename program in
  let marker = Filename.concat "_build" "default" in
  let cwd = Sys.getcwd () in
  let rec find index =
    if index + String.length marker > String.length cwd then
      None
    else if String.sub cwd index (String.length marker) = marker then
      Some (index + String.length marker)
    else
      find (index + 1)
  in
  match find 0 with
  | Some stop ->
      let relative =
        let rest = String.sub cwd stop (String.length cwd - stop) in
        if String.starts_with ~prefix:"/" rest then
          String.sub rest 1 (String.length rest - 1)
        else
          rest
      in
      let path name =
        if relative = "" then
          name
        else
          relative ^ "/" ^ name
      in
      Printf.sprintf "dune exec ./%s -- golden %s --write" (path executable)
        (path dir)
  | None ->
      Printf.sprintf "%s golden %s --write" program dir

let golden_with ~program ~write dir result =
  let existing =
    match Sys.readdir dir with
    | names ->
        List.sort compare (List.filter is_golden (Array.to_list names))
    | exception Sys_error _ ->
        []
  in
  let files = List.concat_map golden_files result.ran in
  let produced = List.map fst files in
  let spec_changed id =
    List.exists
      (fun (name, text) ->
        name = id ^ ".spec.txt"
        &&
        let path = Filename.concat dir name in
        (not (Sys.file_exists path)) || read_file path <> text
      )
      files
  in
  let changes =
    List.filter_map
      (fun (name, text) ->
        let path = Filename.concat dir name in
        if not (List.mem name existing) then
          Some (name, Missing)
        else
          let before = read_file path in
          if before = text then
            None
          else if String.ends_with ~suffix:".spec.txt" name then
            Some (name, Changed (spec_lines ~before ~after:text))
          else if String.ends_with ~suffix:".views.txt" name then
            Some (name, Changed (line_diff before text))
          else
            let id = Filename.remove_extension name in
            if spec_changed id then
              Some
                (name, Changed [ "follows the changes of " ^ id ^ ".spec.txt" ])
            else
              Some (name, Changed (line_diff before text))
      )
      files
    @
    if result.unchecked = None || result.errors <> [] then
      []
    else
      List.filter_map
        (fun name ->
          if List.mem name produced then
            None
          else
            Some (name, Stale)
        )
        existing
  in
  let write = write && result.errors = [] in
  if write then (
    if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
    List.iter
      (fun (name, change) ->
        let path = Filename.concat dir name in
        match change with
        | Stale ->
            Sys.remove path
        | Missing | Changed _ ->
            write_file path (List.assoc name files)
      )
      changes
  );
  {
    result with
    command = "golden";
    golden =
      Some
        { dir; written = write; command = write_command ~program dir; changes };
  }

let golden ?(write = false) dir result =
  golden_with ~program:Sys.argv.(0) ~write dir result

let has_findings result =
  List.exists
    (fun (Ran { report; _ }) ->
      report.violations <> [] || report.unreached <> []
    )
    result.ran
  || (match result.unchecked with Some (_ :: _) -> true | _ -> false)
  ||
  match result.golden with
  | Some { written = false; changes = _ :: _; _ } ->
      true
  | _ ->
      false

let incomplete result =
  List.exists (fun (Ran { report; _ }) -> not report.complete) result.ran

let exit_code result =
  if result.errors <> [] then
    3
  else if has_findings result then
    1
  else if incomplete result then
    2
  else
    0

let outcome = function
  | 0 ->
      "passed"
  | 1 ->
      "findings"
  | 2 ->
      "incomplete"
  | _ ->
      "setup error"

let unchecked_texts machine =
  Check_rules.render
    (Check_rules.finding "unchecked-component" ~values:[ ("machine", machine) ])
    ~state:""

let golden_texts golden name =
  Check_rules.render
    (Check_rules.finding "golden-drift"
       ~values:
         [
           ("file", Filename.concat golden.dir name); ("command", golden.command);
         ]
    )
    ~state:""

let change_kind = function
  | Missing ->
      "missing"
  | Changed _ ->
      "changed"
  | Stale ->
      "stale"

let change_lines = function
  | Missing ->
      [ "the file is not there yet" ]
  | Stale ->
      [ "no entry makes this file" ]
  | Changed lines ->
      lines

let to_json result =
  let open Check_json in
  let code = exit_code result in
  record
    [
      ("format", string "silica-check");
      ("version", int 1);
      ("command", string result.command);
      ("exit_code", int code);
      ("outcome", string (outcome code));
      ("passed", bool (code = 0));
      ( "entries",
        list
          (fun (Ran { check; report }) ->
            Check_json.report
              ?trace_files:(List.assoc_opt (Check.id check) result.trace_files)
              check report
          )
          result.ran
      );
      ( "unchecked",
        list
          (fun machine ->
            let message, hint = unchecked_texts machine in
            record
              [
                ("machine", string machine);
                ("rule", string "unchecked-component");
                ("message", string message);
                ("hint", string hint);
              ]
          )
          (Option.value ~default:[] result.unchecked)
      );
      ( "elsewhere",
        list
          (fun (machine, reason) ->
            record [ ("machine", string machine); ("reason", string reason) ]
          )
          result.elsewhere
      );
      ( "golden",
        option
          (fun golden ->
            record
              [
                ("dir", string golden.dir);
                ("written", bool golden.written);
                ( "changes",
                  list
                    (fun (name, change) ->
                      let message, hint = golden_texts golden name in
                      record
                        [
                          ("file", string name);
                          ("change", string (change_kind change));
                          ("rule", string "golden-drift");
                          ("message", string message);
                          ("hint", string hint);
                          ("lines", list string (change_lines change));
                        ]
                    )
                    golden.changes
                );
              ]
          )
          result.golden
      );
      ( "errors",
        list
          (fun error ->
            record
              [
                ("id", option string error.id); ("message", string error.message);
              ]
          )
          result.errors
      );
    ]

let pp formatter result =
  let quiet =
    result.command = "golden"
    && exit_code result = 0
    && match result.golden with Some { changes = []; _ } -> true | _ -> false
  in
  if not quiet then (
    List.iter
      (fun (Ran { check; report }) ->
        Format.fprintf formatter "%a@." Check.pp_report report;
        List.iteri
          (fun index file ->
            Format.fprintf formatter
              "trace of violation %d: %s; its test: silica script %s %s > %s@."
              (index + 1) file (Check.id check) file
              (Check_json.test_file file)
          )
          (Option.value ~default:[]
             (List.assoc_opt (Check.id check) result.trace_files)
          )
      )
      result.ran;
    List.iter
      (fun machine ->
        let message, hint = unchecked_texts machine in
        Format.fprintf formatter "unchecked-component: %s@.  hint: %s@." message
          hint
      )
      (Option.value ~default:[] result.unchecked);
    List.iter
      (fun (machine, reason) ->
        Format.fprintf formatter "checked elsewhere: %s: %s@." machine reason
      )
      result.elsewhere;
    Option.iter
      (fun golden ->
        List.iter
          (fun (name, change) ->
            let message, _ = golden_texts golden name in
            Format.fprintf formatter "golden-drift: %s@." message;
            List.iter
              (fun line -> Format.fprintf formatter "  %s@." line)
              (change_lines change)
          )
          golden.changes;
        match golden.changes with
        | [] ->
            ()
        | (name, _) :: _ ->
            if golden.written then (
              Format.fprintf formatter "golden: wrote %d files into %s: %s@."
                (List.length golden.changes)
                golden.dir
                (String.concat ", "
                   (List.map
                      (fun (name, change) ->
                        Printf.sprintf "%s (%s)" name
                          ( match change with
                          | Missing ->
                              "new"
                          | Changed _ ->
                              "changed"
                          | Stale ->
                              "removed"
                          )
                      )
                      golden.changes
                   )
                );
              Format.fprintf formatter
                "golden: the golden files now match what the check found, so \
                 the changes above are written, not findings; commit them with \
                 the change of code.@."
            ) else
              Format.fprintf formatter "  hint: %s@."
                (snd (golden_texts golden name))
      )
      result.golden;
    List.iter
      (fun error ->
        Format.fprintf formatter "error%s: %s@."
          (match error.id with Some id -> " in " ^ id | None -> "")
          error.message
      )
      result.errors;
    let code = exit_code result in
    Format.fprintf formatter "%s (exit %d)@." (outcome code) code
  )

let usage =
  "Usage:\n\
  \  main.exe [check] [--json] [--only ID]... [--traces DIR]\n\
  \  main.exe golden DIR [--write] [--json]\n\
  \  main.exe replay ID TRACE_FILE [--json]\n\
  \  main.exe script ID TRACE_FILE [--component EXPR]\n\
  \  main.exe rules [--json]\n\
  \  main.exe conform [--spec DIR] [--only ID]... [--json] TRACE_FILE...\n\
   Exit codes: 0 passed, 1 findings, 2 incomplete, 3 setup error.\n"

type command =
  | Check of { json : bool; only : string list; traces : string option }
  | Golden of { dir : string; write : bool; json : bool }
  | Replay of { id : string; file : string; json : bool }
  | Script of { id : string; file : string; component : string option }
  | Rules of { json : bool }
  | Conform of {
      spec : string;
      only : string list;
      json : bool;
      files : string list;
    }
  | Help

let parse arguments =
  let rec check ~json ~only ~traces = function
    | [] ->
        Ok (Check { json; only = List.rev only; traces })
    | "--json" :: rest ->
        check ~json:true ~only ~traces rest
    | "--only" :: id :: rest ->
        check ~json ~only:(id :: only) ~traces rest
    | "--traces" :: dir :: rest ->
        check ~json ~only ~traces:(Some dir) rest
    | argument :: _ ->
        Error ("check: unknown argument " ^ argument)
  in
  let rec replay ~json = function
    | [ id; file ] ->
        Ok (Replay { id; file; json })
    | "--json" :: rest ->
        replay ~json:true rest
    | [ id; file; "--json" ] ->
        Ok (Replay { id; file; json = true })
    | _ ->
        Error "replay: give the machine id and the trace file"
  in
  let script = function
    | [ id; file ] ->
        Ok (Script { id; file; component = None })
    | [ id; file; "--component"; component ]
    | [ "--component"; component; id; file ] ->
        Ok (Script { id; file; component = Some component })
    | _ ->
        Error "script: give the machine id and the trace file"
  in
  let rec golden ~dir ~write ~json = function
    | [] -> (
        match dir with
        | Some dir ->
            Ok (Golden { dir; write; json })
        | None ->
            Error "golden: give the directory of the golden files"
      )
    | "--write" :: rest ->
        golden ~dir ~write:true ~json rest
    | "--json" :: rest ->
        golden ~dir ~write ~json:true rest
    | argument :: rest
      when dir = None && not (String.starts_with ~prefix:"-" argument) ->
        golden ~dir:(Some argument) ~write ~json rest
    | argument :: _ ->
        Error ("golden: unknown argument " ^ argument)
  in
  let rec conform ~spec ~only ~json ~files = function
    | [] -> (
        match files with
        | [] ->
            Error "conform: give at least one trace file"
        | files ->
            Ok
              (Conform
                 { spec; only = List.rev only; json; files = List.rev files }
              )
      )
    | "--json" :: rest ->
        conform ~spec ~only ~json:true ~files rest
    | "--spec" :: dir :: rest ->
        conform ~spec:dir ~only ~json ~files rest
    | "--only" :: id :: rest ->
        conform ~spec ~only:(id :: only) ~json ~files rest
    | argument :: _ when String.starts_with ~prefix:"-" argument ->
        Error ("conform: unknown argument " ^ argument)
    | file :: rest ->
        conform ~spec ~only ~json ~files:(file :: files) rest
  in
  match arguments with
  | ("--help" | "-help" | "help") :: _ ->
      Ok Help
  | "conform" :: rest ->
      conform ~spec:"spec" ~only:[] ~json:false ~files:[] rest
  | "check" :: rest ->
      check ~json:false ~only:[] ~traces:None rest
  | "golden" :: rest ->
      golden ~dir:None ~write:false ~json:false rest
  | "replay" :: rest ->
      replay ~json:false rest
  | "script" :: rest ->
      script rest
  | [ "rules" ] ->
      Ok (Rules { json = false })
  | [ "rules"; "--json" ] ->
      Ok (Rules { json = true })
  | argument :: _ when not (String.starts_with ~prefix:"-" argument) ->
      Error ("unknown command " ^ argument)
  | rest ->
      check ~json:false ~only:[] ~traces:None rest

let identifier text =
  let mapped =
    String.map
      (fun character ->
        match Char.lowercase_ascii character with
        | ('a' .. 'z' | '0' .. '9') as kept ->
            kept
        | _ ->
            '_'
      )
      text
  in
  String.concat "_"
    (List.filter (fun part -> part <> "") (String.split_on_char '_' mapped))

let trace_names id (violations : (_, _) Check.violation list) =
  let seen = Hashtbl.create 8 in
  List.map
    (fun (violation : (_, _) Check.violation) ->
      let state = identifier violation.state_name in
      let base =
        identifier (id ^ "_" ^ violation.rule)
        ^ "_"
        ^ identifier (String.sub state 0 (min 20 (String.length state)))
      in
      let count = 1 + Option.value ~default:0 (Hashtbl.find_opt seen base) in
      Hashtbl.replace seen base count;
      if count = 1 then
        base
      else
        Printf.sprintf "%s_%d" base count
    )
    violations

let test_file = Check_json.test_file

let remove_stale dir result trace_files =
  let written =
    List.concat_map
      (fun (_, files) -> List.map Filename.basename files)
      trace_files
  in
  let prefixes =
    List.map
      (fun (Ran { check; _ }) -> identifier (Check.id check) ^ "_")
      result.ran
  in
  let owner name =
    List.fold_left
      (fun best prefix ->
        if
          String.starts_with ~prefix name
          &&
          match best with
          | Some other ->
              String.length prefix > String.length other
          | None ->
              true
        then
          Some prefix
        else
          best
      )
      None prefixes
  in
  Array.iter
    (fun name ->
      if
        Filename.check_suffix name ".jsonl"
        && (not (List.mem name written))
        && Option.is_some (owner name)
      then
        Sys.remove (Filename.concat dir name)
    )
    (Sys.readdir dir)

let write_traces dir result =
  match
    if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
    List.map
      (fun (Ran { check; report }) ->
        ( Check.id check,
          List.map2
            (fun (violation : (_, _) Check.violation) name ->
              let file = Filename.concat dir (name ^ ".jsonl") in
              write_file file
                (Silica.Trace.to_jsonl
                   (Check.export check ~rule:violation.rule
                      violation.counterexample
                   )
                );
              file
            )
            report.violations
            (trace_names (Check.id check) report.violations)
        )
      )
      result.ran
  with
  | trace_files ->
      remove_stale dir result trace_files;
      { result with trace_files }
  | exception (Sys_error message | Invalid_argument message) ->
      {
        result with
        errors =
          result.errors
          @ [ { id = None; message = "--traces " ^ dir ^ ": " ^ message } ];
      }

type found = Found : ('s, 'ev) Check.t -> found

let rec find id entries =
  match
    List.find_map
      (function
        | Entry { check; _ } when safe_id check = Ok id ->
            Some (Found check)
        | Entry _ | Elsewhere _ ->
            None
        )
      entries
  with
  | Some found ->
      Some found
  | None ->
      List.find_map
        (function
          | Entry { check; dependents = Some make } -> (
              match make (Check.run check) with
              | children ->
                  find id children
              | exception _ ->
                  None
            )
          | Entry { dependents = None; _ } | Elsewhere _ ->
              None
          )
        entries

let load entries ~id ~file =
  match find id entries with
  | None ->
      Error ("no entry checks the machine " ^ id)
  | Some found -> (
      match read_file file with
      | exception Sys_error message ->
          Error message
      | text -> (
          match Silica.Trace.of_jsonl text with
          | Ok trace ->
              Ok (found, trace)
          | Error message ->
              Error (file ^ ": " ^ message)
        )
    )

let setup_error ~json ~command message =
  if json then
    print_string
      (Check_json.pretty
         (Check_json.record
            [
              ("format", Check_json.string "silica-check");
              ("version", Check_json.int 1);
              ("command", Check_json.string command);
              ("exit_code", Check_json.int 3);
              ("outcome", Check_json.string (outcome 3));
              ("error", Check_json.string message);
            ]
         )
      )
  else
    prerr_endline (command ^ ": " ^ message);
  3

let replay_text (Found check) ~id ~file replay =
  let buffer = Buffer.create 1024 in
  let add format = Printf.bprintf buffer format in
  add "replay %s: %s\n" id file;
  let points, stuck =
    match replay with
    | Check.Replayed points ->
        (points, None)
    | Not_enabled { at; steps; enabled } ->
        (steps, Some (at, enabled))
  in
  List.iteri
    (fun index (point : (_, _) Check.replayed_step) ->
      ( match point.step with
      | None ->
          add "start: %s\n" point.state_name
      | Some step ->
          add "%d. %s -> %s: %s, by %s\n" index step.from step.target
            (match step.event with "" -> "(unnamed event)" | event -> event)
            (Check.describe_cause step.cause)
      );
      List.iter
        (fun (failure : Check.failure) ->
          add "  fails %s: %s\n    hint: %s\n" failure.rule failure.message
            failure.hint
        )
        point.failing
    )
    points;
  ignore check;
  ( match stuck with
  | Some (at, enabled) ->
      add "step %d of the trace is not enabled here. The state offers:\n" at;
      if enabled = [] then add "  nothing\n";
      List.iter (fun step -> add "%s" (step_line step)) enabled
  | None -> (
      match List.rev points with
      | last :: _ ->
          add "state: %s\nview:\n%s" last.state_name (indented last.view)
      | [] ->
          ()
    )
  );
  Buffer.contents buffer

let replay_code = function
  | Check.Not_enabled _ ->
      1
  | Replayed points -> (
      match List.rev points with
      | (last : (_, _) Check.replayed_step) :: _ when last.failing <> [] ->
          1
      | _ ->
          0
    )

let replay_command entries ~id ~file ~json =
  match load entries ~id ~file with
  | Error message ->
      setup_error ~json ~command:"replay" message
  | Ok ((Found check as found), trace) -> (
      match Check.replay_trace check trace with
      | Error message ->
          setup_error ~json ~command:"replay" message
      | Ok replay ->
          let code = replay_code replay in
          if json then
            let points, at, enabled =
              match replay with
              | Replayed points ->
                  (points, None, [])
              | Not_enabled { at; steps; enabled } ->
                  (steps, Some at, enabled)
            in
            print_string
              (Check_json.pretty
                 (Check_json.record
                    [
                      ("format", Check_json.string "silica-check");
                      ("version", Check_json.int 1);
                      ("command", Check_json.string "replay");
                      ("exit_code", Check_json.int code);
                      ("outcome", Check_json.string (outcome code));
                      ("id", Check_json.string id);
                      ("trace_file", Check_json.string file);
                      ( "steps",
                        Check_json.list Fun.id
                          (List.mapi (Check_json.replayed check) points)
                      );
                      ("not_enabled", Check_json.option Check_json.int at);
                      ("enabled", Check_json.steps check ~first:1 enabled);
                    ]
                 )
              )
          else (
            print_string (replay_text found ~id ~file replay);
            Printf.printf "%s (exit %d)\n"
              ( match replay with
              | Not_enabled _ ->
                  "the trace does not replay"
              | Replayed _ when code = 1 ->
                  "the last step breaks a rule"
              | Replayed _ ->
                  "no rule fails"
              )
              code
          );
          code
    )

let formatted ~name text =
  let source = Filename.temp_file "silica_script" ".ml" in
  let output = Filename.temp_file "silica_script" ".formatted" in
  write_file source text;
  let result =
    match
      Sys.command
        (Filename.quote_command "ocamlformat" ~stdout:output
           ~stderr:Filename.null
           [ "--impl"; "--name=" ^ name; source ]
        )
    with
    | 0 ->
        read_file output
    | _ ->
        text
    | exception Sys_error _ ->
        text
  in
  List.iter
    (fun path -> try Sys.remove path with Sys_error _ -> ())
    [ source; output ];
  result

let script_command entries ~id ~file ~component =
  match load entries ~id ~file with
  | Error message ->
      setup_error ~json:false ~command:"script" message
  | Ok (Found check, trace) -> (
      match Check.script ?component check trace with
      | Ok text ->
          let target = test_file file in
          print_string (formatted ~name:target text);
          prerr_endline
            (Printf.sprintf
               "script: the test of the violation %s of %s, from %s. Save it \
                as %s, and add %s to the names of the tests in test/dune."
               ( match trace.header.origin with
               | Checker { rule; _ } ->
                   rule
               | Runtime _ ->
                   "?"
               )
               id file target
               (Filename.remove_extension (Filename.basename target))
            );
          0
      | Error message ->
          prerr_endline
            ("script: " ^ message
           ^ ". No Silica_memory test is written; the command 'replay " ^ id
           ^ " " ^ file
           ^ "' replays the trace in the checker, and exits 1 while the rule \
              fails and 0 after the fix."
            );
          1
    )

let conform_command entries ~spec ~only ~json ~files =
  let found =
    match only with
    | [] ->
        Ok
          (List.filter_map
             (function
               | Entry { check; _ } ->
                   Some (Check_conform.Subject check)
               | Elsewhere _ ->
                   None
               )
             entries
          )
    | ids ->
        List.fold_left
          (fun found id ->
            Result.bind found (fun found ->
                match find id entries with
                | Some (Found check) ->
                    Ok (found @ [ Check_conform.Subject check ])
                | None ->
                    Error ("no entry checks the machine " ^ id)
            )
          )
          (Ok []) ids
  in
  match found with
  | Error message ->
      setup_error ~json ~command:"conform" message
  | Ok subjects ->
      Check_conform.run ~json ~spec_dir:spec ~only ~subjects
        ~elsewhere:
          (List.filter_map
             (function
               | Elsewhere { machine; reason } ->
                   Some (machine, reason)
               | Entry _ ->
                   None
               )
             entries
          )
        files

let print ~json result =
  if json then
    print_string (Check_json.pretty (to_json result))
  else
    Format.printf "%a%!" pp result

let main ?(argv = Sys.argv) entries =
  let arguments = List.tl (Array.to_list argv) in
  let code =
    match parse arguments with
    | Error message ->
        prerr_string (message ^ "\n" ^ usage);
        3
    | Ok Help ->
        print_string usage;
        0
    | Ok (Rules { json }) ->
        if json then
          print_string
            (Check_json.pretty
               (Check_json.record
                  [
                    ("format", Check_json.string "silica-rules");
                    ("version", Check_json.int 1);
                    ("rules", Check_json.list Check_json.rule Check.rules);
                  ]
               )
            )
        else
          List.iter
            (fun (rule : Check.rule) ->
              Printf.printf "%s (%s)\n  %s\n  hint: %s\n" rule.id
                rule.applies_to rule.message rule.hint
            )
            Check.rules;
        0
    | Ok (Check { json; only; traces }) ->
        let only = match only with [] -> None | ids -> Some ids in
        let result = run ?only entries in
        let result =
          match traces with
          | Some dir ->
              write_traces dir result
          | None ->
              result
        in
        print ~json result;
        exit_code result
    | Ok (Replay { id; file; json }) ->
        replay_command entries ~id ~file ~json
    | Ok (Script { id; file; component }) ->
        script_command entries ~id ~file ~component
    | Ok (Conform { spec; only; json; files }) ->
        conform_command entries ~spec ~only ~json ~files
    | Ok (Golden { dir; write; json }) ->
        let result = golden_with ~program:argv.(0) ~write dir (run entries) in
        print ~json result;
        exit_code result
  in
  exit code
