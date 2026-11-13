open Silica

type subject = Subject : ('s, 'ev) Check.t -> subject

type finding = {
  seq : int;
  machine : string;
  instance : string;
  step : string;
  kind : string;
  difference : string;
  hint : string;
  view : string;
}

type checked = {
  file : string;
  trace : Trace.t;
  named : int;
  steps : int;
  snapshots : int;
  not_checked : (string * string) list;
  unchecked : (string * string) list;
  findings : finding list;
}

let read_file path = In_channel.with_open_bin path In_channel.input_all

let spec_files ~only dir =
  let wanted name =
    String.ends_with ~suffix:".spec.txt" name
    &&
    match only with
    | [] ->
        true
    | ids ->
        List.exists
          (fun id ->
            name = id ^ ".spec.txt"
            || String.starts_with ~prefix:(id ^ ".") name
          )
          ids
  in
  List.filter wanted (List.sort compare (Array.to_list (Sys.readdir dir)))

let load_specs ~only dir =
  match spec_files ~only dir with
  | exception Sys_error message ->
      Error message
  | [] ->
      Error
        ("no golden spec (*.spec.txt) in " ^ dir
        ^ match only with [] -> "" | ids -> " for " ^ String.concat ", " ids
        )
  | files ->
      List.fold_left
        (fun specs file ->
          Result.bind specs (fun specs ->
              match Spec.of_text (read_file (Filename.concat dir file)) with
              | Ok spec ->
                  Ok (spec :: specs)
              | Error message ->
                  Error (Filename.concat dir file ^ ": " ^ message)
          )
        )
        (Ok []) files
      |> Result.map List.rev

let token text =
  if text = "" || String.contains text ' ' then
    "\"" ^ text ^ "\""
  else
    text

let step_of (record : Transition.t) =
  let on =
    match Transition.label record with
    | "" ->
        ""
    | event ->
        " on " ^ token event
  in
  match record.change with
  | Started ->
      "start in " ^ token record.target
  | Resumed ->
      "resume in " ^ token record.target
  | Ended ->
      "end in " ^ token record.target
  | Unhandled ->
      token record.from ^ " ignores" ^ on
  | Kept when record.from = record.target ->
      "stay " ^ token record.from ^ on
  | Kept | Entered ->
      token record.from ^ " -> " ^ token record.target ^ on

let offered specs (record : Transition.t) =
  let edges =
    List.concat_map
      (fun (spec : Spec.t) ->
        if spec.id = record.machine then
          List.filter
            (fun (label : Spec.label) -> label.from = record.from)
            spec.edges
        else
          []
      )
      specs
    |> List.sort_uniq compare
  in
  match edges with
  | [] ->
      ""
  | edges ->
      " The specs allow from " ^ token record.from ^ ": "
      ^ String.concat ", "
          (List.map
             (fun (label : Spec.label) ->
               ( if label.kept then
                   "stay"
                 else
                   "-> " ^ token label.target
               )
               ^
               match label.event with
               | "" ->
                   ""
               | event ->
                   " on " ^ token event
             )
             edges
          )
      ^ "."

let golden_hint ~build =
  "If the trace is from this build: run the golden check of the specs (dune \
   test, or main.exe golden DIR). When it reports this step as added, the spec \
   is out of date: review the golden diff and write it with --write. When it \
   does not, the app has behaviour the check does not explore: add samples, \
   inputs or props that reach it. If the trace is from another build (this one \
   is " ^ token build ^ "), check it with the specs of that build."

let code_agrees_hint =
  "The code of this build takes this step from the recorded snapshot, but no \
   spec has it. Run the golden check of the specs (dune test, or main.exe \
   golden DIR): when it reports this step as added, the spec is out of date: \
   review the golden diff and write it with --write. When it does not, the \
   check does not explore this behaviour: add samples, inputs or props that \
   reach it."

let other_build_hint ~build =
  "The trace is from another build (" ^ token build
  ^ "), or the update changed since it was recorded: check the trace with the \
     code and specs of that build. If it is this build, the update depends on \
     something outside the snapshot and the event: make it a function of them."

let not_decodable_hint ~build =
  "The types of the events or snapshots changed since the trace was recorded: \
   the trace is from another build (" ^ token build
  ^ "). Check it with that build, or record a new trace."

let no_codecs_hint machine =
  "This check suite registers no codecs for the machine " ^ machine
  ^ ", so it cannot decode the data of the trace. Register them in the codec \
     module of the app (Codecs.register in app/shared/codecs.ml) with \
     Machine.register_codecs, and call that module at the start of \
     check/main.ml, before Check_suite.main."

let no_spec_hint machine =
  "No check of this suite covers the machine " ^ machine
  ^ ": add a Check_suite entry for it, or list it with Check.child in a joint \
     check, and write its golden spec with main.exe golden DIR --write; or \
     name it with Check_suite.elsewhere when another suite checks it."

let property_hint ~property ~hint =
  "The app reached a snapshot that breaks \"" ^ property
  ^ "\". When silica check does not report it too, the check does not reach \
     this snapshot: add samples, inputs or props that reach it. Then fix it. "
  ^ hint

let conform_trace ~specs ~only ~subjects ~elsewhere file (trace : Trace.t) =
  let build = trace.header.build in
  let records = Hashtbl.create 64 in
  List.iter
    (fun (record : Transition.t) -> Hashtbl.replace records record.seq record)
    trace.transitions;
  let data =
    List.concat_map
      (fun (Subject check) ->
        let conformance = Check.conforms check trace in
        [ conformance ]
      )
      subjects
  in
  let deviations =
    List.concat_map
      (fun (conformance : Check.conformance) -> conformance.deviations)
      data
    |> List.sort_uniq (fun (first : Check.deviation) second ->
        compare
          ( first.seq,
            first.instance,
            Check.difference_to_string first.difference
          )
          ( second.seq,
            second.instance,
            Check.difference_to_string second.difference
          )
    )
  in
  let different seq instance =
    List.exists
      (fun (deviation : Check.deviation) ->
        deviation.seq = seq
        && deviation.instance = instance
        &&
        match deviation.difference with
        | Different_step _ | Not_decodable _ ->
            true
        | Property_fails _ ->
            false
      )
      deviations
  in
  let union field =
    List.sort_uniq compare
      (List.concat_map
         (fun (conformance : Check.conformance) -> field conformance)
         data
      )
  in
  let steps = union (fun conformance -> conformance.steps) in
  let snapshots = union (fun conformance -> conformance.snapshots) in
  let unchecked = union (fun conformance -> conformance.unchecked) in
  let stepped (record : Transition.t) = List.mem record.seq steps in
  let not_checked = ref [] in
  let skip machine reason =
    if not (List.mem_assoc machine !not_checked) then
      not_checked := (machine, reason) :: !not_checked
  in
  let named = ref 0 in
  let names =
    List.filter_map
      (fun (deviation : Spec.deviation) ->
        let record = Hashtbl.find records deviation.seq in
        match deviation.problem with
        | No_spec -> (
            match List.assoc_opt deviation.machine elsewhere with
            | Some reason ->
                skip deviation.machine ("checked elsewhere: " ^ reason);
                None
            | None when only <> [] ->
                skip deviation.machine
                  ("not in the check " ^ String.concat ", " only);
                None
            | None ->
                Some
                  {
                    seq = deviation.seq;
                    machine = deviation.machine;
                    instance = deviation.instance;
                    step = step_of record;
                    kind = "no-spec";
                    difference = "no golden spec has the machine";
                    hint = no_spec_hint deviation.machine;
                    view = "";
                  }
          )
        | Unknown_state _ | Unknown_edge _ ->
            let difference =
              ( match deviation.problem with
                | Unknown_state name ->
                    "no spec of the machine has the state " ^ token name
                | Unknown_edge _ | No_spec ->
                    "no spec of the machine has this step"
                )
              ^ "." ^ offered specs record
            in
            let hint =
              if different deviation.seq deviation.instance then
                other_build_hint ~build
              else if stepped record then
                code_agrees_hint
              else
                golden_hint ~build
            in
            Some
              {
                seq = deviation.seq;
                machine = deviation.machine;
                instance = deviation.instance;
                step = step_of record;
                kind =
                  ( match deviation.problem with
                  | Unknown_state _ ->
                      "unknown-state"
                  | Unknown_edge _ | No_spec ->
                      "unknown-step"
                  );
                difference;
                hint;
                view = "";
              }
      )
      (Spec.conforms specs trace)
  in
  List.iter
    (fun (record : Transition.t) ->
      if List.exists (fun (spec : Spec.t) -> spec.id = record.machine) specs
      then
        incr named
    )
    trace.transitions;
  let data_findings =
    List.map
      (fun (deviation : Check.deviation) ->
        let record = Hashtbl.find records deviation.seq in
        let kind, hint =
          match deviation.difference with
          | Different_step _ ->
              ("different-step", other_build_hint ~build)
          | Not_decodable _ ->
              ( "not-decodable",
                if List.mem deviation.machine (Machine.registered_codecs ())
                then
                  not_decodable_hint ~build
                else
                  no_codecs_hint deviation.machine
              )
          | Property_fails { property; hint; _ } ->
              ("property-fails", property_hint ~property ~hint)
        in
        {
          seq = deviation.seq;
          machine = deviation.machine;
          instance = deviation.instance;
          step = step_of record;
          kind;
          difference = Check.difference_to_string deviation.difference;
          hint;
          view =
            ( match deviation.difference with
            | Property_fails { view; _ } ->
                view
            | Different_step _ | Not_decodable _ ->
                ""
            );
        }
      )
      deviations
  in
  {
    file;
    trace;
    named = !named;
    steps = List.length steps;
    snapshots = List.length snapshots;
    not_checked = List.rev !not_checked;
    unchecked;
    findings =
      List.stable_sort
        (fun first second -> compare first.seq second.seq)
        (names @ data_findings);
  }

let counterexample_note (header : Trace.header) =
  match header.origin with
  | Checker { rule; _ } ->
      Some
        (Printf.sprintf
           "this trace is a counterexample of the checker for %s, not a \
            recorded run: conform compares its steps with the specs and the \
            code, and says nothing about the rule; run replay to see whether \
            it still breaks %s"
           rule rule
        )
  | Runtime _ ->
      None

let text checked =
  let buffer = Buffer.create 1024 in
  let add format = Printf.bprintf buffer format in
  let header = checked.trace.header in
  add "conform %s: root %s, build %s, %d transitions%s\n" checked.file
    header.root (token header.build)
    (List.length checked.trace.transitions)
    ( match header.dropped with
    | 0 ->
        ""
    | dropped ->
        Printf.sprintf " (%d dropped before)" dropped
    );
  Option.iter (fun note -> add "  note: %s\n" note) (counterexample_note header);
  add "  by names: transitions checked against the golden specs: %d\n"
    checked.named;
  add
    "  by data: steps run again in the code: %d; snapshots checked by the \
     properties: %d\n"
    checked.steps checked.snapshots;
  List.iter
    (fun (machine, reason) -> add "  not checked here: %s (%s)\n" machine reason)
    checked.not_checked;
  List.iter
    (fun (property, reason) ->
      add "  property not checked by conform: %s (%s)\n" (token property) reason
    )
    checked.unchecked;
  List.iteri
    (fun index finding ->
      add
        "deviation %d (%s): transition %d, machine %s, instance %s\n\
        \  step: %s\n\
        \  difference: %s\n\
        \  hint: %s\n"
        (index + 1) finding.kind finding.seq finding.machine finding.instance
        finding.step finding.difference finding.hint;
      if finding.view <> "" then (
        add "  view:\n";
        List.iter
          (fun line -> if line <> "" then add "    %s\n" line)
          (String.split_on_char '\n' finding.view)
      )
    )
    checked.findings;
  Buffer.contents buffer

let json_of checked =
  let header = checked.trace.header in
  Check_json.record
    [
      ("file", Check_json.string checked.file);
      ("root", Check_json.string header.root);
      ("build", Check_json.string header.build);
      ("transitions", Check_json.int (List.length checked.trace.transitions));
      ("dropped", Check_json.int header.dropped);
      ("note", Check_json.option Check_json.string (counterexample_note header));
      ("by_names", Check_json.int checked.named);
      ("steps", Check_json.int checked.steps);
      ("snapshots", Check_json.int checked.snapshots);
      ( "not_checked",
        Check_json.list
          (fun (machine, reason) ->
            Check_json.record
              [
                ("machine", Check_json.string machine);
                ("reason", Check_json.string reason);
              ]
          )
          checked.not_checked
      );
      ( "unchecked_properties",
        Check_json.list
          (fun (property, reason) ->
            Check_json.record
              [
                ("property", Check_json.string property);
                ("reason", Check_json.string reason);
              ]
          )
          checked.unchecked
      );
      ( "deviations",
        Check_json.list
          (fun finding ->
            Check_json.record
              [
                ("kind", Check_json.string finding.kind);
                ("seq", Check_json.int finding.seq);
                ("machine", Check_json.string finding.machine);
                ("instance", Check_json.string finding.instance);
                ("step", Check_json.string finding.step);
                ("difference", Check_json.string finding.difference);
                ("hint", Check_json.string finding.hint);
                ( "view",
                  if finding.view = "" then
                    Check_json.null
                  else
                    Check_json.string finding.view
                );
              ]
          )
          checked.findings
      );
    ]

let outcome = function
  | 0 ->
      "conforms"
  | 1 ->
      "deviations"
  | _ ->
      "setup error"

let run ~json ~spec_dir ~only ~subjects ~elsewhere files =
  let loaded =
    Result.bind (load_specs ~only spec_dir) (fun specs ->
        List.fold_left
          (fun traces file ->
            Result.bind traces (fun traces ->
                match read_file file with
                | exception Sys_error message ->
                    Error message
                | text -> (
                    match Trace.of_jsonl text with
                    | Ok trace ->
                        Ok ((file, trace) :: traces)
                    | Error message ->
                        Error (file ^ ": " ^ message)
                  )
            )
          )
          (Ok []) files
        |> Result.map (fun traces -> (specs, List.rev traces))
    )
  in
  match loaded with
  | Error message ->
      if json then
        print_string
          (Check_json.pretty
             (Check_json.record
                [
                  ("format", Check_json.string "silica-check");
                  ("version", Check_json.int 1);
                  ("command", Check_json.string "conform");
                  ("exit_code", Check_json.int 3);
                  ("outcome", Check_json.string (outcome 3));
                  ("error", Check_json.string message);
                ]
             )
          )
      else
        prerr_endline ("conform: " ^ message);
      3
  | Ok (specs, traces) ->
      let checked =
        List.map
          (fun (file, trace) ->
            conform_trace ~specs ~only ~subjects ~elsewhere file trace
          )
          traces
      in
      let code =
        if List.exists (fun checked -> checked.findings <> []) checked then
          1
        else
          0
      in
      if json then
        print_string
          (Check_json.pretty
             (Check_json.record
                [
                  ("format", Check_json.string "silica-check");
                  ("version", Check_json.int 1);
                  ("command", Check_json.string "conform");
                  ("exit_code", Check_json.int code);
                  ("outcome", Check_json.string (outcome code));
                  ("traces", Check_json.list json_of checked);
                ]
             )
          )
      else (
        List.iter (fun checked -> print_string (text checked)) checked;
        Printf.printf "%s (exit %d)\n"
          ( if code = 0 then
              "every trace conforms"
            else
              "a trace does not conform"
          )
          code
      );
      code
