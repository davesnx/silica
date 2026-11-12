module Json = Silica_json.Json

let null = Json.declassify `Null
let string text = Json.declassify (`String text)
let int value = Json.declassify (`Int value)
let bool value = Json.declassify (`Bool value)
let list encode items = Json.declassify (`List (List.map encode items))
let record fields = Json.declassify (`Assoc fields)
let option encode = function Some value -> encode value | None -> null

let pretty json =
  let buffer = Buffer.create 4096 in
  let indent depth = Buffer.add_string buffer (String.make (2 * depth) ' ') in
  let rec write depth json =
    match Json.classify json with
    | `List [] ->
        Buffer.add_string buffer "[]"
    | `Assoc [] ->
        Buffer.add_string buffer "{}"
    | `List items ->
        Buffer.add_string buffer "[\n";
        List.iteri
          (fun index item ->
            if index > 0 then Buffer.add_string buffer ",\n";
            indent (depth + 1);
            write (depth + 1) item
          )
          items;
        Buffer.add_char buffer '\n';
        indent depth;
        Buffer.add_char buffer ']'
    | `Assoc fields ->
        Buffer.add_string buffer "{\n";
        List.iteri
          (fun index (name, value) ->
            if index > 0 then Buffer.add_string buffer ",\n";
            indent (depth + 1);
            Buffer.add_string buffer (Json.to_string (string name));
            Buffer.add_string buffer ": ";
            write (depth + 1) value
          )
          fields;
        Buffer.add_char buffer '\n';
        indent depth;
        Buffer.add_char buffer '}'
    | `Null | `Bool _ | `Int _ | `Float _ | `String _ ->
        Buffer.add_string buffer (Json.to_string json)
  in
  write 0 json;
  Buffer.add_char buffer '\n';
  Buffer.contents buffer

let cause (cause : Check.cause) =
  let typed name fields = record (("type", string name) :: fields) in
  let fields =
    match cause with
    | View { handler; element; role; name } ->
        typed "view"
          [
            ("handler", string handler);
            ("element", string element);
            ("role", option string role);
            ("name", string name);
          ]
    | Input ->
        typed "input" []
    | Timer { id; ms } ->
        typed "timer" [ ("id", string id); ("ms", int ms) ]
    | Answer { id; path } ->
        typed "answer" [ ("id", string id); ("path", string path) ]
    | Live { id; topic } ->
        typed "live" [ ("id", string id); ("topic", string topic) ]
    | Held { id; source } ->
        typed "held" [ ("id", string id); ("source", string source) ]
    | Action_result action ->
        typed "action_result" [ ("action", string action) ]
    | Props ->
        typed "props" []
    | Child { instance; machine } ->
        typed "child"
          [ ("instance", string instance); ("machine", string machine) ]
    | Navigation ->
        typed "navigation" []
    | Address url ->
        typed "address" [ ("url", string url) ]
  in
  match Json.classify fields with
  | `Assoc fields ->
      record (fields @ [ ("text", string (Check.describe_cause cause)) ])
  | _ ->
      fields

let step check index (step : _ Check.step) =
  record
    [
      ("index", int index);
      ("instance", string step.instance);
      ("machine", string step.machine);
      ("from", string step.from);
      ("event", string step.event);
      ("data", option Fun.id (Check.input_json check step.input));
      ("cause", cause step.cause);
      ("target", string step.target);
      ( "root_moved",
        option
          (fun (from, target) ->
            record [ ("from", string from); ("target", string target) ]
          )
          step.root_moved
      );
    ]

let steps check ~first items =
  list Fun.id
    (List.mapi (fun index item -> step check (first + index) item) items)

let test_file trace_file =
  Filename.concat "test"
    (Filename.remove_extension (Filename.basename trace_file) ^ "_replay.ml")

let violation check ?trace_file (violation : (_, _) Check.violation) =
  let kind, trace, loop =
    match violation.counterexample with
    | Path trace ->
        ("path", trace, None)
    | Waits trace ->
        ("waits", trace, None)
    | Loop { stem; cycle } ->
        ("loop", stem, Some cycle)
  in
  record
    [
      ("rule", string violation.rule);
      ("message", string violation.message);
      ("hint", string violation.hint);
      ( "state",
        record
          [
            ("name", string violation.state_name);
            ("data", option Fun.id (Check.state_json check violation.state));
          ]
      );
      ("element", option string violation.element);
      ("view", string violation.view);
      ("kind", string kind);
      ("trace", steps check ~first:1 trace);
      ("loop", option (steps check ~first:(List.length trace + 1)) loop);
      ("trace_file", option string trace_file);
      ("test_file", option string (Option.map test_file trace_file));
    ]

let finding ~rule ~values ~state =
  let message, hint =
    Check_rules.render (Check_rules.finding rule ~values) ~state
  in
  (message, hint)

let report ?(trace_files = []) check (report : (_, _) Check.report) =
  record
    [
      ("id", string (Check.id check));
      ("machine", string report.machine);
      ( "machines",
        list
          (fun (machine, states) ->
            record [ ("machine", string machine); ("states", int states) ]
          )
          report.machines
      );
      ("passed", bool (Check.passed report));
      ("complete", bool report.complete);
      ("truncated", bool report.truncated);
      ("states", int report.states);
      ("transitions", int report.transitions);
      ( "violations",
        list Fun.id
          (List.mapi
             (fun index item ->
               violation check ?trace_file:(List.nth_opt trace_files index) item
             )
             report.violations
          )
      );
      ( "unreached",
        list
          (fun name ->
            let message, hint =
              finding ~rule:"unreached-state" ~values:[] ~state:name
            in
            record
              [
                ("state", string name);
                ("rule", string "unreached-state");
                ("message", string message);
                ("hint", string hint);
              ]
          )
          report.unreached
      );
      ( "unlisted",
        list
          (fun machine ->
            record
              [
                ("machine", string machine);
                ("message", string (Check_rules.unlisted_message machine));
              ]
          )
          report.unlisted
      );
      ( "state_limit",
        if report.complete then
          null
        else
          let message, hint =
            Check_rules.render
              {
                (Check_rules.finding "state-limit"
                   ~values:[ ("states", string_of_int report.states) ]
                )
                with
                custom =
                  Some
                    ( "The search stopped at {states} states, so the check is \
                       incomplete.",
                      report.bounds
                    );
              }
              ~state:""
          in
          record
            [
              ("rule", string "state-limit");
              ("message", string message);
              ("hint", string hint);
            ]
      );
      ( "allowed",
        list
          (fun ((allowance : Check.allowance), count) ->
            record
              [
                ("rule", string allowance.rule);
                ("state", option string allowance.state);
                ("element", option string allowance.element);
                ("reason", string allowance.reason);
                ("count", int count);
              ]
          )
          report.allowed
      );
      ("not_explored", list string report.not_explored);
      ( "vacuous",
        list
          (fun (property, reason) ->
            record [ ("property", string property); ("reason", string reason) ]
          )
          report.vacuous
      );
    ]

let rule (rule : Check.rule) =
  record
    [
      ("id", string rule.id);
      ("message", string rule.message);
      ("hint", string rule.hint);
      ("applies_to", string rule.applies_to);
    ]

let failure (failure : Check.failure) =
  record
    [
      ("rule", string failure.rule);
      ("message", string failure.message);
      ("hint", string failure.hint);
      ("element", option string failure.element);
    ]

let replayed check index (point : (_, _) Check.replayed_step) =
  record
    [
      ("index", int index);
      ("step", option (step check index) point.step);
      ( "state",
        record
          [
            ("name", string point.state_name);
            ("data", option Fun.id (Check.state_json check point.state));
          ]
      );
      ("view", string point.view);
      ("actions", list string point.actions);
      ("failing", list failure point.failing);
    ]
