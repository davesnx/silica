open Silica

exception Invalid_check = Check_model.Invalid_check

type ('s, 'e) context = {
  snapshot : 's;
  view : 'e Node.t;
  event : 'e option;
  name : string;
}

type ('s, 'e) condition = ('s, 'e) context -> bool

let state test context = test context.snapshot
let state_named name context = context.name = name
let view test context = test context.view
let holds test context = test context.snapshot context.view

let event test context =
  match context.event with Some event -> test event | None -> false

let ( &&& ) first second context = first context && second context
let ( ||| ) first second context = first context || second context
let not_ condition context = not (condition context)

type ('s, 'e) property =
  | Always of {
      name : string;
      hint : string option;
      condition : ('s, 'e) condition;
    }

let always ~name ?hint condition = Always { name; hint; condition }

type allowance = Check_rules.allowance = private {
  rule : string;
  state : string option;
  element : string option;
  reason : string;
}

let allow ~rule ?state ?element ~reason () : allowance =
  { Check_rules.rule; state; element; reason }

type sample = Check_model.sample

let answers = Check_model.answers
let states = Check_model.states

type ('p, 'e) input = ('p, 'e) Check_model.input = Event of 'e | Props of 'p

type 's app_state = 's Check_model.app_state = {
  router : 's;
  history : Url.t list;
  index : int;
}

type 'e app_event = 'e Check_model.app_event =
  | Event of 'e
  | Link of Url.t
  | Browser_back
  | Browser_forward

type ('s, 'ev) t =
  | Check : {
      id : string Lazy.t;
      build : unit -> ('n, 's, 'ev, 'e) Check_model.model;
      max_states : int;
      max_violations : int;
      within : 's -> bool;
      properties : ('s, 'e) property list;
      allow : allowance list;
      state_json : 's -> Silica_json.Json.t option;
      input_json : 'ev -> Silica_json.Json.t option;
    }
      -> ('s, 'ev) t

let id (Check check) = Lazy.force check.id
let state_json (Check check) state = check.state_json state
let input_json (Check check) input = check.input_json input

let encode codec value =
  match codec with
  | Ok (codec : _ Codec.t) ->
      Some (codec.encode value)
  | Error _ ->
      None

let component_json machine =
  ( (fun state -> encode (Machine.state_codec machine) state),
    function
    | (Event event : (_, _) input) ->
        encode (Machine.event_codec machine) event
    | Props _ ->
        None
  )

let component ?(max_states = 100_000) ?(max_violations = 50)
    ?(within = fun _ -> true) ?(inputs = fun _ -> []) ?(props = [])
    ?(samples = []) ?(arrivals = 2) ?(keys = Check_rules.default_keys)
    ?(properties = []) ?(allow = []) (component : (_, _, _) Component.t) =
  Check
    {
      id = lazy (Machine.id component.machine);
      build =
        (fun () ->
          Check_model.component_model ~client_only:false ~keys ~inputs ~props
            ~samples ~arrivals component
        );
      max_states;
      max_violations;
      within;
      properties;
      allow;
      state_json = fst (component_json component.machine);
      input_json = snd (component_json component.machine);
    }

let live ?(max_states = 100_000) ?(max_violations = 50)
    ?(within = fun _ -> true) ?(inputs = fun _ -> []) ?(samples = [])
    ?(arrivals = 2) ?(keys = Check_rules.default_keys) ?(properties = [])
    ?(allow = []) (live : (_, _) Live.t) =
  Check
    {
      id = lazy (Machine.id live.component.machine);
      build =
        (fun () ->
          Check_model.component_model ~client_only:true ~keys ~inputs ~props:[]
            ~samples ~arrivals live.component
        );
      max_states;
      max_violations;
      within;
      properties;
      allow;
      state_json = fst (component_json live.component.machine);
      input_json = snd (component_json live.component.machine);
    }

let root_machine ~routes ~print root =
  match routes with
  | [] ->
      None
  | first :: _ ->
      Some (root (print first)).Component.machine

let app_state_json ~routes ~print root (state : _ app_state) =
  Option.bind (root_machine ~routes ~print root) (fun machine ->
      Option.map
        (fun router ->
          Silica_json.Json.declassify
            (`Assoc
               [
                 ("router", router);
                 ( "history",
                   Silica_json.Json.declassify
                     (`List
                        (List.map
                           (fun url ->
                             Silica_json.Json.declassify
                               (`String (Url.to_string url))
                           )
                           state.history
                        )
                     )
                 );
                 ("index", Silica_json.Json.declassify (`Int state.index));
               ]
            )
        )
        (encode (Machine.state_codec machine) state.router)
  )

let app_input_json ~routes ~print root = function
  | (Event event : _ app_event) ->
      Option.bind (root_machine ~routes ~print root) (fun machine ->
          encode (Machine.event_codec machine) event
      )
  | Link _ | Browser_back | Browser_forward ->
      None

let app ?(max_states = 100_000) ?(max_violations = 50) ?(within = fun _ -> true)
    ?(max_history = 4) ?(inputs = fun _ -> []) ?(samples = []) ?(arrivals = 2)
    ?(keys = Check_rules.default_keys) ?(properties = []) ?(allow = []) ~routes
    ~parse ~print ?is_not_found ~url_event root =
  Check
    {
      id =
        lazy
          ( match routes with
          | [] ->
              raise
                (Invalid_check "Check.app needs at least one route in ~routes")
          | first :: _ ->
              Machine.id (root (print first)).Component.machine
          );
      build =
        (fun () ->
          Check_model.app_model ~keys ~max_history ~inputs ~samples ~arrivals
            ~routes ~parse ~print ?is_not_found ~url_event root
        );
      max_states;
      max_violations;
      within;
      properties;
      allow;
      state_json = app_state_json ~routes ~print root;
      input_json = app_input_json ~routes ~print root;
    }

type cause = Check_model.cause =
  | View of {
      handler : string;
      element : string;
      role : string option;
      name : string;
    }
  | Input
  | Timer of { id : string; ms : int }
  | Answer of { id : string; path : string }
  | Live of { id : string; topic : string }
  | Held of { id : string; source : string }
  | Action_result of string
  | Props
  | Child of { instance : string; machine : string }
  | Navigation

let describe_cause = Check_model.describe_cause

type 'ev step = {
  input : 'ev;
  event : string;
  cause : cause;
  instance : string;
  machine : string;
  from : string;
  target : string;
}

type 'ev counterexample =
  | Path of 'ev step list
  | Waits of 'ev step list
  | Loop of { stem : 'ev step list; cycle : 'ev step list }

type ('s, 'ev) violation = {
  rule : string;
  message : string;
  hint : string;
  counterexample : 'ev counterexample;
  state : 's;
  state_name : string;
  view : string;
  element : string option;
}

type children = Check_model.children

let props = Check_model.props

type ('s, 'ev) report = {
  machine : string;
  states : int;
  transitions : int;
  complete : bool;
  truncated : bool;
  violations : ('s, 'ev) violation list;
  unreached : string list;
  allowed : (allowance * int) list;
  not_explored : string list;
  children : children;
  specs : Spec.t list;
  paths : (string * 'ev step list) list;
  views : (string * string) list;
}

type rule = Check_rules.rule = {
  id : string;
  message : string;
  hint : string;
  applies_to : string;
}

let rules = Check_rules.rules

let validate ~properties allow =
  let names = List.map (fun (Always { name; _ }) -> name) properties in
  List.iter
    (fun (allowance : allowance) ->
      if String.trim allowance.reason = "" then
        raise
          (Invalid_check
             ("the allowance of the rule " ^ allowance.rule
            ^ " has no reason: say why the rule does not apply"
             )
          );
      if
        not
          (List.exists (fun (rule : rule) -> rule.id = allowance.rule) rules
          || List.mem allowance.rule names
          )
      then
        raise
          (Invalid_check
             ("the allowance names the rule " ^ allowance.rule
            ^ ", which is neither a built-in rule (see Check.rules) nor a \
               property of the check"
             )
          )
    )
    allow

let property_rule (type n e ev) (model : (n, _, ev, e) Check_model.model)
    (Always { name; hint; condition }) : (n, e, ev) Check_search.rule =
  ( "property \"" ^ name ^ "\"",
    fun node view _ event ->
      if
        condition
          {
            snapshot = model.snapshot node;
            view;
            event = Option.bind event model.event_of;
            name = model.name node;
          }
      then
        []
      else
        [
          {
            Check_rules.rule = name;
            element = None;
            values = [ ("property", name) ];
            custom =
              Some
                ( Check_rules.property_message,
                  Option.value hint ~default:Check_rules.property_hint
                );
          };
        ]
  )

let run (type s ev) (Check check : (s, ev) t) : (s, ev) report =
  validate ~properties:check.properties check.allow;
  let model = check.build () in
  let dead_end node view moves _ =
    if
      model.final node
      || Check_rules.not_explored_of_view view <> []
      || moves <> []
    then
      []
    else
      [ Check_rules.finding "dead-end" ]
  in
  let rules =
    [
      ("view", fun _ view _ _ -> Check_rules.view_rules view);
      ("view", fun _ view _ _ -> Check_accessibility.view_rules view);
      ("view", dead_end);
      ("machine", fun node view _ _ -> model.state_rules node view);
    ]
    @ List.map (property_rule model) check.properties
  in
  let result =
    Check_search.run ~model ~rules ~max_states:check.max_states
      ~max_violations:check.max_violations
      ~within:(fun node -> check.within node)
      ~allow:check.allow
  in
  let step_of parent (move : _ Check_model.move) target =
    let node = Dynarray.get result.nodes parent in
    {
      input = move.event;
      event = model.event_name move.event;
      cause = model.cause node (model.view node) move;
      instance = "0";
      machine = model.machine;
      from = model.name node;
      target;
    }
  in
  let path index =
    List.map
      (fun (parent, move, child) ->
        step_of parent move (model.name (Dynarray.get result.nodes child))
      )
      (Check_search.path result.parents index)
  in
  let violations =
    List.map
      (fun (found : (s, ev) Check_search.found) ->
        let prefix =
          match found.prefix with Some index -> path index | None -> []
        in
        let last =
          match (found.prefix, found.last) with
          | Some index, Some (move, target) ->
              [ step_of index move target ]
          | _ ->
              []
        in
        {
          rule = found.rule;
          message = found.message;
          hint = found.hint;
          counterexample = Path (prefix @ last);
          state = found.state;
          state_name = found.state_name;
          view = found.view;
          element = found.element;
        }
      )
      result.violations
  in
  let unreached, allowed =
    List.fold_left
      (fun (unreached, allowed) name ->
        let rec find index = function
          | [] ->
              None
          | first :: rest ->
              if
                Check_rules.allows first ~rule:"unreached-state" ~state:name
                  ~element:None
              then
                Some index
              else
                find (index + 1) rest
        in
        match find 0 check.allow with
        | Some index ->
            ( unreached,
              List.mapi
                (fun position (allowance, count) ->
                  if position = index then
                    (allowance, count + 1)
                  else
                    (allowance, count)
                )
                allowed
            )
        | None ->
            (name :: unreached, allowed)
      )
      ([], result.allowed) (model.unreached ())
  in
  {
    machine = model.machine;
    states = Dynarray.length result.nodes;
    transitions = result.transitions;
    complete = result.complete;
    truncated = result.truncated;
    violations;
    unreached = List.rev unreached;
    allowed;
    not_explored = result.not_explored;
    children = result.children;
    specs = model.specs ();
    paths = List.map (fun (name, index) -> (name, path index)) result.firsts;
    views = result.views;
  }

let passed report =
  report.violations = [] && report.unreached = [] && report.complete

let steps = function
  | Path steps | Waits steps ->
      steps
  | Loop { stem; cycle } ->
      stem @ cycle

let pp_lines formatter text =
  List.iter
    (fun line -> if line <> "" then Format.fprintf formatter "@,    %s" line)
    (String.split_on_char '\n' text)

let pp_step formatter index step =
  Format.fprintf formatter "@,  %d. %s -> %s: %s, by %s" (index + 1) step.from
    step.target
    (match step.event with "" -> "(unnamed event)" | name -> name)
    (describe_cause step.cause)

let pp_report formatter report =
  Format.fprintf formatter "@[<v>%s: %d states, %d transitions" report.machine
    report.states report.transitions;
  ( if not report.complete then
      let message, hint =
        Check_rules.render
          (Check_rules.finding "state-limit"
             ~values:[ ("states", string_of_int report.states) ]
          )
          ~state:""
      in
      Format.fprintf formatter "@,state-limit: %s@,  hint: %s" message hint
  );
  if report.truncated then
    Format.fprintf formatter
      "@,\
       truncated: the search stopped after %d violations; fix them, or raise \
       ~max_violations to see more"
      (List.length report.violations);
  List.iteri
    (fun index violation ->
      Format.fprintf formatter "@,violation %d: %s@,  %s@,  hint: %s"
        (index + 1) violation.rule violation.message violation.hint;
      ( match violation.counterexample with
      | Path steps | Waits steps ->
          List.iteri (pp_step formatter) steps
      | Loop { stem; cycle } ->
          List.iteri (pp_step formatter) stem;
          Format.fprintf formatter "@,  then forever:";
          List.iteri (pp_step formatter) cycle
      );
      Format.fprintf formatter "@,  state: %s" violation.state_name;
      if violation.view <> "" then (
        Format.fprintf formatter "@,  view:";
        pp_lines formatter violation.view
      )
    )
    report.violations;
  List.iter
    (fun name ->
      let message, hint =
        Check_rules.render (Check_rules.finding "unreached-state") ~state:name
      in
      Format.fprintf formatter "@,unreached-state: %s@,  hint: %s" message hint
    )
    report.unreached;
  List.iter
    (fun ((allowance : allowance), count) ->
      Format.fprintf formatter "@,allowed %s%s%s (%d): %s" allowance.rule
        (match allowance.state with Some name -> " in " ^ name | None -> "")
        (match allowance.element with Some path -> " at " ^ path | None -> "")
        count allowance.reason
    )
    report.allowed;
  List.iter
    (fun entry -> Format.fprintf formatter "@,not explored: %s" entry)
    report.not_explored;
  Format.fprintf formatter "@]"
