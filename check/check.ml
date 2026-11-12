open Silica

exception Invalid_check = Check_model.Invalid_check

type ('s, 'e) context = {
  snapshot : 's;
  view : 'e Node.t Lazy.t;
  event : 'e option;
  name : string;
  from : string option;
  before : 's option;
  actions : 'e Action.host list Lazy.t;
}

type ('s, 'e) condition = {
  test : ('s, 'e) context -> bool;
  reads_step : bool;
  negated : ('s, 'e) condition option;
  guard : ('s, 'e) condition option;
}

let of_point test = { test; reads_step = false; negated = None; guard = None }
let of_step test = { test; reads_step = true; negated = None; guard = None }
let state test = of_point (fun context -> test context.snapshot)
let state_named name = of_point (fun context -> context.name = name)
let view test = of_point (fun context -> test (Lazy.force context.view))

let view_shows = View_text.shows
let shows text = view (fun view -> view_shows view text)

let holds test =
  of_point (fun context -> test context.snapshot (Lazy.force context.view))

let event test =
  of_step (fun context ->
      match context.event with Some event -> test event | None -> false
  )

let action test =
  of_step (fun context -> List.exists test (Lazy.force context.actions))

let entered name =
  of_step (fun context ->
      context.name = name
      && match context.from with Some from -> from <> name | None -> false
  )

let both first second ~test =
  {
    test;
    reads_step = first.reads_step || second.reads_step;
    negated = None;
    guard = None;
  }

let transition test =
  of_step (fun context ->
      match (context.before, context.event) with
      | Some before, Some event ->
          test before event context.snapshot
      | _ ->
          false
  )

let ( &&& ) first second =
  both first second ~test:(fun context ->
      first.test context && second.test context
  )

let ( ||| ) first second =
  {
    (both first second ~test:(fun context ->
         first.test context || second.test context
     )
    )
    with
    guard =
      ( match (first.negated, second.negated) with
      | Some guard, _ | None, Some guard ->
          Some guard
      | None, None ->
          None
      );
  }

let not_ condition =
  {
    condition with
    test = (fun context -> not (condition.test context));
    negated = Some condition;
    guard = None;
  }

type ('s, 'e) property =
  | Always of {
      name : string;
      hint : string option;
      condition : ('s, 'e) condition;
    }
  | Never_after of {
      name : string;
      hint : string option;
      after : ('s, 'e) condition;
      never : ('s, 'e) condition;
    }
  | Leads_to of {
      name : string;
      hint : string option;
      from : ('s, 'e) condition;
      to_ : ('s, 'e) condition;
    }

let always ~name ?hint condition = Always { name; hint; condition }

let never_after ~name ?hint ~after ~never () =
  Never_after { name; hint; after; never }

let leads_to ~name ?hint ~from ~to_ () = Leads_to { name; hint; from; to_ }

let property_name = function
  | Always { name; _ } | Never_after { name; _ } | Leads_to { name; _ } ->
      name

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

type child_event = Check_model.child_event

type ('p, 'e) input = ('p, 'e) Check_model.input =
  | Event of 'e
  | Props of 'p
  | In_child of child_event

type 's app_state = 's Check_model.app_state = {
  router : 's;
  history : Url.t list;
  index : int;
}

let current_url (app : _ app_state) = List.nth app.history app.index

type 'e app_event = 'e Check_model.app_event =
  | Event of 'e
  | Link of Url.t
  | Open of Url.t
  | Browser_back
  | Browser_forward
  | In_child of child_event

let property_finding ~name ~hint ~condition context =
  if condition.test context then
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

type child = Check_model.child

let child ?(samples = []) ?(inputs = fun _ -> []) ?(texts = [])
    ?(within = fun _ -> true) ?(max_instances = 2) ?(properties = [])
    (component : (_, _, _) Component.t) =
  if max_instances < 1 then
    raise
      (Invalid_check
         ("the child "
         ^ Machine.id component.machine
         ^ " has max_instances below 1: give at least 1"
         )
      );
  let always =
    List.map
      (function
        | Always { name; hint; condition } when not condition.reads_step ->
            (name, hint, condition)
        | Always { name; _ } ->
            raise
              (Invalid_check
                 ("the property " ^ name ^ " of the child "
                 ^ Machine.id component.machine
                 ^ " reads the step (Check.event, Check.action or \
                    Check.entered), and a child property is read only on the \
                    first visit of each joint snapshot; put it on the check of \
                    the child alone, where Check.always reads it on every step"
                 )
              )
        | Never_after { name; _ } | Leads_to { name; _ } ->
            raise
              (Invalid_check
                 ("the property " ^ name ^ " of the child "
                 ^ Machine.id component.machine
                 ^ " is temporal: a child takes only Check.always; put \
                    leads_to and never_after on the check of the parent or of \
                    the child alone"
                 )
              )
        )
      properties
  in
  Check_model.Child
    {
      component;
      samples;
      inputs;
      texts;
      within;
      max_instances;
      names = List.map property_name properties;
      check =
        (fun ~snapshot ~view ~event ~name ->
          List.concat_map
            (fun (property, hint, condition) ->
              property_finding ~name:property ~hint ~condition
                {
                  snapshot;
                  view = Lazy.from_val view;
                  event;
                  name;
                  from = None;
                  before = None;
                  actions = Lazy.from_val [];
                }
            )
            always
        );
    }

let lift_property own_of own_actions property =
  let rec lift condition =
    {
      reads_step = condition.reads_step;
      negated = Option.map lift condition.negated;
      guard = Option.map lift condition.guard;
      test =
        (fun context ->
          condition.test
            {
              snapshot = context.snapshot;
              view = lazy (Check_tree.restrict own_of (Lazy.force context.view));
              event = Option.bind context.event own_of;
              name = context.name;
              from = context.from;
              before = context.before;
              actions = lazy (!own_actions (Lazy.force context.actions));
            }
        );
    }
  in
  match property with
  | Always { name; hint; condition } ->
      Always { name; hint; condition = lift condition }
  | Never_after { name; hint; after; never } ->
      Never_after { name; hint; after = lift after; never = lift never }
  | Leads_to { name; hint; from; to_ } ->
      Leads_to { name; hint; from = lift from; to_ = lift to_ }

let joint_actions () =
  ref (fun _ -> invalid_arg "Check: the joint model is not built yet")

let child_names children =
  List.concat_map (fun (Check_model.Child listed) -> listed.names) children

let joint_id root children =
  String.concat "+"
    (root
    :: List.map
         (fun (Check_model.Child listed) -> Machine.id listed.component.machine)
         children
    )

let listed_ids children =
  List.map
    (fun (Check_model.Child listed) -> Machine.id listed.component.machine)
    children

let child_json children (Check_model.Child_event { witness; event; _ }) =
  List.find_map
    (fun (Check_model.Child listed) ->
      match
        Type.Id.provably_equal witness (Machine.type_id listed.component.machine)
      with
      | Some Type.Equal -> (
          match Machine.event_codec listed.component.machine with
          | Ok (codec : _ Codec.t) ->
              Some (codec.encode event)
          | Error _ ->
              None
        )
      | None ->
          None
    )
    children

type 's root =
  | Root : {
      component : url:string option -> ('p, 'm, 'e) Component.t;
      lift : 'm -> url:string option -> 's;
      properties : ('s, 'e) property list;
      routed : bool;
    }
      -> 's root

type ('s, 'ev) t =
  | Check : {
      id : string Lazy.t;
      root : string Lazy.t;
      listed : string list;
      build : unit -> ('n, 's, 'ev, 'e) Check_model.model;
      max_states : int;
      max_violations : int;
      within : 's -> bool;
      properties : ('s, 'e) property list;
      names : string list;
      allow : allowance list;
      state_json : 's -> Silica_json.Json.t option;
      input_json : 'ev -> Silica_json.Json.t option;
      machine_json : 's -> Silica_json.Json.t option;
      input_of : Transition.t -> ('ev option, string) result;
      url : 's -> string option;
      subject : subject;
      starts : unit -> 'ev list;
      root_of : 's root;
      children : Check_model.child list;
    }
      -> ('s, 'ev) t

and subject = Plain | With_props | Routed

let id (Check check) = Lazy.force check.id
let root (Check check) = Lazy.force check.root
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
    | Props _ | In_child _ ->
        None
  )

let decode_at (record : Transition.t) codec json =
  Result.map_error
    (fun error -> Printf.sprintf "transition %d: %s" record.seq error)
    (Result.bind codec (fun (codec : _ Codec.t) -> codec.decode json))

let component_input_of machine (record : Transition.t) =
  match (record.cause, record.props, record.data) with
  | Props, Some json, _ ->
      Result.map
        (fun props -> Some (Props props : (_, _) input))
        (decode_at record (Machine.props_codec machine) json)
  | Props, None, _ ->
      Ok None
  | _, _, Some json ->
      Result.map
        (fun event -> Some (Event event : (_, _) input))
        (decode_at record (Machine.event_codec machine) json)
  | _, _, None ->
      Ok None

let joint_input_of ~into children own_input_of (record : Transition.t) =
  match
    List.find_opt
      (fun (Check_model.Child listed) ->
        Machine.id listed.component.machine = record.machine
      )
      children
  with
  | Some (Check_model.Child listed) -> (
      let machine = listed.component.machine in
      match record.data with
      | Some json ->
          Result.map
            (fun event ->
              Some
                (into
                   (Check_model.Child_event
                      {
                        instance = record.instance;
                        machine = record.machine;
                        witness = Machine.type_id machine;
                        event;
                      }
                   )
                )
            )
            (decode_at record (Machine.event_codec machine) json)
      | None ->
          Ok None
    )
  | None ->
      own_input_of record

let subject_of machine =
  match Machine.default_props machine with
  | Some _ ->
      Plain
  | None ->
      With_props

let component ?(max_states = 100_000) ?(max_violations = 50)
    ?(within = fun _ -> true) ?(inputs = fun _ -> []) ?(props = [])
    ?(samples = []) ?(arrivals = 2) ?(keys = Check_rules.default_keys)
    ?(texts = []) ?(children = []) ?(properties = []) ?(allow = [])
    (component : (_, _, _) Component.t) =
  let flat () =
    Check_model.component_model ~client_only:false ~keys ~texts ~inputs ~props
      ~samples ~arrivals component
  in
  let own_of : (_, _) input -> _ = function
    | Event event ->
        Some event
    | Props _ | In_child _ ->
        None
  in
  let state_json, input_json = component_json component.machine in
  let root = Machine.id component.machine in
  match children with
  | [] ->
      Check
        {
          id = lazy root;
          root = lazy root;
          listed = [];
          build = flat;
          max_states;
          max_violations;
          within;
          properties;
          names = [];
          allow;
          state_json;
          input_json;
          machine_json = state_json;
          input_of = component_input_of component.machine;
          url = (fun _ -> None);
          subject = subject_of component.machine;
          starts =
            (fun () ->
              List.map
                (fun props -> (Props props : (_, _) input))
                (Check_model.start_props component.machine props)
            );
          root_of =
            Root
              {
                component = (fun ~url:_ -> component);
                lift = (fun snapshot ~url:_ -> snapshot);
                properties;
                routed = false;
              };
          children = [];
        }
  | children ->
      let own_actions = joint_actions () in
      let into child_event : (_, _) input = In_child child_event in
      Check
        {
          id = lazy (joint_id root children);
          root = lazy root;
          listed = listed_ids children;
          build =
            (fun () ->
              Check_tree.joint ~own_actions ~keys ~texts ~arrivals
                ~own:(fun event : (_, _) input -> Event event)
                ~own_of ~into
                ~out_of:(function
                  | (In_child child_event : (_, _) input) ->
                      Some child_event
                  | Event _ | Props _ ->
                      None
                  )
                ~children (flat ())
            );
          max_states;
          max_violations;
          within;
          properties = List.map (lift_property own_of own_actions) properties;
          names = child_names children;
          allow;
          state_json;
          input_json =
            (function
            | (In_child child_event : (_, _) input) ->
                child_json children child_event
            | input ->
                input_json input
            );
          machine_json = state_json;
          input_of =
            joint_input_of ~into children (component_input_of component.machine);
          url = (fun _ -> None);
          subject = subject_of component.machine;
          starts =
            (fun () ->
              List.map
                (fun props -> (Props props : (_, _) input))
                (Check_model.start_props component.machine props)
            );
          root_of =
            Root
              {
                component = (fun ~url:_ -> component);
                lift = (fun snapshot ~url:_ -> snapshot);
                properties;
                routed = false;
              };
          children;
        }

let live ?(max_states = 100_000) ?(max_violations = 50)
    ?(within = fun _ -> true) ?(inputs = fun _ -> []) ?(samples = [])
    ?(arrivals = 2) ?(keys = Check_rules.default_keys) ?(texts = [])
    ?(properties = []) ?(allow = []) (live : (_, _) Live.t) =
  Check
    {
      id = lazy (Machine.id live.component.machine);
      root = lazy (Machine.id live.component.machine);
      listed = [];
      build =
        (fun () ->
          Check_model.component_model ~client_only:true ~keys ~texts ~inputs
            ~props:[] ~samples ~arrivals live.component
        );
      max_states;
      max_violations;
      within;
      properties;
      names = [];
      allow;
      state_json = fst (component_json live.component.machine);
      input_json = snd (component_json live.component.machine);
      machine_json = fst (component_json live.component.machine);
      input_of = component_input_of live.component.machine;
      url = (fun _ -> None);
      subject = Plain;
      starts = (fun () -> [ (Props () : (_, _) input) ]);
      root_of =
        Root
          {
            component = (fun ~url:_ -> live.component);
            lift = (fun snapshot ~url:_ -> snapshot);
            properties;
            routed = false;
          };
      children = [];
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
  | Link _ | Open _ | Browser_back | Browser_forward | In_child _ ->
      None

let app_input_of ~routes ~print root (record : Transition.t) =
  match record.data with
  | Some json -> (
      match root_machine ~routes ~print root with
      | Some machine ->
          Result.map
            (fun event -> Some (Event event : _ app_event))
            (decode_at record (Machine.event_codec machine) json)
      | None ->
          Ok None
    )
  | None -> (
      match record.event with
      | "back" ->
          Ok (Some (Browser_back : _ app_event))
      | "forward" ->
          Ok (Some Browser_forward)
      | event when String.starts_with ~prefix:"open " event ->
          Ok
            (Some
               (Open
                  (Url.of_string (String.sub event 5 (String.length event - 5)))
               )
            )
      | event when String.starts_with ~prefix:"link " event ->
          Ok
            (Some
               (Link
                  (Url.of_string (String.sub event 5 (String.length event - 5)))
               )
            )
      | _ ->
          Ok None
    )

let app_root ~routes ~print ~properties root =
  let url_of url =
    match (url, routes) with
    | Some url, _ ->
        Url.of_string url
    | None, first :: _ ->
        print first
    | None, [] ->
        raise (Invalid_check "Check.app needs at least one route in ~routes")
  in
  Root
    {
      component = (fun ~url -> root (url_of url));
      lift = (fun router ~url -> { router; history = [ url_of url ]; index = 0 });
      properties;
      routed = true;
    }

let app ?(max_states = 100_000) ?(max_violations = 50) ?(within = fun _ -> true)
    ?(max_history = 4) ?(inputs = fun _ -> []) ?(samples = []) ?(arrivals = 2)
    ?(keys = Check_rules.default_keys) ?(texts = []) ?(urls = []) ?(pages = [])
    ?(properties = []) ?(allow = []) ~routes ~parse ~print ?is_not_found
    ~url_event root =
  let id =
    lazy
      ( match routes with
      | [] ->
          raise (Invalid_check "Check.app needs at least one route in ~routes")
      | first :: _ ->
          Machine.id (root (print first)).Component.machine
      )
  in
  let flat () =
    Check_model.app_model ~keys ~texts ~urls ~max_history ~inputs ~samples
      ~arrivals ~routes ~parse ~print ?is_not_found ~url_event root
  in
  let own_of : _ app_event -> _ = function
    | Event event ->
        Some event
    | Link _ | Open _ | Browser_back | Browser_forward | In_child _ ->
        None
  in
  let state_json = app_state_json ~routes ~print root
  and input_json = app_input_json ~routes ~print root
  and machine_json (state : _ app_state) =
    Option.bind (root_machine ~routes ~print root) (fun machine ->
        encode (Machine.state_codec machine) state.router
    )
  and url (state : _ app_state) =
    Option.map Url.to_string (List.nth_opt state.history state.index)
  in
  match pages with
  | [] ->
      Check
        {
          id;
          root = id;
          listed = [];
          build = flat;
          max_states;
          max_violations;
          within;
          properties;
          names = [];
          allow;
          state_json;
          input_json;
          machine_json;
          input_of = app_input_of ~routes ~print root;
          url;
          subject = Routed;
          starts = (fun () -> []);
          root_of = app_root ~routes ~print ~properties root;
          children = [];
        }
  | pages ->
      let own_actions = joint_actions () in
      let into child_event : _ app_event = In_child child_event in
      Check
        {
          id = lazy (joint_id (Lazy.force id) pages);
          root = id;
          listed = listed_ids pages;
          build =
            (fun () ->
              Check_tree.joint ~own_actions ~keys ~texts ~arrivals
                ~own:(fun event -> Event event)
                ~own_of ~into
                ~out_of:(function
                  | In_child child_event ->
                      Some child_event
                  | Event _ | Link _ | Open _ | Browser_back | Browser_forward
                    ->
                      None
                  )
                ~link:(fun href -> Link (Url.of_string href))
                ~children:pages (flat ())
            );
          max_states;
          max_violations;
          within;
          properties = List.map (lift_property own_of own_actions) properties;
          names = child_names pages;
          allow;
          state_json;
          input_json =
            (function
            | (In_child child_event : _ app_event) ->
                child_json pages child_event
            | input ->
                input_json input
            );
          machine_json;
          input_of =
            joint_input_of ~into pages (app_input_of ~routes ~print root);
          url;
          subject = Routed;
          starts = (fun () -> []);
          root_of = app_root ~routes ~print ~properties root;
          children = pages;
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
  | Address of string

let describe_cause = Check_model.describe_cause

type 'ev step = {
  input : 'ev;
  event : string;
  cause : cause;
  instance : string;
  machine : string;
  from : string;
  target : string;
  root_moved : (string * string) option;
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

let views_per_state = 16

let component_bounds =
  "Bound the search with ~within (a test of the snapshot), or raise \
   ~max_states."

let app_bounds =
  "Lower ~max_history (default 4), which bounds how many history entries a \
   state keeps: each push of another URL makes new states. Bound the search \
   with ~within, which reads the Check.app_state: the route (router), the \
   history and its index; bound a page with the ~within of its Check.child. Or \
   raise ~max_states."

type 'ev state_views = {
  state : string;
  views : (string * 'ev step list) list;
  more : int;
}

type ('s, 'ev) report = {
  machine : string;
  states : int;
  transitions : int;
  complete : bool;
  truncated : bool;
  violations : ('s, 'ev) violation list;
  unreached : string list;
  unlisted : string list;
  allowed : (allowance * int) list;
  not_explored : string list;
  children : children;
  machines : (string * int) list;
  specs : Spec.t list;
  paths : (string * 'ev step list) list;
  views : 'ev state_views list;
  vacuous : (string * string) list;
  bounds : string;
}

type rule = Check_rules.rule = {
  id : string;
  message : string;
  hint : string;
  applies_to : string;
}

let rules = Check_rules.rules

let validate ~properties ~children allow =
  let names = List.map property_name properties @ children in
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

let property_rule (type n e ev) (model : (n, _, ev, e) Check_model.model) ~name
    ~hint ~condition : (n, e, ev) Check_search.rule =
  ( "property \"" ^ name ^ "\"",
    fun node view _ (arrival : (ev, e) Check_search.arrival) ->
      if
        condition.reads_step
        || condition.test
             {
               snapshot = model.snapshot node;
               view = Lazy.from_val view;
               event = Option.bind arrival.event model.event_of;
               name = model.name node;
               from = arrival.from;
               before = None;
               actions = Lazy.from_val arrival.actions;
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

let vacuous_guard =
  "the left side of its implication (the condition under not_) never holds at \
   an explored point, so the property passed without being tested"

let vacuous_from =
  "its from condition never holds at an explored point, so the property passed \
   without being tested"

let vacuous_after =
  "its after condition never holds at an explored point, so the property \
   passed without being tested"

let temporal_violations (type n s ev e)
    ~(model : (n, s, ev, e) Check_model.model)
    ~(result : (n, s, ev, e) Check_search.result)
    ~(graph : (ev, e) Check_search.graph) ~step_of ~path ~temporal ~allow
    ~max_violations violations =
  let count = Dynarray.length result.nodes in
  let node index = Dynarray.get result.nodes index in
  let names = Array.init count (fun index -> model.name (node index)) in
  let raised = ref [] in
  let label property conditions =
    let labels =
      List.map
        (fun _ ->
          {
            Check_temporal.at_start =
              Array.make (List.length graph.starts) false;
            at_edge = Bytes.make (Array.length graph.targets) '\000';
          }
        )
        conditions
    in
    let failed = ref false in
    let test index context set =
      if not !failed then
        List.iter2
          (fun condition labels ->
            match
              Check_search.guard model.machine (fun () -> condition.test context)
            with
            | Ok true ->
                set labels
            | Ok false ->
                ()
            | Error error ->
                failed := true;
                raised := (property, index, error) :: !raised
          )
          conditions labels
    in
    let context index ~event ~from ~actions =
      let state = node index in
      {
        snapshot = model.snapshot state;
        view = lazy (model.view state);
        event;
        name = names.(index);
        from;
        before = None;
        actions;
      }
    in
    List.iteri
      (fun position
           ({ node = index; start_actions } : _ Check_search.graph_start) ->
        test index
          (context index ~event:None ~from:None
             ~actions:(Lazy.from_val start_actions)
          )
          (fun (labels : Check_temporal.labels) ->
            labels.at_start.(position) <- true
          )
      )
      graph.starts;
    let incoming = Array.make (count + 1) 0 in
    Array.iter
      (fun target ->
        if target >= 0 then incoming.(target + 1) <- incoming.(target + 1) + 1
      )
      graph.targets;
    for index = 1 to count do
      incoming.(index) <- incoming.(index) + incoming.(index - 1)
    done;
    let filled = Array.copy incoming in
    let sorted = Array.make incoming.(count) 0 in
    let sources = Array.make (Array.length graph.targets) 0 in
    for source = 0 to graph.expanded - 1 do
      for edge = graph.first.(source) to graph.first.(source + 1) - 1 do
        sources.(edge) <- source;
        let target = graph.targets.(edge) in
        if target >= 0 then (
          sorted.(filled.(target)) <- edge;
          filled.(target) <- filled.(target) + 1
        )
      done
    done;
    for target = 0 to count - 1 do
      if incoming.(target + 1) > incoming.(target) then
        let shared =
          context target ~event:None ~from:None ~actions:(Lazy.from_val [])
        in
        for position = incoming.(target) to incoming.(target + 1) - 1 do
          let edge = sorted.(position) in
          test target
            {
              shared with
              event = model.event_of graph.moves.(edge).event;
              from = Some names.(sources.(edge));
              before = Some (model.snapshot (node sources.(edge)));
              actions =
                lazy
                  (let _, actions, _ =
                     model.step (node sources.(edge)) graph.moves.(edge)
                   in
                   actions
                  );
            }
            (fun (labels : Check_temporal.labels) ->
              Bytes.set labels.at_edge edge '\001'
            )
        done
    done;
    labels
  in
  let name index = names.(index) in
  let steps links =
    List.map
      (fun (link : Check_temporal.link) ->
        let target = graph.targets.(link.edge) in
        step_of link.source graph.moves.(link.edge)
          ~next:(Some (node target))
          ~target:names.(target)
      )
      links
  in
  let ever condition =
    match label "vacuity" [ condition ] with
    | [ (labels : Check_temporal.labels) ] ->
        Array.exists Fun.id labels.at_start
        || Bytes.contains labels.at_edge '\001'
    | _ ->
        true
  in
  let vacuous =
    List.filter_map
      (function
        | Always { name; condition = { guard = Some guard; _ }; _ } ->
            if ever guard then
              None
            else
              Some (name, vacuous_guard)
        | Always _ ->
            None
        | Leads_to { name; from; _ } ->
            if ever from then
              None
            else
              Some (name, vacuous_from)
        | Never_after { name; after; _ } ->
            if ever after then
              None
            else
              Some (name, vacuous_after)
        )
      temporal
  in
  let findings =
    List.concat_map
      (fun property ->
        match property with
        | Always { name = rule; hint; condition } -> (
            match label rule [ of_point (fun _ -> true); not_ condition ] with
            | [ after; never ] ->
                List.map
                  (fun finding -> (rule, hint, `Always, finding))
                  (Check_temporal.never_after graph ~count ~name ~after ~never)
            | _ ->
                []
          )
        | Never_after { name = rule; hint; after; never } -> (
            match label rule [ after; never ] with
            | [ after; never ] ->
                List.map
                  (fun finding -> (rule, hint, `Never_after, finding))
                  (Check_temporal.never_after graph ~count ~name ~after ~never)
            | _ ->
                []
          )
        | Leads_to { name = rule; hint; from; to_ } -> (
            match label rule [ from; to_ ] with
            | [ from; to_ ] ->
                List.map
                  (fun finding -> (rule, hint, `Leads_to, finding))
                  (Check_temporal.leads_to graph ~count ~name ~from ~to_)
            | _ ->
                []
          )
      )
      temporal
  in
  let violation ~rule ~values ~texts ~counterexample index =
    let state = node index in
    let message, hint =
      Check_rules.render
        { Check_rules.rule; element = None; values; custom = Some texts }
        ~state:names.(index)
    in
    {
      rule;
      message;
      hint;
      counterexample;
      state = model.snapshot state;
      state_name = names.(index);
      view =
        ( match model.view state with
        | view ->
            View_text.to_string view
        | exception _ ->
            ""
        );
      element = None;
    }
  in
  let found =
    List.rev_map
      (fun (rule, index, error) ->
        let message, hint =
          Check_rules.render
            (Check_search.raised ~where:("property \"" ^ rule ^ "\"") error)
            ~state:names.(index)
        in
        {
          rule = "raised";
          message;
          hint;
          counterexample = Path (path index);
          state = model.snapshot (node index);
          state_name = names.(index);
          view = "";
          element = None;
        }
      )
      !raised
    @ List.map
        (fun (rule, hint, kind, (finding : Check_temporal.finding)) ->
          let texts message default = (message, Option.value hint ~default) in
          match finding.counterexample with
          | Check_temporal.Path links ->
              violation ~rule
                ~values:[ ("property", rule) ]
                ~texts:
                  ( match kind with
                  | `Always ->
                      texts Check_rules.property_message
                        Check_rules.property_hint
                  | `Never_after | `Leads_to ->
                      texts Check_temporal.never_after_message
                        Check_temporal.never_after_hint
                  )
                ~counterexample:(Path (steps links))
                finding.node
          | Check_temporal.Waits links ->
              violation ~rule
                ~values:[ ("property", rule) ]
                ~texts:
                  (texts Check_temporal.waits_message Check_temporal.waits_hint)
                ~counterexample:(Waits (steps links))
                finding.node
          | Check_temporal.Loop { stem; cycle } ->
              let cycle_steps = steps cycle in
              let delivered =
                List.sort_uniq String.compare
                  (List.filter_map
                     (fun (link : Check_temporal.link) ->
                       Check_temporal.resource graph.moves.(link.edge).via
                     )
                     cycle
                  )
              in
              violation ~rule
                ~values:
                  [
                    ("property", rule);
                    ( "cycle",
                      String.concat " -> "
                        (names.(finding.node)
                        :: List.map (fun step -> step.target) cycle_steps
                        )
                    );
                    ( "delivered",
                      match delivered with
                      | [] ->
                          "nothing"
                      | delivered ->
                          String.concat ", " delivered
                    );
                  ]
                ~texts:
                  (texts Check_temporal.loop_message Check_temporal.loop_hint)
                ~counterexample:(Loop { stem = steps stem; cycle = cycle_steps })
                finding.node
        )
        findings
  in
  let allowed = Array.of_list (List.map snd result.allowed) in
  let truncated = ref result.truncated in
  let kept = ref (List.length violations) in
  let added =
    List.filter
      (fun (violation : (s, ev) violation) ->
        let rec find index = function
          | [] ->
              None
          | first :: rest ->
              if
                Check_rules.allows first ~rule:violation.rule
                  ~state:violation.state_name ~element:None
              then
                Some index
              else
                find (index + 1) rest
        in
        match find 0 allow with
        | Some index ->
            allowed.(index) <- allowed.(index) + 1;
            false
        | None ->
            if !kept >= max_violations then (
              truncated := true;
              false
            ) else (
              incr kept;
              true
            )
      )
      found
  in
  ( violations @ added,
    List.mapi
      (fun index (allowance, _) -> (allowance, allowed.(index)))
      result.allowed,
    !truncated,
    vacuous
  )

let rules_of (type n e ev) (model : (n, _, ev, e) Check_model.model) properties
    : (n, e, ev) Check_search.rule list =
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
  let accessibility = Hashtbl.create 64 in
  [
    ("view", fun _ view _ _ -> Check_rules.view_rules view);
    ( "view",
      fun _ view _ _ -> Check_accessibility.view_rules ~memo:accessibility view
    );
    ("view", dead_end);
    ("machine", fun node view _ _ -> model.state_rules node view);
    ( "child",
      fun node view _ (arrival : (_, _) Check_search.arrival) ->
        model.tree.rules node view arrival.event
    );
  ]
  @ List.filter_map
      (function
        | Always { name; hint; condition } ->
            Some (property_rule model ~name ~hint ~condition)
        | Never_after _ | Leads_to _ ->
            None
        )
      properties

let root_moved model node next =
  match next with
  | Some next when model.Check_model.name next <> model.name node ->
      Some (model.name node, model.name next)
  | Some _ | None ->
      None

let run (type s ev) (Check check : (s, ev) t) : (s, ev) report =
  validate ~properties:check.properties ~children:check.names check.allow;
  let model = check.build () in
  let rules = rules_of model check.properties in
  let temporal =
    List.filter
      (function
        | Always { condition; _ } ->
            condition.reads_step
        | Never_after _ | Leads_to _ ->
            true
        )
      check.properties
  in
  let result =
    Check_search.run ~model ~rules ~max_states:check.max_states
      ~max_violations:check.max_violations
      ~within:(fun node -> check.within node)
      ~allow:check.allow ~graph:(temporal <> []) ~views_per_state
  in
  let step_of parent (move : _ Check_model.move) ~next ~target =
    let node = Dynarray.get result.nodes parent in
    let view = model.view node in
    let cause = model.cause node view move in
    let event = Check_model.move_label model node (Lazy.from_val view) move in
    match model.tree.actor move.event with
    | None ->
        {
          input = move.event;
          event;
          cause;
          instance = "0";
          machine = model.machine;
          from = model.name node;
          target;
          root_moved = None;
        }
    | Some (instance, machine) ->
        let state node =
          Option.value ~default:"(ended)" (model.tree.state_of node instance)
        in
        let next =
          match next with
          | Some next ->
              Some next
          | None -> (
              match model.step node move with
              | next, _, _ ->
                  Some next
              | exception _ ->
                  None
            )
        in
        {
          input = move.event;
          event;
          cause;
          instance;
          machine;
          from = state node;
          target =
            (match next with Some next -> state next | None -> state node);
          root_moved = root_moved model node next;
        }
  in
  let path index =
    List.map
      (fun (parent, move, child) ->
        let next = Dynarray.get result.nodes child in
        step_of parent move ~next:(Some next) ~target:(model.name next)
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
              [ step_of index move ~next:None ~target ]
          | _ ->
              []
        in
        let message =
          match List.rev last with
          | { instance; machine; from; _ } :: _ when instance <> "0" ->
              Printf.sprintf
                "%s The child %s at instance %s took this step in its state %s."
                found.message machine instance from
          | _ ->
              found.message
        in
        {
          rule = found.rule;
          message;
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
  let violations, allowed, truncated, temporal_vacuous =
    match result.graph with
    | None ->
        (violations, result.allowed, result.truncated, [])
    | Some graph ->
        temporal_violations ~model ~result ~graph ~step_of ~path ~temporal
          ~allow:check.allow ~max_violations:check.max_violations violations
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
      ([], allowed) (model.unreached ())
  in
  {
    machine = model.machine;
    states = Dynarray.length result.nodes;
    transitions = result.transitions;
    complete = result.complete;
    truncated;
    violations;
    unreached = List.rev unreached;
    unlisted = model.unlisted ();
    allowed;
    not_explored =
      List.sort_uniq String.compare (result.not_explored @ model.tree.not_run ());
    children = result.children;
    machines =
      ( match model.tree.counts () with
      | [] ->
          [ (model.machine, Dynarray.length result.nodes) ]
      | counts ->
          counts
      );
    specs = model.specs ();
    paths = List.map (fun (name, index) -> (name, path index)) result.firsts;
    views =
      List.map
        (fun (state, views, more) ->
          {
            state;
            views = List.map (fun (text, index) -> (text, path index)) views;
            more;
          }
        )
        result.views;
    vacuous =
      List.filter_map
        (function
          | Always
              {
                name;
                condition = { guard = Some guard; reads_step = false; _ };
                _;
              } ->
              let holds index =
                let node = Dynarray.get result.nodes index in
                match
                  guard.test
                    {
                      snapshot = model.snapshot node;
                      view = lazy (model.view node);
                      event = None;
                      name = model.name node;
                      from = None;
                      before = None;
                      actions = Lazy.from_val [];
                    }
                with
                | holds ->
                    holds
                | exception _ ->
                    true
              in
              if
                List.exists holds
                  (List.init (Dynarray.length result.nodes) Fun.id)
              then
                None
              else
                Some (name, vacuous_guard)
          | Always _ | Never_after _ | Leads_to _ ->
              None
          )
        check.properties
      @ temporal_vacuous;
    bounds =
      ( match check.subject with
      | Routed ->
          app_bounds
      | Plain | With_props ->
          component_bounds
      );
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

let describe_step ~root step =
  Printf.sprintf "%s%s -> %s: %s, by %s%s"
    ( if step.instance = "0" then
        ""
      else
        Printf.sprintf "[%s at %s] " step.machine step.instance
    )
    step.from step.target step.event
    (describe_cause step.cause)
    ( match step.root_moved with
    | Some (from, target) ->
        Printf.sprintf "; then %s: %s -> %s" root from target
    | None ->
        ""
    )

let pp_step ~root formatter index step =
  Format.fprintf formatter "@,  %d. %s" (index + 1) (describe_step ~root step)

let pp_report formatter report =
  Format.fprintf formatter "@[<v>%s: %d states, %d transitions" report.machine
    report.states report.transitions;
  ( match report.machines with
  | [] | [ _ ] ->
      ()
  | machines ->
      Format.fprintf formatter "@,machines: %s"
        (String.concat ", "
           (List.map
              (fun (machine, count) -> Printf.sprintf "%s %d" machine count)
              machines
           )
        )
  );
  ( if not report.complete then
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
      | Path steps ->
          List.iteri (pp_step ~root:report.machine formatter) steps
      | Waits steps ->
          List.iteri (pp_step ~root:report.machine formatter) steps;
          Format.fprintf formatter
            "@,  then no timer and no call runs: the app waits for the user"
      | Loop { stem; cycle } ->
          List.iteri (pp_step ~root:report.machine formatter) stem;
          Format.fprintf formatter "@,  then forever:";
          List.iteri (pp_step ~root:report.machine formatter) cycle
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
    (fun machine ->
      Format.fprintf formatter "@,unlisted: %s"
        (Check_rules.unlisted_message machine)
    )
    report.unlisted;
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
  List.iter
    (fun (property, reason) ->
      Format.fprintf formatter "@,vacuous: the property %S: %s" property reason
    )
    report.vacuous;
  Format.fprintf formatter "@]"

type failure = {
  rule : string;
  message : string;
  hint : string;
  element : string option;
}

type ('s, 'ev) replayed_step = {
  step : 'ev step option;
  state : 's;
  state_name : string;
  view : string;
  actions : string list;
  failing : failure list;
}

type ('s, 'ev) replay =
  | Replayed of ('s, 'ev) replayed_step list
  | Not_enabled of {
      at : int;
      steps : ('s, 'ev) replayed_step list;
      enabled : 'ev step list;
    }

type 'ev wanted = {
  input : 'ev option;
  event : string;
  cause : cause option;
  choice : int option;
  answer : Silica_json.Json.t option;
  url : string option;
  by_cause : bool;
}

let wanted_label wanted =
  match (wanted.event, wanted.cause) with
  | "", Some cause ->
      Check_model.cause_label ~answer:wanted.answer cause
  | "", None ->
      "(unnamed)"
  | name, _ ->
      name

exception Unfaithful_step of string

type ('n, 's, 'ev, 'e) point = {
  replayed : ('s, 'ev) replayed_step;
  node : 'n;
  before : ('n * 'e Node.t * 'ev Check_model.move) option;
  host : 'e Action.host list;
  how : Machine.how;
  next : ('e Node.t * 'ev Check_model.move list) option;
}

type ('n, 's, 'ev, 'e) walked =
  | Walked of ('n, 's, 'ev, 'e) point list
  | Stuck of {
      at : int;
      points : ('n, 's, 'ev, 'e) point list;
      enabled : 'ev step list;
    }
  | Unfaithful of string

let decode_failure answer =
  match Codec.constructor_of answer with
  | Ok ("Failed", [ error ]) -> (
      match Codec.constructor_of error with
      | Ok ("Decode", [ _ ]) ->
          Codec.constructor "Failed" [ Codec.constructor "Decode" [] ]
      | _ ->
          answer
    )
  | _ ->
      answer

let without_number id =
  match String.index_opt id '/' with
  | Some slash
    when slash > 0
         && String.for_all
              (fun character ->
                character = '.' || ('0' <= character && character <= '9')
              )
              (String.sub id 0 slash) ->
      String.sub id (slash + 1) (String.length id - slash - 1)
  | _ ->
      id

let similar (first : cause) (second : cause) =
  match (first, second) with
  | View first, View second ->
      first.handler = second.handler
      && first.role = second.role && first.name = second.name
  | Timer first, Timer second ->
      without_number first.id = without_number second.id
  | Answer first, Answer second ->
      first.path = second.path
  | Live first, Live second ->
      first.topic = second.topic
  | Held first, Held second ->
      first.source = second.source
  | Action_result first, Action_result second ->
      first = second
  | Input, Input | Props, Props | Navigation, Navigation ->
      true
  | Child first, Child second ->
      first.instance = second.instance
  | _ ->
      false

type ('s, 'ev, 'r) consumer = {
  consume :
    'n 'e.
    ('n, 's, 'ev, 'e) Check_model.model ->
    start:int ->
    ('n, 's, 'ev, 'e) walked ->
    'r;
}

let walk (type s ev r) ?ends_with ?loop_from ?start (Check check : (s, ev) t)
    ~(prefer : _ -> bool) (inputs : ev wanted list)
    (consumer : (s, ev, r) consumer) : (r, exn) result =
  validate ~properties:check.properties ~children:check.names check.allow;
  let model = check.build () in
  let rules = rules_of model check.properties in
  let failures ~state (findings : Check_rules.finding list) =
    List.fold_left
      (fun acc (finding : Check_rules.finding) ->
        if
          List.exists
            (fun allowance ->
              Check_rules.allows allowance ~rule:finding.rule ~state
                ~element:finding.element
            )
            check.allow
        then
          acc
        else
          let message, hint = Check_rules.render finding ~state in
          if
            List.exists
              (fun (other : failure) ->
                other.rule = finding.rule && other.message = message
              )
              acc
          then
            acc
          else
            acc
            @ [
                {
                  rule = finding.rule;
                  message;
                  hint;
                  element = finding.element;
                };
              ]
      )
      [] findings
  in
  let guard f = Check_search.guard model.machine f in
  let point ~step ~before ~host ~how ~extra node =
    let state = model.snapshot node and state_name = model.name node in
    let arrival : (ev, _) Check_search.arrival =
      {
        event =
          Option.map
            (fun (_, _, (move : _ Check_model.move)) -> move.event)
            before;
        from = Option.map (fun (previous, _, _) -> model.name previous) before;
        actions = host;
      }
    in
    let view_text, findings, next =
      match guard (fun () -> model.view node) with
      | Error error ->
          ("", [ Check_search.raised ~where:"view" error ], None)
      | Ok view -> (
          match guard (fun () -> model.enabled node view) with
          | Error error ->
              ( View_text.to_string view,
                [ Check_search.raised ~where:"handlers or inputs" error ],
                None
              )
          | Ok moves ->
              ( View_text.to_string view,
                List.concat_map
                  (fun (where, rule) ->
                    match guard (fun () -> rule node view moves arrival) with
                    | Ok findings ->
                        findings
                    | Error error ->
                        [ Check_search.raised ~where error ]
                  )
                  rules,
                Some (view, moves)
              )
        )
    in
    {
      replayed =
        {
          step;
          state;
          state_name;
          view = view_text;
          actions = List.map Transition.describe host;
          failing = failures ~state:state_name (extra @ findings);
        };
      node;
      before;
      host;
      how;
      next;
    }
  in
  let step_of ?next node view (move : _ Check_model.move) target =
    let step =
      {
        input = move.event;
        event = Check_model.move_label model node (Lazy.from_val view) move;
        cause = model.cause node view move;
        instance = "0";
        machine = model.machine;
        from = model.name node;
        target;
        root_moved = None;
      }
    in
    match model.tree.actor move.event with
    | None ->
        step
    | Some (instance, machine) ->
        let state node =
          Option.value ~default:"(ended)" (model.tree.state_of node instance)
        in
        {
          step with
          instance;
          machine;
          from = state node;
          target = state (Option.value next ~default:node);
          root_moved = root_moved model node next;
        }
  in
  let choose ~at node view moves (wanted : ev wanted) =
    let named (move : _ Check_model.move) =
      match wanted.input with
      | Some input ->
          move.event = input
      | None ->
          Option.value ~default:"" (model.event_name move.event) = wanted.event
    in
    let cause_of move = model.cause node view move in
    let fits move =
      named move
      &&
      match wanted.cause with
      | Some cause ->
          similar cause (cause_of move)
      | None ->
          true
    in
    let reached url move =
      match guard (fun () -> model.step node move) with
      | Ok (next, _, _) ->
          check.url (model.snapshot next) = Some url
      | Error _ ->
          false
    in
    let reaches url (move : _ Check_model.move) =
      match model.event_name move.event with
      | Some ("back" | "forward") ->
          reached url move
      | Some name
        when String.starts_with ~prefix:"link " name
             || String.starts_with ~prefix:"open " name ->
          reached url move
      | _ ->
          false
    in
    let fits_run (move : _ Check_model.move) =
      (match wanted.url with Some url -> reaches url move | None -> fits move)
      &&
      match wanted.answer with
      | Some answer ->
          Option.map decode_failure (model.answer node move)
          = Some (decode_failure answer)
      | None ->
          true
    in
    match wanted.choice with
    | None when wanted.by_cause -> (
        let same_next moves =
          match
            List.map
              (fun move ->
                match guard (fun () -> model.step node move) with
                | Ok (next, _, _) ->
                    Some next
                | Error _ ->
                    None
              )
              moves
          with
          | Some first :: rest ->
              List.for_all (fun next -> next = Some first) rest
          | _ ->
              false
        in
        match List.filter fits_run moves with
        | [] ->
            None
        | [ move ] ->
            Some move
        | move :: _ as several when same_next several ->
            Some move
        | several ->
            raise
              (Unfaithful_step
                 (Printf.sprintf
                    "step %d of the run (%s) fits %d moves of the check, so it \
                     cannot be replayed faithfully"
                    at (wanted_label wanted) (List.length several)
                 )
              )
      )
    | Some position -> (
        match List.nth_opt moves position with
        | Some move when fits move ->
            Some move
        | _ -> (
            match List.filter fits moves with
            | [] ->
                None
            | [ move ] ->
                Some move
            | several ->
                raise
                  (Unfaithful_step
                     (Printf.sprintf
                        "step %d of the trace is move %d with the event %s, \
                         which this check does not offer there, and %d moves \
                         of the check fit its name and cause; the trace comes \
                         from another version of the check, so it cannot be \
                         replayed faithfully: run the check again with \
                         --traces"
                        at position (wanted_label wanted) (List.length several)
                     )
                  )
          )
      )
    | None -> (
        let candidates = List.filter named moves in
        let find test =
          List.find_opt (fun move -> test (cause_of move)) candidates
        in
        match wanted.cause with
        | None ->
            List.nth_opt candidates 0
        | Some cause -> (
            match find (fun other -> other = cause) with
            | Some move ->
                Some move
            | None -> (
                match find (similar cause) with
                | Some move ->
                    Some move
                | None ->
                    if wanted.input = None then
                      None
                    else
                      List.nth_opt candidates 0
              )
          )
      )
  in
  let rec go points last at = function
    | [] ->
        Walked (List.rev points)
    | wanted :: rest -> (
        match last.next with
        | None ->
            Stuck { at; points = List.rev points; enabled = [] }
        | Some (view, moves) -> (
            let node = last.node in
            match choose ~at node view moves wanted with
            | None ->
                Stuck
                  {
                    at;
                    points = List.rev points;
                    enabled =
                      List.map
                        (fun move -> step_of node view move (model.name node))
                        moves;
                  }
            | Some move -> (
                let from = model.name node in
                let values =
                  [
                    ( "event",
                      Check_model.move_label model node (Lazy.from_val view)
                        move
                    );
                    ("from", from);
                  ]
                in
                match guard (fun () -> model.step node move) with
                | Error error ->
                    let failed =
                      {
                        replayed =
                          {
                            step = Some (step_of node view move from);
                            state = model.snapshot node;
                            state_name = from;
                            view = View_text.to_string view;
                            actions = [];
                            failing =
                              failures ~state:from
                                [ Check_search.raised ~where:"update" error ];
                          };
                        node;
                        before = Some (node, view, move);
                        host = [];
                        how = Machine.Unhandled;
                        next = None;
                      }
                    in
                    go (failed :: points) failed (at + 1) rest
                | Ok (next, host, how) ->
                    let ignored =
                      match how with
                      | Machine.Unhandled
                        when not (Check_model.ignorable move.via) ->
                          let cause = model.cause node view move in
                          let element =
                            match cause with
                            | Check_model.View { element; _ } ->
                                Some element
                            | _ ->
                                None
                          in
                          [
                            Check_rules.finding "ignored-event" ?element
                              ~values:
                                [
                                  List.hd values;
                                  ("cause", Check_model.describe_cause cause);
                                ];
                          ]
                      | _ ->
                          []
                    in
                    let actions =
                      match
                        guard (fun () ->
                            model.action_rules ~values
                              (lazy (model.view next))
                              host
                        )
                      with
                      | Ok findings ->
                          findings
                      | Error _ ->
                          []
                    in
                    let reached =
                      point
                        ~step:
                          (Some (step_of ~next node view move (model.name next)))
                        ~before:(Some (node, view, move))
                        ~host ~how ~extra:(ignored @ actions) next
                    in
                    go (reached :: points) reached (at + 1) rest
              )
          )
      )
  in
  let context point =
    {
      snapshot = model.snapshot point.node;
      view =
        lazy
          ( match point.next with
          | Some (view, _) ->
              view
          | None ->
              model.view point.node
          );
      event =
        Option.bind point.before (fun (_, _, (move : _ Check_model.move)) ->
            model.event_of move.event
        );
      name = model.name point.node;
      from =
        Option.map (fun (previous, _, _) -> model.name previous) point.before;
      before =
        Option.map
          (fun (previous, _, _) -> model.snapshot previous)
          point.before;
      actions = Lazy.from_val point.host;
    }
  in
  let waits point =
    match point.next with
    | Some (_, moves) ->
        List.for_all
          (fun (move : _ Check_model.move) ->
            Check_temporal.resource move.via = None
          )
          moves
    | None ->
        false
  in
  let holds condition point =
    match guard (fun () -> condition.test (context point)) with
    | Ok holds ->
        holds
    | Error _ ->
        false
  in
  let temporal points =
    let points = Array.of_list points in
    let count = Array.length points in
    let extra = Array.make count [] in
    let add index rule ~values ~texts:(message, hint) =
      extra.(index) <-
        extra.(index)
        @ [
            {
              Check_rules.rule;
              element = None;
              values = ("property", rule) :: values;
              custom = Some (message, hint);
            };
          ]
    in
    List.iter
      (function
        | Always { condition; _ } when not condition.reads_step ->
            ()
        | Always { name; hint; condition } ->
            Array.iteri
              (fun index point ->
                if not (holds condition point) then
                  add index name ~values:[]
                    ~texts:
                      ( Check_rules.property_message,
                        Option.value hint ~default:Check_rules.property_hint
                      )
              )
              points
        | Never_after { name; hint; after; never } ->
            let seen = ref false in
            Array.iteri
              (fun index point ->
                if holds after point then seen := true;
                if !seen && holds never point then
                  add index name ~values:[]
                    ~texts:
                      ( Check_temporal.never_after_message,
                        Option.value hint
                          ~default:Check_temporal.never_after_hint
                      )
              )
              points
        | Leads_to { name; hint; from; to_ } -> (
            let pending = Array.make count false in
            let waiting = ref false in
            Array.iteri
              (fun index point ->
                if holds from point then waiting := true;
                if holds to_ point then waiting := false;
                pending.(index) <- !waiting;
                if !waiting && waits point then
                  add index name ~values:[]
                    ~texts:
                      ( Check_temporal.waits_message,
                        Option.value hint ~default:Check_temporal.waits_hint
                      )
              )
              points;
            match loop_from with
            | Some start when start < count - 1 ->
                let last = count - 1 in
                let cycle = Array.sub points (start + 1) (last - start) in
                if
                  points.(start).node = points.(last).node
                  && Array.for_all Fun.id
                       (Array.sub pending start (last - start + 1))
                then
                  let delivered =
                    List.sort_uniq String.compare
                      (List.filter_map
                         (fun point ->
                           Option.bind point.before
                             (fun (_, _, (move : _ Check_model.move)) ->
                               Check_temporal.resource move.via
                           )
                         )
                         (Array.to_list cycle)
                      )
                  in
                  let running point =
                    match point.next with
                    | Some (_, moves) ->
                        List.sort_uniq String.compare
                          (List.filter_map
                             (fun (move : _ Check_model.move) ->
                               Check_temporal.resource move.via
                             )
                             moves
                          )
                    | None ->
                        []
                  in
                  let always_running =
                    List.filter
                      (fun resource ->
                        Array.for_all
                          (fun point -> List.mem resource (running point))
                          (Array.sub points start (last - start + 1))
                      )
                      (running points.(start))
                  in
                  if
                    List.for_all
                      (fun resource -> List.mem resource delivered)
                      always_running
                  then
                    add last name
                      ~values:
                        [
                          ( "cycle",
                            String.concat " -> "
                              (model.name points.(start).node
                              :: List.map
                                   (fun point -> model.name point.node)
                                   (Array.to_list cycle)
                              )
                          );
                          ( "delivered",
                            match delivered with
                            | [] ->
                                "nothing"
                            | delivered ->
                                String.concat ", " delivered
                          );
                        ]
                      ~texts:
                        ( Check_temporal.loop_message,
                          Option.value hint ~default:Check_temporal.loop_hint
                        )
            | Some _ | None ->
                ()
          )
        )
      check.properties;
    Array.to_list
      (Array.mapi
         (fun index point ->
           match extra.(index) with
           | [] ->
               point
           | findings ->
               {
                 point with
                 replayed =
                   {
                     point.replayed with
                     failing =
                       point.replayed.failing
                       @ failures ~state:point.replayed.state_name findings;
                   };
               }
         )
         points
      )
  in
  let attempt (node, host) =
    let values = [ ("event", "start"); ("from", model.name node) ] in
    let start_findings =
      match
        guard (fun () ->
            model.action_rules ~values (lazy (model.view node)) host
        )
      with
      | Ok findings ->
          findings
      | Error _ ->
          []
    in
    let first =
      point ~step:None ~before:None ~host ~how:Machine.Entered
        ~extra:(model.setup_findings () @ start_findings)
        node
    in
    match go [ first ] first 1 inputs with
    | Walked points ->
        Walked (temporal points)
    | Stuck stuck ->
        Stuck { stuck with points = temporal stuck.points }
    | Unfaithful reason ->
        Unfaithful reason
    | exception Unfaithful_step reason ->
        Unfaithful reason
  in
  let complete = function
    | Walked _ ->
        true
    | Stuck _ | Unfaithful _ ->
        false
  in
  let faithful = function
    | Walked _ | Stuck _ ->
        true
    | Unfaithful _ ->
        false
  in
  let breaks = function
    | Walked points -> (
        match (ends_with, List.rev points) with
        | Some rule, last :: _ ->
            List.exists
              (fun (failure : failure) -> failure.rule = rule)
              last.replayed.failing
        | _ ->
            false
      )
    | Stuck _ | Unfaithful _ ->
        false
  in
  match guard model.start with
  | Error error ->
      Error error
  | Ok starts -> (
      let indexed =
        List.filteri
          (fun index _ ->
            match start with Some wanted -> wanted = index | None -> true
          )
          (List.mapi (fun index start -> (index, start)) starts)
      in
      let preferred, others =
        List.partition
          (fun (_, (node, _)) -> prefer (model.props_json node))
          indexed
      in
      let attempts =
        List.map
          (fun (index, start) -> (index, attempt start))
          (preferred @ others)
      in
      let find test = List.find_opt (fun (_, walked) -> test walked) attempts in
      match
        match find breaks with
        | Some found ->
            Some found
        | None -> (
            match find complete with
            | Some found ->
                Some found
            | None ->
                find faithful
          )
      with
      | Some (index, walked) ->
          Ok (consumer.consume model ~start:index walked)
      | None -> (
          match attempts with
          | (index, first) :: _ ->
              Ok (consumer.consume model ~start:index first)
          | [] ->
              Error (Invalid_check "the check has no start")
        )
    )

let public_replay = function
  | Walked points ->
      Ok (Replayed (List.map (fun point -> point.replayed) points))
  | Stuck { at; points; enabled } ->
      Ok
        (Not_enabled
           {
             at;
             steps = List.map (fun point -> point.replayed) points;
             enabled;
           }
        )
  | Unfaithful reason ->
      Error reason

let start_failure (type s ev) (Check check : (s, ev) t) error : (s, ev) replay =
  let model = check.build () in
  Replayed
    [
      {
        step = None;
        state = model.initial;
        state_name = model.initial_name;
        view = "";
        actions = [];
        failing =
          [
            (let message, hint =
               Check_rules.render
                 (Check_search.raised ~where:"start" error)
                 ~state:model.initial_name
             in
             { rule = "raised"; message; hint; element = None }
            );
          ];
      };
    ]

let replay check inputs =
  match
    walk check
      ~prefer:(fun _ -> true)
      (List.map
         (fun input ->
           {
             input = Some input;
             event = "";
             cause = None;
             choice = None;
             answer = None;
             url = None;
             by_cause = false;
           }
         )
         inputs
      )
      { consume = (fun _ ~start:_ walked -> public_replay walked) }
  with
  | Ok (Ok replay) ->
      replay
  | Ok (Error reason) ->
      invalid_arg ("Check.replay: " ^ reason)
  | Error error ->
      start_failure check error

let check_cause : Transition.cause -> cause option = function
  | Element { handler; role; name; element; value } ->
      let handler =
        match (handler, value) with
        | ("key_down" | "key_up" | "input"), Some value ->
            handler ^ " " ^ value
        | _ ->
            handler
      in
      Some (View { handler; element; role; name })
  | Timer { id; ms } ->
      Some (Timer { id; ms })
  | Answer { id; path; _ } ->
      Some (Answer { id; path })
  | Live { id; topic; _ } ->
      Some (Live { id; topic })
  | Held { id; source } ->
      Some (Held { id; source })
  | Action_result { action } ->
      Some (Action_result action)
  | Props ->
      Some Props
  | Message ->
      Some Input
  | Start | Child _ | Url _ | Preload _ | Ended ->
      None

let root_steps ?(listed = []) (trace : Trace.t) =
  let of_listed instance =
    List.exists
      (fun machine -> String.ends_with ~suffix:(":" ^ machine) instance)
      listed
  in
  let root =
    List.find_map
      (fun (record : Transition.t) ->
        if record.change = Started && record.machine = trace.header.root then
          Some record
        else
          None
      )
      trace.transitions
  in
  match root with
  | None ->
      Error
        ("the trace has no start of the machine " ^ trace.header.root
       ^ ", the root of its header"
        )
  | Some start ->
      Ok
        ( start,
          List.filter
            (fun (record : Transition.t) ->
              (record.instance = start.instance
              || String.starts_with ~prefix:(start.instance ^ "/")
                   record.instance
                 && of_listed record.instance && record.cause <> Props
              )
              && ( match record.cause with
                | Child { instance } ->
                    not (of_listed instance)
                | _ ->
                    true
                )
              &&
              match record.change with
              | Started | Resumed | Ended ->
                  false
              | Unhandled | Kept | Entered ->
                  true
            )
            trace.transitions
        )

let wanted_of ?(by_cause = false) (Check check) (trace : Trace.t) records =
  let ( let* ) = Result.bind in
  let* choices =
    match trace.header.origin with
    | Checker { choices; _ } ->
        if List.compare_length_with choices (List.length records) = 0 then
          Ok (List.map Option.some choices)
        else
          Error
            (Printf.sprintf
               "the trace has %d steps and %d choices of the checker, so it \
                cannot be replayed faithfully"
               (List.length records) (List.length choices)
            )
    | Runtime _ ->
        Ok (List.map (fun _ -> None) records)
  in
  List.fold_left2
    (fun acc (record : Transition.t) choice ->
      let* wanted = acc in
      let* input = check.input_of record in
      match (input, choice) with
      | None, None when not by_cause ->
          Error
            (Printf.sprintf
               "transition %d (%s) has no event data and no choice of the \
                checker, so it cannot be replayed faithfully: the trace was \
                recorded with names only, or the machine has no event codec \
                (see Silica.Machine.register_codecs). To compare a recorded \
                run with the specs and the code, run conform"
               record.seq (Transition.label record)
            )
      | _ ->
          Ok
            ({
               input;
               event = record.event;
               cause = check_cause record.cause;
               choice;
               answer =
                 ( match record.cause with
                 | Answer { answer; _ } ->
                     answer
                 | _ ->
                     None
                 );
               url = (match record.cause with Url url -> Some url | _ -> None);
               by_cause;
             }
            :: wanted
            )
    )
    (Ok []) records choices
  |> Result.map List.rev

let start_of (trace : Trace.t) =
  match trace.header.origin with
  | Checker { start; _ } ->
      Some start
  | Runtime _ ->
      None

let loop_start (trace : Trace.t) records =
  Option.bind trace.header.loop_from (fun seq ->
      List.find_index (fun (record : Transition.t) -> record.seq = seq) records
  )

let replay_trace check (trace : Trace.t) =
  let (Check { root; listed; _ }) = check in
  if trace.header.root <> Lazy.force root then
    Error
      (Printf.sprintf "the trace is a run of %s, and the check is of %s"
         trace.header.root (Lazy.force root)
      )
  else
    Result.bind (root_steps ~listed trace)
      (fun ((start : Transition.t), records) ->
        Result.map
          (fun wanted ->
            match
              walk check ?loop_from:(loop_start trace records)
                ?start:(start_of trace)
                ~prefer:(fun props -> props = start.props)
                wanted
                { consume = (fun _ ~start:_ walked -> public_replay walked) }
            with
            | Ok replay ->
                replay
            | Error error ->
                Ok (start_failure check error)
          )
          (wanted_of check trace records)
        |> Result.join
    )

let nth_props (type s p e) (Check check : (s, (p, e) input) t) index : p =
  match List.nth_opt (check.starts ()) index with
  | Some (Props props) ->
      props
  | Some (Event _ | In_child _) | None ->
      invalid_arg
        (Printf.sprintf "Check.nth_props: the check of %s has no props %d"
           (Lazy.force check.id) index
        )

let replay_run ?cycle_from ?start:start_index ?(props = []) check
    (trace : Trace.t) =
  let (Check { root; listed; starts; _ }) = check in
  let with_props wanted =
    let given = ref props in
    List.map
      (fun (wanted : _ wanted) ->
        match (wanted.cause, !given) with
        | Some Props, index :: rest ->
            given := rest;
            { wanted with input = List.nth_opt (starts ()) index }
        | _ ->
            wanted
      )
      wanted
  in
  if trace.header.root <> Lazy.force root then
    Error
      (Printf.sprintf "the run is of %s, and the check is of %s"
         trace.header.root (Lazy.force root)
      )
  else
    Result.bind (root_steps ~listed trace)
      (fun ((start : Transition.t), records) ->
        Result.map
          (fun wanted ->
            match
              walk check ?loop_from:cycle_from ?start:start_index
                ~prefer:(fun props -> props = start.props)
                (with_props wanted)
                { consume = (fun _ ~start:_ walked -> public_replay walked) }
            with
            | Ok replay ->
                replay
            | Error error ->
                Ok (start_failure check error)
          )
          (wanted_of ~by_cause:true check
             {
               trace with
               header =
                 { trace.header with origin = Runtime { platform = "run" } };
             }
             records
          )
        |> Result.join
    )

let export (type s ev) (Check check as subject : (s, ev) t) ~rule
    (counterexample : ev counterexample) =
  let items = steps counterexample in
  let wanted =
    List.map
      (fun (step : ev step) ->
        {
          input = Some step.input;
          event = step.event;
          cause = Some step.cause;
          choice = None;
          answer = None;
          url = None;
          by_cause = false;
        }
      )
      items
  in
  let trace (type n e) (model : (n, s, ev, e) Check_model.model) ~start =
    function
    | Unfaithful reason ->
        invalid_arg ("Check.export: " ^ reason)
    | Stuck { at; _ } ->
        invalid_arg
          (Printf.sprintf
             "Check.export: step %d of the counterexample is not enabled in \
              the check"
             at
          )
    | Walked (points : (n, s, ev, e) point list) ->
        let machine = model.machine in
        let choices =
          List.filter_map Fun.id
            (List.mapi
               (fun index (point : (n, s, ev, e) point) ->
                 match (point.before, index) with
                 | _, 0 | None, _ ->
                     None
                 | Some (_, _, move), _ -> (
                     match (List.nth points (index - 1)).next with
                     | Some (_, moves) ->
                         List.find_index (fun other -> other == move) moves
                     | None ->
                         None
                   )
               )
               points
            )
        in
        let record seq point =
          let state = model.snapshot point.node in
          let common : Transition.t =
            {
              seq;
              time_ms = None;
              instance = machine;
              number = 1;
              entry = 0;
              machine;
              from = "";
              target = model.name point.node;
              change = Started;
              event = "";
              data = None;
              props = model.props_json point.node;
              snapshot = check.machine_json state;
              actions = point.replayed.actions;
              cause = Start;
            }
          in
          match (point.replayed.step, point.before) with
          | Some step, Some (node, _, move) ->
              let common =
                if step.instance = "0" then
                  common
                else
                  {
                    common with
                    instance = step.instance;
                    machine = step.machine;
                    target = step.target;
                    props = None;
                    snapshot = None;
                  }
              in
              {
                common with
                from = step.from;
                change =
                  ( match point.how with
                  | Machine.Unhandled ->
                      Unhandled
                  | Machine.Kept ->
                      Kept
                  | Machine.Entered ->
                      Entered
                  );
                event = Option.value ~default:"" (model.event_name move.event);
                data = check.input_json step.input;
                props =
                  (match step.cause with Props -> common.props | _ -> None);
                cause =
                  Check_model.transition_cause ~answer:(model.answer node move)
                    ~url:(check.url state) step.cause;
              }
          | _ ->
              common
        in
        let transitions =
          List.mapi (fun index point -> record (index + 1) point) points
        in
        {
          Trace.header =
            {
              root = machine;
              url =
                ( match points with
                | first :: _ ->
                    check.url (model.snapshot first.node)
                | [] ->
                    None
                );
              build = "check";
              origin = Checker { rule; start; choices };
              dropped = 0;
              loop_from =
                ( match counterexample with
                | Loop { stem; _ } ->
                    Some (List.length stem + 2)
                | Path _ | Waits _ ->
                    None
                );
            };
          transitions;
        }
  in
  let loop_from =
    match counterexample with
    | Loop { stem; _ } ->
        Some (List.length stem)
    | Path _ | Waits _ ->
        None
  in
  match
    walk ~ends_with:rule ?loop_from subject
      ~prefer:(fun _ -> true)
      wanted { consume = trace }
  with
  | Ok trace ->
      trace
  | Error error ->
      invalid_arg
        ("Check.export: the start of the check raised "
       ^ Printexc.to_string error
        )

let view_violations ?rule ~state view =
  List.filter_map
    (fun (finding : Check_rules.finding) ->
      match rule with
      | Some rule when rule <> finding.rule ->
          None
      | _ ->
          Some (fst (Check_rules.render finding ~state))
    )
    (Check_rules.view_rules view @ Check_accessibility.view_rules view)

let not_replayable index text = Error (Printf.sprintf "step %d %s" index text)

let script_action (type n s ev e) (model : (n, s, ev, e) Check_model.model)
    ~props_index ~index ~clock (point : (n, s, ev, e) point) =
  match (point.replayed.step, point.before) with
  | Some step, Some (node, view, move) -> (
      let target role name element =
        let matching =
          List.filter
            (fun item ->
              (not (Accessibility.hidden item))
              && Option.map Accessibility.role_to_string
                   (Accessibility.role item)
                 = role
              && Accessibility.name item = name
            )
            (Accessibility.elements view)
        in
        let position =
          match matching with
          | [] | [ _ ] ->
              None
          | several ->
              List.find_index
                (fun item -> Accessibility.path item = element)
                several
        in
        { Check_script.role; name; position }
      in
      match step.cause with
      | View { handler; element; role; name } -> (
          if role = None && name = "" then
            not_replayable index
              ("presses the element at " ^ element
             ^ ", which has no role and no text, so a role query cannot find it"
              )
          else
            let target = target role name element in
            match String.split_on_char ' ' handler with
            | "input" :: _ ->
                Ok
                  (Check_script.Input
                     (target, String.sub handler 6 (String.length handler - 6))
                  )
            | [ "press" ] ->
                Ok (Check_script.Press target)
            | [ "double_press" ] ->
                Ok (Double_press target)
            | [ "submit" ] ->
                Ok (Submit target)
            | [ "focus" ] ->
                Ok (Focus target)
            | [ "blur" ] ->
                Ok (Blur target)
            | [ "pointer_enter" ] ->
                Ok (Hover target)
            | [ "pointer_leave" ] ->
                Ok (Leave target)
            | [ "key_down"; key ] ->
                Ok (Key_down (target, key))
            | [ "key_up"; key ] ->
                Ok (Key_up (target, key))
            | _ ->
                not_replayable index
                  ("is the handler " ^ handler
                 ^ ", which Silica_memory has no input for"
                  )
        )
      | Timer { id; ms } -> (
          let now, running = !clock in
          match List.assoc_opt id running with
          | None ->
              not_replayable index
                ("fires the timer " ^ id ^ ", which is not running")
          | Some (due, order) -> (
              match
                List.find_opt
                  (fun (other, (other_due, other_order)) ->
                    other <> id
                    && (other_due < due
                       || (other_due = due && other_order < order)
                       )
                  )
                  running
              with
              | Some (other, (other_due, _)) ->
                  not_replayable index
                    (Printf.sprintf
                       "fires the timer %s (%d ms) before the timer %s, which \
                        is due %d ms earlier in real time. The checker lets \
                        running timers fire in any order; Silica_memory keeps \
                        real time, so it cannot replay this order"
                       id ms other (due - other_due)
                    )
              | None ->
                  clock := (due, List.remove_assoc id running);
                  Ok (Advance (due - now))
            )
        )
      | Answer { path; _ } -> (
          match model.answer node move with
          | Some answer ->
              Ok (Reply { path; answer })
          | None ->
              not_replayable index ("answers " ^ path ^ " with no answer data")
        )
      | Props -> (
          match props_index move.event with
          | Some position ->
              Ok (Set_props position)
          | None ->
              not_replayable index "gives props that the check does not list"
        )
      | Navigation -> (
          match step.event with
          | "back" ->
              Ok Back
          | "forward" ->
              Ok Forward
          | event ->
              not_replayable index ("is the navigation " ^ event)
        )
      | Input ->
          not_replayable index
            ("is the event " ^ step.event
           ^ " from ~inputs of the check; no element of the view gives it, so \
              Silica_memory cannot replay it. When it is text typed into a \
              field, give the text with ~texts instead, and the checker types \
              it into the field"
            )
      | Live { topic; _ } ->
          not_replayable index
            ("is an update of the Live topic " ^ topic
           ^ "; Silica_memory does not replay Live updates from a trace"
            )
      | Held { source; _ } ->
          not_replayable index
            ("comes from the held source " ^ source
           ^ "; Silica_memory does not replay held sources from a trace"
            )
      | Action_result action ->
          not_replayable index
            ("is the result of the action " ^ action
           ^ ", which Silica_memory gives at once, not as a step of its own"
            )
      | Child { machine; _ } ->
          not_replayable index ("comes from the child " ^ machine)
      | Address url ->
          Ok (Check_script.Open_url url)
    )
  | _ ->
      not_replayable index "is the start"

let track_timers clock ~index (point : (_, _, _, _) point) =
  let now, running = !clock in
  let running =
    List.fold_left
      (fun running (action : _ Action.host) ->
        match action with
        | Start_timer { id; ms; _ } ->
            (id, (now + ms, index)) :: List.remove_assoc id running
        | Stop id ->
            List.remove_assoc id running
        | Run _ | Start_source _ | Start_call _ | Start_live _ ->
            running
      )
      running point.host
  in
  clock := (now, running)

let script (type s ev) ?component ?check:expression
    (Check check as subject : (s, ev) t) (trace : Trace.t) =
  let ( let* ) = Result.bind in
  let* rule =
    match trace.header.origin with
    | Checker { rule; _ } ->
        Ok rule
    | Runtime { platform } ->
        Error
          ("the trace is a run of the runtime (" ^ platform
         ^ "), not a counterexample of the checker; a script needs the rule \
            that a counterexample breaks"
          )
  in
  let* () =
    if trace.header.root = Lazy.force check.root then
      Ok ()
    else
      Error
        (Printf.sprintf "the trace is a run of %s, and the check is of %s"
           trace.header.root (Lazy.force check.root)
        )
  in
  let* start, records = root_steps ~listed:check.listed trace in
  let* wanted = wanted_of subject trace records in
  let component =
    match component with
    | Some component ->
        component
    | None -> (
        Check_script.module_of_id (Lazy.force check.root)
        ^
        match check.subject with
        | Routed ->
            ".app"
        | Plain | With_props ->
            ".component"
      )
  in
  let expression =
    match expression with
    | Some expression ->
        expression
    | None ->
        "Checks." ^ Check_script.value_of_id (Lazy.force check.root)
  in
  let start_index = Option.value (start_of trace) ~default:0 in
  let props_index input =
    List.find_index (fun other -> other = input) (check.starts ())
  in
  ignore start;
  let mount =
    match check.subject with
    | Plain ->
        Check_script.Plain
    | With_props ->
        Check_script.With_props start_index
    | Routed ->
        Routed (Option.value trace.header.url ~default:"/")
  in
  let build (type n e) (model : (n, s, ev, e) Check_model.model) ~start:_ =
    function
    | Unfaithful reason ->
        Error reason
    | Stuck { at; _ } ->
        Error
          (Printf.sprintf
             "step %d of the trace is not enabled in the check; run the replay \
              command to see the steps that are"
             at
          )
    | Walked (points : (n, s, ev, e) point list) -> (
        let clock = ref (0, []) in
        let rec actions index acc = function
          | [] ->
              Ok (List.rev acc)
          | point :: rest ->
              let* action =
                if index = 0 then
                  Ok None
                else
                  Result.map Option.some
                    (script_action model ~props_index ~index ~clock point)
              in
              track_timers clock ~index point;
              actions (index + 1)
                (match action with Some action -> action :: acc | None -> acc)
                rest
        in
        let* actions = actions 0 [] points in
        let last = List.nth points (List.length points - 1) in
        match
          List.find_opt
            (fun (failure : failure) -> failure.rule = rule)
            last.replayed.failing
        with
        | None ->
            Error
              ("the replay of the trace in the check no longer breaks " ^ rule
             ^ "; run the replay command to see each step"
              )
        | Some failure ->
            let view_rule =
              List.exists
                (fun (known : rule) ->
                  known.id = rule && known.applies_to = "view"
                  && rule <> "dead-end" && rule <> "link-parses"
                )
                rules
            in
            let actions, ending =
              match (rule, List.rev actions) with
              | "ignored-event", last_action :: earlier ->
                  (List.rev earlier, Check_script.Handled last_action)
              | "raised", _ ->
                  (actions, No_error)
              | _ ->
                  if view_rule then
                    (actions, View_rule)
                  else
                    ( actions,
                      Holds
                        {
                          cycle_from = loop_start trace records;
                          start =
                            ( match check.subject with
                            | With_props when List.length (check.starts ()) > 1
                              ->
                                Some start_index
                            | With_props | Plain | Routed ->
                                None
                            );
                        }
                    )
            in
            Ok
              (Check_script.to_string
                 {
                   check = expression;
                   component;
                   mount;
                   actions;
                   rule;
                   state = last.replayed.state_name;
                   message = failure.message;
                   hint = failure.hint;
                   ending;
                 }
              )
      )
  in
  match
    walk ~ends_with:rule ?loop_from:(loop_start trace records)
      ?start:(start_of trace) subject
      ~prefer:(fun props -> props = start.props)
      wanted { consume = build }
  with
  | Ok result ->
      result
  | Error error ->
      Error ("the start of the check raised " ^ Printexc.to_string error)

type difference =
  | Different_step of { logged : string; checked : string }
  | Property_fails of {
      property : string;
      message : string;
      hint : string;
      snapshot : Silica_json.Json.t;
      view : string;
    }
  | Not_decodable of { error : string }

type deviation = {
  seq : int;
  machine : string;
  instance : string;
  difference : difference;
}

type conformance = {
  steps : int list;
  snapshots : int list;
  deviations : deviation list;
}

exception View_unavailable

type target =
  | Target : {
      machine : ('p, 'm, 'e) Machine.t;
      routed : bool;
      view : url:string option -> 'p -> 'm -> 'e Node.t;
      findings :
        url:string option ->
        snapshot:'m ->
        view:'e Node.t Lazy.t ->
        event:'e option ->
        name:string ->
        from:string option ->
        before:'m option ->
        Check_rules.finding list;
    }
      -> target

let root_target (Root root) =
  Target
    {
      machine = (root.component ~url:None).machine;
      routed = root.routed;
      view = (fun ~url props state -> (root.component ~url).view props state);
      findings =
        (fun ~url ~snapshot ~view ~event ~name ~from ~before ->
          List.concat_map
            (function
              | Always { name = property; hint; condition } ->
                  property_finding ~name:property ~hint ~condition
                    {
                      snapshot = root.lift snapshot ~url;
                      view;
                      event;
                      name;
                      from;
                      before =
                        Option.map (fun before -> root.lift before ~url) before;
                      actions = Lazy.from_val [];
                    }
              | Never_after _ | Leads_to _ ->
                  []
              )
            root.properties
        );
    }

let child_target (Check_model.Child listed) =
  Target
    {
      machine = listed.component.machine;
      routed = false;
      view = (fun ~url:_ props state -> listed.component.view props state);
      findings =
        (fun ~url:_ ~snapshot ~view ~event ~name ~from:_ ~before:_ ->
          listed.check ~snapshot ~view:(Lazy.force view) ~event ~name
        );
    }

type kept = {
  props : Silica_json.Json.t option;
  snapshot : Silica_json.Json.t option;
}

let change_of_how : Machine.how -> Transition.change = function
  | Unhandled ->
      Unhandled
  | Kept ->
      Kept
  | Entered ->
      Entered

let decode codec json =
  Result.bind codec (fun (codec : _ Codec.t) -> codec.decode json)

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

let step_text ~from ~target ~change snapshot =
  Printf.sprintf "%s -> %s (%s)%s"
    (match from with "" -> "start" | from -> from)
    target (change_text change)
    (match snapshot with Some text -> ", snapshot " ^ text | None -> "")

let conform_record (Target target) ~allow ~url ~(before : kept)
    (record : Transition.t) =
  let machine = target.machine in
  let props_json =
    match record.props with Some _ as props -> props | None -> before.props
  in
  let props_of json =
    match (Machine.default_props machine, json) with
    | Some props, _ ->
        Some props
    | None, Some json ->
        Result.to_option (decode (Machine.props_codec machine) json)
    | None, None ->
        None
  in
  let state_text state =
    Result.to_option
      (Result.map
         (fun (codec : _ Codec.t) ->
           Silica_json.Json.to_string (codec.encode state)
         )
         (Machine.state_codec machine)
      )
  in
  let recorded = Option.map Silica_json.Json.to_string record.snapshot in
  let logged =
    step_text ~from:record.from ~target:record.target ~change:record.change
      recorded
  in
  let compare ~change state =
    let name = Machine.name machine state in
    let text = Option.bind recorded (fun _ -> state_text state) in
    if
      name = record.target && change = record.change
      && (text = None || text = recorded)
    then
      []
    else
      [
        Different_step
          {
            logged;
            checked = step_text ~from:record.from ~target:name ~change text;
          };
      ]
  in
  let run work =
    match work () with
    | differences ->
        Some differences
    | exception exn ->
        Some
          [
            Different_step
              { logged; checked = "the code raised " ^ Printexc.to_string exn };
          ]
  in
  let snapshot_before () =
    Option.bind before.snapshot (fun json ->
        Result.to_option (decode (Machine.state_codec machine) json)
    )
  in
  let event, event_problems =
    match record.data with
    | None ->
        (None, [])
    | Some json -> (
        match decode (Machine.event_codec machine) json with
        | Ok event ->
            (Some event, [])
        | Error error ->
            (None, [ Not_decodable { error = "the event: " ^ error } ])
      )
  in
  let step =
    match (record.change, record.cause, event) with
    | Started, _, _ when target.routed ->
        None
    | Started, _, _ ->
        Option.bind (props_of props_json) (fun props ->
            run (fun () ->
                compare ~change:Started (fst (Machine.start machine props))
            )
        )
    | (Kept | Entered | Unhandled), Props, _ -> (
        match
          (snapshot_before (), props_of before.props, props_of props_json)
        with
        | Some state, Some previous, Some props ->
            run (fun () ->
                let state, _, how =
                  Machine.set_props machine ~previous props state
                in
                compare ~change:(change_of_how how) state
            )
        | _ ->
            None
      )
    | (Kept | Entered | Unhandled), _, Some event -> (
        match (snapshot_before (), props_of props_json) with
        | Some state, Some props ->
            run (fun () ->
                let state, _, how = Machine.step machine props state event in
                compare ~change:(change_of_how how) state
            )
        | _ ->
            None
      )
    | (Resumed | Ended | Kept | Entered | Unhandled), _, _ ->
        None
  in
  let checked_snapshot, property_problems =
    match record.snapshot with
    | None ->
        (false, [])
    | Some json -> (
        match decode (Machine.state_codec machine) json with
        | Error error ->
            (false, [ Not_decodable { error = "the snapshot: " ^ error } ])
        | Ok snapshot -> (
            let view =
              lazy
                ( match props_of props_json with
                | Some props ->
                    target.view ~url props snapshot
                | None ->
                    raise View_unavailable
                )
            in
            match
              target.findings ~url ~snapshot ~view ~event ~name:record.target
                ~from:(match record.from with "" -> None | from -> Some from)
                ~before:(snapshot_before ())
            with
            | findings ->
                let findings =
                  List.filter
                    (fun (finding : Check_rules.finding) ->
                      not
                        (List.exists
                           (fun allowance ->
                             Check_rules.allows allowance ~rule:finding.rule
                               ~state:record.target ~element:None
                           )
                           allow
                        )
                    )
                    findings
                in
                ( true,
                  List.map
                    (fun (finding : Check_rules.finding) ->
                      let message, hint =
                        Check_rules.render finding ~state:record.target
                      in
                      Property_fails
                        {
                          property = finding.rule;
                          message;
                          hint;
                          snapshot = json;
                          view =
                            ( match Lazy.force view with
                            | view ->
                                View_text.to_string view
                            | exception _ ->
                                ""
                            );
                        }
                    )
                    findings
                )
            | exception View_unavailable ->
                (false, [])
            | exception exn ->
                ( true,
                  [
                    Property_fails
                      {
                        property = "raised";
                        message =
                          "A property raised " ^ Printexc.to_string exn ^ ".";
                        hint = "Make the properties total on every snapshot.";
                        snapshot = json;
                        view = "";
                      };
                  ]
                )
          )
      )
  in
  ( { props = props_json; snapshot = record.snapshot },
    Option.is_some step,
    checked_snapshot,
    event_problems @ Option.value step ~default:[] @ property_problems
  )

let conforms (type s ev) (Check check : (s, ev) t) (trace : Trace.t) =
  let root = Lazy.force check.root in
  let targets =
    (root, root_target check.root_of)
    :: List.map
         (fun (Check_model.Child listed as child) ->
           (Machine.id listed.component.machine, child_target child)
         )
         check.children
  in
  let instances = Hashtbl.create 16 in
  let url = ref trace.header.url in
  let steps = ref [] and snapshots = ref [] in
  let deviations =
    List.concat_map
      (fun (record : Transition.t) ->
        ( match record.cause with
        | Url address when record.machine = root ->
            url := Some address
        | Url _
        | Start
        | Element _
        | Timer _
        | Answer _
        | Live _
        | Held _
        | Action_result _
        | Props
        | Child _
        | Preload _
        | Message
        | Ended ->
            ()
        );
        match List.assoc_opt record.machine targets with
        | None ->
            []
        | Some target ->
            let before =
              Option.value
                (Hashtbl.find_opt instances record.instance)
                ~default:{ props = None; snapshot = None }
            in
            let kept, stepped, tested, differences =
              conform_record target ~allow:check.allow ~url:!url ~before record
            in
            if stepped then steps := record.seq :: !steps;
            if tested then snapshots := record.seq :: !snapshots;
            ( match record.change with
            | Ended ->
                Hashtbl.remove instances record.instance
            | Started | Resumed | Unhandled | Kept | Entered ->
                Hashtbl.replace instances record.instance kept
            );
            List.map
              (fun difference ->
                {
                  seq = record.seq;
                  machine = record.machine;
                  instance = record.instance;
                  difference;
                }
              )
              differences
      )
      trace.transitions
  in
  { steps = List.rev !steps; snapshots = List.rev !snapshots; deviations }

let difference_to_string = function
  | Different_step { logged; checked } ->
      Printf.sprintf "the trace has %s; the code gives %s" logged checked
  | Property_fails { message; _ } ->
      message
  | Not_decodable { error } ->
      "the data does not decode: " ^ error
