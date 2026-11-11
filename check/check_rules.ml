open Silica

type rule = {
  id : string;
  message : string;
  hint : string;
  applies_to : string;
}

let silica_bug = "This is a Silica bug: report it with this trace."

let rules =
  [
    {
      id = "accessible-name";
      message =
        "In state {state}, the {role} at {element} can be pressed but has no \
         accessible name.";
      hint =
        "Give the element at {element} visible text, an aria-label, or an \
         aria-labelledby that points to an element with text.";
      applies_to = "view";
    };
    {
      id = "unique-refs";
      message =
        "In state {state}, the ref {ref} is on more than one element; the \
         second one is at {element}.";
      hint =
        "Make one Ref.make for each element, or render the element with the \
         ref {ref} once.";
      applies_to = "view";
    };
    {
      id = "focus-target";
      message =
        "The step {event} from state {from} runs {action} on the ref {ref}, \
         but the view of state {state} has no element with that ref.";
      hint =
        "Render an element with the ref {ref} in state {state}, or remove the \
         {action} action from this step.";
      applies_to = "step";
    };
    {
      id = "boundary-reset";
      message =
        "In state {state}, the fallback of a boundary has no element with the \
         reset attribute.";
      hint =
        "Put the ~reset attribute that the fallback function gets on a button \
         of the fallback, so that the user can try again.";
      applies_to = "view";
    };
    {
      id = "dead-end";
      message =
        "State {state} is not final, and nothing can happen in it: the view \
         offers no handler, and no timer, call, subscription, source or input \
         gives an event.";
      hint =
        "Add a button, a timer or an input that leaves {state}, or mark \
         {state} final with ~final when the machine is done there.";
      applies_to = "state";
    };
    {
      id = "timer-owner";
      message =
        "In state {state}, the timer {resource} runs, but the state does not \
         own it.";
      hint = silica_bug;
      applies_to = "state";
    };
    {
      id = "call-owner";
      message =
        "In state {state}, the call or subscription {resource} runs, but the \
         state does not own it.";
      hint = silica_bug;
      applies_to = "state";
    };
    {
      id = "ignored-event";
      message =
        "In state {state}, {cause} gives the event {event}, and the update \
         returns Ignore.";
      hint =
        "Handle {event} in state {state}, or do not offer it there: disable \
         the element with Machine.can, or leave its timer or call out of owns \
         of {state}.";
      applies_to = "step";
    };
    {
      id = "raised";
      message = "The {where} raised {exception} in state {state}.";
      hint =
        "Fix the exception in the {where}; the trace reproduces it. Keep an \
         expected failure in the state, not in an exception.";
      applies_to = "step";
    };
    {
      id = "unreached-state";
      message = "No explored path reaches state {state}.";
      hint =
        "Add samples, inputs or props that reach {state}, or allow it with \
         Check.allow ~rule:\"unreached-state\" ~state:\"{state}\" ~reason.";
      applies_to = "machine";
    };
    {
      id = "live-uses-client-source";
      message =
        "In state {state}, the Live component owns the client-only source \
         {source}, but a Live machine runs on the server.";
      hint =
        "Move the {source} source into a client component, or remove it from \
         owns of {state}.";
      applies_to = "state";
    };
    {
      id = "live-uses-client-action";
      message =
        "The step {event} from state {from} runs {action}, which a Live \
         machine cannot run: the protocol carries no answer of the client back \
         to the server.";
      hint = "Run {action} in a client component, or remove it from this step.";
      applies_to = "step";
    };
    {
      id = "route-round-trip";
      message =
        "In state {state}, the route prints as {url}, and parsing {url} does \
         not give the same route back.";
      hint =
        "Make the parse and the print of the route agree, for example the \
         constructor and the data function of its Router.route.";
      applies_to = "route";
    };
    {
      id = "link-parses";
      message =
        "In state {state}, the link at {element} goes to {href}, {problem}.";
      hint =
        "Make the link with Router.link, or write the href that the router \
         prints for the route.";
      applies_to = "view";
    };
    {
      id = "state-limit";
      message =
        "The search stopped at {states} states, so the check is incomplete.";
      hint = "Bound the search with ~within, or raise ~max_states.";
      applies_to = "search";
    };
  ]

let property_message =
  "The property \"{property}\" does not hold in state {state}."

let property_hint =
  "Change the update or the view so that \"{property}\" holds in state \
   {state}, or correct the property."

type allowance = {
  rule : string;
  state : string option;
  element : string option;
  reason : string;
}

let allows (allowance : allowance) ~rule ~state ~element =
  allowance.rule = rule
  && (match allowance.state with None -> true | Some name -> name = state)
  &&
  match allowance.element with
  | None ->
      true
  | Some path ->
      element = Some path

type finding = {
  rule : string;
  element : string option;
  values : (string * string) list;
  custom : (string * string) option;
}

let finding ?element ?(values = []) rule =
  { rule; element; values; custom = None }

let fill values template =
  let buffer = Buffer.create (String.length template + 32) in
  let length = String.length template in
  let rec go index =
    if index < length then
      match template.[index] with
      | '{' -> (
          match String.index_from_opt template index '}' with
          | Some stop -> (
              let key = String.sub template (index + 1) (stop - index - 1) in
              match List.assoc_opt key values with
              | Some value ->
                  Buffer.add_string buffer value;
                  go (stop + 1)
              | None ->
                  Buffer.add_char buffer '{';
                  go (index + 1)
            )
          | None ->
              Buffer.add_char buffer '{';
              go (index + 1)
        )
      | character ->
          Buffer.add_char buffer character;
          go (index + 1)
  in
  go 0;
  Buffer.contents buffer

let render finding ~state =
  let values =
    ("state", state)
    ::
    ( match finding.element with
    | Some element ->
        [ ("element", element) ]
    | None ->
        []
    )
    @ finding.values
  in
  let message, hint =
    match finding.custom with
    | Some texts ->
        texts
    | None -> (
        match List.find_opt (fun rule -> rule.id = finding.rule) rules with
        | Some rule ->
            (rule.message, rule.hint)
        | None ->
            (property_message, property_hint)
      )
  in
  (fill values message, fill values hint)

let rec iter f node =
  let shape = Node.view node in
  f shape;
  match shape with
  | Node.Element (_, _, children) | Node.List children ->
      List.iter (iter f) children
  | Node.Keyed (_, child) ->
      iter f child
  | Node.Portal (_, children) ->
      List.iter (iter f) children
  | Node.Boundary { body; _ } ->
      iter f body
  | Node.Chunk chunk ->
      iter f (Node.chunk_view chunk)
  | Node.Text _ | Node.Component _ ->
      ()

let collect f node =
  let acc = ref [] in
  iter (fun shape -> acc := List.rev_append (f shape) !acc) node;
  List.rev !acc

let dedup items =
  let seen = Hashtbl.create 16 in
  List.filter
    (fun item ->
      let hash = Hashtbl.hash item in
      let bucket = Option.value ~default:[] (Hashtbl.find_opt seen hash) in
      if List.mem item bucket then
        false
      else (
        Hashtbl.replace seen hash (item :: bucket);
        true
      )
    )
    items

let default_keys =
  [ Key.escape; Key.enter; Key.tab; Key.arrow_up; Key.arrow_down ]

let rec attr_events ~keys = function
  | Node.On_press event
  | Node.On_double_press event
  | Node.On_submit event
  | Node.On_focus event
  | Node.On_blur event ->
      [ event ]
  | Node.On_key_down handler | Node.On_key_up handler ->
      List.filter_map handler.run keys
  | Node.On_pointer_enter handler | Node.On_pointer_leave handler ->
      Option.to_list (handler.run Pointer.origin)
  | Node.Rated (_, attr) ->
      attr_events ~keys attr
  | Node.String _ | Node.Bool _ | Node.Ref _ | Node.On_input _ | Node.On_reset
    ->
      []

let rec attr_handlers ~keys = function
  | Node.On_press event ->
      [ ("press", event) ]
  | Node.On_double_press event ->
      [ ("double_press", event) ]
  | Node.On_submit event ->
      [ ("submit", event) ]
  | Node.On_focus event ->
      [ ("focus", event) ]
  | Node.On_blur event ->
      [ ("blur", event) ]
  | Node.On_key_down handler ->
      List.filter_map
        (fun (key : Key.t) ->
          Option.map
            (fun event -> ("key_down " ^ key.key, event))
            (handler.run key)
        )
        keys
  | Node.On_key_up handler ->
      List.filter_map
        (fun (key : Key.t) ->
          Option.map
            (fun event -> ("key_up " ^ key.key, event))
            (handler.run key)
        )
        keys
  | Node.On_pointer_enter handler ->
      List.map
        (fun event -> ("pointer_enter", event))
        (Option.to_list (handler.run Pointer.origin))
  | Node.On_pointer_leave handler ->
      List.map
        (fun event -> ("pointer_leave", event))
        (Option.to_list (handler.run Pointer.origin))
  | Node.Rated (_, attr) ->
      attr_handlers ~keys attr
  | Node.String _ | Node.Bool _ | Node.Ref _ | Node.On_input _ | Node.On_reset
    ->
      []

let is_disabled attrs = List.mem (Node.Bool ("disabled", true)) attrs

let handlers ~keys node =
  dedup
    (collect
       (function
         | Node.Element (_, attrs, _) when not (is_disabled attrs) ->
             List.concat_map (attr_events ~keys) attrs
         | _ ->
             []
         )
       node
    )

type located = {
  handler : string;
  element : string;
  role : string option;
  name : string;
}

let locate ~keys view matches =
  List.find_map
    (fun element ->
      let attrs = Accessibility.attributes element in
      if is_disabled attrs then
        None
      else
        List.find_map
          (fun attr ->
            List.find_map
              (fun (handler, event) ->
                if matches event then
                  Some
                    {
                      handler;
                      element = Accessibility.path element;
                      role =
                        Option.map Accessibility.role_to_string
                          (Accessibility.role element);
                      name = Accessibility.name element;
                    }
                else
                  None
              )
              (attr_handlers ~keys attr)
          )
          attrs
    )
    (Accessibility.elements view)

let is_link attrs =
  List.exists
    (function
      | Node.String ("data-silica-link", _)
      | Node.Bool ("data-silica-link", true) ->
          true
      | _ ->
          false
      )
    attrs

let links node =
  collect
    (function
      | Node.Element ("a", attrs, _) when is_link attrs ->
          List.filter_map
            (function Node.String ("href", href) -> Some href | _ -> None)
            attrs
      | _ ->
          []
      )
    node

let locate_link view href =
  List.find_map
    (fun element ->
      if
        Accessibility.tag element = "a"
        && is_link (Accessibility.attributes element)
        && Accessibility.attribute element "href" = Some href
      then
        Some
          {
            handler = "press";
            element = Accessibility.path element;
            role =
              Option.map Accessibility.role_to_string
                (Accessibility.role element);
            name = Accessibility.name element;
          }
      else
        None
    )
    (Accessibility.elements view)

let not_explored_of_view node =
  collect
    (function
      | Node.Component child ->
          [ "component " ^ Machine.id child.component.machine ]
      | Node.Element (_, attrs, _) ->
          List.filter_map
            (function
              | Node.String ("data-silica-live", channel) ->
                  Some ("island " ^ channel)
              | _ ->
                  None
              )
            attrs
      | _ ->
          []
      )
    node

let pressable attrs =
  List.exists
    (function Node.On_press _ | Node.On_double_press _ -> true | _ -> false)
    attrs

let may_need_name node =
  let found = ref false in
  iter
    (function
      | Node.Element (tag, attrs, _)
        when tag = "button" || tag = "a" || pressable attrs ->
          found := true
      | _ ->
          ()
      )
    node;
  !found

let accessible_name view =
  if not (may_need_name view) then
    []
  else
    List.filter_map
      (fun element ->
        let tag = Accessibility.tag element in
        if
          (tag = "button" || tag = "a"
          || pressable (Accessibility.attributes element)
          )
          && (not (Accessibility.hidden element))
          && Accessibility.name element = ""
        then
          Some
            (finding "accessible-name"
               ~element:(Accessibility.path element)
               ~values:
                 [
                   ( "role",
                     match Accessibility.role element with
                     | Some role ->
                         Accessibility.role_to_string role
                     | None ->
                         tag
                   );
                 ]
            )
        else
          None
      )
      (Accessibility.elements view)

let ref_names node =
  collect
    (function
      | Node.Element (_, attrs, _) ->
          List.filter_map
            (function
              | Node.Ref reference -> Some (Ref.name reference) | _ -> None
              )
            attrs
      | _ ->
          []
      )
    node

let unique_refs view =
  let names = ref_names view in
  if List.length (List.sort_uniq String.compare names) = List.length names then
    []
  else
    let seen = Hashtbl.create 8 in
    List.filter_map
      (fun element ->
        match Accessibility.attribute element "data-silica-ref" with
        | Some name when Hashtbl.mem seen name ->
            Some
              (finding "unique-refs"
                 ~element:(Accessibility.path element)
                 ~values:[ ("ref", name) ]
              )
        | Some name ->
            Hashtbl.replace seen name ();
            None
        | None ->
            None
      )
      (Accessibility.elements view)
    |> function
    | [] ->
        [ finding "unique-refs" ~values:[ ("ref", "?"); ("element", "?") ] ]
    | findings ->
        findings

let rec has_reset node =
  match Node.view node with
  | Node.Element (_, attrs, children) ->
      List.exists
        (function
          | Node.On_reset | Node.Rated (_, Node.On_reset) -> true | _ -> false
          )
        attrs
      || List.exists has_reset children
  | Node.List children | Node.Portal (_, children) ->
      List.exists has_reset children
  | Node.Keyed (_, child) ->
      has_reset child
  | Node.Boundary { body; _ } ->
      has_reset body
  | Node.Chunk chunk ->
      has_reset (Node.chunk_view chunk)
  | Node.Text _ | Node.Component _ ->
      false

let sample_error = { Node.id = "000000"; message = "error"; detail = None }

let boundary_reset view =
  if
    List.for_all Fun.id
      (collect
         (function
           | Node.Boundary { fallback; _ } ->
               [ has_reset (fallback sample_error ~reset:Node.On_reset) ]
           | _ ->
               []
           )
         view
      )
  then
    []
  else
    [ finding "boundary-reset" ]

let view_rules view =
  accessible_name view @ unique_refs view @ boundary_reset view

let action_name : type e. e Action.t -> string = function
  | Action.Focus _ ->
      "Focus"
  | Action.Blur _ ->
      "Blur"
  | Action.Scroll_into_view _ ->
      "Scroll_into_view"
  | Action.Scroll_to _ ->
      "Scroll_to"
  | Action.Measure _ ->
      "Measure"
  | Action.Copy _ ->
      "Copy"
  | Action.Load_script _ ->
      "Load_script"
  | Action.Load_chunk _ ->
      "Load_chunk"
  | Action.Navigate _ ->
      "Navigate"
  | Action.Replace _ ->
      "Replace"
  | Action.Back ->
      "Back"
  | Action.Forward ->
      "Forward"
  | Action.Reload ->
      "Reload"
  | Action.Set_title _ ->
      "Set_title"
  | Action.Set_status _ ->
      "Set_status"
  | Action.Set_document_attribute _ ->
      "Set_document_attribute"
  | Action.Set_cookie _ ->
      "Set_cookie"
  | Action.Resume _ ->
      "Resume"
  | Action.Revert_navigation ->
      "Revert_navigation"
  | Action.Preload _ ->
      "Preload"
  | Action.Remount ->
      "Remount"

let focus_target ~values view actions =
  let targets =
    List.filter_map
      (function
        | Action.Run
            ( ( Action.Focus target
              | Action.Blur target
              | Action.Scroll_into_view target
              | Action.Scroll_to { target = Some target; _ } ) as action
            ) ->
            Some (action_name action, Ref.name target)
        | _ ->
            None
        )
      actions
  in
  match targets with
  | [] ->
      []
  | _ ->
      let present = ref_names (Lazy.force view) in
      List.filter_map
        (fun (action, name) ->
          if List.mem name present then
            None
          else
            Some
              (finding "focus-target"
                 ~values:(("action", action) :: ("ref", name) :: values)
              )
        )
        targets

let server_action = function
  | Action.Run
      ( ( Action.Measure _
        | Action.Copy _
        | Action.Load_script _
        | Action.Resume _
        | Action.Revert_navigation
        | Action.Preload _
        | Action.Load_chunk _
        | Action.Set_status _
        | Action.Set_document_attribute _
        | Action.Set_cookie _ ) as action
      ) ->
      Some (action_name action)
  | _ ->
      None

let client_actions ~values actions =
  List.filter_map
    (fun action ->
      Option.map
        (fun name ->
          finding "live-uses-client-action" ~values:(("action", name) :: values)
        )
        (server_action action)
    )
    actions
