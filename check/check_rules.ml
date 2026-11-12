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
        "In state {state}, the {role} at {element} has no accessible name, so \
         a screen reader cannot say what it does.";
      hint =
        "Give the {role} at {element} visible text, an aria-label, or an \
         aria-labelledby that points to an element with text.";
      applies_to = "view";
    };
    {
      id = "field-label";
      message = "In state {state}, the {role} field at {element} has no label.";
      hint =
        "Give the field at {element} a <label for> with its id, a <label> \
         around it, an aria-label, or an aria-labelledby.";
      applies_to = "view";
    };
    {
      id = "img-alt";
      message = "In state {state}, the image at {element} has no alt text.";
      hint =
        "Give the image at {element} an alt that says what it shows, or \
         alt=\"\" when it is decoration.";
      applies_to = "view";
    };
    {
      id = "unique-ids";
      message =
        "In state {state}, the id {id} is on more than one element; the second \
         one is the {role} {name} at {element}.";
      hint =
        "Make the id {id} unique in the view, for example with the instance \
         key or a value of the props in it.";
      applies_to = "view";
    };
    {
      id = "aria-reference";
      message =
        "In state {state}, the {attribute} of the {role} {name} at {element} \
         names the id {id}, which is not in the view.";
      hint =
        "Render an element with the id {id} in state {state}, or remove {id} \
         from the {attribute} of the element at {element}.";
      applies_to = "view";
    };
    {
      id = "valid-role";
      message =
        "In state {state}, the element at {element} has role=\"{value}\", \
         {problem}.";
      hint =
        "Use an ARIA role at {element} and give it the attributes that the \
         role needs: aria-checked for checkbox, radio, switch, \
         menuitemcheckbox and menuitemradio; aria-level for a heading that is \
         not h1 to h6; aria-expanded for combobox; aria-valuenow for slider.";
      applies_to = "view";
    };
    {
      id = "required-children";
      message =
        "In state {state}, the {role} {name} at {element} holds {child}, but a \
         {role} holds only {allowed} items.";
      hint =
        "Give each item of the {role} at {element} the role {allowed}, and \
         move other content out of the {role}, for example next to it.";
      applies_to = "view";
    };
    {
      id = "nested-interactive";
      message =
        "In state {state}, the {role} {name} at {element} holds the \
         {inner_role} {inner_name} at {inner}.";
      hint =
        "Move the {inner_role} at {inner} out of the {role} at {element}: a \
         screen reader reads the {role} as one control and cannot reach the \
         control inside it.";
      applies_to = "view";
    };
    {
      id = "keyboard-access";
      message =
        "In state {state}, the {role} {name} at {element} has a press handler, \
         but {problem}, so a keyboard cannot reach it.";
      hint =
        "Use a <button> at {element}, or an <a href> for a link; or give it a \
         role and tabindex=\"0\", and handle Enter and Space.";
      applies_to = "view";
    };
    {
      id = "dialog-name";
      message =
        "In state {state}, the {role} at {element} has no accessible name.";
      hint =
        "Name the {role} at {element} with an aria-labelledby that points to \
         the id of its title, or with an aria-label.";
      applies_to = "view";
    };
    {
      id = "dialog-focus";
      message =
        "In state {state}, the view shows the modal {role} {name} at \
         {element}, but the state holds no focus trap on it or on an element \
         around it.";
      hint =
        "Own Source.focus_trap with the ref of the {role} at {element} in \
         state {state}, or use Silica_ui.Dialog.";
      applies_to = "state";
    };
    {
      id = "landmark-main";
      message =
        "In state {state}, the main landmark at {element} {problem}; a page \
         has one visible main landmark.";
      hint =
        "Keep one main element (or role=\"main\") for the page, and make the \
         one at {element} a section, a region or a div. A component check sees \
         the view of its component only; a joint check sees the whole page.";
      applies_to = "view";
    };
    {
      id = "heading-order";
      message =
        "In state {state}, the heading {name} at {element} has level {level}, \
         but the heading before it has level {previous}.";
      hint =
        "Give the heading at {element} level {next} or lower, or add the \
         missing levels between.";
      applies_to = "view";
    };
    {
      id = "name-spacing";
      message =
        "In state {state}, the {role} at {element} has the accessible name \
         {name}: {left} and {right} come from different elements, with no \
         space between them.";
      hint =
        "Put a space between the parts of the {role} at {element}, for example \
         a text node \" \" between the elements, or give the {role} an \
         aria-label.";
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
         the element with Machine.can, or leave its timer or call out of \
         during of {state}.";
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
        "Add samples, texts, inputs, props or (in an app) urls that reach \
         {state}, or allow it with Check.allow ~rule:\"unreached-state\" \
         ~state:\"{state}\" ~reason.";
      applies_to = "machine";
    };
    {
      id = "live-uses-client-source";
      message =
        "In state {state}, the Live component runs the client-only source \
         {source}, but a Live machine runs on the server.";
      hint =
        "Move the {source} source into a client component, or remove it from \
         during of {state}.";
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
      hint =
        "Bound the search with ~within, or raise ~max_states; for Check.app, \
         also lower ~max_history.";
      applies_to = "search";
    };
    {
      id = "unchecked-component";
      message =
        "A checked view renders the component {machine}, and no entry of the \
         suite checks it.";
      hint =
        "Add Check_suite.entry for {machine}, or Check_suite.elsewhere with \
         the reason when another suite checks it.";
      applies_to = "suite";
    };
    {
      id = "golden-drift";
      message = "The golden file {file} differs from what the check found.";
      hint =
        "Read the changes. When they are intended, write the files again with \
         {command}, and commit them.";
      applies_to = "suite";
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

let unlisted_message machine =
  Printf.sprintf
    "Unreached states cannot be reported for the machine %s because it gives \
     no ~states to Machine.make."
    machine

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

let rec attr_handlers ~keys ~texts = function
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
  | Node.On_input input ->
      List.map (fun text -> ("input " ^ text, input text)) texts
  | Node.Rated (_, attr) ->
      attr_handlers ~keys ~texts attr
  | Node.String _ | Node.Bool _ | Node.Ref _ | Node.On_reset ->
      []

let has_attribute attrs name =
  List.exists
    (function
      | Node.String (other, _) | Node.Bool (other, true) ->
          other = name
      | _ ->
          false
      )
    attrs

let natively_disabled =
  [ "button"; "input"; "select"; "textarea"; "option"; "optgroup"; "fieldset" ]

let blocks tag attrs =
  has_attribute attrs "hidden"
  || has_attribute attrs "inert"
  || (List.mem tag natively_disabled && has_attribute attrs "disabled")

let rec iter_reachable f ~blocked node =
  let shape = Node.view node in
  match shape with
  | Node.Element (tag, attrs, children) ->
      let blocked = blocked || blocks tag attrs in
      if not blocked then f shape;
      List.iter (iter_reachable f ~blocked) children
  | Node.List children ->
      List.iter (iter_reachable f ~blocked) children
  | Node.Keyed (_, child) ->
      iter_reachable f ~blocked child
  | Node.Portal (_, children) ->
      List.iter (iter_reachable f ~blocked:false) children
  | Node.Boundary { body; _ } ->
      iter_reachable f ~blocked body
  | Node.Chunk chunk ->
      iter_reachable f ~blocked (Node.chunk_view chunk)
  | Node.Text _ | Node.Component _ ->
      ()

let collect_reachable f node =
  let acc = ref [] in
  iter_reachable
    (fun shape -> acc := List.rev_append (f shape) !acc)
    ~blocked:false node;
  List.rev !acc

let blocked_paths view =
  let paths = Hashtbl.create 16 in
  let rec walk ~blocked ~portal items =
    List.iter
      (function
        | Accessibility.Element element ->
            let path = Accessibility.path element in
            let inside = String.starts_with ~prefix:"portal > " path in
            let blocked =
              (blocked && (portal || not inside))
              || blocks
                   (Accessibility.tag element)
                   (Accessibility.attributes element)
            in
            if blocked then Hashtbl.replace paths path ();
            walk ~blocked ~portal:inside (Accessibility.children element)
        | Accessibility.Text _
        | Accessibility.Component _
        | Accessibility.Loading ->
            ()
        )
      items
  in
  walk ~blocked:false ~portal:false (Accessibility.items view);
  paths

let rec attr_texts ~texts = function
  | Node.On_input input ->
      List.map input texts
  | Node.Rated (_, attr) ->
      attr_texts ~texts attr
  | _ ->
      []

let text_field tag attrs =
  match tag with
  | "textarea" ->
      true
  | "input" -> (
      match
        List.find_map
          (function
            | Node.String ("type", value) ->
                Some (String.lowercase_ascii value)
            | _ ->
                None
            )
          attrs
      with
      | None
      | Some
          ("text" | "search" | "email" | "url" | "tel" | "password" | "number")
        ->
          true
      | Some _ ->
          false
    )
  | _ ->
      false

let typed ~texts node =
  dedup
    (collect_reachable
       (function
         | Node.Element (tag, attrs, _) when text_field tag attrs ->
             List.concat_map (attr_texts ~texts) attrs
         | _ ->
             []
         )
       node
    )

let handlers ~keys node =
  dedup
    (collect_reachable
       (function
         | Node.Element (_, attrs, _) ->
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

let locate ~keys ~texts view matches =
  let blocked = blocked_paths view in
  List.find_map
    (fun element ->
      let attrs = Accessibility.attributes element in
      if Hashtbl.mem blocked (Accessibility.path element) then
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
              (attr_handlers ~keys ~texts attr)
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

let reachable_links node =
  collect_reachable
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
  let blocked = blocked_paths view in
  List.find_map
    (fun element ->
      if
        (not (Hashtbl.mem blocked (Accessibility.path element)))
        && Accessibility.tag element = "a"
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

let view_rules view = unique_refs view @ boundary_reset view

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
