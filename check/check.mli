(** Explicit-state model checking of components and routed apps.

    Make a check with {!component}, {!live} or {!app}, and run it with {!run}.
    States, events and props must be first-order values: they are compared and
    hashed structurally, so a function inside them raises {!Invalid_check}. The
    search is breadth first, so each violation carries a shortest trace. It goes
    on after a violation and reports every violation (see {!type-report}). *)

exception Invalid_check of string
(** A state, event or props value holds a function, or the arguments disagree
    (no props for a component with props, no routes, an allowance with no reason
    or with a rule that does not exist). *)

type ('s, 'ev) t
(** A configured check: the subject, its samples, inputs, bounds, properties and
    allowances. ['s] is what properties read: the snapshot for a component, an
    {!app_state} for an app. ['ev] is the input of a step: [('p, 'e) input] for
    a component, ['e app_event] for an app. *)

(** {1 Properties} *)

type ('s, 'e) condition
(** A test of one point of a run: the snapshot, its view, and the event of the
    step that reached it. *)

val state : ('s -> bool) -> ('s, 'e) condition
(** Holds when the snapshot passes the test. *)

val state_named : string -> ('s, 'e) condition
(** Holds when the state of the snapshot has this name (see
    {!Silica.Machine.name}). *)

val view : ('e Silica.Node.t -> bool) -> ('s, 'e) condition
(** Holds when the view of the snapshot passes the test. *)

val holds : ('s -> 'e Silica.Node.t -> bool) -> ('s, 'e) condition
(** Holds when the snapshot and its view pass the test together. *)

val event : ('e -> bool) -> ('s, 'e) condition
(** Holds on the snapshot reached by a step whose event of the machine passes
    the test. The checker visits each snapshot once, through a shortest path, so
    this reads the event of that path. *)

val ( &&& ) : ('s, 'e) condition -> ('s, 'e) condition -> ('s, 'e) condition
val ( ||| ) : ('s, 'e) condition -> ('s, 'e) condition -> ('s, 'e) condition
val not_ : ('s, 'e) condition -> ('s, 'e) condition

type ('s, 'e) property
(** Something that must hold in every run. *)

val always :
  name:string -> ?hint:string -> ('s, 'e) condition -> ('s, 'e) property
(** [always ~name ?hint condition]: [condition] holds on every reached snapshot.
    A violation has the rule [name] and the [hint]; [hint] may name [{state}]
    for the state of the failing snapshot. Without [hint], the hint asks to
    change the update, the view, or the property. *)

(** {1 Allowances} *)

type allowance = Check_rules.allowance = private {
  rule : string;
  state : string option;
  element : string option;
  reason : string;
}
(** A rule that the check does not report, with the reason. *)

val allow :
  rule:string ->
  ?state:string ->
  ?element:string ->
  reason:string ->
  unit ->
  allowance
(** [allow ~rule ?state ?element ~reason ()] turns off [rule] (a built-in rule
    id of {!val-rules} or the name of a property) in state [state] and at the
    element path [element], or everywhere when they are not given. The report
    lists each allowance and how many distinct violations it turned off. Use
    [~rule:"unreached-state" ~state] for a state that no run reaches on purpose.
*)

(** {1 Samples} *)

type sample
(** The answers of an endpoint or the states of a topic that the checker
    delivers. *)

val answers :
  ?decode:bool ->
  ?rejected:'x list ->
  ('i, 'o, 'x) Silica.Remote.endpoint ->
  'o list ->
  sample
(** [answers endpoint values]: a call to [endpoint] can end with [Ok v] for each
    [v] in [values], with [Rejected x] for each [x] in [rejected], with
    [Failed (Http 500)] and with [Failed (Network "down")]. With [decode] it can
    also end with [Failed (Decode "sample")]. A call to an endpoint with no
    [answers] can end with the two failures only. *)

val states : 's Silica.Topic.t -> 's list -> sample
(** [states topic values]: the states that the server can publish on [topic]. A
    subscription of a state (see {!Silica.Source.live}) gives, in this order of
    possibility: [Loading None] once; then up to [arrivals] new states ([Ready]
    with [stale = false], [seq] from 1), each one any of [values]; once, a loss
    of the connection after a state ([Ready] with the last state and
    [stale = true], [seq] 0), after which one more state can arrive (the
    reconnect); and once, a fatal failure
    ([Failed { error = Network "down"; last }]), after which nothing arrives. A
    topic with no [states] gives [Loading None] and the failure only. *)

(** {1 Making a check} *)

(** A step of a component check. *)
type ('p, 'e) input = ('p, 'e) Check_model.input =
  | Event of 'e  (** An event of the machine. *)
  | Props of 'p  (** The parent gives other props. *)

val component :
  ?max_states:int ->
  ?max_violations:int ->
  ?within:('s -> bool) ->
  ?inputs:('s -> 'e list) ->
  ?props:'p list ->
  ?samples:sample list ->
  ?arrivals:int ->
  ?keys:Silica.Key.t list ->
  ?properties:('s, 'e) property list ->
  ?allow:allowance list ->
  ('p, 's, 'e) Silica.Component.t ->
  ('s, ('p, 'e) input) t
(** The check starts once with each of [props] (default: [()] for a machine made
    by {!Silica.Machine.make}; a machine with props needs them), and in every
    state the parent can give any other of [props]. Events in a state are the
    handlers of the rendered view (press, double press, submit, focus, blur,
    pointer enter and leave), the key handlers of the view called with each key
    of [keys] (default: Escape, Enter, Tab, Arrow up, Arrow down), the events of
    the held sources of the state, [inputs s] (use it for [On_input], which
    needs sample strings), the events of running timers, the answers of running
    calls and the updates of running subscriptions (see {!answers} and
    {!val-states}; [arrivals] defaults to 2). A [Global_keys] source gives the
    event of each key in its table, which is the press of that key;
    [Outside_press] gives its event; [Resize] gives [on_resize] with
    {!Silica.Rect.zero}; [Intersection] gives [on_change] with [true] and with
    [false]. [Scroll] gives [on_scroll] with {!Silica.Scroll.zero};
    [Visibility], [Online] and [Media] give their event with [true] and with
    [false]. A [Block] gives [on_attempt] with a push and with a back, both to
    ["/"]. A [Dismiss] layer gives its Escape event, its outside event and its
    focus-out event. A [Focus_trap], [Scroll_lock] and [Anchor] give no event.
    An action with a result event answers once, at any later step and in any
    state: [Measure] with [Some Silica.Rect.zero], [Copy] and [Load_script] with
    [true]. The checker treats {!Silica.Rate} limits as pass-through: every
    value arrives, and none is held back. Elements inside a
    {!Silica.Node.Portal} and the body of a {!Silica.Node.Boundary} are part of
    the view. Time is abstract: a running timer can fire at any step, in any
    order. A timer, call or subscription starts with its start action and stops
    with [Stop]; a call ends with its answer and a timer when it fires. So
    nothing of a state that was left arrives. Handlers on elements with
    [disabled] set are not enabled.

    The built-in rules are in {!val-rules}. [max_states] (default 100000) bounds
    the search; when it stops the search, the report is not [complete]. [within]
    bounds an unbounded state space on purpose: snapshots outside it are neither
    checked nor expanded, and the report holds for the bounded space.
    [max_violations] (default 50) stops the search after that many violations;
    the report is then [truncated]. The checker does not enter component nodes
    in the view: it lists them in [not_explored] and keeps their props in
    [children]. To check a child, make a check of its own machine with [~props]
    from {!props}. *)

val live :
  ?max_states:int ->
  ?max_violations:int ->
  ?within:('s -> bool) ->
  ?inputs:('s -> 'e list) ->
  ?samples:sample list ->
  ?arrivals:int ->
  ?keys:Silica.Key.t list ->
  ?properties:('s, 'e) property list ->
  ?allow:allowance list ->
  ('s, 'e) Silica.Live.t ->
  ('s, (unit, 'e) input) t
(** Like {!component} for a Live component, whose machine runs on the server,
    with two more rules: [live-uses-client-source] (a reachable state owns a
    client-only source: {!Silica.Source.focus_trap}, [scroll_lock],
    [outside_press], [keys], [resize], [intersection], [dismiss], [anchor],
    [scroll], [visibility], [online] or [block]) and [live-uses-client-action]
    (a step runs [Measure], [Copy], [Load_script], [Load_chunk], [Resume],
    [Revert_navigation], [Preload], [Set_status], [Set_document_attribute] or
    [Set_cookie]: the protocol carries no answer of the client back, see
    {!Silica.Protocol.host_of_host_action}). *)

type 's app_state = 's Check_model.app_state = {
  router : 's;  (** The snapshot of the root machine. *)
  history : Silica.Url.t list;  (** Oldest first. *)
  index : int;  (** Position of the current URL in [history]. *)
}

type 'e app_event = 'e Check_model.app_event =
  | Event of 'e  (** An event of the root machine. *)
  | Link of Silica.Url.t  (** Press on a [data-silica-link] anchor. *)
  | Browser_back
  | Browser_forward

val app :
  ?max_states:int ->
  ?max_violations:int ->
  ?within:('s app_state -> bool) ->
  ?max_history:int ->
  ?inputs:('s app_state -> 'e list) ->
  ?samples:sample list ->
  ?arrivals:int ->
  ?keys:Silica.Key.t list ->
  ?properties:('s app_state, 'e) property list ->
  ?allow:allowance list ->
  routes:'s list ->
  parse:(Silica.Url.t -> 's) ->
  print:('s -> Silica.Url.t) ->
  ?is_not_found:('s -> bool) ->
  url_event:(Silica.Url.t -> 'e) ->
  (Silica.Url.t -> (unit, 's, 'e) Silica.Component.t) ->
  ('s app_state, 'e app_event) t
(** Explores the root component of a routed app, for example the result of
    [Router.make], started at [print] of the first route, with a simulated
    history. The root view is the only view rendered: the page is a component
    node in it, so the page is listed in [not_explored] and its machine is not
    run. Actions [Navigate], [Replace], [Back], [Forward] and [Reload] change
    the simulated history and become the event [url_event url] of the root
    machine. [Resume], [Revert_navigation], [Preload] and [Set_status] do
    nothing here. [max_history] (default 4) bounds the history; the oldest
    entries are dropped, which keeps the state space finite.

    Besides the component rules, two rules hold:
    - [route-round-trip]: [parse (print route) = route] for every route in
      [routes] and for the route of every reached state, so a
      {!Silica.Router.val-route} whose constructor and data function disagree is
      found. Routes with a query: list one value with the default and one
      without.
    - [link-parses]: for the [href] of every [data-silica-link] anchor in a
      rendered view, [print (parse href)] is the same URL string (the link is
      canonical), and, when [is_not_found] is given, [parse href] is not the
      not-found route. A [parse] that raises [Failure] fails the rule. *)

val id : ('s, 'ev) t -> string
(** The id of the root machine. *)

val state_json : ('s, 'ev) t -> 's -> Silica_json.Json.t option
(** The snapshot as JSON by the state codec of the root machine (see
    {!Silica.Machine.register_codecs}), or [None] when it has none. For an app:
    an object with ["router"] (the snapshot of the root machine), ["history"]
    (the URLs, oldest first) and ["index"]. *)

val input_json : ('s, 'ev) t -> 'ev -> Silica_json.Json.t option
(** The event of a step as JSON by the event codec of the root machine, or
    [None] when it has none or the step is no event of the machine (new props, a
    link, back, forward). *)

(** {1 Reports} *)

(** Where a step came from. *)
type cause = Check_model.cause =
  | View of {
      handler : string;
          (** ["press"], ["double_press"], ["submit"], ["focus"], ["blur"],
              ["pointer_enter"], ["pointer_leave"], ["key_down Enter"],
              ["key_up Escape"]. *)
      element : string;
          (** The path of the element, such as ["main > form > button[2]"] (see
              {!Silica.Accessibility.path}). *)
      role : string option;
      name : string;  (** The accessible name of the element. *)
    }
  | Input  (** One of [inputs]. *)
  | Timer of { id : string; ms : int }
  | Answer of { id : string; path : string }  (** The answer of a call. *)
  | Live of { id : string; topic : string }  (** An update of a subscription. *)
  | Held of { id : string; source : string }  (** A held source. *)
  | Action_result of string  (** The result of the action with this name. *)
  | Props  (** The parent gave other props. *)
  | Child of { instance : string; machine : string }
  | Navigation  (** The back or forward button of the browser. *)

val describe_cause : cause -> string
(** One phrase, such as
    ["the view offers press on button \"Save\" at form > button"] or
    ["the timer save/saved/after 2000ms (2000 ms)"]. *)

type 'ev step = {
  input : 'ev;
  event : string;
      (** The name of the event by {!Silica.Machine.event_name}, ["props"] for
          new props, ["link <url>"], ["back"] or ["forward"]. *)
  cause : cause;
  instance : string;  (** ["0"] for the root machine. *)
  machine : string;  (** The machine id. *)
  from : string;  (** The name of the state before the step. *)
  target : string;
      (** The name of the state after the step; [from] for a step that the
          update ignored or that raised. *)
}

type 'ev counterexample =
  | Path of 'ev step list
      (** Finite steps from a start to the failing point. *)
  | Waits of 'ev step list
      (** Finite steps to a state where nothing runs and the app waits for the
          user. *)
  | Loop of { stem : 'ev step list; cycle : 'ev step list }
      (** Steps to a cycle that repeats forever. *)

type ('s, 'ev) violation = {
  rule : string;  (** A built-in rule id of {!val-rules}, or a property name. *)
  message : string;
      (** One sentence with the state, the element and the event. *)
  hint : string;  (** How to fix it. *)
  counterexample : 'ev counterexample;
  state : 's;
      (** The snapshot at the failing point: for [ignored-event] and [raised],
          the snapshot before the last step. *)
  state_name : string;
  view : string;
      (** {!Silica.View_text} of the view of [state], or [""] when the view
          raised. *)
  element : string option;  (** The path of the element, for view rules. *)
}

type children
(** The props that the views of a check gave to child components. *)

val props : children -> ('p, 's, 'e) Silica.Component.t -> 'p list
(** [props children c]: the props, without duplicates, of the component nodes of
    [c] that the check rendered. Give them to the check of [c] with [~props]. *)

type ('s, 'ev) report = {
  machine : string;  (** The id of the root machine. *)
  states : int;  (** The explored snapshots. *)
  transitions : int;  (** Every explored pair of a snapshot and a step. *)
  complete : bool;  (** [false] when [max_states] stopped the search. *)
  truncated : bool;  (** [true] when [max_violations] stopped the search. *)
  violations : ('s, 'ev) violation list;
      (** In the order found, so shortest first. The first violation of each
          rule, state name, element and message. *)
  unreached : string list;
      (** The names of the states of the machine (see
          {!Silica.Machine.val-shape}) that no step reached and no allowance
          covers. *)
  allowed : (allowance * int) list;
      (** Each allowance and how many distinct violations it turned off. *)
  not_explored : string list;
      (** Sorted, without duplicates: what the views showed and the checker did
          not run: ["component <machine id>"] for each child component node and
          ["island <channel>"] for each Live island (an element with
          [data-silica-live]). *)
  children : children;
  specs : Silica.Spec.t list;
      (** The spec that the run found for the root machine: the states it
          reached, initial state first then by name, each with the timers and
          sources of the first snapshot reached in it, and the steps it took,
          with event names. *)
  paths : (string * 'ev step list) list;
      (** For each state name, in the order the search reached them, a shortest
          path from a start to its first snapshot. *)
  views : (string * string) list;
      (** For each state name, in the same order, {!Silica.View_text} of the
          view of its first snapshot. *)
}

val run : ('s, 'ev) t -> ('s, 'ev) report
(** Runs the search. Raises {!Invalid_check} on a setup error. An exception in
    the update, the view, the handlers or a property is a violation of the rule
    [raised]: the search does not go on from that snapshot. *)

val passed : ('s, 'ev) report -> bool
(** No violation, nothing unreached, and [complete]. *)

val steps : 'ev counterexample -> 'ev step list
(** Every step of the counterexample, in order: for a [Loop], the stem then the
    cycle once. *)

val pp_report : Format.formatter -> ('s, 'ev) report -> unit
(** The report as text: the counts, then each violation with its message, hint,
    numbered steps with their causes, state and view, then the unreached states,
    the allowances and what was not explored. *)

(** {1 Rules} *)

type rule = Check_rules.rule = {
  id : string;
  message : string;
      (** The message, with places such as [{state}], [{element}] and [{event}]
          that a violation fills. *)
  hint : string;  (** How to fix it, with the same places. *)
  applies_to : string;
      (** ["view"], ["state"], ["step"], ["machine"], ["route"], ["search"] or
          ["suite"] (found by {!Check_suite}). *)
}

val rules : rule list
(** Every built-in rule. *)

(** {1 Temporal properties}

    {!never_after} and {!leads_to} talk about runs, not single snapshots. The
    checker stores the graph that the search explored (each snapshot, and each
    step with its event, cause and actions) when a check has one of them, and
    checks each one after the search, on the product of that graph with one bit
    of memory, so a property visits at most twice the explored snapshots.

    A condition is tested at each point of a run: at a start, and at the
    snapshot that each step reaches. There, {!val-event}, {!action} and
    {!entered} read that step, not only the step of a shortest path.

    {b Runs and fairness.} A run starts at a start of the check and takes steps
    of the graph. It may stop in a snapshot where no timer, no call and no
    action result runs: the app waits for the user, who may do nothing, and a
    server may never publish, so running subscriptions, held sources, inputs and
    the view do not keep a run going. It may not stop while a timer, a call or
    an action result runs. An infinite run counts only when it is weakly fair:
    every timer and call that runs at every step from some point on is also
    delivered again and again. So every timer fires and every call answers in
    the end, even if the user keeps typing, and the checker does not report a
    run where the user prevents that forever. "Eventually" means: on every such
    run.

    {b Not checked.} Real durations: time is abstract, so a timer that another
    timer always restarts before it can fire is assumed to fire. Which answer a
    call gets: fairness delivers an answer, not a good one, so a retry on
    failure can fail forever. Runs of the parts outside [within] or after
    [max_states]: a counterexample uses only explored snapshots and steps, so it
    is a real run, and a step that left the explored graph is never taken to be
    missing. A report with [complete = false] may miss counterexamples; one with
    [truncated] may miss counterexamples past [max_violations]. *)

val action : ('e Silica.Action.host -> bool) -> ('s, 'e) condition
(** Holds on the snapshot reached by a step that ran a matching action, such as
    [Run (Focus button)], and on a start whose start actions match. *)

val entered : string -> ('s, 'e) condition
(** Holds on the snapshot reached by a step from a state with another name into
    the state with this name. It does not hold on a start. *)

val never_after :
  name:string ->
  ?hint:string ->
  after:('s, 'e) condition ->
  never:('s, 'e) condition ->
  unit ->
  ('s, 'e) property
(** [never_after ~name ~after ~never ()]: in every run, from the first point
    that matches [after] on, that point included, no point matches [never]. A
    violation has a [Path] counterexample: a shortest run to the first point
    that matches [never], one for each state name. *)

val leads_to :
  name:string ->
  ?hint:string ->
  from:('s, 'e) condition ->
  to_:('s, 'e) condition ->
  unit ->
  ('s, 'e) property
(** [leads_to ~name ~from ~to_ ()]: every run that reaches a point that matches
    [from] later reaches a point that matches [to_]; the same point counts. Runs
    are the ones of the section above. Two counterexamples, both real runs of
    the explored graph:
    - [Waits steps]: a shortest run to a snapshot where nothing runs, after
      [from] and with no [to_] since; one for each state name.
    - [Loop { stem; cycle }]: [stem] is a shortest run to the first state of
      [cycle]; [cycle] returns to it and can repeat forever, and it delivers
      each timer and call that runs at each of its steps. One for each state
      name where a cycle starts. A set of states where the app can also wait is
      reported as [Waits] only, which is shorter.

    The message names the cycle and what it delivers; the hint says what to
    change. [hint] replaces the hint. *)

(** {1 Replay}

    Run a counterexample or a trace again, without a search, through the same
    step function as {!run}. *)

type failure = {
  rule : string;  (** A built-in rule id of {!val-rules}, or a property name. *)
  message : string;
  hint : string;
  element : string option;
}
(** A rule that fails at a point of a replay. Allowances apply as in {!run}. *)

type ('s, 'ev) replayed_step = {
  step : 'ev step option;  (** [None] for the start. *)
  state : 's;  (** The snapshot after the step. *)
  state_name : string;
  view : string;  (** {!Silica.View_text} of its view, [""] when it raised. *)
  actions : string list;
      (** The actions of the step, as {!Silica.Transition.describe} writes them.
      *)
  failing : failure list;
      (** The rules that fail at this point: the rules of the view and the
          state, the properties, and the rules of the step ([ignored-event],
          [raised], [focus-target] and the other action rules). At the start:
          also the rules of the setup, such as [route-round-trip]. *)
}

type ('s, 'ev) replay =
  | Replayed of ('s, 'ev) replayed_step list
      (** The start, then one point for each step, in order. *)
  | Not_enabled of {
      at : int;  (** The number of the step, from 1, that is not enabled. *)
      steps : ('s, 'ev) replayed_step list;  (** The points before it. *)
      enabled : 'ev step list;  (** What the check offers there instead. *)
    }

val replay : ('s, 'ev) t -> 'ev list -> ('s, 'ev) replay
(** [replay check inputs] starts the check and takes each input as the next
    step, when the state offers it. A check with several [props] tries each
    start in order and keeps the first one where every input is enabled. When an
    input comes from more than one source (a button and a timer give the same
    event), the first source in the order of {!component} is taken. After a step
    that raised, nothing is enabled. *)

val replay_trace :
  ('s, 'ev) t -> Silica.Trace.t -> (('s, 'ev) replay, string) result
(** Replays the steps of the root instance of a trace, such as one that
    {!export} wrote. Each step is matched by its event data, decoded with the
    event and props codecs of the root machine (see
    {!Silica.Machine.register_codecs}), and by its cause: the same element,
    timer, call or source when the check offers it. A step with no data is
    matched by its event name and cause. [Error] when the trace is of another
    machine, has no start of its root, or holds data that does not decode. *)

val export : ('s, 'ev) t -> rule:string -> 'ev counterexample -> Silica.Trace.t
(** A counterexample as a trace file (see {!Silica.Trace}): the start, then one
    transition for each step, with the cause of each step, the answer of each
    call by the codecs of its endpoint, the actions, and the event, props and
    snapshot data when the machine registered codecs. The header has the origin
    [Checker { rule }], the build ["check"], no time, and for a [Loop] the [seq]
    where the cycle starts; the cycle is written once. A step from [inputs] has
    the cause [Message]; a back or forward has the cause [Url] with the URL it
    reached. A check with several [props] starts with the props where the last
    step breaks [rule]. Raises [Invalid_argument] when a step of the
    counterexample is not enabled in the check. *)

val script :
  ?component:string -> ('s, 'ev) t -> Silica.Trace.t -> (string, string) result
(** [script check trace] is the OCaml text of a test that drives the component
    in [Silica_memory] through the steps of a counterexample that {!export}
    wrote, with the queries a person uses: an element by its role and name, an
    answer to the pending call of a path, the clock moved to the timer. It ends
    with a check of the rule of the trace, and it fails while the bug is there:
    - [ignored-event]: the last input is given when the view still offers its
      element, and the update must handle it;
    - [raised]: no error reached [on_error];
    - a rule of the view: {!view_violations} of that rule is empty;
    - any other rule or a property: the view is not the view that broke it.

    [component] is the OCaml expression of the component (default: the module
    named after the machine id, as [Issue_list.component] for [issue-list], or
    [.app] for an app). [Error] says why the steps cannot run in
    [Silica_memory]: a timer that fires before another timer that real time
    fires first (the checker lets running timers fire in any order), an event
    from [inputs], a Live update, a held source, the result of an action, an
    element with no role and no text; also when the replay in the check no
    longer breaks the rule. Replay such a trace in the check with
    {!replay_trace}. *)

val view_violations :
  ?rule:string -> state:string -> 'e Silica.Node.t -> string list
(** The messages of the built-in rules of a view (the rules of {!val-rules} that
    apply to ["view"], but [dead-end] and [link-parses]) that fail on the view,
    or of the one [rule]. [state] names the state in the messages. A test reads
    it on the view of [Silica_memory.view]. *)
