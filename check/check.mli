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
(** A test of one point of a run. A point is a start of the check, or the
    snapshot that one step reaches, with that step. Point conditions
    ({!val-state}, {!state_named}, {!val-view}, {!holds}) read only the snapshot
    and its view. Step conditions ({!val-event}, {!action}, {!entered}) also
    read the step into the point; at a start, {!val-event} and {!entered} do not
    hold and {!action} reads the start actions. A combination ([&&&], [|||],
    [not_]) is a step condition when one of its parts is.

    Each property reads its conditions on every point of every run that the
    search explored:
    - {!always} with a point condition: on each explored snapshot (a snapshot is
      the same point on every path that reaches it);
    - {!always} with a step condition: on each start and on each explored step,
      not only the step of a shortest path into each snapshot;
    - {!never_after} and {!leads_to}: on each start and each explored step (see
      {!section-temporal});
    - a property of a {!val-child}: only point conditions; a step condition
      raises {!Invalid_check}. *)

val state : ('s -> bool) -> ('s, 'e) condition
(** Holds when the snapshot passes the test. *)

val state_named : string -> ('s, 'e) condition
(** Holds when the state of the snapshot has this name (see
    {!Silica.Machine.name}). *)

val view : ('e Silica.Node.t -> bool) -> ('s, 'e) condition
(** Holds when the view of the snapshot passes the test. *)

val shows : string -> ('s, 'e) condition
(** A point condition: holds when the text is a part of the text that a person
    sees in the view ({!Silica.View_text.visible}): the text of any element,
    with or without a role, but not of hidden ones. *)

val view_shows : 'e Silica.Node.t -> string -> bool
(** [view_shows view text]: the test of {!shows} as a function, for {!holds} and
    {!val-view} when the text depends on the state:
    [holds (fun state view -> view_shows view (title state))]. *)

val holds : ('s -> 'e Silica.Node.t -> bool) -> ('s, 'e) condition
(** Holds when the snapshot and its view pass the test together. *)

val event : ('e -> bool) -> ('s, 'e) condition
(** A step condition: holds on the point reached by a step whose event of the
    machine passes the test. It does not hold at a start, nor after a step with
    no event of the machine (new props, a link, back, forward). *)

val transition : ('s -> 'e -> 's -> bool) -> ('s, 'e) condition
(** [transition test]: a step condition that holds on the point reached by a
    step with an event of the machine when [test before event after] holds, with
    the snapshots before and after the step. It does not hold at a start, nor
    after a step with no event of the machine. "Increment adds the step":
    {[
      Check.always ~name:"increment adds the step"
        Check.(
          not_ (event (( = ) Increment))
          ||| transition (fun before _ after ->
              after.count = before.count + before.step
          )
        )
    ]}
    The [not_ (event ...)] part makes the property hold at a start and on the
    other steps. *)

val ( &&& ) : ('s, 'e) condition -> ('s, 'e) condition -> ('s, 'e) condition
val ( ||| ) : ('s, 'e) condition -> ('s, 'e) condition -> ('s, 'e) condition
val not_ : ('s, 'e) condition -> ('s, 'e) condition

type ('s, 'e) property
(** Something that must hold in every run. *)

val always :
  name:string -> ?hint:string -> ('s, 'e) condition -> ('s, 'e) property
(** [always ~name ?hint condition]: [condition] holds at every point of every
    explored run. With a point condition, it is checked on each explored
    snapshot. With a step condition, it is checked on each start and on each
    explored step, so
    [always ~name (not_ (event (( = ) Reset)) ||| state is_zero)] fails on a
    Reset step that keeps the count even when the snapshot it reaches was first
    reached by another step; the checker then stores the explored graph, as for
    {!never_after}. A violation has the rule [name], a [Path] to the first
    failing point, and the [hint]; [hint] may name [{state}] for the state of
    the failing point. Without [hint], the hint asks to change the update, the
    view, or the property. *)

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

(** {1 Children} *)

type child
(** A child machine that runs inside the check of its parent (see {!component}
    [?children] and {!app} [?pages]). *)

val child :
  ?samples:sample list ->
  ?inputs:('s -> 'e list) ->
  ?texts:string list ->
  ?within:('s -> bool) ->
  ?max_instances:int ->
  ?properties:('s, 'e) property list ->
  ('p, 's, 'e) Silica.Component.t ->
  child
(** [child component]: every component node of the machine of [component] in the
    views of the check runs as an instance of that machine, the way
    {!Silica.Instance} runs it. An instance is known by its place in the view of
    its parent, its machine id and its key. It starts with the props of its
    node, gets new props through [on_props] when the node gives other props,
    sends the events of [output] to its parent after each step it handles and
    the events of [on_final] once when it reaches a final state, and ends (with
    the stops of what it owns) when its node is no longer rendered. Its view
    replaces its node, so the view rules see the whole page.

    The bounds of a joint check:
    - Only listed children run. Other component nodes stay in [not_explored].
    - [max_instances] (default 2) instances of the machine run at once. A
      further instance shows the view of its start state, takes no step, and is
      listed in [not_explored] as
      ["component <id> (up to <n> instances were shown at once, and only the
       first 2 ran, ...)"], with how to raise the bound. The [within] of the
      parent does not bound it: a parent state inside [within] can still show
      more rows than [max_instances].
    - [samples] answer the calls and subscriptions of the child, [inputs] give
      it events, [texts] are typed into its fields ({!component} explains the
      three; the [texts] of the check go only to the fields of the parent), and
      [within] bounds it: a joint snapshot where a child is outside its [within]
      is not explored.
    - The [max_states] of the check bounds the joint snapshots. The report gives
      the number of distinct snapshots of each machine in [machines].

    [properties] are {!always} properties with point conditions. They hold on
    every snapshot of every instance: the view is the resolved view of the
    instance with only its own handlers. A violation inside a child names the
    machine and the instance. Unreached states of a child are named
    ["<machine id>: <state>"]. Raises {!Invalid_check} when [max_instances] is
    below 1, when a property reads the step ({!val-event}, {!action},
    {!entered}: a child property is read only on the first visit of each joint
    snapshot, so put it on the check of the child alone), or when a property is
    temporal: put {!leads_to} and {!never_after} on the parent, where they see
    the whole page, or on the check of the child alone. *)

type child_event = Check_model.child_event
(** An event of a child instance, as a step of a joint check holds it. *)

(** {1 Making a check} *)

(** A step of a component check. *)
type ('p, 'e) input = ('p, 'e) Check_model.input =
  | Event of 'e  (** An event of the machine. *)
  | Props of 'p  (** The parent gives other props. *)
  | In_child of child_event
      (** An event of a child instance, in a check with [children]. *)

val component :
  ?max_states:int ->
  ?max_violations:int ->
  ?within:('s -> bool) ->
  ?inputs:('s -> 'e list) ->
  ?props:'p list ->
  ?samples:sample list ->
  ?arrivals:int ->
  ?keys:Silica.Key.t list ->
  ?texts:string list ->
  ?children:child list ->
  ?properties:('s, 'e) property list ->
  ?allow:allowance list ->
  ('p, 's, 'e) Silica.Component.t ->
  ('s, ('p, 'e) input) t
(** The check starts once with each of [props] (default: [()] for a machine made
    by {!Silica.Machine.make}; a machine with props needs them), and in every
    state the parent can give any other of [props]. Events in a state are the
    handlers of the rendered view (press, double press, submit, focus, blur,
    pointer enter and leave), the key handlers of the view called with each key
    of [keys] (default: Escape, Enter, Tab, Arrow up, Arrow down), the input
    handler of each text field of the view (a [textarea], or an [input] with no
    [type] or a type of text, search, email, url, tel, password or number; not a
    [select] or a checkbox) called with each text of [texts] (default: none),
    the events of the held sources of the state, [inputs s] (for events that no
    element of the view gives), the events of running timers, the answers of
    running calls and the updates of running subscriptions (see {!answers} and
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
    nothing of a state that was left arrives. As in a browser and in
    [Silica_memory], a handler is not enabled on an element that has [hidden] or
    [inert] or is inside one, or that is a disabled [button], [input], [select],
    [textarea], [option], [optgroup] or [fieldset] or is inside one; a portal
    starts again outside them.

    The built-in rules are in {!val-rules}. [max_states] (default 100000) bounds
    the search; when it stops the search, the report is not [complete]. [within]
    bounds an unbounded state space on purpose: snapshots outside it are neither
    checked nor expanded, and the report holds for the bounded space.
    [max_violations] (default 50) stops the search after that many violations;
    the report is then [truncated]. The checker does not enter component nodes
    in the view: it lists them in [not_explored] and keeps their props in
    [children]. To check a child, make a check of its own machine with [~props]
    from {!props}, or list it in [children] to run it inside this check (see
    {!val-child}). With [children], properties see the whole resolved view with
    only the handlers of the root machine. *)

val live :
  ?max_states:int ->
  ?max_violations:int ->
  ?within:('s -> bool) ->
  ?inputs:('s -> 'e list) ->
  ?samples:sample list ->
  ?arrivals:int ->
  ?keys:Silica.Key.t list ->
  ?texts:string list ->
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
  router : 's;  (** The snapshot of the root machine: the current route. *)
  history : Silica.Url.t list;
      (** Oldest first, at most [max_history] entries. *)
  index : int;  (** Position of the current URL in [history]. *)
}
(** What a property and the [within] of {!app} read. A page that runs as a
    {!val-child} is not in it: bound a page with the [within] of its child. For
    example, with routes [Home], [Issue of int] and [Settings]:
    {[
      Check.app
        ~within:(fun (app : _ Check.app_state) ->
          List.length app.history <= 3
          && match app.router with Issue number -> number < 3 | _ -> true
        )
        ~max_history:3 ...
    ]} *)

val current_url : 's app_state -> Silica.Url.t
(** The URL of the current history entry. *)

type 'e app_event = 'e Check_model.app_event =
  | Event of 'e  (** An event of the root machine. *)
  | Link of Silica.Url.t  (** Press on a [data-silica-link] anchor. *)
  | Open of Silica.Url.t
      (** The user opens the URL, as in the address bar: a push, from [urls]. *)
  | Browser_back
  | Browser_forward
  | In_child of child_event  (** An event of a page or of its children. *)

val app :
  ?max_states:int ->
  ?max_violations:int ->
  ?within:('s app_state -> bool) ->
  ?max_history:int ->
  ?inputs:('s app_state -> 'e list) ->
  ?samples:sample list ->
  ?arrivals:int ->
  ?keys:Silica.Key.t list ->
  ?texts:string list ->
  ?urls:string list ->
  ?pages:child list ->
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
    nothing here. [urls] are URLs that the user opens, as in the address bar, in
    any state: each is a push, with the cause "the user opens <url>", and a
    script opens it with {!Silica_memory.open_url}. Keep [inputs] for events
    that no element and no URL gives. [max_history] (default 4) bounds the
    history; the oldest entries are dropped, which keeps the state space finite.

    [pages] run the listed page components (and their children, when listed)
    inside the check, as {!component} [?children] does: links in pages are
    followed, and the navigation actions of a page change the history after the
    events that its step raised are handled. Check a router with one or a few
    pages per check, so the product of the router and its pages stays small.

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
(** The id of the root machine; for a check with [children] or [pages], the root
    and the listed children joined by ["+"], such as ["router+issue-list"]. *)

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
              ["key_up Escape"], or ["input 0"] for the text ["0"] of [texts]
              typed into the field. *)
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
  | Address of string  (** The user opens this URL, from [urls]. *)

val describe_cause : cause -> string
(** One phrase, such as
    ["the view offers press on button \"Save\" at form > button"] or
    ["the timer save/saved/after 2000ms (2000 ms)"]. *)

type 'ev step = {
  input : 'ev;
  event : string;
      (** The name of the event by {!Silica.Machine.event_name}, ["props"] for
          new props, ["link <url>"], ["back"] or ["forward"]; for a machine with
          no [event_name], the label of its cause by
          {!Silica.Transition.cause_label}, such as ["press button \"Save\""].
      *)
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

type 'ev state_views = {
  state : string;
  views : (string * 'ev step list) list;
      (** The distinct {!Silica.View_text} views of the snapshots of the state,
          in the order the search reached them, each with a shortest path to the
          first snapshot that shows it. At most {!views_per_state}. *)
  more : int;  (** How many more distinct views the bound left out. *)
}
(** The views of one state name. *)

val views_per_state : int
(** The most views of one state that a report keeps. *)

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
      (** The names of the states of the machines (the [states] given to
          {!Silica.Machine.make}) that no step reached and no allowance covers.
          A child state is named ["<machine id>: <state>"]. *)
  unlisted : string list;
      (** The ids of the machines that give no [states], for which the check
          cannot report unreached states. A note, not a finding: it does not
          change {!passed}. *)
  allowed : (allowance * int) list;
      (** Each allowance and how many distinct violations it turned off. *)
  not_explored : string list;
      (** Sorted, without duplicates: what the views showed and the checker did
          not run: ["component <machine id>"] for each child component node that
          is not listed in [children] or [pages], the instances over
          [max_instances], and ["island <channel>"] for each Live island (an
          element with [data-silica-live]). *)
  children : children;
  machines : (string * int) list;
      (** For each machine, the root first then each listed child: the number of
          its distinct snapshots that the search met. With no children, the root
          and [states]. *)
  specs : Silica.Spec.t list;
      (** The specs that the run found, the root machine first, then each listed
          child that started. A spec holds the states it reached, initial state
          first then by name, each with the timers and sources of the first
          snapshot reached in it, and the steps it took, with event names. *)
  paths : (string * 'ev step list) list;
      (** For each state name, in the order the search reached them, a shortest
          path from a start to its first snapshot. *)
  views : 'ev state_views list;
      (** For each state name, in the same order as [paths], its distinct views.
      *)
  vacuous : (string * string) list;
      (** Each property that passed without being tested, with why: an [always]
          whose condition is an implication [not_ a ||| b] (or [b ||| not_ a])
          where [a] holds at no explored point, a [leads_to] whose [from], or a
          [never_after] whose [after], holds at no explored point. A warning,
          not a finding: it does not change {!passed} or the exit code. *)
  bounds : string;
      (** How to bound the search of this check, which the hint of [state-limit]
          gives: for {!app}, [max_history] and what it bounds. *)
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
    the machines that give no [states], the allowances and what was not
    explored. *)

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

(** {1:temporal Temporal properties}

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
    {!export} wrote, and in a joint check the steps of the listed children (not
    their props and not what their parents did on their [output]). It never
    guesses a step:
    - A trace that {!export} wrote holds the choice of each step (see
      {!Silica.Trace.type-origin}): the position of its move among the moves the
      check offered there. With the same check, each step replays exactly, with
      no codec. When the check changed, the step is the one move that fits its
      event name, data and cause; with no such move the step is not enabled
      ([Not_enabled]); with several, the result is [Error].
    - A recorded trace is matched by the event data of each step, decoded with
      the event and props codecs of the root machine (see
      {!Silica.Machine.register_codecs}), and by its cause. A step with no data
      is an [Error]: a trace recorded with names only cannot replay faithfully.

    [Error] also when the trace is of another machine, has no start of its root,
    or holds data that does not decode. *)

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
  ?component:string ->
  ?check:string ->
  ('s, 'ev) t ->
  Silica.Trace.t ->
  (string, string) result
(** [script check trace] is the OCaml text of a test that drives the component
    in [Silica_memory] through the steps of a counterexample that {!export}
    wrote, with the queries a person uses: an element by its role and name, an
    answer to the pending call of a path, the clock moved to the timer. It ends
    with a check of the rule of the trace that fails exactly while the rule is
    broken on that run:
    - [ignored-event]: the last input is given when the view still offers its
      element, and the update must handle it;
    - [raised]: no error reached [on_error];
    - a rule of the view: {!view_violations} of that rule is empty;
    - any other rule or a property: {!Check_expect.holds} runs the steps, then
      evaluates the rule in the check itself along the run of the page
      ({!replay_run}), with the same meaning as the checker (for a [Loop], the
      run goes once around the cycle).

    The test refers to the check by [check], an OCaml expression (default: the
    value named after the machine id in the module [Checks], as
    [Checks.issue_list] for [issue-list]), so the checks of the project live in
    a library that tests can use. Props come from the check too ({!nth_props}),
    so no props codec is needed. [component] is the OCaml expression of the
    component (default: the module named after the machine id, as
    [Issue_list.component], or [.app] for an app). [Error] says why the steps
    cannot run in [Silica_memory]: a timer that fires before another timer that
    real time fires first (the checker lets running timers fire in any order),
    an event from [inputs], a Live update, a held source, the result of an
    action, an element with no role and no text; also when the replay in the
    check no longer breaks the rule. Replay such a trace in the check with
    {!replay_trace}. In a joint check, the steps of children are steps of the
    trace too: an element of a child is found in the whole page. *)

val view_violations :
  ?rule:string -> state:string -> 'e Silica.Node.t -> string list
(** The messages of the built-in rules of a view (the rules of {!val-rules} that
    apply to ["view"], but [dead-end] and [link-parses]) that fail on the view,
    or of the one [rule]. [state] names the state in the messages. A test reads
    it on the view of [Silica_memory.view]. *)

(** {1 Conformance}

    Did a run of the app, from development or from production, do what the
    checked code does? {!Silica.Spec.conforms} reads the names of a trace;
    {!conforms} reads its data. *)

type difference =
  | Different_step of { logged : string; checked : string }
      (** From the snapshot before, with the event (or the props) of the record,
          the code of this build gives another state name, another change
          ([unhandled], [kept], [entered]) or another snapshot. [logged] and
          [checked] are the step as ["from -> to (change), snapshot <json>"]. *)
  | Property_fails of {
      property : string;
      message : string;
      hint : string;
      snapshot : Silica_json.Json.t;  (** The snapshot of the record. *)
      view : string;
          (** {!Silica.View_text} of the view of the snapshot, or [""]. *)
    }  (** An {!always} property does not hold on the snapshot of the record. *)
  | Not_decodable of { error : string }
      (** The event or the snapshot of the record does not decode with the
          codecs of this build. *)

type deviation = {
  seq : int;  (** The [seq] of the record in the trace. *)
  machine : string;
  instance : string;  (** The instance path of the record. *)
  difference : difference;
}

type conformance = {
  steps : int list;  (** The [seq] of each record whose step the code ran. *)
  snapshots : int list;
      (** The [seq] of each record whose snapshot the properties read. *)
  deviations : deviation list;  (** In the order of the trace. *)
}

val conforms : ('s, 'ev) t -> Silica.Trace.t -> conformance
(** [conforms check trace] runs, for each record of the root machine of [check]
    and of its listed children (see {!val-child}), the code of this build on the
    data of the trace, with no search:
    - The step: a start runs {!Silica.Machine.start} with the props of the
      record (not for the root of an app, whose start depends on the URL, which
      the record does not hold); a step runs {!Silica.Machine.step} on the
      snapshot of the record before it in the same instance, with the event of
      the record; new props run {!Silica.Machine.set_props}. The state name, the
      change and the snapshot (by the state codec) must be the ones of the
      record.
    - The properties: every {!always} property of the check (of the child, for a
      child) holds on the snapshot of the record and its view, with the event of
      the record. For an app, the URL is the last [Url] cause of the root, or
      the URL of the header. The view is the view of that machine alone, with
      its children as component nodes. {!action} never holds; temporal
      properties are not checked here. Allowances apply.

    A record is skipped when the data it needs is not there: a trace with names
    only, a machine with no registered codecs, a step whose snapshot before was
    dropped by a full recorder, a machine with props and no props codec (no
    view, so no properties). [steps] and [snapshots] say what ran. *)

val difference_to_string : difference -> string
(** One sentence, such as
    ["the trace has drafting -> sending (entered); the code gives drafting ->
     drafting (unhandled)"]. *)

val replay_run :
  ?cycle_from:int ->
  ?start:int ->
  ?props:int list ->
  ('s, 'ev) t ->
  Silica.Trace.t ->
  (('s, 'ev) replay, string) result
(** [replay_run check run] replays a run recorded by the runtime, such as
    {!Silica_memory.trace}, in the check, and evaluates every rule and property
    of the check at each point, as {!val-replay} does. A step of the run is the
    one move of the check that fits its cause: the same element and handler
    (with the typed text or the key), the same timer, the answer of the call
    with the same data, the link or history step that reaches the same URL. No
    codec is needed. [Error] when a step fits several moves. A step that fits no
    move is [Not_enabled].

    [start] is the position of the start among the starts of the check (one for
    each of its [props]), as a trace of {!export} records it. [props] gives, in
    order, the position in the [props] of the check of the props of each props
    step of the run, which a run without a props codec does not hold.
    [cycle_from] is the number of steps before a cycle that the run went once
    around: a [leads_to] property then fails at the last point when the run is
    back at the snapshot where the cycle started, as for a [Loop]
    counterexample. *)

val nth_props : ('s, ('p, 'e) input) t -> int -> 'p
(** [nth_props check index]: the props of the start at [index] of a component
    check (see the [start] of {!Silica.Trace.type-origin}). Raises
    [Invalid_argument] when there is none. *)
