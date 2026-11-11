(** Explicit-state model checking of components and routed apps.

    States and events must be first-order values: they are compared and hashed
    structurally, so closures are reported as [Error]. The checker explores
    breadth first, so a [Violation] carries a shortest trace. *)

type ('s, 'e) invariant = {
  name : string;
  holds : 's -> 'e Silica.Node.t -> bool;
}
(** A property of a state and the view rendered from it. *)

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

type children
(** The props that the views of a check gave to child components. *)

val props : children -> ('p, 's, 'e) Silica.Component.t -> 'p list
(** [props children c]: the props, without duplicates, of the component nodes of
    [c] that the check rendered. Give them to the check of [c] with [~props]. *)

type ('s, 'ev) report =
  | Ok of {
      states : int;
      transitions : int;
      not_explored : string list;
      edges : Silica.Spec.label list;
      children : children;
      spec : Silica.Spec.t;
      paths : (string * 'ev list) list;
    }
      (** [transitions] counts every explored (state, event) pair.
          [not_explored] lists, sorted and without duplicates, what the checker
          saw in the views it rendered but did not run:
          ["component <machine id>"] for each child component node and
          ["island <channel>"] for each Live island (an element with
          [data-silica-live]). The checker never runs a child machine inside the
          check of its parent: it never looks inside a component node, so a page
          check lists its children here instead of skipping them silently. Check
          each listed machine alone with {!component}, with the props from
          [children]. [edges] are the steps that the run took, sorted, with the
          event names from [label]: a state entered from another state or
          entered again, and the steps that kept the state ([kept]). [spec] is
          the spec that the run found: the states it reached, initial state
          first then by name, each with the timers and sources of the first
          snapshot reached in it, and the edges it took; export it with
          {!Silica.Spec.to_quint} and {!Silica.Spec.to_dot}. [paths] gives, for
          each state name in the order the search reached them, a shortest list
          of steps from the start to it. *)
  | Violation of { invariant : string; trace : 'ev list; state : 's }
  | Unreached of { states : string list }
      (** The search ended with no violation, but these names of the [states] of
          the machine (see {!Silica.Machine.make}) were never reached and are
          not in [allow_unreached]. *)
  | Limit of { states : int }
  | Error of string

(** A step of a component check. *)
type ('p, 'e) input =
  | Event of 'e  (** An event of the machine. *)
  | Props of 'p  (** The parent gives other props. *)

val pp_input :
  (Format.formatter -> 'e -> unit) -> Format.formatter -> ('p, 'e) input -> unit
(** Print an input with the given event printer; props print as ["new props"].
*)

val component :
  ?max_states:int ->
  ?within:('s -> bool) ->
  ?inputs:('s -> 'e list) ->
  ?props:'p list ->
  ?samples:sample list ->
  ?arrivals:int ->
  ?allow_unreached:string list ->
  ?label:('e -> string) ->
  ?keys:Silica.Key.t list ->
  ?invariants:('s, 'e) invariant list ->
  ('p, 's, 'e) Silica.Component.t ->
  ('s, ('p, 'e) input) report
(** The check starts once with each of [props] (default: [()] for a machine made
    by {!Silica.Machine.make}; a machine with props needs them), and in every
    state the parent can give any other of [props]. Events in a state are the
    handlers of the rendered view (press, double press, submit, focus, blur,
    pointer enter and leave), the key handlers of the view called with each key
    of [keys] (default: Escape, Enter, Tab, Arrow up, Arrow down), the events of
    the held sources of the state, [inputs s] (use it for [On_input], which
    needs sample strings), the events of running timers, the answers of running
    calls and the updates of running subscriptions (see {!answers} and
    {!states}; [arrivals] defaults to 2). A [Global_keys] source gives the event
    of each key in its table, which is the press of that key; [Outside_press]
    gives its event; [Resize] gives [on_resize] with {!Silica.Rect.zero};
    [Intersection] gives [on_change] with [true] and with [false]. [Scroll]
    gives [on_scroll] with {!Silica.Scroll.zero}; [Visibility], [Online] and
    [Media] give their event with [true] and with [false]. A [Block] gives
    [on_attempt] with a push and with a back, both to ["/"]. A [Dismiss] layer
    gives its Escape event, its outside event and its focus-out event. A
    [Focus_trap], [Scroll_lock] and [Anchor] give no event. An action with a
    result event answers once, at any later step and in any state: [Measure]
    with [Some Silica.Rect.zero], [Copy] and [Load_script] with [true]. The
    checker treats {!Silica.Rate} limits as pass-through: every value arrives,
    and none is held back. Elements inside a {!Silica.Node.Portal} and the body
    of a {!Silica.Node.Boundary} are part of the view. Time is abstract: a
    running timer can fire at any step, in any order. A timer, call or
    subscription starts with its start action and stops with [Stop]; a call ends
    with its answer and a timer when it fires. So nothing of a state that was
    left arrives.

    Built-in invariants: pressable nodes have an accessible name; refs are
    unique in a view; [Focus], [Blur], [Scroll_into_view] and a [Scroll_to] with
    a target name a ref in the next view ([focus-target]); [boundary-reset]: the
    fallback of every boundary holds an element with {!Silica.Node.On_reset};
    [dead-end]: some event is enabled unless the machine is final or its view
    holds a child component or an island (the checker does not run those, and
    they can raise events); [timer-owner]: every running timer belongs to the
    current state; [call-owner]: every running call and subscription belongs to
    the current state; [ignored-event]: the machine handles every event of a
    handler of the view, of a timer, of a call answer and of a subscription
    ({!Silica.Machine.can}); it may ignore the events of held sources, of
    [inputs], new props and action results (a result can arrive after the state
    that asked for it was left). The trace of [ignored-event] ends with the
    ignored event and [state] is the state before it, so an update that ends
    with [| _ -> Ignore] cannot hide a button that does nothing. When the search
    ends with no violation, each name of the [states] of the machine that the
    search never reached and that is not in [allow_unreached] is reported in
    [Unreached]. [label] names the events of the taken [edges] (default: [""]).

    Handlers on elements with [disabled] set are not enabled. [max_states]
    defaults to 100000. [within] bounds an unbounded state space: states outside
    it are neither checked nor expanded, so [Ok] holds for the bounded space
    only. The checker does not enter component nodes in the view: it lists them
    in [not_explored] and keeps their props in [children]. To check a child,
    call [component] on its own machine with [~props] from {!props}. *)

val live :
  ?max_states:int ->
  ?within:('s -> bool) ->
  ?inputs:('s -> 'e list) ->
  ?samples:sample list ->
  ?arrivals:int ->
  ?allow_unreached:string list ->
  ?label:('e -> string) ->
  ?keys:Silica.Key.t list ->
  ?invariants:('s, 'e) invariant list ->
  ('s, 'e) Silica.Live.t ->
  ('s, (unit, 'e) input) report
(** Like {!component} for a Live component, whose machine runs on the server. A
    reachable state that owns a client-only source ({!Silica.Source.focus_trap},
    [scroll_lock], [outside_press], [keys], [resize], [intersection], [dismiss]
    or [anchor], [scroll], [visibility], [online], [block]) is a [Violation]
    named ["live-uses-client-source"]: the server has no page to run it on. A
    step that runs [Measure], [Copy] or [Load_script] is a [Violation] named
    ["live-uses-client-action"]: the protocol carries no answer of the client
    back to the machine (see {!Silica.Protocol.host_of_host_action}). So is a
    step that runs [Resume], [Revert_navigation], [Preload] or [Set_status]:
    they never cross the wire. *)

type 's app_state = {
  router : 's;  (** The snapshot of the root machine. *)
  history : Silica.Url.t list;  (** Oldest first. *)
  index : int;  (** Position of the current URL in [history]. *)
}

type 'e app_event =
  | Event of 'e  (** An event of the root machine. *)
  | Link of Silica.Url.t  (** Press on a [data-silica-link] anchor. *)
  | Browser_back
  | Browser_forward

val app :
  ?max_states:int ->
  ?within:('s app_state -> bool) ->
  ?max_history:int ->
  ?inputs:('s app_state -> 'e list) ->
  ?samples:sample list ->
  ?arrivals:int ->
  ?allow_unreached:string list ->
  ?label:('e -> string) ->
  ?keys:Silica.Key.t list ->
  ?invariants:('s app_state, 'e) invariant list ->
  routes:'s list ->
  parse:(Silica.Url.t -> 's) ->
  print:('s -> Silica.Url.t) ->
  ?is_not_found:('s -> bool) ->
  url_event:(Silica.Url.t -> 'e) ->
  (Silica.Url.t -> (unit, 's, 'e) Silica.Component.t) ->
  ('s app_state, 'e app_event) report
(** Explores the root component of a routed app, for example the result of
    [Router.make], started at [print] of the first route, with a simulated
    history. The root view is the only view rendered: the page is a component
    node in it, so the page is listed in [not_explored] and its machine is not
    run. Actions [Navigate], [Replace], [Back], [Forward] and [Reload] change
    the simulated history and become the event [url_event url] of the root
    machine. [Resume], [Revert_navigation], [Preload] and [Set_status] do
    nothing here. [max_history] (default 4) bounds the history; the oldest
    entries are dropped, which keeps the state space finite.

    Besides the component built-ins, two invariants hold:
    - [route-round-trip]: [parse (print route) = route] for every route in
      [routes] and for the route of every reached state, so a
      {!Silica.Router.val-route} whose constructor and data function disagree is
      found. Routes with a query: list one value with the default and one
      without.
    - [link-parses]: for the [href] of every [data-silica-link] anchor in a
      rendered view, [print (parse href)] is the same URL string (the link is
      canonical), and, when [is_not_found] is given, [parse href] is not the
      not-found route. A [parse] that raises [Failure] fails the invariant. *)

val pp_app_event :
  (Format.formatter -> 'e -> unit) -> Format.formatter -> 'e app_event -> unit
(** Print an app event with the given event printer. *)

val pp_report :
  pp_event:(Format.formatter -> 'ev -> unit) ->
  ?pp_state:(Format.formatter -> 's -> unit) ->
  Format.formatter ->
  ('s, 'ev) report ->
  unit
(** Prints a counterexample as numbered steps. *)
