(** The assertions of the tests that {!Check.script} writes: each one states a
    rule that a counterexample broke, in terms of what a {!Silica_memory} page
    shows, and fails while the bug is there.

    A failing assertion prints the rule, its message, its hint and what it saw
    on standard error, and exits with code 1, as a dune test does. *)

type broken
(** A rule that a counterexample broke, in the state where it broke. *)

val rule : string -> state:string -> message:string -> hint:string -> broken
(** [rule id ~state ~message ~hint]: the rule (a built-in rule id or a property
    name) with the message and hint of the checker. *)

val fail : broken -> string -> 'a
(** Print the rule, its message and hint, then the text, and exit with 1. *)

val handles :
  broken ->
  ?position:int ->
  ?name:string ->
  _ Silica_memory.t ->
  Silica.Accessibility.role ->
  (Silica_memory.element -> unit) ->
  unit
(** [handles broken page role ~name input] ([ignored-event]): when the page
    shows an element of [role] with [name] (the one at [position], from 0, when
    several match), give it [input]; fails when the update of the instance
    ignores it. Passes when no such element is shown, or when it is disabled,
    hidden or inert (see {!Silica_memory.Not_reachable}): the user cannot give
    that input any more. *)

val handles_input : broken -> _ Silica_memory.t -> (unit -> unit) -> unit
(** [handles_input broken page input] ([ignored-event] for a timer or an
    answer): run [input]; fails when the update ignores the step it gives. *)

val on_error : broken -> exn -> Silica.Node.error -> unit
(** [raised]: give it as [~on_error] to the mount; it fails at the first error
    that reaches the error page. *)

val view_passes : broken -> _ Silica_memory.t -> unit
(** A rule of the view: fails while {!Check.view_violations} of the rule finds
    the rule broken on the view of the page. *)

val holds :
  broken ->
  ?cycle_from:int ->
  ?start:int ->
  ('s, 'ev) Check.t ->
  _ Silica_memory.t ->
  (unit -> unit) ->
  unit
(** [holds broken check page steps] runs [steps] on the page, then replays the
    run of the page in [check] ({!Check.replay_run}) and fails while the rule of
    [broken] fails at a point of that run: a property ([always], [leads_to],
    [never_after]) or a built-in rule, evaluated as the checker evaluates it, on
    the real states and views the steps reached. So the test passes exactly when
    the rule holds on that run, and a fix that only changes the view does not
    pass it. When a step cannot run any more (its element is disabled, hidden or
    gone, see {!Silica_memory.Not_reachable}), the run stops there and the rule
    is evaluated on the steps that ran. [cycle_from] and [start] are as in
    {!Check.replay_run}. It also fails when the run cannot be replayed in the
    check. *)
