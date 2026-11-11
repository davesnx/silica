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

val not_view : broken -> _ Silica_memory.t -> string -> unit
(** Any other rule or an [always] or [never_after] property: fails while the
    {!Silica.View_text} of the page is the view that broke the rule. *)

val does_not_wait : broken -> _ Silica_memory.t -> string -> unit
(** A [leads_to] property with a [Waits] counterexample: fails while the page
    waits for the user (no pending request and no timer) in the view that broke
    the rule, which the to_ condition did not accept. *)

val leaves_cycle : broken -> _ Silica_memory.t -> string -> unit
(** A [leads_to] property with a [Loop] counterexample, after the stem and one
    round of the cycle: fails while the page is back at the view where the cycle
    starts, so the run can repeat forever. *)

val props : ('p, _, _) Silica.Component.t -> string -> 'p
(** [props component json]: the props in the JSON text, by the props codec of
    the machine (see {!Silica.Machine.register_codecs}). Exits with 1 when they
    do not decode. *)
