(** The static shape of a machine: states, declared transitions, timers and
    sources. Built by {!Machine.spec} without running any handler. *)

type state = {
  name : string;
  final : bool;
  timers : int list;  (** Delays in milliseconds of the [after] timers. *)
  sources : string list;  (** Endpoint paths of the [invoke] calls. *)
  held : string list;
      (** Names of the other [invoke] sources, such as ["focus_trap"]: they are
          resources of a client renderer. *)
}

type t = {
  id : string;
  init : string;
  states : state list;
  edges : (string * string) list;
      (** Sorted, without duplicates. An edge comes from a [~to_] entry, from
          [always ~to_] for every state that is not final, or as a self edge
          from a state that has a timer, a source, an [entry] or an [exit]:
          [reenter] to the same state runs them again. [goto] to the same state
          adds no edge. *)
}

type kind = Unreachable | Dead_end
type issue = { machine : string; state : string; kind : kind }

val lint : t -> issue list
(** [Unreachable]: no path from the initial state. [Dead_end]: a state that is
    not final and has no edge to another state, in a machine with more than one
    state. Self edges do not count for either. *)

val issue_to_string : issue -> string
(** ["machine.state: unreachable"] or ["machine.state: non-final dead end"]. *)

type label = { from : string; target : string; event : string; kept : bool }
(** A step that a checker run took, with the name of the event that took it.
    [event] is [""] when the run gave no names. [kept] is [true] for a step that
    kept the state ([goto] to the current state or [stay]: the data may change,
    the timers and sources keep running); then [target = from]. *)

val to_dot : ?labels:label list -> t -> string
(** A Graphviz digraph. Timers and sources are listed in the state label; final
    states are double circles. An edge shows the events of its [labels]; kept
    steps are not shown. *)

val to_quint : ?labels:label list -> t -> string
(** A Quint module named after the machine id, with ['-'] as ['_']. Variables:
    [state] (one constructor for each state), [running] (the timers and sources
    that run, as strings ["state/after 300ms"], ["state/invoke /api/path"] and
    ["state/hold name"]) and [last] (the event of the last step: [Start] at
    init, the label of the edge, or [Any] for an edge with no label). One action
    for each pair of a declared or labelled edge and its event; [enter] starts
    the resources of the target and stops the others. One action for each kept
    step, named [<state>_stays_on_<event>], which changes only [last].
    Invariants: [resources_owned] (every running resource belongs to the current
    state), [can_move] (the state is final, has an edge or a kept step) and
    [inv], both together. Handler code and data are not exported: the model
    holds the edges only, so it allows more runs than the machine. *)
