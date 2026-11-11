(** The shape of a machine as a checker run found it: the states it reached, the
    transitions it took, and the timers and sources of each state. The [spec] of
    [Check.Ok] is one. *)

type state = {
  name : string;
  final : bool;
  timers : int list;
      (** Delays in milliseconds of the {!Source.after} timers. *)
  sources : string list;
      (** Endpoint paths of the calls and ["live:" ^ topic] of the
          subscriptions. *)
  held : string list;
      (** Names of the other sources, such as ["focus_trap"]: they are resources
          of a client renderer. *)
}

type t = {
  id : string;
  init : string;
  states : state list;
  edges : (string * string) list;
      (** Sorted, without duplicates: the steps that entered a state, from
          another state or from the same one ([Reenter]). A step that kept the
          state adds no edge. *)
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
    kept the state ([Go] to a state with the same name, or [Stay]: the data may
    change, the timers and sources keep running); then [target = from]. *)

val to_dot : ?labels:label list -> t -> string
(** A Graphviz digraph. Timers and sources are listed in the state label; final
    states are double circles. An edge shows the events of its [labels]; kept
    steps are not shown. *)

val to_quint : ?labels:label list -> t -> string
(** A Quint module named after the machine id, with ['-'] as ['_']. Variables:
    [state] (one constructor for each state), [running] (the timers and sources
    that run, as strings ["state/after 300ms"], ["state/invoke /api/path"] and
    ["state/hold name"]) and [last] (the event of the last step: [Start] at
    init, the label of the edge, or [Any] for an edge with no label). One
    transition for each pair of an edge and its event, and one for each kept
    step; each transition is written as a Quint [action]. [enter] starts the
    resources of the target and stops the others. The transition of a kept step
    is named [<state>_stays_on_<event>] and changes only [last]. Invariants:
    [resources_owned] (every running resource belongs to the current state),
    [can_move] (the state is final, has an edge or a kept step) and [inv], both
    together. Handler code and data are not exported: the model holds the edges
    only, so it allows more runs than the machine. *)
