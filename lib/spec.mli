(** The shape of a machine as a checker run found it: the states it reached, the
    steps it took with the names of their events, and the timers and sources of
    each state. [Check.run] gives one in the [specs] of its report. *)

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

type label = { from : string; target : string; event : string; kept : bool }
(** A step that a checker run took, with the name of the event that took it.
    [event] is [""] when the machine gives no event names. [kept] is [true] for
    a step that kept the state ([Go] to a state with the same name, or [Stay]:
    the data may change, the timers and sources keep running); then
    [target = from]. *)

type t = {
  id : string;
  init : string;
  states : state list;
  edges : label list;
      (** Sorted, without duplicates: the steps of the run. A step that entered
          a state, from another state or from the same one ([Reenter]), has
          [kept = false]; a step that kept the state has [kept = true]. *)
}

val arrows : t -> (string * string) list
(** The pairs [(from, target)] of the steps that entered a state, sorted,
    without duplicates: the arrows of a diagram. Kept steps are not arrows. *)

type kind = Unreachable | Dead_end
type issue = { machine : string; state : string; kind : kind }

val lint : t -> issue list
(** [Unreachable]: no path from the initial state. [Dead_end]: a state that is
    not final and has no arrow to another state, in a machine with more than one
    state. Self arrows do not count for either. *)

val issue_to_string : issue -> string
(** ["machine.state: unreachable"] or ["machine.state: non-final dead end"]. *)

val details : state -> string list
(** The timers and sources of a state as lines: ["after 300ms"],
    ["invoke /api/save"], ["hold focus_trap"]. *)

val to_dot : t -> string
(** A Graphviz digraph. Timers and sources are listed in the state label; final
    states are double circles. An arrow shows the events of its edges; kept
    steps are not shown. *)

val to_quint : t -> string
(** A Quint module named after the machine id, with ['-'] as ['_']. Variables:
    [state] (one constructor for each state), [running] (the timers and sources
    that run, as strings ["state/after 300ms"], ["state/invoke /api/path"] and
    ["state/hold name"]) and [last] (the event of the last step: [Start] at
    init, the event of the edge, or [Any] for an edge with no event name). One
    transition for each arrow and event, and one for each kept step; each
    transition is written as a Quint [action]. [enter] starts the resources of
    the target and stops the others. The transition of a kept step is named
    [<state>_stays_on_<event>] and changes only [last]. Invariants:
    [resources_owned] (every running resource belongs to the current state),
    [can_move] (the state is final, has an edge or a kept step) and [inv], both
    together. Handler code and data are not exported: the model holds the edges
    only, so it allows more runs than the machine. *)

val to_text : t -> string
(** A line based text form to read and diff. One line for each item, in the
    order of [t]:
    {v
machine toast
initial empty
state empty
state done final
state running
  after 100ms
  invoke /api/save
  hold focus_trap
edge running -> done on tick
edge running -> empty
stay running on props tick
    v}
    One [edge] line for each arrow, with the names of the events of its edges;
    one [stay] line for each state with kept steps, with their event names. An
    edge whose only event name is [""] has no [on]. A word with a space, a
    quote, a backslash or no character is written in double quotes with
    backslash escapes. Kept steps of a state that is not in [states] are left
    out. *)

val of_text : string -> (t, string) result
(** Reads {!to_text}. The edges come back sorted, without duplicates, as [Check]
    gives them. [Error] names the line it cannot read. *)

val to_mermaid : t -> string
(** A Mermaid [stateDiagram-v2] block, left to right. A state shows its
    {!details} in parentheses; an arrow shows the events of its edges; final
    states lead to [[*]]. Kept steps are not shown. *)

val to_json : t -> Silica_json.Json.t
(** An object with the fields ["id"], ["init"], ["states"] (objects with
    ["name"], ["final"], ["timers"], ["sources"], ["held"]) and ["edges"]
    (objects with ["from"], ["target"], ["event"], ["kept"]). *)

val of_json : Silica_json.Json.t -> (t, string) result
(** Reads {!to_json}. [Error] names the field it cannot read. *)
