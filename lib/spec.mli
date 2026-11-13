(** The shape of a machine as a checker run found it: the states it reached, the
    steps it took with the names of their events, and the resources of each
    state. [Check.run] gives one in the [specs] of its report. The resources of
    a state are those of every snapshot of it that the run reached: a timer that
    runs only for some data of the state is listed. *)

type state = {
  name : string;
  final : bool;
  timers : int list;  (** Delays in milliseconds of the {!Host.after} timers. *)
  network : string list;
      (** Endpoint paths of the calls and ["live:" ^ topic] of the
          subscriptions. *)
  held : string list;
      (** Names of the held resources, such as ["focus_trap"]: they are
          resources of a client renderer. *)
}

type label = { from : string; target : string; event : string; kept : bool }
(** A step that a checker run took, with the name of the event that took it.
    [event] is [""] when the machine gives no event names. [kept] is [true] for
    a step that kept the state ([Go] to a state with the same name, or [Stay]:
    the data may change, the resources keep running); then [target = from]. A
    step between two sub-states of one state, such as ["list.live"] to
    ["list.offline"] (see {!Machine.make}), keeps the timers and resources but
    changes the name, so it has [kept = false]. *)

type t = {
  id : string;
  init : string;
  states : state list;
  edges : label list;
      (** Sorted, without duplicates: the steps of the run. A step that entered
          a state, from another state or from the same one ([Reenter]), or that
          moved to another sub-state, has [kept = false]; a step that kept the
          state has [kept = true]. *)
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
(** The resources of a state as lines: ["after 300ms"], ["invoke /api/save"],
    ["hold focus_trap"]. *)

val to_dot : t -> string
(** A Graphviz digraph. Timers and resources are listed in the state label;
    final states are double circles. An arrow shows the events of its edges;
    kept steps are not shown. *)

val to_quint : t -> string
(** A Quint module named after the machine id, with ['-'] as ['_']. Variables:
    [state] (one constructor for each state), [running] (the resources that run,
    as strings ["state/after 300ms"], ["state/invoke /api/path"] and
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
    ["name"], ["final"], ["timers"], ["network"], ["held"]) and ["edges"]
    (objects with ["from"], ["target"], ["event"], ["kept"]). *)

val of_json : Silica_json.Json.t -> (t, string) result
(** Reads {!to_json}. [Error] names the field it cannot read. *)

(** A difference between two specs of one machine. *)
type change =
  | Initial_changed of { before : string; after : string }
  | State_added of state
  | State_removed of state
  | State_changed of { before : state; after : state }
      (** The same name, with another [final], timers or resources. *)
  | Edge_added of label
  | Edge_removed of label

val diff : t -> t -> change list
(** [diff before after]: what changed from [before] to [after], in this order:
    the initial state, the removed states, the added and changed states (in the
    order of [after]), the removed edges, the added edges. [[]] when the specs
    are equal. A step that now goes to another target is one removed and one
    added edge. *)

val change_to_string : change -> string
(** The change in words, with the names as {!to_text} writes them:
    {v
    changed initial state from idle to loading
    added state archived
    removed state draft
    changed state saving: now final; added after 300ms; removed invoke /api/save
    removed edge idle -> done on save
    added edge idle -> saving on save
    added stay running on tick
    v} *)

(** {1 Conformance}

    Does a run of the app, as a trace file holds it, stay inside the specs that
    a check found? This level reads names only, so it works on a trace with no
    user data (see {!Trace.names_only}). *)

type problem =
  | No_spec  (** No spec is of the machine of the record. *)
  | Unknown_state of string  (** No spec of the machine has this state. *)
  | Unknown_edge of label
      (** No spec of the machine has this step: an edge, or a kept step. *)

type deviation = {
  seq : int;  (** The [seq] of the record in the trace. *)
  machine : string;
  instance : string;  (** The instance path of the record. *)
  problem : problem;
}

val conforms : t list -> Trace.t -> deviation list
(** [conforms specs trace]: the records of [trace] that [specs] do not allow, in
    the order of the trace. A start, a resume and an end must be in a state of a
    spec of the machine; an unhandled event in a state of it; a step that
    entered a state must be an edge, and a step that kept it a kept step, with
    the same event name ([props] for new props). An event that its machine does
    not name (see {!Transition.label}) matches a step between the same states
    whatever its label: a trace with names only no longer holds the accessible
    name or the answer that the label of the spec shows. A machine can have
    several specs, such as the specs of the joint checks of a parent with each
    of its children: a record conforms when one of them allows it. [[]] when the
    whole trace conforms. *)

val deviation_to_string : deviation -> string
(** One line, such as
    ["transition 14, machine comment-composer, instance app/0:comment-composer:
     no spec has the edge drafting -> archived on send"]. *)
