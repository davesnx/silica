(** Statecharts with typed states.

    A machine has one [State] module: a GADT whose constructors are the states
    and whose index is the data the state carries. States must be constant
    constructors. A snapshot is [At (state, data)]. The machine is a table with
    one case for each state, built by {!Make}.

    Timers ([after]) and sources ([invoke]) belong to the state. They start on
    entry and stop on exit. [reenter] to the current state stops and starts them
    again. Hosts carry this out through the actions {!Action.host}:
    [Start_timer], [Start_call] and [Stop]; a host drops a timer or answer whose
    id was stopped. *)

exception
  Undeclared_target of { machine : string; from : string; target : string }
(** A handler went to a state that is not in the [~to_] list of the case that
    handled the event. Going to the current state needs no entry. [target] is
    ["#n"] when the state has no case; [n] is its index as in {!Missing_case}.
*)

exception Missing_case of { machine : string; index : int }
(** A state has no case. [index] is the position of the state constructor in the
    [State] type, counted from 0. Raised by [make] for the initial state and for
    every state in a [to_] list, so a handler cannot reach a state with no case:
    going to one that is not in [to_] raises {!Undeclared_target}. *)

type ('s, 'e) t
(** A machine over snapshots ['s] and events ['e]. *)

type ('s, 'e) outcome
(** What a handler decided. Built with [goto], [reenter], [stay] and [unhandled]
    from {!Make}. *)

type resource =
  | Timer of { id : string; ms : int }
  | Call of { id : string; path : string }
  | Subscription of { id : string; topic : string }
  | Held of { id : string; name : string }
      (** A timer, call, subscription or held source that the state of a
          snapshot owns. [id] is the id used by the actions [Start_timer],
          [Start_call], [Start_source], [Start_live] and [Stop] of
          {!Action.host}. *)

val id : ('s, 'e) t -> string
(** The id of the machine, as given to {!Make.make}. *)

val init : ('s, 'e) t -> 's
(** The snapshot that the machine starts in. *)

val key : ('s, 'e) t -> 's -> string
(** The name of the state of the snapshot. *)

val is_final : ('s, 'e) t -> 's -> bool
(** A final state ignores every event, including [always] handlers. *)

val spec : ('s, 'e) t -> Spec.t
(** The states, events and transitions of the machine, for the checker and for
    export. *)

val type_id : ('s, 'e) t -> ('s * 'e) Type.Id.t
(** A type id made by [Make.make] and by {!map_snapshot}, one for each machine.
    The checker uses it to find the props that a parent gives to this machine.
*)

val active : ('s, 'e) t -> 's -> resource list
(** The timers and sources that the state of the snapshot owns. After [start] or
    [send], the actions and this list agree: a resource is running if and only
    if its id was started and not stopped. *)

val held : ('s, 'e) t -> 's -> 'e Action.source list
(** The held sources (see {!Source.focus_trap}) that the state of the snapshot
    owns. The checker reads them to know which presses can happen. *)

val start : ('s, 'e) t -> 's * 'e Action.host list
(** The initial snapshot, the [start] actions of the machine and the actions of
    entering it. *)

val exit : ('s, 'e) t -> 's -> 'e Action.host list
(** The actions of leaving the snapshot: [exit], then timer and source stops. *)

val step :
  ('s, 'e) t ->
  's ->
  'e ->
  's * 'e Action.host list * [ `Unhandled | `Kept | `Entered ]
(** Like {!send}, and tells whether the step entered a state: [`Entered] when
    the state changed or the handler used [reenter], [`Kept] when the state
    stayed (a [goto] to the current state, or [stay]). Hosts use it to give each
    entry of a state its own instance (see {!Instance}). *)

val send :
  ('s, 'e) t -> 's -> 'e -> 's * 'e Action.host list * [ `Handled | `Unhandled ]
(** Run the handler of the current state, then the [always] handlers in order. A
    final state handles nothing. An unhandled event leaves the snapshot
    unchanged. On a change of state the actions are: [exit] and stops of the old
    state, the actions of the handler, [entry] and starts of the new state.
    [goto] to the current state keeps the state's timers and sources; [reenter]
    restarts them.

    @raise Undeclared_target when the target is not declared. *)

val map_snapshot : ('s, 'e) t -> get:('s -> 'a) -> set:('a -> 's) -> ('a, 'e) t
(** The same machine over another view of the snapshot. [get (set a)] must equal
    [a]. Used by {!Remote_machine} and by machines that show a plain record or
    variant while they run on a GADT snapshot. *)

module Make (State : sig
  type 'p t
end) : sig
  type snapshot = At : 'p State.t * 'p -> snapshot

  type targets =
    | []
    | ( :: ) : 'p State.t * targets -> targets
        (** A list of states with different data, as in [~to_:[ A; B ]]. *)

  type 'e case
  (** One row of the table: a state, or [always]. *)

  val at : 'p State.t -> 'p -> snapshot
  (** [at state data] is the snapshot in [state] with [data]. *)

  val same : 'a State.t -> 'b State.t -> bool
  (** Two states are the same when they are the same constructor. *)

  val goto :
    ?actions:'e Action.t list -> 'p State.t -> 'p -> (snapshot, 'e) outcome
  (** Go to a state. Going to the current state only changes its data: no exit,
      no entry, timers and sources keep running. *)

  val reenter :
    ?actions:'e Action.t list -> 'p State.t -> 'p -> (snapshot, 'e) outcome
  (** Like [goto], but exit and enter the state even when it is the current one.
      A debounce is [reenter] on a state with an [after] timer. *)

  val stay : 'e Action.t list -> (snapshot, 'e) outcome
  (** Handle the event, keep the snapshot, run the actions. *)

  val unhandled : (snapshot, 'e) outcome
  (** The event has no handler in this state. The [always] handlers get it next;
      if none takes it, the snapshot does not change. *)

  val ignore : 'p -> 'e -> (snapshot, 'e) outcome
  (** The handler of a state with no events. Same as [fun _ _ -> unhandled]. *)

  val state :
    'p State.t ->
    string ->
    ?to_:targets ->
    ?entry:('p -> 'e Action.t list) ->
    ?exit:('p -> 'e Action.t list) ->
    ?after:(int * 'e) list ->
    ?invoke:('p, 'e) Source.t list ->
    ?final:bool ->
    ('p -> 'e -> (snapshot, 'e) outcome) ->
    'e case
  (** [state st name ... handler] is the case of [st]. [to_] declares the states
      the handler may go to. [after] timers, in milliseconds, and [invoke]
      sources start on entry and stop on exit. [name] is the name in the spec,
      the checker and the ids of timers and sources. *)

  val always : ?to_:targets -> ('e -> (snapshot, 'e) outcome) -> 'e case
  (** A handler tried in every state that is not final, after the handler of the
      state returns [unhandled]. [to_] declares its targets. *)

  val make :
    id:string ->
    init:snapshot ->
    ?start:'e Action.t list ->
    'e case list ->
    (snapshot, 'e) t
  (** [start] are actions that run once when the machine starts, before the
      entry of [init].

      @raise Missing_case
        when the initial state, or a state in a [to_] list, has no case. *)

  val cases_cover : (snapshot, 'e) t -> targets -> unit
  (** For tests. Checks that every listed state has a case.

      @raise Missing_case for the first listed state with no case. *)
end
