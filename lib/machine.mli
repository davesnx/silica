(** Statecharts with typed states.

    A machine has one [State] module: a GADT whose constructors are the states
    and whose index is the data the state carries. States must be constant
    constructors. A snapshot is [At (state, data)]. The machine is a table with
    one case for each state, built by {!Make}.

    Timers ([after]) and sources ([invoke]) belong to the state. They start on
    entry and stop on exit. [reenter] to the current state stops and starts them
    again. Hosts carry this out through the commands {!Cmd.host}: [Start_timer],
    [Start_call] and [Stop]; a host drops a timer or answer whose id was
    stopped. *)

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
          snapshot owns. [id] is the id used by the commands [Start_timer],
          [Start_call], [Start_source], [Start_live] and [Stop] of {!Cmd.host}.
      *)

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
    [send], the commands and this list agree: a resource is running if and only
    if its id was started and not stopped. *)

val held : ('s, 'e) t -> 's -> 'e Cmd.source list
(** The held sources (see {!Source.focus_trap}) that the state of the snapshot
    owns. The checker reads them to know which presses can happen. *)

val start : ('s, 'e) t -> 's * 'e Cmd.host list
(** The initial snapshot, the [start] commands of the machine and the commands
    of entering it. *)

val exit : ('s, 'e) t -> 's -> 'e Cmd.host list
(** The commands of leaving the snapshot: [exit], then timer and source stops.
*)

val step :
  ('s, 'e) t ->
  's ->
  'e ->
  's * 'e Cmd.host list * [ `Unhandled | `Kept | `Entered ]
(** Like {!send}, and tells whether the step entered a state: [`Entered] when
    the state changed or the handler used [reenter], [`Kept] when the state
    stayed (a [goto] to the current state, or [stay]). Hosts use it to give each
    entry of a state its own instance (see {!Instance}). *)

val send :
  ('s, 'e) t -> 's -> 'e -> 's * 'e Cmd.host list * [ `Handled | `Unhandled ]
(** Run the handler of the current state, then the [always] handlers in order. A
    final state handles nothing. An unhandled event leaves the snapshot
    unchanged. On a change of state the commands are: [exit] and stops of the
    old state, the commands of the handler, [entry] and starts of the new state.
    [goto] to the current state keeps the state's timers and sources; [reenter]
    restarts them.

    @raise Undeclared_target when the target is not declared. *)

val map_snapshot : ('s, 'e) t -> get:('s -> 'a) -> set:('a -> 's) -> ('a, 'e) t
(** The same machine over another view of the snapshot. [get (set a)] must equal
    [a]. Used by {!Remote_machine} and by machines that show a plain record or
    variant while they run on a GADT snapshot. *)

(** {1 Machines over a plain variant}

    The states are the constructors of a plain variant, with their data inside,
    and one function decides every step:

    {[
      type state = Idle | Saving | Saved of string | Failed of Remote.error

      let machine =
        Machine.make ~id:"save" ~init:Idle ~name:state_name
          ~owns:(function
            | Saving ->
                [ Source.call save (fun () -> "draft") answer ]
            | Saved _ ->
                [ Source.after 2000 Reset ]
            | Idle | Failed _ ->
                []
            )
          (fun state event ->
            match (state, event) with
            | (Idle | Failed _), Save ->
                Go Saving
            | _ ->
                Ignore
          )
    ]}

    The machine enters a state when [name] changes. A step to a state with the
    same name keeps its timers and sources running; [Reenter] stops and starts
    them again. *)

type ('s, 'e) next =
  | Ignore  (** The event is not handled: the snapshot does not change. *)
  | Stay of 'e Cmd.t list
      (** Handled: keep the snapshot and run the commands. *)
  | Go of 's
      (** Go to the snapshot. When its name is the name of the current one, only
          the data changes: no exit, no entry, timers and sources keep running.
      *)
  | Go_with of 's * 'e Cmd.t list
      (** Like [Go], and run the commands between the exit and the entry. *)
  | Reenter of 's
      (** Like [Go], but exit and enter also when the name does not change. *)

val make :
  id:string ->
  init:'s ->
  name:('s -> string) ->
  ?owns:('s -> (unit, 'e) Source.t list) ->
  ?entry:('s -> 'e Cmd.t list) ->
  ?exit:('s -> 'e Cmd.t list) ->
  ?final:('s -> bool) ->
  ?start:'e Cmd.t list ->
  ('s -> 'e -> ('s, 'e) next) ->
  ('s, 'e) t
(** [make ~id ~init ~name update] is a machine over the snapshots ['s].

    [name] names the state of a snapshot in the spec, the checker and the ids of
    timers and sources. [owns] lists the timers ({!Source.after}) and sources of
    a snapshot: they start on entry, with the data of the snapshot then, and
    stop on exit. For one name, [owns] must give the same list of sources, in
    the same order. [entry] and [exit] give the commands of entering and
    leaving. A [final] snapshot handles no event. [start] are commands that run
    once, before the entry of [init].

    The machine declares no edges: {!spec} lists the initial state only and no
    edge. The checker finds the states and edges by running the machine; see the
    [spec] of [Check.Ok]. *)

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

  val goto : ?cmds:'e Cmd.t list -> 'p State.t -> 'p -> (snapshot, 'e) outcome
  (** Go to a state. Going to the current state only changes its data: no exit,
      no entry, timers and sources keep running. *)

  val reenter :
    ?cmds:'e Cmd.t list -> 'p State.t -> 'p -> (snapshot, 'e) outcome
  (** Like [goto], but exit and enter the state even when it is the current one.
      A debounce is [reenter] on a state with an [after] timer. *)

  val stay : 'e Cmd.t list -> (snapshot, 'e) outcome
  (** Handle the event, keep the snapshot, run the commands. *)

  val unhandled : (snapshot, 'e) outcome
  (** The event has no handler in this state. The [always] handlers get it next;
      if none takes it, the snapshot does not change. *)

  val ignore : 'p -> 'e -> (snapshot, 'e) outcome
  (** The handler of a state with no events. Same as [fun _ _ -> unhandled]. *)

  val state :
    'p State.t ->
    string ->
    ?to_:targets ->
    ?entry:('p -> 'e Cmd.t list) ->
    ?exit:('p -> 'e Cmd.t list) ->
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
    ?start:'e Cmd.t list ->
    'e case list ->
    (snapshot, 'e) t
  (** [start] are commands that run once when the machine starts, before the
      entry of [init].

      @raise Missing_case
        when the initial state, or a state in a [to_] list, has no case. *)

  val cases_cover : (snapshot, 'e) t -> targets -> unit
  (** For tests. Checks that every listed state has a case.

      @raise Missing_case for the first listed state with no case. *)
end
