(** Statecharts over a plain variant.

    The states are the constructors of a plain OCaml variant, with their data
    inside, and one function decides every step:

    {[
      type event = Save | Answer of (string, string) Remote.answer | Reset
      type state = Idle | Saving | Saved of string | Failed of Remote.error

      let name = function
        | Idle ->
            "idle"
        | Saving ->
            "saving"
        | Saved _ ->
            "saved"
        | Failed _ ->
            "failed"

      let machine =
        Machine.make ~id:"save" ~init:Idle ~name
          ~during:(function
            | Saving ->
                [ Remote.call save "draft" (fun answer -> Answer answer) ]
            | Saved _ ->
                [ Source.after 2000 Reset ]
            | Idle | Failed _ ->
                []
            )
          (fun state event ->
            match (state, event) with
            | (Idle | Failed _), Save ->
                Go Saving
            | Saving, Answer (Ok time) ->
                Go (Saved time)
            | _ ->
                Ignore
          )
    ]}

    The machine enters a state when its [name] changes. A step to a state with
    the same name keeps its timers and sources running, except those whose
    {!Source.key} changed; {!Reenter} stops and starts them all again. Hosts
    carry this out through the actions {!Action.host}: [Start_timer],
    [Start_call], [Start_live], [Start_source] and [Stop]; a host drops a timer
    or answer whose id was stopped.

    A component with props uses {!make_with_props}: the update, [during],
    [entry] and [exit] get the props, and the props are not an event. *)

type ('s, 'e) next =
  | Ignore  (** The event is not handled: the snapshot does not change. *)
  | Stay of 'e Action.t list
      (** Handled: keep the snapshot and run the actions. *)
  | Go of 's
      (** Go to the snapshot. When its name is the name of the current one, only
          the data changes: no exit, no entry, and the timers and sources whose
          key did not change keep running. *)
  | Go_with of 's * 'e Action.t list
      (** Like [Go], and run the actions between the exit and the entry. *)
  | Reenter of 's
      (** Like [Go], but exit and enter also when the name does not change: all
          timers and sources start again. A debounce is [Reenter] on a state
          with a timer in [during]. *)

val map_next : ('a -> 'b) -> ('a, 'e) next -> ('b, 'e) next
(** Change the snapshot of a [Go], [Go_with] or [Reenter]. A helper that decides
    the step of a part of the state, such as a record field, gives a [next] of
    that part; [map_next] puts the part back into the whole state. *)

type ('p, 's, 'e) t
(** A machine with props ['p], snapshots ['s] and events ['e]. *)

val make :
  id:string ->
  init:'s ->
  name:('s -> string) ->
  ?states:string list ->
  ?event_name:('e -> string) ->
  ?during:('s -> 'e Source.t list) ->
  ?entry:('s -> 'e Action.t list) ->
  ?exit:('s -> 'e Action.t list) ->
  ?final:('s -> bool) ->
  ?start:'e Action.t list ->
  ('s -> 'e -> ('s, 'e) next) ->
  (unit, 's, 'e) t
(** [make ~id ~init ~name update] is a machine with no props, for example
    [Machine.make ~id:"save" ~init:Idle ~name ~during update].

    [id] names the machine in the spec, in the checker and in the ids of timers
    and sources. [name] names the state of a snapshot: two snapshots with one
    name are one state, which the machine enters once.

    [states] lists every name that [name] gives, so that the checker can report
    the states that no run reached; with no [states], the checker says that it
    cannot report them for this machine.

    [event_name] names an event on the edges of the spec, in the checker and in
    traces, and [Check.event] finds an event by it. With no [event_name], the
    tools label an event by its cause, such as [press button "Save"],
    [after 2000ms] or [ok /api/save] (see {!Transition.cause_label}).

    [during] lists the timers ({!Source.after}), calls ({!Remote.call}) and
    sources that run during a snapshot (see {!Source.key}). [entry] and [exit]
    give the actions of entering and leaving a state. A [final] snapshot handles
    no event. [start] are actions that run once, before the entry of [init].

    The update ends with [| _ -> Ignore] in most machines; the checker reports
    an event that the view offers and the machine ignores (see [Check]). *)

val make_with_props :
  id:string ->
  init:'s ->
  name:('s -> string) ->
  ?states:string list ->
  ?event_name:('e -> string) ->
  ?during:('p -> 's -> 'e Source.t list) ->
  ?entry:('p -> 's -> 'e Action.t list) ->
  ?exit:('p -> 's -> 'e Action.t list) ->
  ?final:('s -> bool) ->
  ?start:'e Action.t list ->
  ?on_props:('p option -> 'p -> 's -> ('s, 'e) next) ->
  ('p -> 's -> 'e -> ('s, 'e) next) ->
  ('p, 's, 'e) t
(** Like {!make}, for a component with props: the parent gives them with
    {!Component.node}, and the update, [during], [entry] and [exit] get the
    props of the moment. [on_props previous props state] runs when the component
    starts ([previous = None]) and when the parent gives other props; it
    defaults to [Ignore]. After it, the sources are compared by key, as after a
    step, so a source that depends on the props starts again when they change.
*)

val id : ('p, 's, 'e) t -> string
(** The id of the machine, as given to {!make}. *)

val init : ('p, 's, 'e) t -> 's
(** The snapshot that the machine starts in. *)

val name : ('p, 's, 'e) t -> 's -> string
(** The name of the state of the snapshot. *)

val states : ('p, 's, 'e) t -> string list option
(** The [states] given to {!make}. *)

val event_name : ('p, 's, 'e) t -> 'e -> string option
(** The name of the event by the [event_name] given to {!make}, or [None] when
    the machine has none. *)

val register_codecs :
  ('p, 's, 'e) t ->
  ?props:'p Codec.t ->
  ?state:'s Codec.t ->
  ?event:'e Codec.t ->
  unit ->
  unit
(** [register_codecs machine ?props ?state ?event ()] gives the machine JSON
    codecs of its props, snapshots and events, for traces, replay and
    development tools. A second call for the same machine replaces the first.

    The state codec also lets a machine keep its snapshot across a hot reload
    and a restart of the server under [silica dev]: the old code encodes the
    snapshot and the new code decodes it (see {!Handoff}). A machine with no
    state codec starts again, unless it is at the snapshot it starts in.

    The codecs are not part of the machine definition on purpose: only the code
    that calls [register_codecs] references them. Call it from development entry
    points (tests, the checker suite, a development client), and a release
    bundle that never calls it carries no codec. Leave out a codec when the
    values hold closures or other data that is not plain data. *)

val props_codec : ('p, 's, 'e) t -> ('p Codec.t, string) result
(** The props codec given to {!register_codecs}, or an [Error] as for
    {!state_codec}. *)

val state_codec : ('p, 's, 'e) t -> ('s Codec.t, string) result
(** The state codec given to {!register_codecs}. [Error] names the machine and
    says how to register it. *)

val event_codec : ('p, 's, 'e) t -> ('e Codec.t, string) result
(** The event codec given to {!register_codecs}, or an [Error] as for
    {!state_codec}. *)

val registered_codecs : unit -> string list
(** The ids of the machines with registered codecs, the latest first. A test of
    an app checks with it that the codec module registers every machine whose
    events are plain data. *)

val is_final : ('p, 's, 'e) t -> 's -> bool
(** A final state ignores every event. *)

val default_props : ('p, 's, 'e) t -> 'p option
(** [Some ()] for a machine made by {!make}, [None] for one made by
    {!make_with_props}. *)

val type_id : ('p, 's, 'e) t -> ('p * 's * 'e) Type.Id.t
(** A type id made by {!make}, one for each machine. Hosts use it to know that
    two machines with the same id have the same types. *)

type resource =
  | Timer of { id : string; ms : int }
  | Call of { id : string; path : string }
  | Subscription of { id : string; topic : string }
  | Held of { id : string; name : string }
      (** A timer, call, subscription or held source that runs during a
          snapshot. [id] is the id used by the actions [Start_timer],
          [Start_call], [Start_source], [Start_live] and [Stop] of
          {!Action.host}. *)

val active : ('p, 's, 'e) t -> 'p -> 's -> resource list
(** The timers and sources that run during the snapshot. After [start], [step]
    or [set_props], the actions and this list agree: a resource is running if
    and only if its id was started and not stopped. *)

val held : ('p, 's, 'e) t -> 'p -> 's -> 'e Action.source list
(** The held sources (see {!Source.focus_trap}) that run during the snapshot.
    The checker reads them to know which presses can happen. *)

val can : ('p, 's, 'e) t -> 'p -> 's -> 'e -> bool
(** The machine handles the event in this snapshot: it is not final and the
    update does not give [Ignore]. A view can disable a button with it. *)

type how = Unhandled | Kept | Entered

val start : ('p, 's, 'e) t -> 'p -> 's * 'e Action.host list
(** The initial snapshot after the [start] actions of the machine, the actions
    of entering it, and [on_props None props]. *)

val resume : ('p, 's, 'e) t -> 'p -> 's -> 'e Action.host list
(** [resume machine props snapshot] gives the actions that run the machine again
    at a snapshot it reached before, in another process or bundle: the starts of
    the timers, calls, subscriptions and held sources that run during the
    snapshot, in the order of [during], with the ids of {!active}. A timer
    starts with its full delay and a call is sent again.

    It gives no [start] actions, no [entry] and no [on_props]: the snapshot
    entered its state before, when they ran, and their effects (a focus, a
    title, a navigation, a call made by an action) must not happen twice. After
    it, the actions and {!active} agree, as after {!start}. *)

val step :
  ?finished:string ->
  ('p, 's, 'e) t ->
  'p ->
  's ->
  'e ->
  's * 'e Action.host list * how
(** Run the update. A final state handles nothing; [Ignore] leaves the snapshot
    unchanged ([Unhandled]). On a change of state the actions are: [exit] and
    stops of the old state, the actions of the step, [entry] and starts of the
    new state ([Entered]). A step that keeps the name stops the sources whose
    key went away, runs the actions and starts the new sources ([Kept]).
    [Reenter] is [Entered] even when the name stays. Hosts use [Entered] to give
    each entry of a state its own instance (see {!Instance}).

    [finished] is the id of the timer or call that raised the event: it ended
    when it delivered. A handled step that keeps the name and still has a source
    with that id in [during] starts it again, so a state whose [during] gives
    [Source.after 1000 Tick] and handles [Tick] with [Go] ticks every second.
    Hosts give it for timers and calls; subscriptions and held sources do not
    end when they deliver. *)

val set_props :
  ('p, 's, 'e) t -> previous:'p -> 'p -> 's -> 's * 'e Action.host list * how
(** The parent gave new props: run [on_props (Some previous) props], then
    compare the sources by key. [Unhandled] when nothing changed. *)

val exit : ('p, 's, 'e) t -> 'p -> 's -> 'e Action.host list
(** The actions of leaving the snapshot: [exit], then the stops of its timers
    and sources. *)
