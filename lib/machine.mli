(** Statecharts over a plain variant.

    The states are the constructors of a plain OCaml variant, with their data
    inside, and one function decides every step:

    {[
      type state = Idle | Saving | Saved of string | Failed of Remote.error
      [@@deriving state_shape]

      let machine =
        Machine.make ~id:"save" ~init:Idle ~shape:state_shape
          ~owns:(function
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

    A component with props uses {!make_with_props}: the update, [owns], [entry]
    and [exit] get the props, and the props are not an event. *)

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
          that owns a timer. *)

val map_next : ('a -> 'b) -> ('a, 'e) next -> ('b, 'e) next
(** Change the snapshot of a [Go], [Go_with] or [Reenter]. A helper that decides
    the step of a part of the state, such as a record field, gives a [next] of
    that part; [map_next] puts the part back into the whole state. *)

type ('p, 's, 'e) t
(** A machine with props ['p], snapshots ['s] and events ['e]. *)

type 's shape
(** What a machine knows about its state type: the name of the state of each
    snapshot, and the list of every name. Make it with {!val-shape}, or with the
    deriver [[@@deriving state_shape]] of [silica.ppx]:

    {[
      type state = Idle | Saving | Saved of string [@@deriving state_shape]
      (* state_shape : state Machine.shape *)
    ]}

    The deriver names each constructor in lower case ([Saved] is ["saved"]);
    [[@state_name "x"]] on a constructor gives another name, and two
    constructors with one name are a compile error. *)

val shape : name:('s -> string) -> ?names:string list -> unit -> 's shape
(** [shape ~name ?names ()]: [name] names the state of a snapshot in the spec,
    the checker and the ids of timers and sources. [names] lists every name, for
    the checker to report the states that a run never reached. *)

val shape_name : 's shape -> 's -> string
(** The name of the state of a snapshot. *)

val shape_names : 's shape -> string list
(** Every name, or [[]] when the shape has no [names]. *)

val make :
  id:string ->
  init:'s ->
  shape:'s shape ->
  ?owns:('s -> 'e Source.t list) ->
  ?entry:('s -> 'e Action.t list) ->
  ?exit:('s -> 'e Action.t list) ->
  ?final:('s -> bool) ->
  ?start:'e Action.t list ->
  ('s -> 'e -> ('s, 'e) next) ->
  (unit, 's, 'e) t
(** [make ~id ~init ~shape update] is a machine with no props, for example
    [Machine.make ~id:"save" ~init:Idle ~shape:state_shape ~owns update].

    [id] names the machine in the spec, in the checker and in the ids of timers
    and sources. [shape] names its states (see {!type-shape}). [owns] lists the
    timers ({!Source.after}), calls ({!Remote.call}) and sources of a snapshot
    (see {!Source.key}). [entry] and [exit] give the actions of entering and
    leaving a state. A [final] snapshot handles no event. [start] are actions
    that run once, before the entry of [init].

    The update ends with [| _ -> Ignore] in most machines; the checker reports
    an event that the view offers and the machine ignores (see [Check]). *)

val make_with_props :
  id:string ->
  init:'s ->
  shape:'s shape ->
  ?owns:('p -> 's -> 'e Source.t list) ->
  ?entry:('p -> 's -> 'e Action.t list) ->
  ?exit:('p -> 's -> 'e Action.t list) ->
  ?final:('s -> bool) ->
  ?start:'e Action.t list ->
  ?on_props:('p option -> 'p -> 's -> ('s, 'e) next) ->
  ('p -> 's -> 'e -> ('s, 'e) next) ->
  ('p, 's, 'e) t
(** Like {!make}, for a component with props: the parent gives them with
    {!Component.node}, and the update, [owns], [entry] and [exit] get the props
    of the moment. [on_props previous props state] runs when the component
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
(** The [names] of the shape of the machine. *)

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
      (** A timer, call, subscription or held source that a snapshot owns. [id]
          is the id used by the actions [Start_timer], [Start_call],
          [Start_source], [Start_live] and [Stop] of {!Action.host}. *)

val active : ('p, 's, 'e) t -> 'p -> 's -> resource list
(** The timers and sources that the snapshot owns. After [start], [step] or
    [set_props], the actions and this list agree: a resource is running if and
    only if its id was started and not stopped. *)

val held : ('p, 's, 'e) t -> 'p -> 's -> 'e Action.source list
(** The held sources (see {!Source.focus_trap}) that the snapshot owns. The
    checker reads them to know which presses can happen. *)

val can : ('p, 's, 'e) t -> 'p -> 's -> 'e -> bool
(** The machine handles the event in this snapshot: it is not final and the
    update does not give [Ignore]. A view can disable a button with it. *)

type how = Unhandled | Kept | Entered

val start : ('p, 's, 'e) t -> 'p -> 's * 'e Action.host list
(** The initial snapshot after the [start] actions of the machine, the actions
    of entering it, and [on_props None props]. *)

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
    when it delivered. A handled step that keeps the name and still owns a
    source with that id starts it again, so a state that owns
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
