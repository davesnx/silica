(** The instance tree: what runs while components are rendered.

    A tree has a root component. Rendering a view that holds component nodes
    (see {!Node.val-component}) mounts, updates and unmounts child instances:

    - A child is identified by its position in the view of its parent, by the
      {!Machine.id} of its machine and by its key, not by the machine value. A
      key keeps the child when the siblings are reordered, a key that is removed
      ends the child, and a key that comes back starts a new one.
    - One id is one machine in a tree. The state of a child is kept only when
      its machine has the same type as before, and the types are known equal
      only for the same {!Machine.make} call ({!Machine.type_id}). So a tree
      that meets two machine values with one id, such as a machine made again in
      a view or two machines that share an id, raises [Invalid_argument] with
      that id, rather than starting the child again at each render. Make each
      machine once, at the top level.
    - A new child starts its machine with its props. A child that stays gets its
      new props when they changed (see {!Machine.set_props}).
    - A child that is no longer rendered ends: the actions of leaving its state,
      and a {!Action.Stop} for every timer and source that still runs, come
      first for its own children, then for it.
    - Each entry of a state gets a number. Timer and source ids are
      ["<instance>.<entry>/<machine>/<state>/<key>"] (see {!Source.key}), so two
      instances of one machine never share an id. A message from a timer or
      source of an earlier entry, or of an ended instance, is dropped.
    - The result event of an action (such as {!Action.Measure}) goes to the
      instance that ran the action, in any state. It is dropped when that
      instance has ended.
    - A view must be a function of its snapshot. An instance whose snapshot is
      physically the same as at its last render, whose props did not change,
      under which no instance stepped and no boundary failed or was reset, and
      that was not remounted, is not rendered again: its last resolved view is
      used.

    Server render, the browser host and the tests use this one tree. The checker
    explores one machine at a time and does not use it. *)

type message
(** An event addressed to one instance. Hosts keep it as it is, and give it back
    to {!update} when the DOM event, timer or answer it stands for happens. *)

type ('p, 's, 'e) t
(** A tree whose root component has props ['p], snapshots ['s] and events ['e].
*)

(** {1 Observing a tree}

    A tree, or the Live runner of the server, can give each start, step and end
    of each instance to an {!observer}. The values it gives hold the typed state
    of the moment and cost nothing to make; {!inspect} reads them, and
    [Transition.observer] turns them into records. All the code that reads them
    is reached only through an observer, so an app with no observer carries none
    of it. *)

type ('p, 's, 'e) cell
(** A running instance in a tree. *)

type change =
  | Started  (** The instance started. *)
  | Resumed
      (** The instance started again at a snapshot it reached before (see
          {!Machine.resume}). *)
  | Unhandled  (** The machine ignored the event or the props. *)
  | Kept  (** The step kept the state name. *)
  | Entered  (** The step entered a state. *)
  | Ended  (** The instance ended. *)

(** What made a step, with the values the host had. *)
type cause =
  | Start
  | Props  (** The parent gave new props. *)
  | End
  | Message  (** A message of the host for the root (see {!val-message}). *)
  | Handler
      (** A handler of the view whose message is made with the view, such as a
          press: the element is the one that holds the delivered message (see
          {!inspected}). *)
  | Element of message Node.attr * string option * Key.t option
      (** The handler attribute, physically as it is in the view the input
          happened on; the text of an input; the key of a key handler. *)
  | Timer of string * int  (** The id and the delay of a timer. *)
  | Answer :
      string * ('i, 'o, 'x) Endpoint.t * ('o, 'x) Endpoint.answer
      -> cause  (** The id of a call, its endpoint and its answer. *)
  | Live : string * 's Topic.t * 's Topic.update -> cause
      (** The id of a subscription, its topic and the update. *)
  | Source : string * 'm Action.source -> cause
      (** The id of a held source and the source. *)
  | Result : 'm Action.t -> cause  (** The action whose result this is. *)
  | Child : ('p, 's, 'e) cell -> cause
      (** The [output] or [on_final] of this child. *)
  | Route of Router.event  (** A URL change or a preload of the runtime. *)

type observed =
  | Observed :
      ('p, 's, 'e) cell
      * Machine.how
      * 's
      * 'e option
      * message Action.host list
      * cause
      -> observed
      (** A start ({!Start}), step, props change or end ({!End}) in a tree, with
          the snapshot before. The [how] of a start is [Machine.Entered] when
          the instance resumed a snapshot (see {!type-resume}), which {!inspect}
          gives as [Resumed]. Read it with {!inspect} while the observer runs:
          the tree changes after. *)
  | Detached : {
      instance : string;
      machine : ('p, 's, 'e) Machine.t;
      props : 'p;
      entry : int;
      from : 's option;
      snapshot : 's;
      change : change;
      event : 'e option;
      actions : 'e Action.host list;
      cause : cause;
    }
      -> observed
      (** A step of a machine that runs with no tree, such as a Live island on
          the server. *)

type observer = {
  observe : observed -> unit;
  handler : 'e. (cause -> 'e -> message) -> 'e Node.attr -> message Node.attr;
      (** Resolve an attribute of a view with the function that makes the
          message of an event with its cause. An observer gives an {!Element}
          cause to the handlers whose message is made at the time of the input
          (input, keys, pointer), and {!Handler} to the others. *)
}

type inspected =
  | Inspected : {
      instance : string;
          (** The instance path (see [Transition.t.instance]). *)
      number : int;  (** As in timer ids. *)
      entry : int;  (** The entry after the step, as in timer ids. *)
      machine : ('p, 's, 'e) Machine.t;
      props : 'p;
      from : 's option;  (** [None] for [Started] and [Resumed]. *)
      snapshot : 's;  (** After the step. *)
      change : change;
      event : 'e option;
      actions : 'm Action.host list;  (** The actions of the step, in order. *)
      cause : cause;
      view : message Node.t option;
          (** The last view of the tree, the one an input happened on. *)
      message : message option;
          (** The message that the last {!update} delivered: for a [Handler]
              step, the element of [view] that holds it. *)
    }
      -> inspected

val inspect : observed -> inspected
(** The values of the step. Call it while the observer runs. *)

val cell_path : ('p, 's, 'e) cell -> string
(** The instance path of a cell. *)

type ('p, 's, 'e) asked
(** An instance that starts (see {!asked_path}, {!asked_component} and
    {!asked_props}). *)

val asked_path : ('p, 's, 'e) asked -> string
(** The instance path of the instance (see {!cell_path}). *)

val asked_component : ('p, 's, 'e) asked -> ('p, 's, 'e) Component.t
val asked_props : ('p, 's, 'e) asked -> 'p

type resume = {
  take : 'p 's 'e. ('p, 's, 'e) asked -> ('s * 'e Action.host list) option;
}
(** How a tree starts from the snapshots of an earlier run, at a hot reload (see
    [Handoff.hook]). While the first {!update} runs, each instance that starts
    asks [take]. [Some (snapshot, actions)] starts it there with those actions
    (the observer sees [Resumed]); [None] starts it as usual. *)

val mount :
  ?dev:bool ->
  ?on_error:(exn -> Node.error -> unit) ->
  ?observe:observer ->
  ?resume:resume ->
  (unit, 's, 'e) Component.t ->
  (unit, 's, 'e) t
(** Create the root and start its machine. The actions are returned by the first
    {!update}.

    An exception from the machine step or the view of a component under a
    {!Node.val-boundary} does not leave {!update}: the boundary shows its
    fallback. [on_error] gets the exception and the {!Node.error} the fallback
    gets; with no hook the error goes to stderr. [dev] (default [false]) puts
    the details in {!Node.error.detail}. An exception with no boundary above it
    leaves {!update}; the tree is then not safe to use and the host should drop
    it.

    [observe] gets one {!observed} for each start, step (also an ignored event),
    props change and end of each instance of the tree, at the time it happens,
    with its cause: the element of a handler (only with a hook are handler
    messages made with their element), the timer, the answer, the Live update,
    the held source, the action, the props, the child or the message of the
    host. Give it [Transition.observer] or [Trace.Recorder.observer]. With no
    observer the cost is one test of an option per step. A step that raises is
    not observed.

    [resume] starts the tree from the snapshots of an earlier run (see
    {!type-resume}). *)

val mount_with_props :
  ?dev:bool ->
  ?on_error:(exn -> Node.error -> unit) ->
  ?observe:observer ->
  ?resume:resume ->
  props:'p ->
  ('p, 's, 'e) Component.t ->
  ('p, 's, 'e) t
(** Like {!mount}, for a root component with props. *)

val set_props : ('p, 's, 'e) t -> 'p -> unit
(** Give the root new props, as a parent gives a child new props: the [on_props]
    of the machine runs and the sources are compared by key. The actions come
    with the next {!update}. Props equal to the current ones do nothing. *)

val failure :
  ?dev:bool -> ?on_error:(exn -> Node.error -> unit) -> exn -> Node.error
(** Like {!report} for an exception that happened with no tree, such as while
    the root machine starts. *)

val perform : message -> unit
(** Run a message made by {!callback}, or any message, with no tree. *)

val report : ('p, 's, 'e) t -> exn -> Node.error
(** Make the {!Node.error} for an exception that no boundary caught, and send it
    to [on_error] (or stderr), as a boundary does. The error names the root
    machine and its state. *)

val callback : (unit -> unit) -> message
(** A message that runs the function when it is delivered. A host uses it for
    the handlers of its own views, such as a top-level error page. *)

val update :
  ('p, 's, 'e) t -> message option -> message Node.t * message Action.host list
(** Deliver the message, if any. Then process the events that components raised
    for their parents, and render until nothing changes. Returns the resolved
    view (it has no component nodes) and the actions raised since the last call,
    in order. Timer and source ids in the actions are the ids described above.
*)

val waiting : ('p, 's, 'e) t -> bool
(** The last {!update} rendered a {!Node.val-chunk} whose value is not here. A
    host that adopts a server screen waits for the chunks before its first
    paint. An instance that shows a chunk that is loading renders again at each
    update, with its parents, until the value is here. *)

val message : ('p, 's, 'e) t -> 'e -> message
(** An event for the root component, such as a URL change. *)

val route : ('p, 's, 'e) t -> Router.event -> 'e -> message
(** Like {!val-message}, for the event that the root makes of a URL change or a
    preload: an observer gets the cause {!constructor-Route}. *)

val path : ('p, 's, 'e) t -> string
(** The instance path of the root: the id of its machine. Children have the
    paths that [Transition.t.instance] describes. *)

val unmount : ('p, 's, 'e) t -> message Action.host list
(** End the root, children first. Later messages are dropped. *)

val snapshot : ('p, 's, 'e) t -> 's
(** The snapshot of the root machine. *)

val count : ('p, 's, 'e) t -> int
(** The number of instances now mounted, the root included. *)

type running =
  | Running : {
      path : string;  (** The instance path (see {!cell_path}). *)
      machine : ('p, 's, 'e) Machine.t;
      props : 'p;
      snapshot : 's;
    }
      -> running  (** An instance that is mounted. *)

val running : ('p, 's, 'e) t -> running list
(** Every mounted instance, parents before their children, with its snapshot. A
    hot reload hands them to the next run (see [Handoff]). *)
