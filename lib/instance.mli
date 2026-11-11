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
      and a {!Action.Stop} for every timer and source it still owns, come first
      for its own children, then for it.
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

val mount :
  ?dev:bool ->
  ?on_error:(exn -> Node.error -> unit) ->
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
    it. *)

val mount_with_props :
  ?dev:bool ->
  ?on_error:(exn -> Node.error -> unit) ->
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

val unmount : ('p, 's, 'e) t -> message Action.host list
(** End the root, children first. Later messages are dropped. *)

val snapshot : ('p, 's, 'e) t -> 's
(** The snapshot of the root machine. *)

val count : ('p, 's, 'e) t -> int
(** The number of instances now mounted, the root included. *)
