(** What a machine asks its host to run during a state: the resources in the
    [during] list of {!Machine.make}, which are timers, calls, subscriptions and
    held resources. A resource starts when it comes into the list and stops when
    it goes out of the list.

    Each resource has a {!key}. After each step the machine compares the keys of
    the old and the new [during] list: it stops the resources whose key went
    away and starts the new ones, and the others keep running. So the calls of
    one state can change with its data. Calls are made with {!Remote.call} and
    subscriptions with {!Topic.subscribe}. *)

type 'e t
(** A resource that raises events ['e]. *)

val after : int -> 'e -> 'e t
(** A timer: the event is raised once, this many milliseconds after the timer
    starts, unless it was stopped first. *)

val focus_trap : ?return_focus:bool -> Ref.t -> 'e t
(** Keep focus inside the element with this ref while the resource runs.
    [return_focus] defaults to [true]. See {!Action.Focus_trap}. *)

val scroll_lock : 'e t
(** The page does not scroll while the resource runs. *)

val outside_press : Ref.t -> 'e -> 'e t
(** A press outside the element with this ref raises the event. *)

val keys : ?prevent:bool -> ?rate:Rate.t -> (Key.t * 'e) list -> 'e t
(** A key press anywhere in the page, from the table, raises its event. The
    table is data, so the checker presses each key of the active state. Keys
    typed in a text field and keys that an element handler prevented do not
    count (see [Action.Global_keys]). *)

val dismiss : ?escape:'e -> ?outside:'e -> ?focus_out:'e -> Ref.t list -> 'e t
(** A dismiss layer for a popup whose elements have these refs: Escape raises
    [escape] in the newest layer only, a press outside the elements raises
    [outside], and focus that moves outside the elements raises [focus_out],
    unless the press or the focus is inside a newer layer. See
    {!Action.Dismiss}. *)

val anchor : ?place:Place.t -> anchor:Ref.t -> Ref.t -> 'e t
(** Keep the popup with this ref next to the [anchor] element while the resource
    runs. [place] defaults to {!Place.default}. See {!Action.Anchor}. *)

val resize : ?rate:Rate.t -> Ref.t -> (Rect.t -> 'e) -> 'e t
(** The element changes size. *)

val intersection : ?rate:Rate.t -> Ref.t -> (bool -> 'e) -> 'e t
(** The element enters ([true]) or leaves ([false]) the viewport. *)

val scroll : ?rate:Rate.t -> ?target:Ref.t -> (Scroll.t -> 'e) -> 'e t
(** The element with the ref [target], or the page when [target] is not given,
    scrolls. The event is raised once at the start and for each scroll. See
    {!Action.Scroll}. *)

val visibility : (bool -> 'e) -> 'e t
(** The document becomes visible ([true]) or hidden ([false]). The event is
    raised once at the start with the value then. *)

val online : (bool -> 'e) -> 'e t
(** The browser goes online ([true]) or offline ([false]). The event is raised
    once at the start with the value then. *)

val media : string -> (bool -> 'e) -> 'e t
(** The media query, such as ["(prefers-color-scheme: dark)"], matches ([true])
    or not ([false]). The event is raised once at the start with the value then
    and again at each change. See {!Action.Media}. *)

val block : (Nav.t -> 'e) -> 'e t
(** While the resource runs, every navigation stops before the URL changes and
    raises the event instead: a page with unsaved data asks, then sends
    {!Action.Resume} with the navigation to let it happen. Closing or reloading
    the page asks the user first. See {!Action.Block}. *)

val map_refs : (Ref.t -> Ref.t) -> 'e t -> 'e t
(** The same held resource with each of its refs replaced. A component that can
    have more than one instance in a page uses it to give each instance its own
    refs, for example from an id in its props. Other resources are returned
    unchanged. *)

val key : 'e t -> string
(** The identity of the resource in a [during] list: the path and encoded input
    of a call, the topic and key of a subscription, ["after <ms>ms"] for a
    timer, the kind and refs of a held resource, and the place of an {!anchor}.
    When an [during] list holds two resources with the same key, the second one
    gets ["#2"] after its key, and so on. *)

val same : 'e t -> 'e t -> bool
(** Two resources with the same key also have the same data: the event of a
    timer, the table of {!keys}, the events of {!outside_press} and {!dismiss}.
    Compared with [=]. Functions cannot be compared: a part that is a function
    counts as the same.

    The rule after a step that keeps the state: a resource whose key went away
    stops, a new key starts, and a resource whose key stays but whose data is
    not {!same} stops and starts again, so it raises the event of the current
    [during] list. A function, such as the answer function of a call, is the one
    of its first start while the key and the data stay: put the data it needs in
    the state, or in the input of the call. *)

val name : 'e t -> string
(** The endpoint path of a call, ["live:" ^ topic name] for a
    {!Topic.subscribe}, ["after <ms>ms"] for a timer, or the name of a held
    resource as {!Action.held_name} gives it. *)

val delay : 'e t -> int option
(** The milliseconds of an {!after} timer. *)

val topic : 'e t -> string option
(** The topic name of a {!Topic.subscribe} resource. *)

val is_held : 'e t -> bool
(** A held resource: a resource that is not a call, a subscription or a timer.
*)

val held : 'e t -> 'e Action.held option
(** The held resource, when the resource is not a call, a subscription or a
    timer. *)

val start : string -> 'e t -> 'e Action.host
(** [start id resource] is the action that starts [resource] under [id]. *)

(**/**)

val remote_call :
  ?timeout_ms:int ->
  ('i, 'o, 'x) Endpoint.t ->
  'i ->
  (('o, 'x) Endpoint.answer -> 'e) ->
  'e t

val topic_subscribe :
  's Live_topic.t -> string -> ('s Live_topic.update -> 'e) -> 'e t
