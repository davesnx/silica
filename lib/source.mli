(** Sources of events owned by a state. A source starts when the state is
    entered and stops when it is exited. *)

type ('p, 'e) t
(** A source for a state that carries ['p]. *)

val call :
  ?timeout_ms:int ->
  ('i, 'o) Remote.endpoint ->
  ('p -> 'i) ->
  (('o, Remote.error) result -> 'e) ->
  ('p, 'e) t
(** One request to the endpoint, with the input computed from the state's data
    on entry. The result is raised as an event unless the state was exited
    first. There is no timeout unless [timeout_ms] is given. When it passes
    before the answer, the call ends: the state gets
    [on_result (Error (Network "timeout"))], and a later answer is dropped. *)

val live : 's Topic.t -> ('p -> string) -> ('s Topic.update -> 'e) -> ('p, 'e) t
(** Subscribe to the topic under the key that the state's data gives, from the
    entry of the state to its exit. The event is raised once with [Loading None]
    while the socket connects, and again for every state the server publishes,
    for every loss of the connection (the old data, [stale = true]) and for
    every failure. The client reconnects by itself. *)

val focus_trap : ?return_focus:bool -> Ref.t -> ('p, 'e) t
(** Keep focus inside the element with this ref while the state is active.
    [return_focus] defaults to [true]. See {!Cmd.Focus_trap}. *)

val scroll_lock : ('p, 'e) t
(** The page does not scroll while the state is active. *)

val outside_press : Ref.t -> 'e -> ('p, 'e) t
(** A press outside the element with this ref raises the event. *)

val keys : ?prevent:bool -> ?rate:Rate.t -> (Key.t * 'e) list -> ('p, 'e) t
(** A key press anywhere in the page, from the table, raises its event. The
    table is data, so the checker presses each key of the active state. Keys
    typed in a text field and keys that an element handler prevented do not
    count (see [Cmd.Global_keys]). *)

val dismiss :
  ?escape:'e -> ?outside:'e -> ?focus_out:'e -> Ref.t list -> ('p, 'e) t
(** A dismiss layer for a popup whose elements have these refs: Escape raises
    [escape] in the newest layer only, a press outside the elements raises
    [outside], and focus that moves outside the elements raises [focus_out],
    unless the press or the focus is inside a newer layer. See {!Cmd.Dismiss}.
*)

val anchor : ?place:('p -> Place.t) -> anchor:Ref.t -> Ref.t -> ('p, 'e) t
(** Keep the popup with this ref next to the [anchor] element while the state is
    active. [place] gets the data of the state on entry, so a component can take
    the place from its props; it defaults to {!Place.default}. See
    {!Cmd.Anchor}. *)

val resize : ?rate:Rate.t -> Ref.t -> (Rect.t -> 'e) -> ('p, 'e) t
(** The element changes size. *)

val intersection : ?rate:Rate.t -> Ref.t -> (bool -> 'e) -> ('p, 'e) t
(** The element enters ([true]) or leaves ([false]) the viewport. *)

val scroll : ?rate:Rate.t -> ?target:Ref.t -> (Scroll.t -> 'e) -> ('p, 'e) t
(** The element with the ref [target], or the page when [target] is not given,
    scrolls. The event is raised once at the start and for each scroll. See
    {!Cmd.Scroll}. *)

val visibility : (bool -> 'e) -> ('p, 'e) t
(** The document becomes visible ([true]) or hidden ([false]). The event is
    raised once at the start with the value then. *)

val online : (bool -> 'e) -> ('p, 'e) t
(** The browser goes online ([true]) or offline ([false]). The event is raised
    once at the start with the value then. *)

val media : string -> (bool -> 'e) -> ('p, 'e) t
(** The media query, such as ["(prefers-color-scheme: dark)"], matches ([true])
    or not ([false]). The event is raised once at the start with the value then
    and again at each change. See {!Cmd.Media}. *)

val block : (Nav.t -> 'e) -> ('p, 'e) t
(** While the state is active, every navigation stops before the URL changes and
    raises the event instead: a page with unsaved data asks, then sends
    {!Cmd.Resume} with the navigation to let it happen. Closing or reloading the
    page asks the user first. See {!Cmd.Block}. *)

val name : ('p, 'e) t -> string
(** The endpoint path of a {!call}, ["live:" ^ topic name] for a {!live}, or the
    name of a held source as {!Cmd.source_name} gives it. *)

val topic : ('p, 'e) t -> string option
(** The topic name of a {!live} source. *)

val map_refs : ('p -> Ref.t -> Ref.t) -> ('p, 'e) t -> ('p, 'e) t
(** The same held source with each of its refs replaced by [f data ref], where
    [data] is the data of the state when it is entered. A component that can
    have more than one instance in a page uses it to give each instance its own
    refs, for example from an id in its props. Calls and subscriptions are
    returned unchanged. *)

val is_held : ('p, 'e) t -> bool
(** A source that is not a call or a subscription. *)

val held : 'p -> ('p, 'e) t -> 'e Cmd.source option
(** The held resource for the data of the state, for a source that is not a call
    or a subscription. *)

val start : string -> 'p -> ('p, 'e) t -> 'e Cmd.host
(** [start id data source] is the command that starts [source] under [id]. *)
