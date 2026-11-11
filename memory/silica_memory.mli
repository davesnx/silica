(** An in-memory platform for {!Silica.Runtime}, on native OCaml and with no
    DOM. It runs the same host runtime as the browser, so a test drives a
    component through the same queue of events, timers, calls, subscriptions and
    error page.

    The platform keeps:
    - the last painted view, as a {!Silica.Node.t} and as HTML;
    - a manual clock: nothing scheduled runs until {!advance};
    - the HTTP requests of Remote calls, which the test answers one at a time;
    - the Live sockets, which the test opens, sends messages into and closes;
    - a history of URLs with an index, which the navigation actions, link
      presses, {!back} and {!forward} change;
    - the actions that the renderer ran, the running sources, the title, the log
      lines and the number of times the session was gone.

    Input follows the browser renderer: a handler on the nearest ancestor of the
    element answers, as with event delegation; a press on an element inside an
    [a] with [data-silica-link] (and no [target] or [download]) pushes (or, with
    [data-silica-replace], replaces) the history entry and reports the URL to
    the root as {!Silica.Router.event.Url_changed}; a key goes to the key
    handlers of the focused element, then to the [Global_keys] sources and the
    Escape of the newest dismiss layer; rated handlers use
    {!Silica.Rate.limiter} with the memory clock.

    Navigation follows the browser renderer too: while a [Block] source runs, a
    link press, [Navigate], [Replace], {!back} and {!forward} change nothing and
    raise its [on_attempt] (the newest block gets it); [Resume] does the
    navigation past every block; [Revert_navigation] undoes the last reported
    change without a report. After a push or a back or forward to another path,
    a task on the clock with delay 0 moves focus to the first element with
    [data-silica-route-focus] (or to no element), unless a [Focus] action ran
    since, and announces the title (see {!announcements}).

    Out of scope: islands ([data-silica-live]) are painted as empty elements and
    never connect; [Focus_trap] (other than as a dismiss layer), [Scroll_lock],
    [Resize], [Intersection], [Anchor], [Scroll], [Visibility] and [Online]
    sources are kept in {!sources} but do nothing; a [Media] source answers from
    {!set_media}; [Scroll_into_view], [Scroll_to] and [Reload] are only
    recorded; [Measure] answers {!Silica.Rect.zero} when the ref is painted and
    [None] when it is not; [Copy] and [Load_script] answer [true]; a portal is
    searchable but its target is ignored; [Set_status] is only recorded; links
    with [data-silica-preload="viewport"] never preload. *)

open Silica

type t
(** A mounted runtime and its platform. *)

(** {1 Mount} *)

val mount :
  ?dev:bool ->
  ?on_error:(exn -> Node.error -> unit) ->
  ?answers:Answers.t ->
  ?live_config:Live_client.config ->
  ?csrf:string ->
  ?url:string ->
  ?media:(string * bool) list ->
  ('s, 'e) Component.t ->
  t
(** Start the runtime with the component as the root, and paint. [answers]
    answers calls before the first paint, as a server render does. [csrf] is the
    page token (default [""]). [url] is the first history entry (default ["/"]).
    [media] gives the media queries that match at the start (see {!set_media});
    a query that is not in it does not match. With [dev], an exception during
    the first paint is raised again, as {!Silica.Runtime.start} says. *)

val mount_app :
  ?dev:bool ->
  ?on_error:(exn -> Node.error -> unit) ->
  ?answers:Answers.t ->
  ?live_config:Live_client.config ->
  ?csrf:string ->
  ?url:string ->
  ?media:(string * bool) list ->
  (Url.t -> ('s, Router.event) Component.t) ->
  t
(** Like {!val-mount} for a router app: the app starts at the current URL, and
    each URL change reaches the root as {!Silica.Router.event.Url_changed}, each
    link preload as {!Silica.Router.event.Preload}. *)

(** {1 View} *)

val view : t -> Instance.message Node.t
(** The last painted view. *)

val html : t -> string
(** The last painted view as HTML, by {!Silica.Render.to_string}. Portals are
    left out. *)

val paints : t -> int
(** The number of paints since the start. *)

(** {1 Elements} *)

type element
(** A position in the painted view. It is looked up again in the current view at
    each use, and raises [Failure] when the view no longer has it. *)

val all : ?within:element -> t -> element list
(** Every element in document order. [within] keeps only the descendants of that
    element. *)

val by_id : t -> string -> element
(** The first element with this [id]. Raises [Failure] when there is none. *)

val select : ?within:element -> t -> string -> string -> element list
(** [select t name value] is every element whose attribute [name] equals [value]
    exactly, in document order. A ref counts as [data-silica-ref]. [within]
    keeps only the descendants of that element. *)

val by_text : ?within:element -> t -> string -> element
(** The innermost element whose text equals the string. Raises [Failure] when
    there is none. *)

val text : element -> string
(** The text of the element and its descendants. *)

val attribute : element -> string -> string option
(** The value of an attribute; [Some ""] for a [true] boolean attribute. *)

val tag : element -> string
(** The tag name. *)

(** {1 Input} *)

val press : element -> unit
(** A press: the outside-press sources and dismiss layers see the press start,
    then a link is followed or the nearest press handler runs. *)

val double_press : element -> unit
(** A double press reaches the nearest double press handler. *)

val input : element -> string -> unit
(** The field now holds the text: the nearest input handler gets it. *)

val submit : element -> unit
(** A submit reaches the nearest submit handler. *)

val focus : element -> unit
(** Move focus to the element: the blur handler of the element that had focus,
    then the focus handler of this one, then the focus-out of dismiss layers. *)

val blur : t -> unit
(** No element has focus any more. *)

val focused : t -> element option
(** The element with focus. *)

val key : t -> Key.t -> unit
(** A key goes down on the focused element (or the page when none has focus).
    Element key handlers run first; Enter and Space press an element that is not
    a native control; then the [Global_keys] sources and the dismiss layers get
    the key, unless a handler prevented it. *)

val key_up : t -> Key.t -> unit
(** A key goes up on the focused element: only element handlers see it. *)

val hover : element -> unit
(** The pointer enters the element. Inside a link with
    [data-silica-preload="intent"], the root gets {!Silica.Router.event.Preload}
    with the URL of the link. {!focus} on such a link does the same. *)

val back : t -> unit
(** The back button of the browser: move to the previous history entry, if there
    is one, and report its URL. *)

val forward : t -> unit
(** The forward button of the browser: move to the next history entry, if there
    is one, and report its URL. *)

(** {1 Clock} *)

val now : t -> int
(** Milliseconds since the start. *)

val advance : t -> int -> unit
(** Move the clock forward by the milliseconds, and run each task that becomes
    due, in time order, including tasks that those tasks schedule. Raises
    [Invalid_argument] for a negative time. *)

val timers : t -> int list
(** The delays, in milliseconds, of the scheduled tasks that have not run and
    were not cancelled, in the order they were scheduled. *)

(** {1 HTTP} *)

type request
(** A Remote call request. *)

val path : request -> string
(** The path of the request. *)

val body : request -> string
(** The JSON body of the request. *)

val request_csrf : request -> string
(** The CSRF token sent with the request. *)

val requests : t -> request list
(** Every request sent, oldest first. *)

val pending : t -> request list
(** The requests with no answer yet, oldest first. *)

val reply : ?status:int -> request -> string -> unit
(** Answer the request with the status (default [200]) and the body. Raises
    [Invalid_argument] when it already has an answer. *)

val network_error : request -> string -> unit
(** Answer the request with a network failure. Raises [Invalid_argument] when it
    already has an answer. *)

(** {1 Live sockets} *)

type socket
(** A Live socket that the runtime opened. *)

val sockets : t -> socket list
(** Every socket opened, oldest first. *)

val sent : socket -> string list
(** The messages that the client sent, oldest first. *)

val closed : socket -> int option
(** The code that the client closed the socket with. *)

val socket_open : socket -> unit
(** The socket opens. *)

val socket_message : socket -> string -> unit
(** The server sends a text message. *)

val socket_close : socket -> int -> unit
(** The socket closes with the code. *)

val socket_error : socket -> unit
(** The socket reports an error. *)

(** {1 Records} *)

val url : t -> string
(** The URL of the current history entry. *)

val history : t -> string list
(** The URLs of the history entries, oldest first. *)

val history_index : t -> int
(** The index of the current entry in {!val-history}. *)

val actions : t -> Instance.message Action.t list
(** The actions that the renderer ran, oldest first. *)

val sources : t -> (string * Instance.message Action.source) list
(** The running sources with their ids, in the order they started. *)

val title : t -> string
(** The document title (default [""]). *)

val document_attribute : t -> string -> string option
(** The value of an attribute of the document element, as the
    {!Silica.Action.Set_document_attribute} actions left it. *)

val cookies : t -> (string * string) list
(** The cookies that {!Silica.Action.Set_cookie} stored, as name and value, in
    the order of their last write. *)

val set_media : t -> string -> bool -> unit
(** The media query now matches, or not: each running {!Silica.Action.Media}
    source of that query raises its event at once. *)

val logs : t -> string list
(** The log lines of the runtime, oldest first. *)

val leave_asks : t -> bool
(** [true] while a {!Silica.Action.Block} source runs: closing or reloading the
    page would ask the user first. *)

val announcements : t -> string list
(** The titles that the live region announced after navigations, oldest first.
*)

val session_gone : t -> int
(** How many times the runtime said that the server no longer knows the session.
*)
