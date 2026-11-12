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

type 'p t
(** A mounted runtime and its platform, with a root component that has props
    ['p]. *)

(** {1 Mount} *)

val mount :
  ?dev:bool ->
  ?on_error:(exn -> Node.error -> unit) ->
  ?answers:Answers.t ->
  ?live_config:Live_client.config ->
  ?csrf:string ->
  ?url:string ->
  ?media:(string * bool) list ->
  (unit, 's, 'e) Component.t ->
  unit t
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
  (Url.t -> (unit, 's, Router.event) Component.t) ->
  unit t
(** Like {!val-mount} for a router app: the app starts at the current URL, and
    each URL change reaches the root as {!Silica.Router.event.Url_changed}, each
    link preload as {!Silica.Router.event.Preload}. *)

val mount_with_props :
  ?dev:bool ->
  ?on_error:(exn -> Node.error -> unit) ->
  ?answers:Answers.t ->
  ?live_config:Live_client.config ->
  ?csrf:string ->
  ?url:string ->
  ?media:(string * bool) list ->
  props:'p ->
  ('p, 's, 'e) Component.t ->
  'p t
(** Like {!val-mount}, for a root component with props. After an error page,
    "Try again" starts the root with the last props. *)

val set_props : 'p t -> 'p -> unit
(** Give the root new props, as a parent does: the [on_props] of the machine
    runs, and the view paints again. Props equal to the current ones do nothing.
*)

(** {1 View} *)

val view : _ t -> Instance.message Node.t
(** The last painted view. *)

val html : _ t -> string
(** The last painted view as HTML, by {!Silica.Render.to_string}. Portals are
    left out. *)

val paints : _ t -> int
(** The number of paints since the start. *)

val view_text : _ t -> string
(** The last painted view as text, by {!Silica.View_text.to_string}. *)

val shows : _ t -> string -> bool
(** [shows t text]: [text] is a part of the text that a person sees on the page
    ({!Silica.View_text.visible}), in any element, with or without a role, but
    not in hidden ones. *)

(** {1 Elements} *)

type element
(** A position in the painted view. It is looked up again in the current view at
    each use, and raises [Failure] when the view no longer has it. *)

val all : ?within:element -> _ t -> element list
(** Every element in document order. [within] keeps only the descendants of that
    element. *)

val by_id : _ t -> string -> element
(** The first element with this [id]. Raises [Failure] when there is none. *)

val select : ?within:element -> _ t -> string -> string -> element list
(** [select t name value] is every element whose attribute [name] equals [value]
    exactly, in document order. A ref counts as [data-silica-ref]. [within]
    keeps only the descendants of that element. *)

val by_text : ?within:element -> _ t -> string -> element
(** The innermost element whose text equals the string. Raises [Failure] when
    there is none. *)

val text : element -> string
(** The text of the element and its descendants. *)

val attribute : element -> string -> string option
(** The value of an attribute; [Some ""] for a [true] boolean attribute. *)

val tag : element -> string
(** The tag name. *)

(** {1 Roles and names}

    Find elements as a screen reader does: by their role and accessible name, as
    {!Silica.Accessibility} computes them. Hidden elements are left out. *)

val by_role :
  ?within:element ->
  ?name:string ->
  ?level:int ->
  ?checked:bool ->
  ?selected:bool ->
  ?expanded:bool ->
  _ t ->
  Accessibility.role ->
  element
(** The one element of the role that has the name (as {!accessible_name} gives
    it) and the states. [within] keeps only the descendants of that element.
    Raises [Failure] when none or several match: the message lists the elements
    of that role with their names, states and paths. *)

val all_by_role :
  ?within:element -> ?name:string -> _ t -> Accessibility.role -> element list
(** Every element of the role, with the name when it is given, in document
    order. *)

val role : element -> Accessibility.role option
(** The role of the element. *)

val accessible_name : element -> string
(** The accessible name of the element. *)

val disabled : element -> bool
(** The element is disabled: [aria-disabled="true"] or [disabled] on it (see
    {!Silica.Accessibility.disabled}), or it is inside a disabled [fieldset] or
    other native control. A press reaches an element with only [aria-disabled],
    as in a browser (see {!Not_reachable}). *)

val hidden : element -> bool
(** The element is not in the accessibility tree (see
    {!Silica.Accessibility.hidden}). *)

val checked : element -> bool option
(** See {!Silica.Accessibility.checked}. *)

val selected : element -> bool option
(** See {!Silica.Accessibility.selected}. *)

val expanded : element -> bool option
(** See {!Silica.Accessibility.expanded}. *)

(** {1 Input}

    As in a browser, {!press}, {!double_press}, {!val-input}, {!submit} and
    {!focus} reach only an element that a user can reach: they raise
    {!Not_reachable} when the element, or an element around it, is a disabled
    [button], [input], [select], [textarea], [option], [optgroup] or [fieldset],
    or has [hidden] or [inert]. [aria-disabled] and [aria-hidden] do not stop
    input, as in a browser. *)

(** Why an input cannot reach an element. *)
type unreachable =
  | Disabled
      (** The element, or a [fieldset] around it, is a disabled native control.
      *)
  | Hidden  (** The element or an element around it has [hidden]. *)
  | Inert  (** The element or an element around it has [inert]. *)

exception Not_reachable of { reason : unreachable; message : string }
(** The input cannot reach the element. [message] names the element by role,
    accessible name and path, and says why, such as
    ["Silica_memory: the button \"Save\" at main > button[3] is disabled, so a
     browser does not deliver a press to it."]. A test that expects the refusal
    matches it:
    {[
      match Silica_memory.press save with
      | () ->
          failwith "the disabled Save button took a press"
      | exception Silica_memory.Not_reachable { reason = Disabled; _ } ->
          ()
    ]} *)

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

val blur : _ t -> unit
(** No element has focus any more. *)

val focused : _ t -> element option
(** The element with focus. *)

val key : _ t -> Key.t -> unit
(** A key goes down on the focused element (or the page when none has focus).
    Element key handlers run first; Enter and Space press an element that is not
    a native control; then the [Global_keys] sources and the dismiss layers get
    the key, unless a handler prevented it. *)

val key_up : _ t -> Key.t -> unit
(** A key goes up on the focused element: only element handlers see it. *)

val hover : element -> unit
(** The pointer enters the element: the nearest pointer enter handler gets
    {!Silica.Pointer.origin}. Inside a link with [data-silica-preload="intent"],
    the root gets {!Silica.Router.event.Preload} with the URL of the link.
    {!focus} on such a link does the same. *)

val leave : element -> unit
(** The pointer leaves the element: the nearest pointer leave handler gets
    {!Silica.Pointer.origin}. *)

val back : _ t -> unit
(** The back button of the browser: move to the previous history entry, if there
    is one, and report its URL. *)

val forward : _ t -> unit
(** The forward button of the browser: move to the next history entry, if there
    is one, and report its URL. *)

val open_url : _ t -> string -> unit
(** The user opens the URL, as in the address bar: a new history entry, which
    the root gets as {!Silica.Router.event.Url_changed}, unless a
    {!Silica.Action.Block} source holds it. The page does not load again: this
    is the push of a single-page app. *)

(** {1 Clock} *)

val now : _ t -> int
(** Milliseconds since the start. *)

val advance : _ t -> int -> unit
(** Move the clock forward by the milliseconds, and run each task that becomes
    due, in time order, including tasks that those tasks schedule. Raises
    [Invalid_argument] for a negative time. *)

val timers : _ t -> int list
(** The delays, in milliseconds, of the scheduled tasks that have not run and
    were not cancelled, in the order they were scheduled. *)

(** {1 Transition log} *)

val trace : _ t -> Trace.t
(** Every transition since the start, with data: the runtime always records (see
    {!Silica.Instance.mount} and {!Silica.Trace}). Each record has the time of
    the memory clock. Event data, props and snapshots are there for the machines
    that registered their codecs ({!Silica.Machine.register_codecs}); answers,
    Live updates and input text are always there. The header has the first URL,
    the build ["memory"] and the platform ["memory"]. Read it with
    {!Silica.Trace.to_text} or write it with {!Silica.Trace.to_jsonl}.

    After an error page, "Try again" starts a new tree: its records follow, with
    their numbers going on and instance numbers starting again at 1. *)

(** {1 HTTP} *)

type request
(** A Remote call request. *)

val path : request -> string
(** The path of the request. *)

val body : request -> string
(** The JSON body of the request. *)

val request_csrf : request -> string
(** The CSRF token sent with the request. *)

val requests : _ t -> request list
(** Every request sent, oldest first. *)

val pending : _ t -> request list
(** The requests with no answer yet, oldest first. *)

val pending_request : _ t -> string -> request
(** The oldest request to the path that has no answer yet. Raises [Failure] when
    there is none; the text lists the paths of the pending requests. *)

val reply : ?status:int -> request -> string -> unit
(** Answer the request with the status (default [200]) and the body. Raises
    [Invalid_argument] when it already has an answer. *)

val network_error : request -> string -> unit
(** Answer the request with a network failure. Raises [Invalid_argument] when it
    already has an answer. *)

(** {1 Live sockets} *)

type socket
(** A Live socket that the runtime opened. *)

val sockets : _ t -> socket list
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

val url : _ t -> string
(** The URL of the current history entry. *)

val history : _ t -> string list
(** The URLs of the history entries, oldest first. *)

val history_index : _ t -> int
(** The index of the current entry in {!val-history}. *)

val actions : _ t -> Instance.message Action.t list
(** The actions that the renderer ran, oldest first. *)

val sources : _ t -> (string * Instance.message Action.source) list
(** The running sources with their ids, in the order they started. *)

val title : _ t -> string
(** The document title (default [""]). *)

val document_attribute : _ t -> string -> string option
(** The value of an attribute of the document element, as the
    {!Silica.Action.Set_document_attribute} actions left it. *)

val cookies : _ t -> (string * string) list
(** The cookies that {!Silica.Action.Set_cookie} stored, as name and value, in
    the order of their last write. *)

val set_media : _ t -> string -> bool -> unit
(** The media query now matches, or not: each running {!Silica.Action.Media}
    source of that query raises its event at once. *)

val logs : _ t -> string list
(** The log lines of the runtime, oldest first. *)

val leave_asks : _ t -> bool
(** [true] while a {!Silica.Action.Block} source runs: closing or reloading the
    page would ask the user first. *)

val announcements : _ t -> string list
(** The titles that the live region announced after navigations, oldest first.
*)

val session_gone : _ t -> int
(** How many times the runtime said that the server no longer knows the session.
*)

(** {1 Replay} *)

(** The outcome of {!val-replay}. [seq] is the transition of the trace. *)
type replay_result =
  | Replayed  (** Every step gave the same state names. *)
  | Different of { seq : int; expected : string; got : string }
      (** The runtime reached another state, or no transition. *)
  | Not_realisable of { seq : int; reason : string }
      (** The step has no input in this platform: a timer that real time does
          not fire first, a Live update, a held source, the result of an action,
          an event of the host or of the inputs of a check, a preload, or a step
          whose data the trace does not hold. *)
  | Not_found of { seq : int; error : string }
      (** The element or the pending request of the step is not there. *)

val replay : _ t -> Trace.t -> replay_result
(** [replay t trace] gives the runtime of [t], just mounted, the input of each
    step of [trace], such as a counterexample of the checker ([Check.export]) or
    a recorded run, and compares each step with the state names of the trace:
    - an element handler: the element of that role and accessible name (by its
      path when several match), then {!press}, {!double_press}, {!submit},
      {!focus}, {!blur}, {!hover}, {!val-input} with the text, or {!focus} and
      {!val-key} (or {!key_up}) with the key;
    - a timer: {!advance} by the time left on it, when no other running timer is
      due before it;
    - an answer: the pending request of that path gets the answer: [Ok] as a 200
      reply, [Rejected] as a 422 reply, [Http] with its status, [Network] as
      {!network_error};
    - props: {!set_props} with the props decoded by the props codec of the
      machine;
    - a URL: {!back}, {!forward}, or a press on the link to it.

    Steps caused by other steps (the start of a child, its output, its end) are
    not inputs. The replay stops at the first step that is not the same. *)
