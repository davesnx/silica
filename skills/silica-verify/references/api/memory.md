# API: the library silica.memory

## Silica_memory

An in-memory platform for `Silica.Runtime`, on native OCaml and with no DOM. It runs the same host runtime as the browser, so a test drives a component through the same queue of events, timers, calls, subscriptions and error page.

The platform keeps:

- the last painted view, as a `Silica.Node.t` and as HTML;
- a manual clock: nothing scheduled runs until `advance`;
- the HTTP requests of Remote calls, which the test answers one at a time;
- the Live sockets, which the test opens, sends messages into and closes, or which `live_update` drives as a server does;
- a history of URLs with an index, which the navigation actions, link presses, `back` and `forward` change;
- the actions that the renderer ran, the running resources, the title, the log lines and the number of times the session was gone.

Input follows the browser renderer: a handler on the nearest ancestor of the element answers, as with event delegation; a press on an element inside an `a` with `data-silica-link` (and no `target` or `download`) pushes (or, with `data-silica-replace`, replaces) the history entry and reports the URL to the root as `Silica.Router.event.Url_changed`; a key goes to the key handlers of the focused element, then to the `Global_keys` resources and the Escape of the newest dismiss layer; rated handlers use `Silica.Rate.limiter` with the memory clock.

Navigation follows the browser renderer too: while a `Block` resource runs, a link press, `Navigate`, `Replace`, `back` and `forward` change nothing and raise its `on_attempt` (the newest block gets it); `Resume` does the navigation past every block; `Revert_navigation` undoes the last reported change without a report. After a push or a back or forward to another path, a task on the clock with delay 0 moves focus to the first element with `data-silica-route-focus` (or to no element), unless a `Focus` action ran since, and announces the title (see `announcements`).

Out of scope: islands (`data-silica-live`) are painted as empty elements and never connect; `Focus_trap` (other than as a dismiss layer), `Scroll_lock`, `Resize`, `Intersection`, `Anchor`, `Scroll`, `Visibility` and `Online` resources are kept in `held` but do nothing; a `Media` resource answers from `set_media`; `Scroll_into_view`, `Scroll_to` and `Reload` are only recorded; `Measure` answers `Silica.Rect.zero` when the ref is painted and `None` when it is not; `Copy` and `Load_script` answer `true`; a portal is searchable but its target is ignored; `Set_status` is only recorded; links with `data-silica-preload="viewport"` never preload.

```ocaml
type 'p t
```

A mounted runtime and its platform, with a root component that has props `'p`.

**Mount**

```ocaml
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
```

Start the runtime with the component as the root, and paint. `answers` answers calls before the first paint, as a server render does. `csrf` is the page token (default `""`). `url` is the first history entry (default `"/"`). `media` gives the media queries that match at the start (see `set_media`); a query that is not in it does not match. With `dev`, an exception during the first paint is raised again, as `Silica.Runtime.start` says.

```ocaml
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
```

Like `mount` for a router app: the app starts at the current URL, and each URL change reaches the root as `Silica.Router.event.Url_changed`, each link preload as `Silica.Router.event.Preload`.

```ocaml
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
```

Like `mount`, for a root component with props. After an error page, "Try again" starts the root with the last props.

```ocaml
val set_props : 'p t -> 'p -> unit
```

Give the root new props, as a parent does: the `on_props` of the machine runs, and the view paints again. Props equal to the current ones do nothing. It sends `Silica.Runtime.event.Props` to the runtime.

```ocaml
val stop : _ t -> unit
```

Stop the runtime (`Silica.Runtime.event.Stop`): the view is empty, no resource is held, and no timer, call or subscription delivers again.

```ocaml
val lifecycle : _ t -> Runtime.state
```

The state of the runtime: running, showing the error page, or stopped.

**Instances**

```ocaml
val running : _ t -> Instance.running list
```

Every mounted instance of the current tree, parents before their children (see `Silica.Instance.running`). After an error page it is the tree of "Try again".

```ocaml
val send : _ t -> Instance.message -> unit
```

Give the message to the runtime, as the renderer gives the message of a handler: it runs through the queue, and the view paints again. Make it with the `message` of an instance from `running`.

**View**

```ocaml
val view : _ t -> Instance.message Node.t
```

The last painted view.

```ocaml
val html : _ t -> string
```

The last painted view as HTML, by `Silica.Render.to_string`. Portals are left out.

```ocaml
val paints : _ t -> int
```

The number of paints since the start.

```ocaml
val view_text : _ t -> string
```

The last painted view as text, by `Silica.View_text.to_string`.

```ocaml
val shows : _ t -> string -> bool
```

`shows t text`: `text` is a part of the text that a person sees on the page (`Silica.View_text.visible`), in any element, with or without a role, but not in hidden ones.

**Elements**

```ocaml
type element
```

A position in the painted view. It is looked up again in the current view at each use, and raises `Failure` when the view no longer has it.

```ocaml
val all : ?within:element -> _ t -> element list
```

Every element in document order. `within` keeps only the descendants of that element.

```ocaml
val by_id : _ t -> string -> element
```

The first element with this `id`. Raises `Failure` when there is none.

```ocaml
val select : ?within:element -> _ t -> string -> string -> element list
```

`select t name value` is every element whose attribute `name` equals `value` exactly, in document order. A ref counts as `data-silica-ref`. `within` keeps only the descendants of that element.

```ocaml
val by_text : ?within:element -> _ t -> string -> element
```

The innermost element whose text equals the string. Raises `Failure` when there is none.

```ocaml
val text : element -> string
```

The text of the element and its descendants.

```ocaml
val attribute : element -> string -> string option
```

The value of an attribute; `Some ""` for a `true` boolean attribute.

```ocaml
val tag : element -> string
```

The tag name.

**Roles and names**

Find elements as a screen reader does: by their role and accessible name, as `Silica.Accessibility` computes them. Hidden elements are left out.

```ocaml
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
```

The one element of the role that has the name (as `accessible_name` gives it) and the states. `within` keeps only the descendants of that element. Raises `Failure` when none or several match: the message lists the elements of that role with their names, states and paths.

```ocaml
val all_by_role :
  ?within:element -> ?name:string -> _ t -> Accessibility.role -> element list
```

Every element of the role, with the name when it is given, in document order.

```ocaml
val by_name : ?within:element -> _ t -> string -> element
```

The one element with no role that has the accessible name, such as ``<input type_=`password>`` in a `<label>`, which has no role. Raises `Failure` when none or several match; when an element with a role has the name, the message names its role, for `by_role`.

```ocaml
val all_by_name : ?within:element -> _ t -> string -> element list
```

Every element with no role that has the accessible name, in document order.

```ocaml
val role : element -> Accessibility.role option
```

The role of the element.

```ocaml
val accessible_name : element -> string
```

The accessible name of the element.

```ocaml
val disabled : element -> bool
```

The element is disabled: `aria-disabled="true"` or `disabled` on it (see `Silica.Accessibility.disabled`), or it is inside a disabled `fieldset` or other native control. A press reaches an element with only `aria-disabled`, as in a browser (see `Not_reachable`).

```ocaml
val hidden : element -> bool
```

The element is not in the accessibility tree (see `Silica.Accessibility.hidden`).

```ocaml
val checked : element -> bool option
```

See `Silica.Accessibility.checked`.

```ocaml
val selected : element -> bool option
```

See `Silica.Accessibility.selected`.

```ocaml
val expanded : element -> bool option
```

See `Silica.Accessibility.expanded`.

**Input**

As in a browser, `press`, `double_press`, `input`, `submit` and `focus` reach only an element that a user can reach: they raise `Not_reachable` when the element, or an element around it, is a disabled `button`, `input`, `select`, `textarea`, `option`, `optgroup` or `fieldset`, or has `hidden` or `inert`, or is behind an open modal (`Silica.Accessibility.behind_modal`: the browser renderer makes that page `inert`). `aria-disabled` and `aria-hidden` do not stop input, as in a browser.

```ocaml
type unreachable =
  | Disabled
      (** The element, or a [fieldset] around it, is a disabled native control.
      *)
  | Hidden  (** The element or an element around it has [hidden]. *)
  | Inert
```

Why an input cannot reach an element.

```ocaml
exception Not_reachable of { reason : unreachable; message : string }
```

```ocaml
val press : element -> unit
```

A press: the outside-press resources and dismiss layers see the press start, then a link is followed or the nearest press handler runs. Then, as in a browser, a press on a submit button (a `button` with no `type` or `type="submit"`, or an `input` of type `submit` or `image`) that is still in the view submits its form: the nearest submit handler from the form runs.

```ocaml
val double_press : element -> unit
```

A double press reaches the nearest double press handler.

```ocaml
val input : element -> string -> unit
```

The field now holds the text: the nearest input handler gets it.

```ocaml
val submit : element -> unit
```

A submit reaches the nearest submit handler.

```ocaml
val focus : element -> unit
```

Move focus to the element: the blur handler of the element that had focus, then the focus handler of this one, then the focus-out of dismiss layers.

```ocaml
val blur : _ t -> unit
```

No element has focus any more.

```ocaml
val focused : _ t -> element option
```

The element with focus.

```ocaml
val key : _ t -> Key.t -> unit
```

A key goes down on the focused element (or the page when none has focus). Element key handlers run first; Enter and Space press an element that is not a native control; then the `Global_keys` resources and the dismiss layers get the key, unless a handler prevented it.

```ocaml
val key_up : _ t -> Key.t -> unit
```

A key goes up on the focused element: only element handlers see it.

```ocaml
val hover : element -> unit
```

The pointer enters the element: the nearest pointer enter handler gets `Silica.Pointer.origin`. Inside a link with `data-silica-preload="intent"`, the root gets `Silica.Router.event.Preload` with the URL of the link. `focus` on such a link does the same.

```ocaml
val leave : element -> unit
```

The pointer leaves the element: the nearest pointer leave handler gets `Silica.Pointer.origin`.

```ocaml
val back : _ t -> unit
```

The back button of the browser: move to the previous history entry, if there is one, and report its URL.

```ocaml
val forward : _ t -> unit
```

The forward button of the browser: move to the next history entry, if there is one, and report its URL.

```ocaml
val open_url : _ t -> string -> unit
```

The user opens the URL, as in the address bar: a new history entry, which the root gets as `Silica.Router.event.Url_changed`, unless a `Silica.Action.Block` resource holds it. The page does not load again: this is the push of a single-page app.

**Clock**

```ocaml
val now : _ t -> int
```

Milliseconds since the start.

```ocaml
val advance : _ t -> int -> unit
```

Move the clock forward by the milliseconds, and run each task that becomes due, in time order, including tasks that those tasks schedule. Raises `Invalid_argument` for a negative time.

```ocaml
val timers : _ t -> int list
```

The delays, in milliseconds, of the scheduled tasks that have not run and were not cancelled, in the order they were scheduled.

**Transition log**

```ocaml
val trace : _ t -> Trace.t
```

Every transition since the start, with data: the runtime always records (see `Silica.Instance.mount` and `Silica.Trace`). Each record has the time of the memory clock. Event data, props and snapshots are there for the machines that registered their codecs (`Silica.Machine.register_codecs`); answers, Live updates and input text are always there, except text typed into a password field, which is masked (see `Silica.Transition.observer`). The header has the first URL, the build `"memory"` and the platform `"memory"`. Read it with `Silica.Trace.to_text` or write it with `Silica.Trace.to_jsonl`.

After an error page, "Try again" starts a new tree: its records follow, with their numbers going on and instance numbers starting again at 1.

```ocaml
val trace_in_clear : _ t -> Trace.t
```

`trace` with the text typed into password fields in clear, for a replay of the run in the same process, as `Check_expect.holds` does. Never write it to a file.

**HTTP**

```ocaml
type request
```

A Remote call request.

```ocaml
val path : request -> string
```

The path of the request.

```ocaml
val body : request -> string
```

The JSON body of the request.

```ocaml
val request_csrf : request -> string
```

The CSRF token sent with the request.

```ocaml
val requests : _ t -> request list
```

Every request sent, oldest first.

```ocaml
val pending : _ t -> request list
```

The requests with no answer yet, oldest first.

```ocaml
val pending_request : _ t -> string -> request
```

The oldest request to the path that has no answer yet. Raises `Failure` when there is none; the text lists the paths of the pending requests.

```ocaml
val reply : ?status:int -> request -> string -> unit
```

Answer the request with the status (default `200`) and the body. Raises `Invalid_argument` when it already has an answer.

```ocaml
val network_error : request -> string -> unit
```

Answer the request with a network failure. Raises `Invalid_argument` when it already has an answer.

**Live sockets**

```ocaml
type socket
```

A Live socket that the runtime opened.

```ocaml
val sockets : _ t -> socket list
```

Every socket opened, oldest first.

```ocaml
val sent : socket -> string list
```

The messages that the client sent, oldest first.

```ocaml
val closed : socket -> int option
```

The code that the client closed the socket with.

```ocaml
val socket_open : socket -> unit
```

The socket opens.

```ocaml
val socket_message : socket -> string -> unit
```

The server sends a text message.

```ocaml
val socket_close : socket -> int -> unit
```

The socket closes with the code.

```ocaml
val socket_error : socket -> unit
```

The socket reports an error.

```ocaml
val live_update : _ t -> topic:string -> string -> unit
```

`live_update t ~topic update` gives the subscription to `topic` the update that a Live step of a trace holds: `update` is written as `Silica.Topic.update_codec` writes it, such as `{"seq":1,"status":["Ready",{"data":[],"stale":false}]}`. It acts as the server and the network that give that update:

- `Idle` or `Loading`: nothing; the subscription gives it when it starts.
- `Ready` with `stale = false`: the server sends the state with its `seq`. First, when that has not happened, the socket opens and the server answers the hello; after a loss, the client connects again at once, without the wait of `Silica.Backoff`, and the clock does not move.
- `Ready` with `stale = true`: the socket is lost (close code 1006), and the client keeps the last state.
- `Failed` with `Network code`: the server sends the error `code`; with `Http 401`, `403`, `404` or `426`, it closes the socket with `Silica.Protocol.close_session`, `close_csrf`, `close_unknown` or `close_version`.

The server answers each ping on a socket that it answered the hello of, so the heartbeat never drops the socket while the clock moves. Opening a socket opens every socket that waits to open. Raises `Invalid_argument` when no subscription to `topic` is open or waits to connect again, or when `update` is none of these.

**Records**

```ocaml
val url : _ t -> string
```

The URL of the current history entry.

```ocaml
val history : _ t -> string list
```

The URLs of the history entries, oldest first.

```ocaml
val history_index : _ t -> int
```

The index of the current entry in `history`.

```ocaml
val actions : _ t -> Instance.message Action.t list
```

The actions that the renderer ran, oldest first.

```ocaml
val held : _ t -> (string * Instance.message Action.held) list
```

The running held resources with their ids, in the order they started.

```ocaml
val title : _ t -> string
```

The document title (default `""`).

```ocaml
val document_attribute : _ t -> string -> string option
```

The value of an attribute of the document element, as the `Silica.Action.Set_document_attribute` actions left it.

```ocaml
val cookies : _ t -> (string * string) list
```

The cookies that `Silica.Action.Set_cookie` stored, as name and value, in the order of their last write.

```ocaml
val set_media : _ t -> string -> bool -> unit
```

The media query now matches, or not: each running `Silica.Action.Media` resource of that query raises its event at once.

```ocaml
val logs : _ t -> string list
```

The log lines of the runtime, oldest first.

```ocaml
val leave_asks : _ t -> bool
```

`true` while a `Silica.Action.Block` resource runs: closing or reloading the page would ask the user first.

```ocaml
val announcements : _ t -> string list
```

The titles that the live region announced after navigations, oldest first.

```ocaml
val session_gone : _ t -> int
```

How many times the runtime said that the server no longer knows the session.

**Replay**

```ocaml
type replay_result =
  | Replayed  (** Every step gave the same state names. *)
  | Different of { seq : int; expected : string; got : string }
      (** The runtime reached another state, or no transition. *)
  | Not_realisable of { seq : int; reason : string }
      (** The step has no input in this platform: a timer that real time does
          not fire first, a held resource, the result of an action, an event of
          the host or of the inputs of a check, a preload, a step whose data the
          trace does not hold, or text typed into a password field, which a
          trace masks (see {!Silica.Secret}). *)
  | Not_found of { seq : int; error : string }
```

The outcome of `replay`. `seq` is the transition of the trace.

```ocaml
val replay : _ t -> Trace.t -> replay_result
```

`replay t trace` gives the runtime of `t`, just mounted, the input of each step of `trace`, such as a counterexample of the checker (`Check.export`) or a recorded run, and compares each step with the state names of the trace:

- an element handler: the element of that role and accessible name (by its path when several match), then `press`, `double_press`, `submit`, `focus`, `blur`, `hover`, `input` with the text, or `focus` and `key` (or `key_up`) with the key;
- a timer: `advance` by the time left on it, when no other running timer is due before it;
- an answer: the pending request of that path gets the answer: `Ok` as a 200 reply, `Rejected` as a 422 reply, `Http` with its status, `Network` as `network_error`;
- props: `set_props` with the props decoded by the props codec of the machine;
- a Live update: `live_update` with the update; the subscription gives the first update (`Loading`) when it starts, so that step is not an input;
- a URL: `back`, `forward`, or a press on the link to it.

Steps caused by other steps (the start of a child, its output, its end) are not inputs. The replay stops at the first step that is not the same.
