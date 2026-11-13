(** Browser host: the runtime (event queue, timers, calls, error boundary,
    answer replay) and the DOM renderer, wired together.

    Mounting adopts the HTML that the server painted into the root element
    instead of building it again: the first paint walks the existing nodes
    beside the view and keeps them (see {!mount}). *)

open Silica

type live_config = Live_client.config = { ping_ms : int; pong_ms : int }
(** Times of the live heartbeat, in milliseconds. [ping_ms] is the wait before
    each ping. [pong_ms] is the wait for the pong; when it passes, the host
    closes the socket and connects again. The default is
    {!Silica.Protocol.ping_interval_ms} and {!Silica.Protocol.pong_timeout_ms}.
    A test uses small values. *)

type live =
  | Live : ('s, 'e) Live.t -> live
      (** A live component that can be embedded as an island: an element with
          [data-silica-live="<channel>"] (see [Live.slot]). The host opens a
          session when the element appears, never diffs its children, and closes
          the session when the element is removed. The server HTML inside an
          island stays until the first message of the session. An unknown
          channel is logged to the console. The host renders the view of a live
          component with no instance tree: that view must not hold component
          nodes. *)

val mount :
  ?live_config:live_config ->
  ?on_session_gone:(unit -> unit) ->
  ?dev:bool ->
  ?on_error:(exn -> Node.error -> unit) ->
  ?hydrate:bool ->
  ?lives:live list ->
  ?observe:Instance.observer ->
  root:string ->
  (unit, 's, 'e) Component.t ->
  unit
(** [on_session_gone] runs when a Live socket closes with
    {!Silica.Protocol.close_session}: the server no longer knows the session,
    for example after a restart. The default reloads the page once, which gets a
    new session and token; a second call within 10 seconds only logs, so a page
    that cannot keep a cookie does not reload for ever. A topic subscription
    also gets [Failed (Http 401)] with its last data. The setting works the same
    in {!mount_app} and {!mount_live}.

    Mount the component as the root of an instance tree and paint it into the
    element with the given id. Child components in its view run their own
    machines while they are rendered. Events raised while one is handled are
    queued. [Navigate] and [Replace] change the history but deliver nothing, as
    the root has no URL event. [Set_title] sets [document.title].

    After the first paint the root element gets the attribute
    [data-silica-ready]: from then on the page reacts to input. Tests wait for
    it.

    Hydration. With [hydrate] (default [true]) the first paint keeps the nodes
    that the server painted: the walk checks tag, node type and child count
    against the view, removes the [<!-- -->] markers that the server put between
    adjacent texts, takes the client value for text and attributes that differ,
    and attaches nothing else, because events are delegated. An [input] or
    [textarea] whose text the user changed before the first paint keeps that
    text, and the host then sends it as an [input] event, so the machine gets
    it. When the structure differs, it clears the root and renders again, and
    logs the first difference to the console. In dev mode it throws [Failure]
    instead, after the error page shows. With [hydrate:false] the root is
    cleared first.

    Early input. The server HTML looks ready before the page can react. A press
    (click, double click, key, submit) that comes after the host made its
    renderer but before its first paint, as while a lazy chunk of the first page
    loads or before the first state of an island, is held and runs after that
    paint, against the adopted view, in order: at most 16, none older than 10
    seconds; a submit is stopped at once, so the browser does not send the form.
    A press that comes before the code of the page runs at all is kept by the
    script that the server puts in the head of the page ({!Silica.Early_input}),
    and the host presses it again after its first paint, with the same limits.

    Live topics. A state with a {!Silica.Topic.subscribe} subscribes to its
    topic. The host sends [Loading None] at once, then a [Ready] for each state.
    While the socket is down it sends the old data once with [stale = true] and
    connects again. A fatal close code ends the subscription with [Failed]
    ([Http 403], [Http 404] or [Http 426]; [Network code] for an [Error]
    message). Leaving the state closes the socket.

    Remote answers. If the page has the table that the server wrote
    ({!Silica.Answers.to_script}), the host answers each [Start_call] from it
    before the first paint, so no call runs twice. Each answer is used once; a
    call with no answer runs as usual.

    Errors. The host is a top-level boundary: an exception that no
    {!Silica.Node.val-boundary} catches shows an error page and runs [on_error]
    and the console. In dev mode ([dev], default [false]) the page shows the
    exception, machine, state and a trace; otherwise only a short message and an
    id. The page has "Try again" (starts the root again with fresh state) and
    "Reload the page". A step that raises changes nothing: the state, the view
    and the actions of that step are dropped with the tree. In dev mode an
    exception during the first paint is raised again after the error page shows,
    so a test sees it.

    Transition log. [observe] gets each start, step and end of each instance
    (see {!Silica.Instance.mount}); "Try again" keeps the same hook. The setting
    works the same in {!mount_app} and {!mount_with_renderer}. *)

val recorder :
  ?capacity:int -> ?data:bool -> build:string -> unit -> Trace.Recorder.t
(** A {!Silica.Trace.Recorder} with the browser clock ([Date.now]) and the
    platform ["browser"], names only unless [data] (see
    {!Silica.Trace.Recorder.create}):

    {[
      let recorder = Silica_browser.recorder ~build:"1.4.2" () in
      Silica_browser.mount_app ~root:"app"
        ~observe:(Trace.Recorder.observer recorder)
        app
    ]}

    Read the trace with {!Silica.Trace.Recorder.val-trace} and send
    {!Silica.Trace.to_jsonl} through an endpoint of the app. The observer is a
    value, not the recorder, so an app that makes no recorder carries no
    recorder code. *)

val mount_app :
  ?live_config:live_config ->
  ?on_session_gone:(unit -> unit) ->
  ?dev:bool ->
  ?on_error:(exn -> Node.error -> unit) ->
  ?hydrate:bool ->
  ?lives:live list ->
  ?observe:Instance.observer ->
  root:string ->
  (Url.t -> (unit, 's, Router.event) Component.t) ->
  unit
(** Mount the app component, made for the current URL (see [Router.make]), and
    paint it into the element with the given id; hydration, answers and errors
    work as in {!mount}. A press on [a[data-silica-link]] pushes a history entry
    and sends [Url_changed]; when the anchor has [data-silica-replace], the
    press replaces the entry instead. A press with a modifier key, another
    button, or on an anchor with [target] or [download] is left to the browser.
    [popstate] and the actions [Navigate] and [Replace] send [Url_changed] too.
    A page sets [document.title] with the action [Set_title]; [Set_status] is
    ignored.

    Blocks. While a {!Silica.Action.Block} source runs, a link press,
    [Navigate], [Replace], back and forward change nothing and raise
    [on_attempt] of the newest block. For back and forward the host goes back to
    the entry it left with [history.go] and reports nothing for that move.
    [Resume] does the navigation past every block. While a block runs,
    [beforeunload] asks the user. [Revert_navigation] undoes the last change the
    host reported: back one entry after a push, the old URL after a replace, the
    other way after back or forward.

    History. The host keeps a key and an index in [history.state] for each
    entry, and the scroll position of each entry in [sessionStorage], so a
    reload restores it. It sets [history.scrollRestoration] to ["manual"]. After
    the paint that follows a push, it scrolls to the element whose id is the
    hash of the URL, or to the top when the path changed. A push that changes
    only the query, and a replace, do not scroll. After [popstate] it restores
    the saved position of the entry. A browser without [window.scrollTo] does
    not scroll.

    Focus. After a push, back or forward to another path, once the events of
    that change are done, the host moves focus, with no scroll, to the first
    element in the root with [data-silica-route-focus], or else to the root; it
    gives the element [tabindex="-1"] when it has no [tabindex]. A [Focus]
    action after the change cancels this move. A polite live region
    ([data-silica-announcer], added to the body) then reads the document title.

    Preload. The pointer entering, or focus on, an anchor with
    [data-silica-preload="intent"] sends {!Silica.Router.event.Preload} with its
    URL; an anchor with ["viewport"] sends it once, when it first enters the
    viewport (an [IntersectionObserver] watches each such anchor from its
    creation). *)

val mount_live :
  ?live_config:live_config ->
  ?on_session_gone:(unit -> unit) ->
  ?dev:bool ->
  root:string ->
  ('s, 'e) Live.t ->
  unit
(** Connect to [/_silica/live] and render the latest server state with the
    component's view. The hello names the channel and an instance id that the
    host makes once and keeps across reconnects. The server HTML in the root
    stays until the first state; that paint adopts it as in {!mount}. Events are
    sent to the server with growing ids.

    Reconnection. When the socket closes or fails, the host connects again after
    a delay ({!Silica.Backoff.delay_ms}) and the server sends a full state.
    While the socket is down the root has the attribute [data-silica-offline]
    (removed at the next state). An event made before the first state of a
    socket (at the start, or while it is down) is held and sent after that
    state, in order: at most 16, none older than 10 seconds (see
    {!Silica.Live_client.send_event}). A press before the first paint, while the
    server HTML is on screen, is held the same way and runs against the adopted
    view (see Early input in {!mount}). The host closes a socket that gets no
    pong in time (see {!live_config}). It does not connect again after a fatal
    close code ({!Silica.Protocol.close_version}, {!Silica.Protocol.close_csrf},
    {!Silica.Protocol.close_unknown}) or an [Error] message; the page keeps its
    last render and the console logs it. *)

val mount_with_renderer :
  ?dev:bool ->
  ?on_error:(exn -> Node.error -> unit) ->
  ?answers:Answers.t ->
  ?observe:Instance.observer ->
  renderer:Instance.message Renderer.make ->
  (unit, 's, 'e) Component.t ->
  unit
(** Run the component with any {!Silica.Renderer} in place of the DOM renderer,
    and the browser clock and network. The runtime is {!Silica.Runtime}, the one
    that {!mount} uses. *)

val cookie : string -> string option
(** The value of the cookie [name] of the page, as the server would read it from
    the [cookie] header. An app reads a stored choice with it before it mounts,
    so its first props equal those of the server render. *)

val import : (unit -> 'a Js.Promise.t) -> 'a Silica.Chunk.t
(** A chunk loaded by a dynamic import, for the browser module of an app that
    holds its lazy routes:
    [let detail = Silica_browser.import (fun () -> Js.import Detail.view)]. The
    bundler (esbuild with [--splitting]) puts [Detail] and the code only it uses
    in a file of its own, loaded the first time the chunk is needed. The native
    module of the app defines the same name with
    [Silica.Chunk.ready Detail.view]. A failed import is a load error (see
    {!Silica.Node.val-chunk}). *)
