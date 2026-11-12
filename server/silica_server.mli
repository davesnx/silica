(** Server side of Silica: page rendering, remote endpoints and live sessions,
    independent of any HTTP library. *)

type request = {
  meth : string;
  path : string;
  query : (string * string) list;
  headers : (string * string) list;
  body : string;
}

type response = {
  status : int;
  headers : (string * string) list;
  body : string;
}

module Session = Silica_server_kit.Session_store
module Assets = Silica_server_kit.Assets

module type IO = sig
  type 'a t

  val return : 'a -> 'a t
  (** A task that gives the value at once. *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (** Run the second task with the result of the first. *)

  val sleep : float -> unit t
  (** Seconds. *)

  val async : (unit -> unit t) -> unit
  (** Run in the background. *)

  val now : unit -> float
  (** The wall clock, in seconds. *)

  type 'a promise
  (** A value that arrives once, from another task. *)

  val promise : unit -> 'a promise
  (** A new promise with no value. *)

  val resolve : 'a promise -> 'a -> unit
  (** The first call sets the value. Later calls do nothing. *)

  val await : 'a promise -> timeout:float -> 'a option t
  (** Wait for the value, at most [timeout] seconds. [None] when the time ends
      first. Other tasks run while it waits. *)
end

type origins =
  | Origins of string list
      (** The allowed values of the [Origin] header, matched in full, such as
          ["https://app.example.com"]. *)
  | Localhost
      (** For development only: the [Origin] must be [scheme://host] with the
          [Host] header of the request, and that host must be [localhost],
          [127.0.0.1] or [[::1]]. *)

type observer = {
  log : source:string -> string -> unit;
      (** [source] is ["silica.http"], ["silica.live"], ["silica.remote"] or
          ["silica.dev"]. *)
  on_request :
    meth:string -> path:string -> status:int -> seconds:float -> unit;
  on_socket : [ `Opened | `Closed ] -> unit;
}

val stderr_observer : observer
(** [log] writes one line to stderr; the other callbacks do nothing. *)

type dev = {
  state_file : string;
      (** Where {!Make.shutdown} saves the sessions, the Live islands and the
          kept values, and where {!Make.create} reads them. *)
  inspect : string option;
      (** The Unix socket of the inspector of [silica dev]. With [Some], the
          server is an inspector target named ["server"]: it sends each start,
          step and end of its Live islands there (see {!Silica.Inspect}). The
          instance path of an island is its machine id, ["#"] and the instance
          id of the island in the page. *)
}
(** Development mode: the server keeps its state across a restart. *)

type config = {
  dev : dev option;
      (** [None] in production. With [Some _], the server keeps its state across
          a restart (see {!Make.shutdown}), {!Make.render_page_settled} shows
          detailed error pages, and each HTML page loads the script at
          {!dev_client_path}. The state file is never read or written with
          [None]. *)
  origins : origins;
  secret : string;
      (** Key of the CSRF tokens. At least 16 bytes; {!Make.create} fails
          otherwise. *)
  secure_cookie : bool;  (** Add [Secure] to the session cookie (TLS). *)
  sessions : Session.ops;
  now : unit -> float;
  max_body : int;  (** Bytes. A larger request body gets status 413. *)
  max_message : int;
      (** Bytes of one incoming Live message. A larger one is dropped and the
          client gets [{"t":"error","code":"too_big"}]. *)
  heartbeat : float;
      (** Seconds. {!Make.sweep} closes a socket with no traffic for two
          heartbeats. *)
  live_keep : float;
      (** Seconds that the Live state of an island or topic stays after its last
          socket closed. *)
  warn_outgoing : int;
      (** Bytes. An outgoing Live message above it is sent, and a warning goes
          to the [silica.live] log. *)
  assets_prefix : string;  (** URL prefix of static files, ["/static/"]. *)
  observer : observer;
}

val default_config : origins:origins -> secret:string -> config
(** [dev] is [None]. [now] is [Unix.gettimeofday]. Sessions are in memory with
    the defaults of {!Session.create}: 8 hours idle, 7 days absolute, 10000
    sessions. [max_body] is 1 MiB, [max_message] 64 KiB, [heartbeat] 30,
    [live_keep] 1800, [warn_outgoing] 1 MiB, [assets_prefix] ["/static/"],
    [secure_cookie] false and [observer] {!stderr_observer}. *)

val dev_of_env : unit -> dev option
(** [Some] with the file named by the environment variable [SILICA_DEV_STATE]
    when it is set and not empty, else [None]; [inspect] is the environment
    variable [SILICA_INSPECT] when it is set and not empty. [silica dev] sets
    both. *)

val secret_of_env : origins:origins -> dev:dev option -> (string, string) result
(** The secret from the environment variable [SILICA_SECRET], which needs at
    least 16 bytes. When it is not set: an [Error] for a server with [Origins]
    (a public server, where a random secret would end the sessions at each
    restart and deploy) or with [dev] (whose sessions outlive the process); with
    [Localhost] and no [dev], a random secret, and a line on stderr says so. The
    [Error] message says what to do. *)

val cookie : request -> string -> string option
(** The value of the cookie [name] in the [cookie] header of the request. A page
    reads a stored choice with it, such as a theme. *)

val cookie_name : string
(** ["silica_sid"]. *)

val csrf_header : string
(** ["x-silica-csrf"]. *)

val live_path : string
(** ["/_silica/live"]. *)

val health_path : string
(** ["/_silica/health"]. *)

val dev_client_path : string
(** ["/_silica/dev/client.js"]: the script of [silica dev] that shows the build
    status and the compile errors, and reloads the page after a build. With
    [config.dev], each HTML page that the [page] function of {!Make.create}
    answers loads it, before [</body>]. *)

module Make (Io : IO) : sig
  type handler
  (** A function that answers calls to one endpoint. *)

  type context = { request : request; session : Session.session }
  (** What a handler or a Live island gets: the request (for a socket, the
      request of the upgrade) and the session of the visitor. *)

  val implement :
    ('i, 'o, 'x) Silica.Remote.endpoint ->
    (context -> 'i -> ('o, 'x) result Io.t) ->
    handler
  (** An [Error x] is an application error, such as a failed validation: status
      422 with [x] encoded by the [rejected] codec of the endpoint, which the
      client gets as {!Silica.Remote.Rejected}. An exception that the handler
      raises while it runs synchronously gives status 500 with the fixed body
      ["internal error"]; the detail goes to the [silica.remote] log. An input
      that does not decode gives status 400. *)

  type island = Island : ('s, 'e) Silica.Live.t -> island

  (** {2 Rendering} *)

  val render_page :
    (Silica.Url.t -> (unit, 's, 'e) Silica.Component.t) ->
    status:('s -> int) ->
    scripts:string list ->
    ?preload:string list ->
    ?head:string ->
    ?dev:bool ->
    ?on_error:(exn -> Silica.Node.error -> unit) ->
    ?error_page:(Silica.Node.error -> response) ->
    ?csrf:string ->
    ?islands:island list ->
    Silica.Url.t ->
    response
  (** Mounts the app root (for example [Router.make routes ~render]) at the URL
      and renders its view, with the page components it holds, as their machines
      are after the start with their props. Actions are ignored, except these:
      the last [Set_title] is the document title; the last
      [Set_document_attribute] of each name is written on the [<html>] tag (a
      [None] value writes nothing); a [Navigate] or [Replace] (such as a router
      guard [Redirect] at the first URL) makes the answer a redirect, status 303
      with a [location] header of that URL and no body. The client runs the
      start again. [status] gives the HTTP status from the snapshot of the root,
      for example 404 when [Router.is_not_found]; the last [Set_status], if any,
      replaces it. [head] is inserted raw. [scripts] are URLs of module scripts
      (see {!asset}), written at the end of the body; each one also gets a
      [<link rel="modulepreload">] in the head, so the browser loads it while
      the HTML arrives. [preload] are more module URLs that get such a link and
      no script, such as the chunks that a script imports (see [page.imports]).
      [csrf] is written as [<meta name="silica-csrf" content="...">] in the
      head, where the client reads it.

      Text nodes that are next to each other print with [<!-- -->] between them,
      so the browser keeps them as separate nodes and can adopt them. Empty text
      prints nothing. A {!Silica.Node.Portal} prints nothing. A Live island
      ({!Silica.Live.slot}) prints its server HTML: the view of the Live
      component at its initial state, when [islands] holds its channel. The
      first state of a connection is that state, so the first Live paint of the
      browser adopts this HTML.

      Errors. An exception that no {!Silica.Node.val-boundary} catches, from the
      root machine or from the view of any component, gives status 500 and
      [error_page] (default: a short HTML page with an error id; with [dev]
      (default [false]) the page also shows the exception, machine, state and
      trace). [on_error] gets the exception and the {!Silica.Node.error} (it
      runs for errors that a boundary catches too); with no hook the error goes
      to stderr. *)

  type page = {
    context : context;
    url : Silica.Url.t;
    csrf : string;  (** The token for this session; the render writes it. *)
    nonce : string;  (** For the [Content-Security-Policy] of this response. *)
    asset : string -> string;
        (** The URL of a logical asset name (see {!Assets}). Raises [Failure]
            when the name is not in the manifest. *)
    imports : string -> string list;
        (** The public paths that the asset at a public path imports statically
            (see {!Assets.imports}). Pass them as [preload], so the browser
            fetches the shared chunks of a split bundle with the entry. *)
    handlers : handler list;  (** The handlers given to {!create}. *)
    islands : island list;  (** The islands given to {!create}. *)
    dev : bool;
        (** [config.dev] is set. {!render_page_settled} takes it as the default
            of its [dev]. *)
  }
  (** What the [page] function of {!create} gets for one page request. *)

  val render_page_settled :
    (Silica.Url.t -> (unit, 's, 'e) Silica.Component.t) ->
    status:('s -> int) ->
    scripts:string list ->
    ?preload:string list ->
    ?head:string ->
    ?dev:bool ->
    ?on_error:(exn -> Silica.Node.error -> unit) ->
    ?error_page:(Silica.Node.error -> response) ->
    ?settle_timeout_ms:int ->
    page ->
    response Io.t
  (** Like {!render_page} at [page.url], with a settle step first and the
      [csrf], [nonce], [islands] and [handlers] of the page. [dev] defaults to
      [page.dev]. It starts the app, then runs the [Start_call] actions of every
      rendered component against the handlers (with [page.context]), steps the
      machines with the answers, renders again, and repeats until no call is
      left. [Start_timer] actions, sources and Live subscriptions do not run.
      Only calls that answer with success count: a call that fails, has no
      handler or does not answer within [settle_timeout_ms] (default 1000, for
      the whole settle step) is left unanswered, and the page shows that state
      as it is at that moment. An application error ({!implement}) is not an
      answer either. A [Navigate] or [Replace] at any step ends the settle step
      with the redirect of {!render_page}; [Set_status] at any step works as
      there. The answers are written into the page as a
      [<script type="application/json" id="silica-answers">] element (see
      {!Silica.Answers}), with [nonce] as its [nonce] attribute. The browser
      answers the same calls from the table before its first paint, so no call
      runs twice and the first view has the HTML of the server.

      The step waits for the calls with [Io.await]; it never polls. Calls run
      with [Io.async]. Errors work as in {!render_page}; an exception that an
      asynchronous [Io] raises in a later continuation is the host's to handle.
  *)

  val render_component :
    (unit, 's, 'e) Silica.Component.t ->
    title:string ->
    scripts:string list ->
    ?preload:string list ->
    ?head:string ->
    ?dev:bool ->
    ?on_error:(exn -> Silica.Node.error -> unit) ->
    ?error_page:(Silica.Node.error -> response) ->
    ?csrf:string ->
    ?islands:island list ->
    unit ->
    response
  (** Like {!render_page} for a component with no URL. A [Set_title] action
      replaces [title]. The status is 200, or the last [Set_status]. *)

  (** {2 One island} *)

  type runner
  (** The machine of one island and the queue of its events. {!val-upgrade}
      makes one for each island subscription; the functions here also serve a
      host that has no socket and tests. *)

  val live :
    ?on_close:(unit -> unit) ->
    ?observe:Silica.Instance.observer ->
    ('s, 'e) Silica.Live.t ->
    handlers:handler list ->
    context:context ->
    send:(string -> unit Io.t) ->
    runner Io.t
  (** Starts the machine and sends the first state message ([seq] 1, [ack] 0).
      Each step sends one more state message, with [seq] one higher and [ack]
      the id of the last event handled. [Start_call] actions run against
      [handlers] directly and [Start_timer] actions through [Io.sleep]; the
      resulting events are stepped like client events, one at a time. A
      [Start_call] with [timeout_ms] also starts a timeout: the first of the
      answer and the timeout steps the machine, with
      [Failed (Network "timeout")] for the timeout, and the other is dropped. A
      [Stop] action, for a timer or call, makes the runner drop its event when
      it arrives. [Start_live] actions do nothing: an island has no topics.

      When a step raises (a machine, a codec or [send]), the error is logged to
      stderr with the channel name, queued events are discarded, the runner
      closes like {!val-close}, and [on_close] runs once so the host can close
      the socket. Only an exception raised synchronously while the step runs is
      caught: [Io] has no catch, so a failure inside a later continuation of an
      asynchronous [Io] is the host's to handle.

      Transition log. [observe] gets the start, each step (also an ignored
      event) and the end ({!val-close}) of the machine, as
      {!Silica.Instance.mount} gives them: the instance path is the machine id,
      a client event has the cause {!Silica.Instance.Message}, and timers and
      calls have their own causes. Give it [Silica.Trace.Recorder.observer] to
      get a trace of the session with no browser. *)

  val receive : runner -> string -> unit Io.t
  (** Handle one client message, an [event] (see {!Silica.Protocol}). An event
      whose [id] is not higher than the last one handled is dropped, so a resend
      does nothing. Messages that do not decode, or that are not events, are
      logged to stderr and dropped. *)

  val resend : runner -> unit Io.t
  (** Send the current state again, with the next [seq]. *)

  val set_send : runner -> (string -> unit Io.t) option -> unit
  (** Where state messages go. [None] while no socket is attached: the runner
      goes on, and the states are not sent. *)

  val close : runner -> unit
  (** Later events, timers and call results are dropped. *)

  (** {2 The server} *)

  type topic_def =
    | Topic_def : {
        topic : 's Silica.Topic.t;
        init : context -> key:string -> ('s, string) result Io.t;
      }
        -> topic_def
        (** [init] makes the first state of a key that nobody has published yet.
        *)

  type t

  val create :
    config ->
    ?assets:Assets.t ->
    ?observe_live:(context -> channel:string -> Silica.Instance.observer) ->
    read:(string -> string option Io.t) ->
    handlers:handler list ->
    islands:island list ->
    topics:topic_def list ->
    page:(page -> response Io.t) ->
    unit ->
    t
  (** [read name] gives the bytes of the file [name] under [assets_prefix], or
      [None]. The server only asks for names without [..], a leading slash or
      control characters. Raises [Invalid_argument] when [config.secret] has
      fewer than 16 bytes.

      [observe_live context ~channel] is the hook of each island instance that a
      socket starts (see {!val-live}), for example a
      [Silica.Trace.Recorder.observer] kept by session id.

      With [config.dev], [create] reads the file that {!shutdown} wrote, if it
      exists, and then removes it. It reads the file only when it is a regular
      file of this user with no rights for others (mode 600); else it logs why
      and ignores it. It trusts the file no more than a cookie:

      - The sessions go to [config.sessions.load], which skips the expired and
        malformed ones (see {!Session.val-load}).
      - An island needs a session of the file, a channel of [islands] that runs
        the same machine id, and a snapshot that the state codec of the machine
        decodes (see {!Silica.Machine.register_codecs}). Else that island alone
        is dropped, the [silica.dev] log says why, and it starts again from the
        initial state when its socket comes back. A restored island waits for a
        socket with its session, channel and instance id, as a held island does
        (see [live_keep]); then it runs again at its snapshot with
        {!Silica.Machine.resume}, with the request of that socket, and the
        observer gets a [Resumed] start. Its event ids go on from the saved
        ones.
      - The kept values wait for {!keep}.

      A file that is not JSON is logged and ignored; [create] does not fail on
      it. *)

  val handle : t -> request -> response Io.t
  (** The whole HTTP policy.

      [GET] [health_path] answers 200 ["ok"]. A [GET] under [assets_prefix]
      serves a file: a name with a hash (see {!Assets.is_hashed}) gets
      [cache-control: public, max-age=31536000, immutable] and
      [vary: accept-encoding]; when [accept-encoding] allows [br] or [gzip] and
      [read] gives the file [name ^ ".br"] or [name ^ ".gz"] (in that order),
      that file is the body, with [content-encoding]. Others get
      [cache-control: no-cache] and an [etag], and an [if-none-match] that
      matches gets 304. A request for a path of a handler must be [POST] (405
      otherwise). Any other [GET] is a page: the session is found by the cookie
      or made (the response then has [set-cookie] with [HttpOnly],
      [SameSite=Lax], [Path=/], and [Secure] when configured, no [Max-Age]), and
      the response has [cache-control: private, no-store].

      Every request that is not [GET] must pass, in this order: the body is at
      most [max_body] (else 413); [sec-fetch-site], when present, is
      [same-origin] or [none] (else 403); [origin] is present and allowed (else
      403); the cookie names a live session (else 403); [content-type] starts
      with [application/json] (else 415); [x-silica-csrf] is the token of the
      session (else 403). Every response has [x-content-type-options: nosniff]
      and [referrer-policy: same-origin]; HTML pages also have a
      [content-security-policy] with the nonce of the page. *)

  type socket = { send : string -> unit Io.t; close : int -> unit }
  (** What the adapter gives for one accepted WebSocket. [send] writes one text
      message; [close code] closes the socket with the close code. *)

  type connection = { receive : string -> unit Io.t; closed : unit -> unit }
  (** What the adapter calls: [receive] for each whole text message, [closed]
      once when the socket is gone. *)

  type upgrade = Refused of response | Accepted of (socket -> connection)

  val upgrade : t -> request -> upgrade Io.t
  (** A WebSocket request for [live_path]. It is refused (404, 403) when the
      path is another, the method is not [GET], [origin] is missing or not
      allowed, or [sec-fetch-site] is present and not [same-origin]. When the
      cookie names no live session (for example after a restart), the upgrade is
      accepted and the socket is closed at once with
      {!Silica.Protocol.close_session}: a browser cannot read the status of a
      refused upgrade, so a refusal would look like a lost network. The first
      message of the connection must be a {!Silica.Protocol.Hello} with version
      1, the CSRF token of the session and an island or a topic that the server
      knows; the server answers with [Hello_ok] and then a state. Otherwise it
      closes the socket with the close code of {!Silica.Protocol}. An island
      with the same session, channel and instance that is still held (see
      [live_keep]) is attached again and sent its state; the event ids go on. A
      message above [max_message] gets an [error] message and is dropped. A step
      that raises closes the socket with {!Silica.Protocol.close_server_error}
      and ends the island. *)

  val publish : t -> 's Silica.Topic.t -> key:string -> 's -> unit Io.t
  (** The new full state of a topic key. Its [seq] is one higher than the last
      one, and every subscriber gets it. Call it from a handler. *)

  val sweep : t -> unit
  (** Housekeeping, called now and then by the adapter (about every 5 seconds):
      closes sockets with no traffic for two heartbeats, ends islands and topic
      states that have had no socket for [live_keep], and removes expired
      sessions. *)

  val sockets : t -> int
  (** The number of open sockets. *)

  val islands : t -> int
  (** The number of held island instances, attached or not, with the islands
      restored by {!create} that no socket has claimed yet. *)

  val keep :
    t ->
    name:string ->
    save:(unit -> string) ->
    load:(string -> (unit, string) result) ->
    unit
  (** Keep app data across a restart in development, such as an in-memory store.
      With [config.dev], {!shutdown} saves [save ()] under [name], and when the
      file that {!create} read had a value under [name], [keep] calls [load]
      with it at once. An [Error] or an exception of [load] or [save] goes to
      the [silica.dev] log. With no [config.dev], [keep] does nothing. A second
      [keep] with the same name replaces the first. *)

  val shutdown : t -> unit
  (** Stop the server, at the end of the process: give it as [on_stop] to the
      adapter. With [config.dev], first write [config.dev.state_file]: the
      sessions (by [config.sessions.save]), each held Live island with its
      session, channel, instance id, machine id, last event id, entry number and
      snapshot (by the state codec of its machine), the restored islands that no
      socket claimed, and the kept values. The file is JSON, written to a new
      file with mode 600 that then replaces the old one. An island whose machine
      has no state codec is not saved, and the [silica.dev] log names the
      machine and how to register the codec. A failure to write goes to the log
      too. Then every island ends. A second call does nothing. *)
end
