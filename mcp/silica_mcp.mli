(** An MCP server that lets an agent use an app through the states of its
    machines and the routes of its router, not through the screen. It speaks the
    MCP revision 2026-07-28 only: no [initialize], and each request gives its
    protocol version in [_meta]. It answers over stdio ({!serve}) or over
    streamable HTTP ({!http}).

    The app runs on a host: in memory ({!memory}) or on the silica server
    ({!server}). The tool list is the same on every request: [navigate] when
    routes are published, [read], then the tools of each publication
    ({!Silica.Mcp.tools}), in the order given. A call of a tool of a machine
    decodes the arguments, finds the instance, checks that its state handles the
    event ({!Silica.Machine.can_with_props}), sends the event and waits for the
    host to settle. Its result gives the state of the instance after that. A
    call that cannot run is a result with [isError] and a sentence that says
    why. *)

type routes
(** The routes that the tool [navigate] can open. *)

val routes :
  'r Silica.Router.routes -> published:('r -> bool) -> patterns:string -> routes
(** [navigate] opens a URL whose route is [published]. [patterns] tells the
    agent the published routes, in the description of [navigate]. *)

val protocol_version : string
(** ["2026-07-28"], the only version the server speaks. *)

type host
(** Where the app runs: its URL, its running instances, and the page that [read]
    shows. *)

val memory :
  answer:(Silica_memory.request -> int * string) -> _ Silica_memory.t -> host
(** The app in memory. [answer] gives the status and body of each Remote call of
    the app. Before each tool call, and after each event or URL, the host runs
    the timers that are due and answers the calls, until none is left (at most
    50 rounds). [read] gives the timers that wait and the page. *)

type instance
(** A running instance of a machine that the host does not find by itself. *)

val instance :
  ('p, 's, 'e) Silica.Machine.t ->
  path:string ->
  props:'p ->
  snapshot:(unit -> 's) ->
  send:('e -> (bool, string) result) ->
  instance
(** The instance [path] of the machine with its props. [snapshot] gives its
    state now. [send] steps it with an event and returns once the step is done:
    [Ok false] when its state ignored the event. *)

type island
(** A Live island of the page, with the state it shows. *)

val island : ('s, 'e) Silica.Live.t -> (unit -> 's) -> island
(** The island of the channel shows the view of the Live component at the state
    that the function gives. *)

val server :
  ?url:string ->
  app:(Silica.Url.t -> (unit, 's, 'e) Silica.Component.t) ->
  islands:island list ->
  instance list ->
  host
(** A host whose machines run on a server, outside the agent, such as the shared
    instances of [Silica_server] (see [Silica_server.Make.share]). The agent has
    its own URL, [url] at first (default ["/"]); [navigate] sets it and changes
    nothing else. [read] gives the page: the view text of [app] at that URL,
    with each island of [islands] at its current state (see
    {!Silica.View_text.to_string}). The machines of [app] start fresh for each
    [read], with no timers or calls; only the instances of the list take events.
*)

type t
(** A server over one app. *)

val make :
  name:string ->
  version:string ->
  ?instructions:string ->
  ?routes:routes ->
  Silica.Mcp.publication list ->
  host ->
  t
(** The server [name] at [version], over the app on the host. [instructions]
    tell the model how to use the server, in the answer of [server/discover].
    Raises [Invalid_argument] when two tools have the same name, [navigate] and
    [read] included. *)

val handle : t -> Silica_json.Json.t -> Silica_json.Json.t option
(** The answer to one JSON-RPC message: [server/discover], [tools/list] and
    [tools/call]. [None] for a notification. *)

val serve : ?input:in_channel -> ?output:out_channel -> t -> unit
(** Read JSON-RPC 2.0 messages, one per line, from [input] (default [stdin])
    until its end, and write the answers to [output] (default [stdout]). *)

val http : t -> Silica_server.request -> Silica_server.response
(** The streamable HTTP binding of MCP 2026-07-28, for one request to the MCP
    endpoint (see [Silica_server.Make.create ?mcp], which checks the origin
    first). A method other than [POST] gets 405, a body that is not
    [application/json] 415, a body that does not parse 400 with the JSON-RPC
    error [-32700], and a body that is not one object with a [method] 400 with
    [-32600]. A request (a message with an [id]) must have the header
    [MCP-Protocol-Version], equal to the version in its [_meta] when that is
    there, and [Mcp-Method], equal to its method; a [tools/call] must have
    [Mcp-Name], equal to [params.name] or its Base64 form [=?base64?...?=].
    Otherwise 400 with [-32020] ([HeaderMismatch]). Then {!handle} answers: a
    notification gets 202 with no body; an answer is [application/json], with
    status 404 for [-32601] (method not found), 400 for [-32022] (unsupported
    version), and 200 otherwise. The answer is never an SSE stream. *)
