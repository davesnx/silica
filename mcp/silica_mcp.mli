(** An MCP server over stdio that lets an agent use an app through the states of
    its machines and the routes of its router, not through the screen. It speaks
    the MCP revision 2026-07-28 only: no [initialize], and each request gives
    its protocol version in [_meta].

    The app runs in memory ({!Silica_memory}). The tool list is the same on
    every request: [navigate] when routes are published, [read], then the tools
    of each publication ({!Silica.Mcp.tools}), in the order given. A call of a
    tool of a machine decodes the arguments, finds the instance, checks that its
    state handles the event ({!Silica.Machine.can_with_props}), sends the event,
    answers the Remote calls of the app with [answer] and runs the timers that
    are due. Its result gives the state of the instance after that. A call that
    cannot run is a result with [isError] and a sentence that says why. *)

type routes
(** The routes that the tool [navigate] can open. *)

val routes :
  'r Silica.Router.routes -> published:('r -> bool) -> patterns:string -> routes
(** [navigate] opens a URL whose route is [published]. [patterns] tells the
    agent the published routes, in the description of [navigate]. *)

val protocol_version : string
(** ["2026-07-28"], the only version the server speaks. *)

type t
(** A server over one app. *)

val make :
  name:string ->
  version:string ->
  ?instructions:string ->
  answer:(Silica_memory.request -> int * string) ->
  ?routes:routes ->
  Silica.Mcp.publication list ->
  _ Silica_memory.t ->
  t
(** The server [name] at [version], over the app. [instructions] tell the model
    how to use the server, in the answer of [server/discover]. [answer] gives
    the status and body of each Remote call of the app. Raises
    [Invalid_argument] when two tools have the same name, [navigate] and [read]
    included. *)

val handle : t -> Silica_json.Json.t -> Silica_json.Json.t option
(** The answer to one JSON-RPC message: [server/discover], [tools/list] and
    [tools/call]. [None] for a notification. *)

val serve : ?input:in_channel -> ?output:out_channel -> t -> unit
(** Read JSON-RPC 2.0 messages, one per line, from [input] (default [stdin])
    until its end, and write the answers to [output] (default [stdout]). *)
