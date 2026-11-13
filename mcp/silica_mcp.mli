(** An MCP server over stdio that lets an agent use an app through the states of
    its machines and the routes of its router, not through the screen.

    The app runs in memory ({!Silica_memory}). The tools at a moment are
    [navigate], [read], and each published event of each running instance of a
    published machine whose state name is published. A call delivers the event
    to that instance, answers the Remote calls with [answer] and runs the timers
    that are due, and gives back the state names before and after and the text
    of the page. *)

type 'e event
(** An event of a machine that an agent can send, as one tool. *)

val event :
  name:string ->
  description:string ->
  schema:Silica_json.Json.t ->
  (Silica_json.Json.t -> ('e, string) result) ->
  'e event
(** [event ~name ~description ~schema decode]: the tool [name], with [schema] as
    its JSON Schema for the arguments, and [decode] from the arguments to the
    event. *)

type publication

val machine :
  ('p, 's, 'e) Silica.Machine.t ->
  states:string list ->
  'e event list ->
  publication
(** The events, offered while an instance of the machine is in one of the
    [states] (state names, see {!Silica.Machine.name}). When several instances
    are in such a state, each tool takes the argument [instance], an instance
    path. *)

val routes :
  'r Silica.Router.routes ->
  published:('r -> bool) ->
  patterns:string ->
  publication
(** The tool [navigate] opens a URL whose route is [published]. [patterns] tells
    the agent the published routes. *)

val protocol_version : string
(** ["2025-11-25"]. *)

val serve :
  ?input:in_channel ->
  ?output:out_channel ->
  name:string ->
  version:string ->
  answer:(Silica_memory.request -> int * string) ->
  publication list ->
  _ Silica_memory.t ->
  unit
(** Read JSON-RPC 2.0 messages, one per line, from [input] (default [stdin])
    until its end, and write the answers to [output] (default [stdout]).
    [answer] gives the status and body of each Remote call of the app. *)
