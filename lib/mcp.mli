(** Tools that an agent calls over the Model Context Protocol (MCP), each one
    bound to an event of a machine.

    Declare a tool with its arguments, then bind it to the events of a machine
    in a module that only the MCP server references, so a browser bundle carries
    no tool:

    {[
      let set_title =
        Mcp.make ~name:"new_issue.title"
          ~description:"Set the title of the new issue"
          Mcp.Input.(field "title" ~description:"The title" string)

      let submit =
        Mcp.make ~name:"new_issue.submit" ~description:"Create the issue"
          Mcp.Input.none

      let tools =
        Mcp.tools Create_issue.machine
          [
            Mcp.on set_title (fun title -> Title title);
            Mcp.on submit (fun () -> Submit);
          ]
    ]}

    A call of a tool decodes its arguments and sends the event to a running
    instance of the machine. The update decides: a state that ignores the event
    refuses the call. The server is [Silica_mcp]. *)

(** The arguments of a tool: one value gives the JSON Schema and the decoder,
    and they agree. [decode] accepts exactly the JSON that [schema] accepts: an
    object with no other keys than its fields, and every field that is not
    optional. *)
module Input : sig
  type 'a t
  (** Arguments that decode to ['a]. *)

  val none : unit t
  (** No arguments: the empty object. Its schema is
      [{"type": "object", "additionalProperties": false}]. *)

  val string : string t
  (** A JSON string. *)

  val int : int t
  (** A JSON integer that fits an [int]. [1.0] is an integer, as JSON Schema
      says. *)

  val bool : bool t
  (** A JSON boolean. *)

  val enum : (string * 'a) list -> 'a t
  (** One of the strings of the list, which gives its value. Raises
      [Invalid_argument] when a string is twice in the list. *)

  val field : string -> ?description:string -> 'a t -> 'a t
  (** An object with one field that is required. [description] tells the agent
      what the field is. *)

  val optional : string -> ?description:string -> 'a t -> 'a option t
  (** An object with one field that can be left out: [None]. A [null] is not a
      missing field. *)

  val both : 'a t -> 'b t -> ('a * 'b) t
  (** The fields of two objects in one object, such as
      [both (field "title" string) (optional "priority" int)]. Raises
      [Invalid_argument] when one of the two is not an object or when both have
      a field with the same name. *)

  val schema : 'a t -> Silica_json.Json.t
  (** The JSON Schema (2020-12) of the arguments. *)

  val decode : 'a t -> Silica_json.Json.t -> ('a, string) result
  (** The value of the arguments. [Error] is a sentence that names the field,
      for the agent. *)
end

type annotations
(** Hints for the client about what a tool does. *)

val annotations :
  ?read_only:bool ->
  ?destructive:bool ->
  ?idempotent:bool ->
  ?open_world:bool ->
  unit ->
  annotations
(** The MCP hints [readOnlyHint] (the tool changes nothing, default [false]),
    [destructiveHint] (it can change or delete data, default [true]),
    [idempotentHint] (a second call with the same arguments changes nothing
    more, default [false]) and [openWorldHint] (it reaches the world outside the
    app, default [true]). A hint that is not given is not sent, and the client
    takes the default. The client does not trust them. *)

type 'a t
(** A tool whose arguments decode to ['a]. *)

val make :
  name:string ->
  ?title:string ->
  description:string ->
  ?annotations:annotations ->
  'a Input.t ->
  'a t
(** [make ~name ~description input]: the tool [name], which the agent chooses by
    its [description]. [title] is a name for people. A name has 1 to 128
    characters of [A-Z], [a-z], [0-9], [_], [-] and [.], such as ["edit.save"].
    Raises [Invalid_argument] for another name, and when [input] is not an
    object (make it with {!Input.none}, {!Input.field}, {!Input.optional} or
    {!Input.both}). *)

val name : 'a t -> string
(** The name of the tool. *)

val input : 'a t -> 'a Input.t
(** The arguments of the tool. *)

val definition : 'a t -> Silica_json.Json.t
(** The tool as [tools/list] gives it: [name], [title], [description],
    [inputSchema] and [annotations]. *)

type 'e binding
(** A tool bound to an event ['e]. *)

val on : 'a t -> ('a -> 'e) -> 'e binding
(** A call of the tool sends the event that the function makes from the
    arguments. *)

type 'e call = {
  event : 'e;
  instance : string option;
      (** The instance path that the agent gave, if any. *)
}
(** The decoded arguments of a tool of a machine. *)

type publication =
  | Publication : {
      machine : ('p, 's, 'e) Machine.t;
      tools : 'e call t list;
    }
      -> publication
      (** The tools of a machine. Each one takes the arguments of its {!Input.t}
          and the optional argument [instance]. A server reads them. *)

val tools : ('p, 's, 'e) Machine.t -> 'e binding list -> publication
(** The bound tools of the machine, in the order of the list. Each tool gets the
    optional argument [instance]: the instance path of the running instance of
    the machine that gets the event. Without it, the event goes to the only
    running instance. Raises [Invalid_argument] when two tools have the same
    name, or when a tool has its own field [instance]. *)
