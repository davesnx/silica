(** The inspector protocol: what a running app tells a tool about its machines.

    Under [silica dev], each page of the app (a browser target) and the server
    (a server target, for its Live islands) send the messages of their instance
    trees to the hub of [silica dev]. Clients, such as [silica inspect] or a
    panel, ask the hub for the targets, the instance trees and the traces, and
    subscribe to the messages of the targets, with filters.

    Each message is one JSON object with a field ["t"]. The transports carry one
    message per WebSocket text frame (path [/_silica/inspect] of the page) or
    one message per line (the Unix socket [_build/silica-dev/inspect.sock]). The
    hub answers a request of a client with messages that carry the ["id"] of the
    request.

    {v
    {"t":"hello","role":"target","version":1,"target":"tab-3f2a","kind":"browser","url":"/"}
    {"t":"step","target":"tab-3f2a","seq":2,"instance":"counter","machine":"counter",...,"owns":[]}
    v}

    A [step] holds a {!Transition.t} as {!Transition.codec} writes it, with the
    fields ["t"], ["target"] and ["owns"] added. *)

val version : int
(** The version of the protocol: [1]. A [hello] with another version gets an
    [error] and the connection is closed. *)

(** {1 Messages} *)

type kind = Browser | Server  (** A page of the app, or the app server. *)

type resource = {
  resource : Machine.resource;
  since : float option;
      (** The clock of the target, in milliseconds, when the resource started.
      *)
}
(** A timer or source that an instance owns (see {!Machine.active}). *)

type instance = {
  path : string;  (** The instance path, as in {!Transition.t.instance}. *)
  machine : string;
  state : string;
  data : Silica_json.Json.t option;
      (** The snapshot, by the state codec of the machine (see
          {!Machine.register_codecs}). *)
  props : Silica_json.Json.t option;  (** By the props codec of the machine. *)
  owns : resource list;
}
(** A running instance. *)

type restarted = { path : string; machine : string; reason : string }
(** An instance that a hot reload started again, and why. *)

type direction = In | Out  (** A Live message to the target, or from it. *)

(** What a target says, and what the hub forwards to clients. *)
type event =
  | Tree of instance list
      (** Every running instance, parents before children. *)
  | Mount of instance  (** An instance started. *)
  | Unmount of string  (** The instance at this path ended. *)
  | Step of { transition : Transition.t; owns : resource list option }
      (** One record of the transition log, with what the instance owns after
          it. [owns] is [None] for an [unhandled] step, or when the target does
          not know it. *)
  | Live of { direction : direction; channel : string; message : string }
      (** A message of a Live socket. *)
  | Resumed of { kept : string list; restarted : restarted list }
      (** A hot reload ran: the instance paths that kept their state and the
          ones that started again. *)
  | Paused of int
      (** The target is paused at this [seq] (time travel, not implemented yet).
      *)

type filter = {
  machines : string list;  (** Machine ids; [[]] is every machine. *)
  path : string option;
      (** An instance path: the instance and the instances under it. *)
  events : string list;
      (** Event labels (see {!Transition.label}); [[]] is every event. *)
  changes : Transition.change list;  (** [[]] is every change. *)
}

val every : filter
(** Lets every message pass. *)

type target = { target : string; kind : kind; url : string option }

type request =
  | Targets  (** The targets that are connected. *)
  | Get_tree of string option
      (** The tree of the target, or of each target with [None]. *)
  | Export of string option
      (** The trace of the steps that the hub kept for the target. [None] works
          when only one target is connected. *)
  | Travel of { target : string; seq : int }
      (** Pause the target at a step (not implemented yet). *)
  | Return_live of string  (** Leave time travel (not implemented yet). *)
  | Seen of string
      (** The steps of this machine id that the hub got since [silica dev]
          started, from every target, also the targets that left or connected
          again after a hot reload. *)

type message =
  | Hello_target of target
      (** The first message of a target; the hub forwards it to the clients when
          the target connects. *)
  | Hello_client of string  (** The first message of a client, with its name. *)
  | Event of { id : int option; target : string; event : event }
      (** A message of a target. [id] is the one of the request that a [Tree]
          answers. *)
  | Subscribe of { id : int option; target : string option; filter : filter }
      (** Forward the messages of the target (of every target with [None]) that
          pass the filter, from now on. *)
  | Request of { id : int option; request : request }
  | Subscribed of { id : int option }  (** The answer to [Subscribe]. *)
  | Target_list of { id : int option; targets : target list }
      (** The answer to [Targets], and the first message to a client. *)
  | Trace_file of { id : int option; target : string; trace : string }
      (** The answer to [Export]: a trace file (see {!Trace.to_jsonl}). *)
  | Seen_steps of {
      id : int option;
      machine : string;
      steps : Transition.t list;
    }
      (** The answer to [Seen]: one step of each kind, in the order the hub
          first got it, with no user data (see {!Transition.strip}). Two steps
          are of one kind when they have the same change, states, event and
          label. *)
  | Gone of string  (** The target disconnected. *)
  | Failure of { id : int option; message : string }

val encode : message -> Silica_json.Json.t
val decode : Silica_json.Json.t -> (message, string) result

val to_string : message -> string
(** One line of JSON. *)

val of_string : string -> (message, string) result

val passes : filter -> event -> bool
(** A step passes when its machine, its instance path, its event and its change
    pass; a mount or unmount when its machine and path pass (for an unmount, the
    path only); the other events always pass. *)

val to_text : message -> string option
(** One line for a person or an agent to read, for the messages that a [tail]
    shows:
    [[tab-3f2a] 12 +840ms counter idle -> idle (kept) on increment <- press
     button "+"] (see {!Trace.line}); [None] for the others. *)

(** {1 Targets} *)

(** The target side: an {!Instance.observer} that keeps a table of the running
    instances and turns each start, step and end into messages. *)
module Target : sig
  type t

  val create :
    target:string -> clock:(unit -> float) -> send:(event -> unit) -> unit -> t
  (** [send] gets [Mount], then [Step] for a start; [Step] for a step; [Step],
      then [Unmount] for an end. *)

  val observer : ?path:(string -> string) -> t -> Instance.observer
  (** The observer of one tree or one Live island. [path] changes the instance
      paths of its records, such as to tell two islands of one machine apart. *)

  val combine :
    Instance.observer -> Instance.observer option -> Instance.observer
  (** [combine inspector app] gives each observed step to [app], then to
      [inspector]; the handlers are the ones of [app] when it is given. *)

  val tree : t -> instance list
  (** The running instances, in the order they started. *)

  val name : t -> string
end
