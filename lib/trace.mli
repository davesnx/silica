(** A trace: the transitions of one run, as a file that a program can read.

    The file is JSON Lines: one header object on the first line, then one
    {!Transition.t} per line (see {!Transition.codec}), in [seq] order. Lines
    can be appended and streamed, and a file cut after any line still parses.

    {v
    {"root":"app","url":"/","build":"dev","origin":{"type":"runtime","platform":"memory"},"dropped":0}
    {"seq":1,"time_ms":0,"instance":"app","number":1,"entry":0,"machine":"app","from":"","to":"loading","change":"started","event":"","actions":["start call 1.0/app/loading//api/issues null /api/issues"],"cause":{"type":"start"}}
    {"seq":2,"time_ms":40,"instance":"app","number":1,"entry":1,"machine":"app","from":"loading","to":"ready","change":"entered","event":"loaded","actions":["stop 1.0/app/loading//api/issues null"],"cause":{"type":"answer","id":"1.0/app/loading//api/issues null","path":"/api/issues","answer":["Ok",[]]}}
    v}

    What a trace holds, for replay: the start of each instance with its props
    and snapshot, each step with its event and its cause (the element and its
    accessible name, the timer, the answer of a call, the Live update, the URL),
    and the time of each step. The data of events, props and snapshots is there
    only when the machine registered those codecs (see
    {!Machine.register_codecs}) and the recorder keeps data.

    What a trace does not hold: values with no registered codec; the view (only
    the element of each handler, by role, name and path); focus, scroll and
    other browser state that is not in a snapshot; the HTTP status and body of
    an answer (only the answer as the machine got it); the Live socket frames;
    the server state behind calls and topics; the events of a step that raised
    (the step is not recorded); the records dropped by a full recorder (only
    their number).

    A trace with names only still holds the ids of timers, calls and sources,
    and the id of a call holds its encoded input (see {!Source.key}). *)

type origin =
  | Checker of { rule : string }  (** A counterexample of the checker. *)
  | Runtime of { platform : string }
      (** A run of the runtime: ["browser"], ["memory"] or ["server"]. *)

type header = {
  root : string;  (** The machine id of the root instance. *)
  url : string option;  (** The URL of the page, when there is one. *)
  build : string;  (** The version or commit of the app, given by the app. *)
  origin : origin;
  dropped : int;  (** The records that a full recorder dropped. *)
  loop_from : int option;
      (** For a run that loops forever: the [seq] where the loop starts. *)
}

type t = { header : header; transitions : Transition.t list }

val to_jsonl : t -> string
(** The file: the header and each transition, each on one line ending with a
    newline. *)

val of_jsonl : string -> (t, string) result
(** Read a file. Empty lines are skipped. The error names the line number and
    the reason, such as ["line 3: field \"seq\": expected an integer"]. *)

val to_text : t -> string
(** One line for the header, then one line for each transition, for a person or
    an agent to read:
    [12 +840ms issue-list loading -> ready (entered) on loaded <- answer
     /api/issues]. Unhandled steps are marked [(unhandled)]; actions follow on
    the same line after [=>]. *)

(** A ring buffer of transitions, to keep the last ones of a run. *)
module Recorder : sig
  type trace := t
  type t

  val create :
    ?capacity:int ->
    ?data:bool ->
    ?clock:(unit -> float) ->
    ?platform:string ->
    build:string ->
    unit ->
    t
  (** [capacity] (default 1000) is the number of transitions kept; older ones
      are dropped and counted. With [data] (default [false]) the records keep
      event data, props, snapshots, answers, Live updates and input text;
      without it they keep names only (see {!Transition.strip}). [clock] gives
      the time of each record in milliseconds; with no clock records have no
      time. [platform] (default ["browser"]) goes in the header. *)

  val observer : t -> Instance.observer
  (** The observer to give a host, such as {!Instance.mount},
      [Silica_browser.mount ~observe] or [Server.live ~observe]. *)

  val record : t -> Transition.t -> unit
  (** Keep a record made elsewhere. *)

  val transitions : t -> Transition.t list
  (** The records kept, oldest first. Their elements are computed now (see
      {!Transition.pending}). *)

  val trace : t -> url:string option -> trace
  (** The trace of the records kept. The root is the machine of the first record
      whose instance has no parent. *)

  val clear : t -> unit
  (** Drop every record and set [dropped] to 0. The numbering goes on. *)
end
