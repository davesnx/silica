(** What a hot reload hands from the running page to the new code: the snapshot
    of each mounted instance as JSON text, by its instance path, the instance
    ids of the Live islands, and the URL.

    The old code encodes each snapshot with the state codec registered for its
    machine (see {!Machine.register_codecs}); the new code decodes it with its
    own. No value of the old code goes to the new code: a snapshot that does not
    decode, such as one of a state type that changed, starts again.

    Only development code uses this module: [silica dev] and its browser hook,
    and tests. A release bundle does not reference it. *)

type snapshot =
  | Encoded of string
      (** The JSON text of the snapshot, from the state codec of the old code.
      *)
  | At_start
      (** The machine has no state codec, and the snapshot is the one that
          {!Machine.start_with_props} gives for its props. *)
  | Not_encoded of string
      (** The machine has no state codec, with the reason from
          {!Machine.state_codec}. *)

type entry = {
  machine : string;  (** The {!Machine.id}. *)
  snapshot : snapshot;
}

type t = {
  instances : (string * entry) list;  (** By instance path. *)
  islands : (string * string) list;
      (** The channel and the instance id of each Live island, in page order. *)
  url : Url.t;
}

type codecs = {
  state : 'p 's 'e. ('p, 's, 'e) Machine.t -> ('s Codec.t, string) result;
}
(** How a hot reload finds the state codec of a machine. *)

val registered : codecs
(** {!Machine.state_codec}: the codecs given to {!Machine.register_codecs}. *)

val of_running :
  ?codecs:codecs ->
  islands:(string * string) list ->
  url:Url.t ->
  Instance.running list ->
  t
(** The handoff of the instances that {!Instance.val-running} gives, each
    snapshot encoded by its state codec from [codecs] (default {!registered}).
*)

type reason =
  | Other_machine of string
      (** Another machine, with this id, ran at the path before. *)
  | No_codec of string
      (** The old or the new code has no state codec for the machine, with the
          reason from {!Machine.state_codec}. *)
  | Not_decoded of string
      (** The state codec of the new code does not decode the snapshot, with the
          error: the state type changed. *)
  | Render_failed of string
      (** The first render of the resumed instance raised this exception, so it
          started again (see {!Instance.type-resume}). *)

val reason_text : reason -> string
(** As in ["its snapshot does not decode: ..."]. *)

type restarted = { path : string; machine : string; reason : reason }

type outcome = { kept : string list; restarted : restarted list }
(** The instances that resumed, by path, and the ones that started again. An
    instance with no entry in the handoff is in neither list. *)

val hook : ?codecs:codecs -> t -> Instance.resume * (unit -> outcome)
(** The {!Instance.type-resume} of the handoff, and what it did so far. An
    instance whose path is not in the handoff takes the entry of the same
    machine id when the handoff has exactly one, not taken yet: a view edit that
    moves a component keeps its state.

    An [Encoded] snapshot resumes when the state codec of the new code, from
    [codecs] (default {!registered}), decodes it. An [At_start] snapshot resumes
    at the snapshot that {!Machine.start_with_props} gives in the new code, with
    no entry: a closed popup with no codec keeps being closed. Any other
    instance starts again from its initial snapshot, and the outcome says why.
*)
