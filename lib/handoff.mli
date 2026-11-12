(** What a hot reload hands from the running page to the new code: the snapshot
    of each mounted instance, by its instance path, with the fingerprint of its
    state type (see {!Fingerprint}), the instance ids of the Live islands, and
    the URL.

    Only development code uses this module: [silica dev] and its browser hook,
    and tests. A release bundle does not reference it. *)

type entry = {
  machine : string;  (** The {!Machine.id}. *)
  fingerprint : (string, string) result option;
      (** The {!Fingerprint.digest} of the state type, or [None] when the shape
          has no fingerprint. *)
  snapshot : Obj.t;  (** The snapshot, as the old code made it. *)
}

type t = {
  instances : (string * entry) list;  (** By instance path. *)
  islands : (string * string) list;
      (** The channel and the instance id of each Live island, in page order. *)
  url : Url.t;
}

val of_running :
  islands:(string * string) list -> url:Url.t -> Instance.running list -> t
(** The handoff of the instances that {!Instance.val-running} gives. *)

type reason =
  | Other_machine of string
      (** Another machine, with this id, ran at the path before. *)
  | No_fingerprint  (** The shape of the machine has no fingerprint. *)
  | Not_handed_over of string
      (** The fingerprint of the new code gives this reason (see
          {!Fingerprint.digest}). *)
  | Changed  (** The fingerprints differ: the state type changed. *)
  | Holds_function
      (** The snapshot holds a function, which would run the old code. *)
  | Render_failed of string
      (** The first render of the resumed instance raised this exception, so it
          started again (see {!Instance.type-resume}). *)

val reason_text : reason -> string
(** As in ["its state type changed"]. *)

val take :
  holds_function:(Obj.t -> bool) ->
  t ->
  path:string ->
  ('p, 's, 'e) Machine.t ->
  ('s, reason) result option
(** [take ~holds_function handoff ~path machine] is the snapshot that the
    instance at [path] had, for [machine] in the new code: [None] when no
    instance ran at the path, [Error] when the snapshot can not be given.

    This function holds the only cast of a hot reload, from [Obj.t] to ['s]. It
    happens only when the machine id at the path is the same, both fingerprints
    are [Ok] and equal, and [holds_function] finds no function in the snapshot.
    Why that is safe: an equal fingerprint is the same digest of the same
    printed declarations of the state type and of every type it mentions,
    transitively, made by the same deriver with the same Melange and OCaml
    versions (see {!Fingerprint}). The deriver prints the types declared in the
    same file, and checks at compile time, with a re-export, that each one is
    the type with that name where the state type is declared; it takes the
    fingerprint of a type of another module from that module, found by name and
    checked by the type parameter of {!Fingerprint.t}; a type of Silica counts
    by the digest of the interfaces of Silica. A type that it can not see
    (abstract, extensible, from an [open], or from a module with no fingerprint
    for it), and functions, objects, [exn] and [lazy], give no digest, so their
    machines are never given a snapshot. Two builds that compiled the same
    declarations with the same compiler give the values of that type the same
    JavaScript representation, so the snapshot of the old build is a valid value
    of ['s] in the new one. A function inside it would still be code of the old
    build, which [holds_function] refuses. *)

type restarted = { path : string; machine : string; reason : reason }

type outcome = { kept : string list; restarted : restarted list }
(** The instances that resumed, by path, and the ones that started again. An
    instance with no entry in the handoff is in neither list. *)

val hook :
  holds_function:(Obj.t -> bool) -> t -> Instance.resume * (unit -> outcome)
(** The {!Instance.type-resume} of the handoff, and what it did so far. An
    instance whose path is not in the handoff takes the entry of the same
    machine id when the handoff has exactly one, not taken yet: a view edit that
    moves a component keeps its state.

    When a snapshot can not be given because the machine has no fingerprint, or
    its fingerprint gives a reason, the instance starts from its initial
    snapshot (with {!Machine.start} on its props, as for a new instance); when
    that snapshot is structurally equal to the old one, the instance counts as
    kept and resumes there, with no entry. The old value is only compared, never
    used: a router, whose state comes from the URL, keeps its route. *)
