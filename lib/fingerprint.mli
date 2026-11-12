(** The fingerprint of a type: what a hot reload compares before it gives a
    snapshot of the old page to the new code (see {!Handoff}).

    Two equal fingerprints mean that the two builds compiled the same
    declarations, with the same compiler, for the type and every type it
    mentions. The derivers of [silica.ppx] make them: [[@@deriving state_shape]]
    gives [state_fingerprint] next to [state_shape] and puts it in the shape;
    [[@@deriving fingerprint]] gives one to any other type, so that a state type
    in another file can mention it. A type with no fingerprint can not be handed
    over: a machine whose state mentions it starts again at a hot reload, and
    the reason names the type.

    A state type that [state_shape] does not take, such as a record, gets its
    fingerprint from [[@@deriving fingerprint]], and the shape written by hand
    passes it:
    [Machine.shape ~name:(fun _ -> "todo") ~fingerprint:state_fingerprint ()]. A
    shape written by hand with no [~fingerprint] starts again at each hot
    reload, unless its initial snapshot equals the old one (see
    {!Handoff.hook}).

    A fingerprint written by hand, such as the one of a type made by a functor,
    goes in [[%fingerprint ...]]: the expression in a development build, {!off}
    in a release build, so a release bundle computes nothing.

    The derivers give fingerprints only in a development build: the release
    profile of a project sets [SILICA_FINGERPRINTS=false] for them (see the root
    [dune] file), so a release bundle holds none; there
    [[@@deriving fingerprint]] gives {!off}. *)

type part =
  | Declared of string * part list
      (** The digest of the printed declarations that a deriver saw, with the
          fingerprints of the types from other modules that they mention. *)
  | Silica of string * part list
      (** A type of Silica, named as in ["Remote.error"], with the fingerprints
          of its parameters. It stands for the interfaces of this build of
          Silica and the Melange version. *)
  | Unknown of string
      (** A type that has no fingerprint, named as it is written. *)
  | Refused of string  (** A type that can not be handed over, with why. *)

(** The fingerprint of the type ['a]. The type parameter lets the compiler check
    that a fingerprint found by name is the one of the type with that name. *)
type 'a t = Fingerprint of part [@@unboxed]

val part : 'a t -> part
(** The fingerprint with no type, as a part of another one. *)

val declared : string -> part list -> 'a t
(** [declared digest parts], for the derivers. *)

val unknown : string -> 'a t
(** The fingerprint of a type that has none, named as it is written. *)

val refused : string -> 'a t
(** The fingerprint of a type that can not be handed over, such as a function.
    The text says why, as in ["its state type holds a function"]. *)

val off : 'a t
(** What [[@@deriving fingerprint]] gives in a release build, where fingerprints
    are off: it gives no digest. A shape written by hand can then pass
    [~fingerprint:state_fingerprint] in every build. *)

val digest : 'a t -> (string, string) result
(** The text that two builds compare, or the reason there is none, as in
    ["its state type mentions Issue.t, which has no fingerprint"]. *)
