(** The fingerprint of a type: what a hot reload compares before it gives a
    snapshot of the old page to the new code (see {!Handoff}).

    Two equal fingerprints mean that the two builds compiled the same
    declarations, with the same compiler, for the type and every type it
    mentions. The deriver [[@@deriving fingerprint]] of [silica.ppx] makes them:
    on a state type it gives [state_fingerprint], which the machine passes to
    {!Machine.make} as [~fingerprint:state_fingerprint]; on any other type it
    gives one so that a state type in another file can mention it. A type with
    no fingerprint can not be handed over: a machine whose state mentions it
    starts again at a hot reload, and the reason names the type. A machine with
    no [~fingerprint] starts again at each hot reload, unless its initial
    snapshot equals the old one (see {!Handoff.hook}).

    A fingerprint written by hand, such as the one of a type made by a functor,
    goes in [[%fingerprint ...]]: the expression in a development build, {!off}
    in a release build, so a release bundle computes nothing.

    The deriver gives fingerprints only in a development build: the release
    profile of a project sets [SILICA_FINGERPRINTS=false] for it (see the root
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
    are off: it gives no digest. A machine can then pass
    [~fingerprint:state_fingerprint] in every build. *)

val digest : 'a t -> (string, string) result
(** The text that two builds compare, or the reason there is none, as in
    ["its state type mentions Issue.t, which has no fingerprint"]. *)
