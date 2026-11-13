(** An optimistic view of data: the last confirmed value with the changes that
    the user made and the server has not yet confirmed. Pure: no host is
    involved. A machine keeps one in its snapshot data: the value is first-order
    (no functions), so the checker can compare and hash it.

    For a Remote mutation: {!OPTIMISTIC.apply} when the call starts,
    {!OPTIMISTIC.settle} when the answer arrives, {!OPTIMISTIC.drop} when it
    fails. For Live data that the changes go to by Remote calls: the same, and
    also {!OPTIMISTIC.rebase} for each state of the topic. A state that already
    holds a change can arrive before the answer to its call, so the change is
    then in the confirmed value and still pending: {!CHANGE.apply} must be
    idempotent (see {!CHANGE.apply}). *)

(** How a change goes on a value. *)
module type CHANGE = sig
  type value
  type change

  val apply : value -> change -> value
  (** [apply value change] is the value with the change on it. With Live data it
      must be idempotent: [apply (apply value change) change] equals
      [apply value change], so a change that a Live state already holds does not
      apply twice. An add checks that the item is not there yet; a set of a
      field is idempotent already. *)
end

type ('value, 'change) state
(** The confirmed value and the pending changes. *)

val codec : 'value Codec.t -> 'change Codec.t -> ('value, 'change) state Codec.t
(** [{"confirmed": value, "pending": [[id, change], ...]}], for the state codec
    of a machine that holds one (see {!Machine.register_codecs}). *)

module type OPTIMISTIC = sig
  type value
  type change
  type t = (value, change) state

  val make : value -> t
  (** [make confirmed]: no pending change. *)

  val apply : id:int -> change -> t -> t
  (** Add a pending change. An [id] that is already pending replaces its change.
  *)

  val settle : ?confirmed:value -> id:int -> t -> t
  (** The server confirmed the change [id]: it is no longer pending. With
      [confirmed], that value becomes the confirmed value, and the changes that
      are still pending go on top of it. Without it, the change is folded into
      the confirmed value. An unknown [id] changes nothing. *)

  val drop : id:int -> t -> t
  (** The change failed or was cancelled: it is removed and the value goes back.
      An unknown [id] changes nothing. *)

  val rebase : value -> t -> t
  (** A new confirmed value, for example a Live state. Pending changes stay. *)

  val value : t -> value
  (** The confirmed value with the pending changes applied, oldest first. *)

  val confirmed : t -> value
  (** The confirmed value, with no pending change applied. *)

  val pending : t -> (int * change) list
  (** The pending changes with their ids, oldest first, such as for a [during]
      that makes one Remote call for each. *)

  val is_pending : id:int -> t -> bool
  (** The change [id] is pending. *)
end

module Make (Change : CHANGE) :
  OPTIMISTIC with type value = Change.value and type change = Change.change
