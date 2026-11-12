(** An optimistic view of data: the last confirmed value with the changes that
    the user made and the server has not yet confirmed. Pure: no host is
    involved. A machine keeps one in its snapshot data: the value is first-order
    (no functions), so the checker can compare and hash it.

    For a Remote mutation: {!OPTIMISTIC.apply} when the call starts,
    {!OPTIMISTIC.settle} when the answer arrives, {!OPTIMISTIC.drop} when it
    fails. For Live data: {!OPTIMISTIC.apply} when the change is sent, and
    {!OPTIMISTIC.settle} when a state with a higher [seq] arrives. *)

(** How a change goes on a value. *)
module type CHANGE = sig
  type value
  type change

  val apply : value -> change -> value
  (** [apply value change] is the value with the change on it. *)
end

type ('value, 'change) state
(** The confirmed value and the pending changes. *)

val fingerprint :
  'value Fingerprint.t ->
  'change Fingerprint.t ->
  ('value, 'change) state Fingerprint.t
(** The fingerprint of a {!state} from the ones of the value and the change, for
    a state type that holds one (see {!Fingerprint}). *)

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

  val pending : t -> int
  (** The number of pending changes. *)

  val is_pending : id:int -> t -> bool
  (** The change [id] is pending. *)
end

module Make (Change : CHANGE) :
  OPTIMISTIC with type value = Change.value and type change = Change.change
