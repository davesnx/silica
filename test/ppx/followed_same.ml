(** The same declarations as in [Followed_one], written another way. *)

type item = { name : string;  (** The name. *) tags : string list }

(** The state. *)
type state = Idle | Saved of item [@@deriving fingerprint]
