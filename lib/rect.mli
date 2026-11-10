(** The size and position of an element, as a portable value. *)

type t = { x : float; y : float; width : float; height : float }

val zero : t
(** All four values are [0.]. *)
