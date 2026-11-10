(** A pointer event as a portable value. Positions are in CSS pixels, relative
    to the viewport. *)

type t = { x : float; y : float; button : int }

val origin : t
(** A press with the main button at [(0, 0)]. *)
