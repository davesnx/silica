(** SHA-1 hash (FIPS 180-4). *)

val digest : string -> string
(** [digest data] is the 20 raw bytes of the SHA-1 hash of [data]. *)
