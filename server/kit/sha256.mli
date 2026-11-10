(** SHA-256 hash (FIPS 180-4). *)

val digest : string -> string
(** [digest data] is the 32 raw bytes of the SHA-256 hash of [data]. *)

val hex : string -> string
(** [hex data] is the lowercase hex text of [digest data]. *)
