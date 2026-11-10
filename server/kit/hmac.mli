(** HMAC (RFC 2104). *)

val sha256 : key:string -> string -> string
(** [sha256 ~key data] is the 32 raw bytes of HMAC-SHA-256. A key longer than
    the 64 byte block is hashed first. *)
