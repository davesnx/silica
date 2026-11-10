(** Random bytes from the operating system. *)

val bytes : int -> string
(** [bytes n] reads [n] bytes from /dev/urandom. Raises [Failure] if it cannot
    read all bytes. There is no fallback. *)

val hex_id : bytes:int -> string
(** [hex_id ~bytes] is [bytes] random bytes as lowercase hex text. *)
