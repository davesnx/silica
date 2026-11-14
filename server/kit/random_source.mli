(** Random bytes from the operating system. *)

val bytes : ?source:string -> int -> (string, string) result
(** [bytes n] reads [n] bytes from [source] (default [/dev/urandom]). An [Error]
    names the source and the reason when it cannot read all bytes, for example
    when the process has no free file descriptor. There is no fallback. *)

val hex_id : bytes:int -> (string, string) result
(** [hex_id ~bytes] is [bytes] random bytes as lowercase hex text, or the
    [Error] of {!val-bytes}. *)
