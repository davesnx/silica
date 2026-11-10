(** Sessions in memory. Safe to share between threads. Times are in seconds. *)

type session = {
  id : string;
  created : float;
  last_seen : float;
  data : (string * string) list;
}

type t

val create : ?max_sessions:int -> ?idle:float -> ?absolute:float -> unit -> t
(** Defaults: 10_000 sessions, 8 hours idle, 7 days absolute. *)

val get : t -> now:float -> string -> session option
(** [None] when the session is absent or expired. A session is expired when
    [now -. last_seen > idle] or [now -. created > absolute]. An expired session
    is removed. *)

val set : t -> session -> unit
(** When the store is full and the id is new, the session with the oldest
    [last_seen] is removed first. *)

val remove : t -> string -> unit
(** Remove the session with this id. An unknown id does nothing. *)

val expire : t -> now:float -> int
(** Removes all expired sessions. Returns how many. *)

val count : t -> int
(** The number of sessions in the store, expired ones included. *)

type ops = {
  get : now:float -> string -> session option;
  set : session -> unit;
  remove : string -> unit;
  expire : now:float -> int;
}
(** The store as a record of functions, so the server can use another store. *)

val ops : t -> ops
(** The store as {!type-ops}. *)
