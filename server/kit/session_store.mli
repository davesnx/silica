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

val update : t -> now:float -> string -> (session -> session) -> session option
(** [update t ~now id f] replaces the session [id] with [f session] in one step:
    no other call of the store runs between the read and the write. [None], and
    no change, when the session is absent or expired (see {!val-get}). The [id]
    of the result is always [id]. *)

val remove : t -> string -> unit
(** Remove the session with this id. An unknown id does nothing. *)

val expire : t -> now:float -> int
(** Removes all expired sessions. Returns how many. *)

val count : t -> int
(** The number of sessions in the store, expired ones included. *)

val save : t -> string
(** Every session of the store as JSON text, expired ones included, for
    {!val-load} in another process. The text holds the session ids, which are as
    secret as the cookies that carry them. *)

val load : t -> now:float -> string -> (int, string) result
(** [load t ~now text] adds the sessions of a text from {!val-save} to the
    store, and gives how many it added. It trusts the text no more than a
    cookie: a session is skipped when it is expired, when its id is not 1 to 128
    lower case hexadecimal digits, when [last_seen] is before [created] or after
    [now], when a field is missing or of another type, or when the store already
    has its id. [Error] when the text is not a JSON array; the store does not
    change then. *)

type ops = {
  get : now:float -> string -> session option;
  set : session -> unit;
  update : now:float -> string -> (session -> session) -> session option;
  remove : string -> unit;
  expire : now:float -> int;
  save : unit -> string;
  load : now:float -> string -> (int, string) result;
}
(** The store as a record of functions, so the server can use another store.
    [save] and [load] keep the sessions across a restart in development (see
    [Silica_server.config.dev]); a store that outlives the process can give
    [save = fun () -> "[]"] and a [load] that returns [Ok 0]. *)

val ops : t -> ops
(** The store as {!type-ops}. *)
