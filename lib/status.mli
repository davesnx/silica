(** Where Remote or Live data is. A plain value that a machine keeps in its
    snapshot data. The old data stays when there is some. *)

type 'a t =
  | Idle  (** Nothing was asked for. *)
  | Loading of 'a option
      (** An answer is on its way. [Some old] while a reload runs. *)
  | Ready of { data : 'a; stale : bool }
      (** [stale] is always [false] for Remote data. For Live data it is [true]
          while the socket is down and the old data is kept. *)
  | Failed of { error : Remote.error; last : 'a option }
      (** The last attempt failed. [last] is the data from before. *)

val data : 'a t -> 'a option
(** The data of [Ready], the old data of [Loading], or the [last] of [Failed].
*)

val is_loading : 'a t -> bool
(** [true] for [Loading]. *)

val is_stale : 'a t -> bool
(** [true] for [Ready] with [stale = true]. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Change the data, old data included, and keep the status. *)
