(** A limit on how often a host delivers an input to a machine. The last value
    always wins: a value that is held back is replaced by a newer one. *)

type t =
  | Throttle of int
      (** The first value goes through at once. Values that arrive in the next
          [ms] milliseconds are held back, and only the last one is delivered
          when the window ends. *)
  | Debounce of int
      (** Nothing is delivered until [ms] milliseconds pass with no new value,
          then the last value is delivered. *)

type scheduler = int -> (unit -> unit) -> unit -> unit
(** [schedule ms callback] runs [callback] after [ms] milliseconds and returns a
    function that cancels it. A host gives its own timer. *)

val limiter : schedule:scheduler -> t -> ('a -> unit) -> 'a -> unit
(** [limiter ~schedule rate deliver] is a function that calls [deliver] under
    the rate. It keeps its own state: make one for each element and event. *)
