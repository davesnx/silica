(** Delays between reconnect attempts of a Live socket. *)

val delay_ms : attempt:int -> jitter:float -> int
(** The wait before attempt number [attempt] (0 is the first retry). The base
    delay is 100 ms, doubled for each attempt, capped at 5000 ms. [jitter] is a
    value from 0 to 1 that adds up to 25 percent. The result never exceeds 5000
    ms plus that 25 percent, and does not fall under the base delay. *)

val cap_ms : int
(** The cap of the base delay, [5000] ms. *)
