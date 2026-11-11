(** A ready-made machine for one Remote endpoint. Its states hold the input of
    the last {!Load}, so [Retry] can use it, and the data from before. A parent
    reads the state through function props (see {!Component}). *)

type ('i, 'o, 'x) state =
  | Idle  (** Nothing was asked for. *)
  | Loading of { input : 'i; last : 'o option }
      (** The call runs. [last] is the data from before. *)
  | Ready of { input : 'i; data : 'o }
  | Rejected of { input : 'i; reason : 'x; last : 'o option }
      (** The handler answered with an application error. *)
  | Failed of { input : 'i; error : Remote.error; last : 'o option }

type ('i, 'o, 'x) event =
  | Load of 'i
      (** Start a call, or restart the one in flight. A restarted call stops the
          old one, so a host never delivers an old answer. *)
  | Retry
      (** In [Rejected] and [Failed]: call again with the same input. Ignored
          elsewhere. *)
  | Settled of ('o, 'x) Remote.answer
      (** The answer. Handled only while loading. *)
  | Reset  (** Back to [Idle], with no data. Stops a call in flight. *)

val data : ('i, 'o, 'x) state -> 'o option
(** The data of [Ready], or the [last] data of the other states. *)

val state_codec : ('i, 'o, 'x) Remote.endpoint -> ('i, 'o, 'x) state Codec.t
(** The states as [[@@deriving json]] writes them, with the codecs of the
    endpoint, for {!Machine.register_codecs}. *)

val event_codec : ('i, 'o, 'x) Remote.endpoint -> ('i, 'o, 'x) event Codec.t
(** The events, with the codecs of the endpoint. *)

val make :
  ?timeout_ms:int ->
  id:string ->
  ('i, 'o, 'x) Remote.endpoint ->
  (unit, ('i, 'o, 'x) state, ('i, 'o, 'x) event) Machine.t
(** [Loading] owns a {!Remote.call} on the endpoint, with [timeout_ms] when
    given. The events are named ["load"], ["retry"], ["settled ok"],
    ["settled rejected"], ["settled error"] and ["reset"]. There is no automatic
    retry: a [Retrying] state that owns a {!Source.after} timer and counts the
    attempts is a pattern for the app. *)
