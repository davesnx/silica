(** A ready-made machine for one Remote endpoint. Its snapshot holds a
    {!Status.t}, so a view reads the status from plain data. A parent reads it
    through function props (see {!Component}). *)

type ('i, 'o) state = { input : 'i option; status : 'o Status.t }
(** [input] is the input of the last {!Load}; [Retry] uses it. *)

type ('i, 'o) event =
  | Load of 'i
      (** Start a call, or restart the one in flight. The old data stays in
          [Loading (Some old)]. A restarted call stops the old one, so a host
          never delivers an old answer. *)
  | Retry
      (** In [Failed]: call again with the same input. Unhandled elsewhere. *)
  | Settled of ('o, Remote.error) result
      (** The answer. Handled only while loading. *)
  | Reset  (** Back to [Idle], with no data. Stops a call in flight. *)

val make :
  ?timeout_ms:int ->
  id:string ->
  ('i, 'o) Remote.endpoint ->
  (('i, 'o) state, ('i, 'o) event) Machine.t
(** [Loading] owns a {!Source.call} on the endpoint, with [timeout_ms] when
    given. There is no automatic retry: a [Retrying] state with [after] and an
    attempt count is a pattern for the app. [Failed] keeps the input and the
    data from before. *)
