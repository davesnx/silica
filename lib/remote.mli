(** Typed HTTP endpoints declared in shared code, and calls to them. *)

type ('i, 'o, 'x) endpoint = ('i, 'o, 'x) Endpoint.t
(** An endpoint with input ['i], output ['o] and application error ['x]. *)

type error = Endpoint.error =
  | Http of int  (** The server answered with this status. *)
  | Decode of string  (** The answer did not decode. *)
  | Network of string
      (** No answer: the network failed or the call timed out. *)

type ('o, 'x) answer = ('o, 'x) Endpoint.answer =
  | Ok of 'o  (** The output. *)
  | Rejected of 'x
      (** The handler answered [Error x]: an application error, such as a failed
          validation. The server answers it with status 422, not 500. *)
  | Failed of error  (** The call did not give an output or an error. *)

val declare :
  path:string ->
  input:'i Codec.t ->
  output:'o Codec.t ->
  rejected:'x Codec.t ->
  ('i, 'o, 'x) endpoint
(** Declare an endpoint in shared code. The client sends the input to [path]
    with POST. The server answers with the output, or with an application error
    encoded by [rejected]. *)

val path : ('i, 'o, 'x) endpoint -> string
(** The URL path of the endpoint. *)

val input : ('i, 'o, 'x) endpoint -> 'i Codec.t
(** The codec of the input. *)

val output : ('i, 'o, 'x) endpoint -> 'o Codec.t
(** The codec of the output. *)

val rejected : ('i, 'o, 'x) endpoint -> 'x Codec.t
(** The codec of the application error. *)

val answer_id : ('i, 'o, 'x) endpoint -> ('o * 'x) Type.Id.t
(** A type id made by [declare], one for each endpoint. The checker uses it to
    find the sample answers of an endpoint. *)

val call :
  ?timeout_ms:int ->
  ('i, 'o, 'x) endpoint ->
  'i ->
  (('o, 'x) answer -> 'e) ->
  'e Source.t
(** One request to the endpoint with this input, as a source that a state owns
    (see {!Machine.make}). The answer is raised as an event unless the call was
    stopped first. The key of the call is its path and its encoded input: a step
    that keeps the state but changes the input stops the old call and starts a
    new one. There is no timeout unless [timeout_ms] is given. When it passes
    before the answer, the call ends with [Failed (Network "timeout")], and a
    later answer is dropped. *)

val error_to_string : error -> string
(** A short English sentence for the error, for a view:
    ["The server answered 500"], ["The server sent data that cannot be read"] or
    ["The network failed"]. *)

type request = Endpoint.request =
  | Request : ('i, 'o, 'x) endpoint * 'i -> request
      (** One call of an endpoint with its input, as {!Action.Preload} carries
          it. *)

val request : ('i, 'o, 'x) endpoint -> 'i -> request
(** [request endpoint input] is [Request (endpoint, input)]. *)
