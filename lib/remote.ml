type ('i, 'o, 'x) endpoint = ('i, 'o, 'x) Endpoint.t
type error = Endpoint.error =
  | Http of int
  | Decode of string
  | Network of string

type ('o, 'x) answer = ('o, 'x) Endpoint.answer =
  | Ok of 'o
  | Rejected of 'x
  | Failed of error

let declare = Endpoint.declare
let path = Endpoint.path
let input = Endpoint.input
let output = Endpoint.output
let rejected = Endpoint.rejected
let answer_id = Endpoint.answer_id
let call = Source.remote_call
let error_codec = Endpoint.error_codec
let answer_codec = Endpoint.answer_codec

let error_to_string = function
  | Http code ->
      "The server answered " ^ string_of_int code
  | Decode _ ->
      "The server sent data that cannot be read"
  | Network _ ->
      "The network failed"

type request = Endpoint.request =
  | Request : ('i, 'o, 'x) endpoint * 'i -> request

let request endpoint input = Request (endpoint, input)

let error_fingerprint : error Fingerprint.t =
  Fingerprint (Silica ("Remote.error", []))

let answer_fingerprint (Fingerprint.Fingerprint output : _ Fingerprint.t)
    (Fingerprint.Fingerprint rejected : _ Fingerprint.t) :
    (_, _) answer Fingerprint.t =
  Fingerprint (Silica ("Remote.answer", [ output; rejected ]))
