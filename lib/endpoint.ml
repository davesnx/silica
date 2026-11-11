type ('i, 'o, 'x) t = {
  path : string;
  input : 'i Codec.t;
  output : 'o Codec.t;
  rejected : 'x Codec.t;
  answer_id : ('o * 'x) Type.Id.t;
}

type error = Http of int | Decode of string | Network of string

type ('o, 'x) answer = Ok of 'o | Rejected of 'x | Failed of error

let declare ~path ~input ~output ~rejected =
  { path; input; output; rejected; answer_id = Type.Id.make () }

let path endpoint = endpoint.path
let input endpoint = endpoint.input
let output endpoint = endpoint.output
let rejected endpoint = endpoint.rejected
let answer_id endpoint = endpoint.answer_id

type request = Request : ('i, 'o, 'x) t * 'i -> request
