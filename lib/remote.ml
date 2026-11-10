type ('i, 'o) endpoint = {
  path : string;
  input : 'i Codec.t;
  output : 'o Codec.t;
  output_id : 'o Type.Id.t;
}

type error = Http of int | Decode of string | Network of string

let declare ~path ~input ~output =
  { path; input; output; output_id = Type.Id.make () }
let path e = e.path
let input e = e.input
let output e = e.output
let output_id e = e.output_id

type request = Request : ('i, 'o) endpoint * 'i -> request

let request endpoint input = Request (endpoint, input)
