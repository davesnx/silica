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

let ( let* ) = Result.bind

let unknown name = Error ("unknown constructor " ^ name)

let error_codec : error Codec.t =
  {
    encode =
      (function
      | Http status ->
          Codec.constructor "Http" [ Codec.int.encode status ]
      | Decode reason ->
          Codec.constructor "Decode" [ Codec.string.encode reason ]
      | Network reason ->
          Codec.constructor "Network" [ Codec.string.encode reason ]
      );
    decode =
      (fun json ->
        let* name, arguments = Codec.constructor_of json in
        match (name, arguments) with
        | "Http", [ status ] ->
            Result.map (fun status -> Http status) (Codec.int.decode status)
        | "Decode", [ reason ] ->
            Result.map (fun reason -> Decode reason) (Codec.string.decode reason)
        | "Network", [ reason ] ->
            Result.map
              (fun reason -> Network reason)
              (Codec.string.decode reason)
        | name, _ ->
            unknown name
      );
  }

let answer_codec (output : 'o Codec.t) (rejected : 'x Codec.t) :
    ('o, 'x) answer Codec.t =
  Codec.make
    ~encode:(function
      | Ok value ->
          Codec.constructor "Ok" [ output.encode value ]
      | Rejected value ->
          Codec.constructor "Rejected" [ rejected.encode value ]
      | Failed error ->
          Codec.constructor "Failed" [ error_codec.encode error ]
      )
    ~decode:(fun json ->
      let* name, arguments = Codec.constructor_of json in
      match (name, arguments) with
      | "Ok", [ value ] ->
          Result.map (fun value -> Ok value) (output.decode value)
      | "Rejected", [ value ] ->
          Result.map (fun value -> Rejected value) (rejected.decode value)
      | "Failed", [ error ] ->
          Result.map (fun error -> Failed error) (error_codec.decode error)
      | name, _ ->
          unknown name
    )
