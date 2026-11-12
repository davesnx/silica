module Json = Silica_json.Json

let rec to_silica (j : Jsonkit.t) : Json.t =
  match Jsonkit.classify j with
  | (`Null | `Bool _ | `Int _ | `Float _ | `String _) as leaf ->
      Json.declassify leaf
  | `List l ->
      Json.declassify (`List (List.map to_silica l))
  | `Assoc l ->
      Json.declassify (`Assoc (List.map (fun (k, v) -> (k, to_silica v)) l))

let rec of_silica (j : Json.t) : Jsonkit.t =
  match Json.classify j with
  | (`Null | `Bool _ | `Int _ | `Float _ | `String _) as leaf ->
      Jsonkit.declassify leaf
  | `List l ->
      Jsonkit.declassify (`List (List.map of_silica l))
  | `Assoc l ->
      Jsonkit.declassify (`Assoc (List.map (fun (k, v) -> (k, of_silica v)) l))

let codec ~to_json ~of_json : _ Silica.Codec.t =
  Silica.Codec.make
    ~encode:(fun x -> to_silica (to_json x))
    ~decode:(fun j ->
      match of_json (of_silica j) with
      | x ->
          Ok x
      | exception (Jsonkit.Of_json_error _ as e) ->
          Error (Printexc.to_string e)
    )

let to_json (codec : _ Silica.Codec.t) value = of_silica (codec.encode value)

let of_json (codec : _ Silica.Codec.t) json =
  match codec.decode (to_silica json) with
  | Ok value ->
      value
  | Error message ->
      Jsonkit.of_json_msg_error message

let writer to_json =
  Silica.Codec.make
    ~encode:(fun value -> to_silica (to_json value))
    ~decode:(fun _ -> Error "this codec only writes")

let reader of_json =
  Silica.Codec.make
    ~encode:(fun _ -> invalid_arg "this codec only reads")
    ~decode:(fun json ->
      match of_json (of_silica json) with
      | value ->
          Ok value
      | exception (Jsonkit.Of_json_error _ as error) ->
          Error (Printexc.to_string error)
    )

type error = Silica.Remote.error

let error_to_json error = to_json Silica.Remote.error_codec error
let error_of_json json = of_json Silica.Remote.error_codec json

type ('o, 'x) answer = ('o, 'x) Silica.Remote.answer

let answer_to_json output rejected =
  to_json (Silica.Remote.answer_codec (writer output) (writer rejected))

let answer_of_json output rejected =
  of_json (Silica.Remote.answer_codec (reader output) (reader rejected))

type 'a status = 'a Silica.Status.t

let status_to_json data = to_json (Silica.Status.codec (writer data))
let status_of_json data = of_json (Silica.Status.codec (reader data))

type 's update = 's Silica.Topic.update

let update_to_json state = to_json (Silica.Topic.update_codec (writer state))
let update_of_json state = of_json (Silica.Topic.update_codec (reader state))

type ('value, 'change) optimistic = ('value, 'change) Silica.Optimistic.state

let optimistic_to_json value change =
  to_json (Silica.Optimistic.codec (writer value) (writer change))

let optimistic_of_json value change =
  of_json (Silica.Optimistic.codec (reader value) (reader change))
