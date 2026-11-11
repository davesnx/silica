type 's t = { name : string; state : 's Codec.t; state_id : 's Type.Id.t }

let declare ~name ~state = { name; state; state_id = Type.Id.make () }
let name t = t.name
let state t = t.state
let state_id t = t.state_id

type 's update = { seq : int; status : 's Status.t }

let ( let* ) = Result.bind

let update_codec (state : 's Codec.t) : 's update Codec.t =
  let status = Status.codec state in
  Codec.make
    ~encode:(fun { seq; status = value } ->
      Silica_json.Json.declassify
        (`Assoc
           [ ("seq", Codec.int.encode seq); ("status", status.encode value) ]
        )
    )
    ~decode:(fun json ->
      let* seq = Codec.field "seq" Codec.int json in
      let* value = Codec.field "status" status json in
      Ok { seq; status = value }
    )
