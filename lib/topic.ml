type 's t = { name : string; state : 's Codec.t; state_id : 's Type.Id.t }

let declare ~name ~state = { name; state; state_id = Type.Id.make () }
let name t = t.name
let state t = t.state
let state_id t = t.state_id

type 's update = { seq : int; status : 's Status.t }
