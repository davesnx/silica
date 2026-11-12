open Jsonkit.Primitives

module Item = struct
  type t = { id : int; title : string; price_cents : int; tags : string list }
  [@@deriving json, fingerprint]
end

module Counter_event = struct
  type t = Incr | Decr | Reset [@@deriving json]

  let name = function Incr -> "incr" | Decr -> "decr" | Reset -> "reset"
end

let item_endpoint =
  Silica.Remote.declare ~path:"/api/item" ~input:Silica.Codec.int
    ~output:(Silica_jsonkit.codec ~to_json:Item.to_json ~of_json:Item.of_json)
    ~rejected:Silica.Codec.string

let counter_state = Silica.Codec.int

let counter_event =
  Silica_jsonkit.codec ~to_json:Counter_event.to_json
    ~of_json:Counter_event.of_json

let votes_topic = Silica.Topic.declare ~name:"votes" ~state:Silica.Codec.int

let vote_endpoint =
  Silica.Remote.declare ~path:"/api/vote" ~input:Silica.Codec.unit
    ~output:Silica.Codec.int ~rejected:Silica.Codec.string
