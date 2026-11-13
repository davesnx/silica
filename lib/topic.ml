type 's t = 's Live_topic.t
type 's update = 's Live_topic.update = { seq : int; status : 's Status.t }

let declare = Live_topic.declare
let subscribe = Source.topic_subscribe
let name = Live_topic.name
let state = Live_topic.state
let state_id = Live_topic.state_id
let update_codec = Live_topic.update_codec
