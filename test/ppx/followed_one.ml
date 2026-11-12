type item = { name : string; tags : string list }
type state = Idle | Saved of item [@@deriving state_shape]
