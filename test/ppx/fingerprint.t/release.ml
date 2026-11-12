type item = { name : string }
type state = Idle | Saved of item [@@deriving state_shape]
type 'a box = { content : 'a; items : 'a Other.list } [@@deriving fingerprint]
