type item = { name : string }
type state = Idle | Saved of item [@@deriving fingerprint]
type 'a box = { content : 'a; items : 'a Other.list } [@@deriving fingerprint]
