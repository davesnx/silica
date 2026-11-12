type item = { name : string; tags : int list }
type state = Idle | Saved of item [@@deriving fingerprint]
