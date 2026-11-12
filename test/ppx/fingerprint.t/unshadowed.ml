type item = { name : string; mutable seen : bool }
type count = int
type state = Idle | Saved of item * count list [@@deriving fingerprint]
