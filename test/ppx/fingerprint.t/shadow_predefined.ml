open Other

type state = Idle | Saved of int list [@@deriving fingerprint]
