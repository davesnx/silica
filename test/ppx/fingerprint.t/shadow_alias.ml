type count = int

open Other

type state = Idle | Counted of count [@@deriving fingerprint]
