type item = { name : string }

open Other

type state = Idle | Saved of item [@@deriving fingerprint]
