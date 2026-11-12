type item = { a : string }
type state = Idle | Holding of item Boxes.box [@@deriving fingerprint]
