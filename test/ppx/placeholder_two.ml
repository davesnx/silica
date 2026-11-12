type item = { a : string }
type state = Idle | Holding of item Boxes.box [@@deriving state_shape]
