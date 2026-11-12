type item = { a : int }
type state = Idle | Holding of item Boxes.box [@@deriving state_shape]
