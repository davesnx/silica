type _ state = Idle : unit state | Loaded : int -> int state
[@@deriving state_shape]
