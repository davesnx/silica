type event = Saved of { at : int } [@event_name_of fun _ -> "saved"]
[@@deriving event_shape]
