type event = Retry [@event_name_of fun _ -> "retry"] [@@deriving event_shape]
