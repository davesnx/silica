type event = Moved of int [@event_name "move"] [@event_name_of string_of_int]
[@@deriving event_shape]
