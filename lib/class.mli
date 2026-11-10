(** Class names for [class_], with conditions.

    Write each class as a complete string literal, so that the Tailwind scan of
    the source files finds it. A name built at run time, such as
    ["bg-" ^ colour], is not found and gets no CSS. *)

val join : string list -> string
(** The non-empty strings, separated by one space. A condition is an empty
    string: [Class.join [ "row"; (if active then "bg-gray-100" else "") ]]. *)
