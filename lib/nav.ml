type t =
  | Push of Url.t
  | Replace of Url.t
  | Traverse of { delta : int; url : Url.t }
