type t =
  | Push of Url.t
  | Replace of Url.t
  | Traverse of { delta : int; url : Url.t }
let fingerprint : t Fingerprint.t = Fingerprint (Silica ("Nav.t", []))
