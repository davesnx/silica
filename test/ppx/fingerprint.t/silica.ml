module Fingerprint = struct
  type part =
    | Declared of string * part list
    | Unknown of string
    | Refused of string

  type 'a t = Fingerprint of part [@@unboxed]

  let part (Fingerprint part) = part
  let declared digest parts = Fingerprint (Declared (digest, parts))
  let unknown name = Fingerprint (Unknown name)
  let refused reason = Fingerprint (Refused reason)
end
