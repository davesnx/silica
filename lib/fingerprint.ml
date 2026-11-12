type part =
  | Declared of string * part list
  | Silica of string * part list
  | Unknown of string
  | Refused of string

type 'a t = Fingerprint of part [@@unboxed]

let part (Fingerprint part) = part
let declared digest parts = Fingerprint (Declared (digest, parts))
let unknown name = Fingerprint (Unknown name)
let refused reason = Fingerprint (Refused reason)

let rec text = function
  | Declared (digest, parts) ->
      Result.map (fun inner -> digest ^ inner) (texts parts)
  | Silica (name, parts) ->
      Result.map
        (fun inner -> "silica " ^ Silica_interfaces.digest ^ " " ^ name ^ inner)
        (texts parts)
  | Unknown name ->
      Error ("its state type mentions " ^ name ^ ", which has no fingerprint")
  | Refused reason ->
      Error reason

and texts parts =
  List.fold_left
    (fun result part ->
      Result.bind result (fun before ->
          Result.map (fun next -> before ^ "(" ^ next ^ ")") (text part)
      )
    )
    (Ok "") parts

let off = Fingerprint (Refused "fingerprints are off in this build")
let digest (Fingerprint part) = text part
