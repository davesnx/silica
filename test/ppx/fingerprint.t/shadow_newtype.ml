type item = { name : string }

let wrap (type item) (value : item) =
  let module Local = struct
    type state = Idle | Saved of item [@@deriving fingerprint]
  end in
  Local.Saved value
