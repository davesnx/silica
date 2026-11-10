let block = 64

let sha256 ~key data =
  let key =
    if String.length key > block then
      Sha256.digest key
    else
      key
  in
  let key = key ^ String.make (block - String.length key) '\000' in
  let xor byte = String.map (fun c -> Char.chr (Char.code c lxor byte)) key in
  Sha256.digest (xor 0x5c ^ Sha256.digest (xor 0x36 ^ data))
