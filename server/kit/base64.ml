let encode_with alphabet ~pad data =
  let len = String.length data in
  let buf = Buffer.create ((len + 2) / 3 * 4) in
  let byte i =
    if i < len then
      Char.code data.[i]
    else
      0
  in
  let i = ref 0 in
  while !i < len do
    let n = (byte !i lsl 16) lor (byte (!i + 1) lsl 8) lor byte (!i + 2) in
    let chars = min 4 (len - !i + 1) in
    for j = 0 to 3 do
      if j < chars then
        Buffer.add_char buf alphabet.[(n lsr (18 - (6 * j))) land 63]
      else if pad then
        Buffer.add_char buf '='
    done;
    i := !i + 3
  done;
  Buffer.contents buf

let encode =
  encode_with "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    ~pad:true

let encode_url =
  encode_with "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
    ~pad:false
