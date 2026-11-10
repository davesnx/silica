let mask32 = 0xffffffff
let rotl x n = (x lsl n) lor (x lsr (32 - n)) land mask32

let pad data =
  let len = String.length data in
  let zeros = (55 - len) land 63 in
  let tail = Bytes.make (1 + zeros + 8) '\000' in
  Bytes.set tail 0 '\x80';
  Bytes.set_int64_be tail (1 + zeros) (Int64.of_int (len * 8));
  data ^ Bytes.to_string tail

let digest data =
  let msg = pad data in
  let h = [| 0x67452301; 0xefcdab89; 0x98badcfe; 0x10325476; 0xc3d2e1f0 |] in
  let w = Array.make 80 0 in
  for block = 0 to (String.length msg / 64) - 1 do
    for t = 0 to 15 do
      w.(t) <-
        Int32.to_int (String.get_int32_be msg ((block * 64) + (t * 4)))
        land mask32
    done;
    for t = 16 to 79 do
      w.(t) <- rotl (w.(t - 3) lxor w.(t - 8) lxor w.(t - 14) lxor w.(t - 16)) 1
    done;
    let a = ref h.(0) and b = ref h.(1) and c = ref h.(2) in
    let d = ref h.(3) and e = ref h.(4) in
    for t = 0 to 79 do
      let f, k =
        if t < 20 then
          (!b land !c lor (lnot !b land mask32 land !d), 0x5a827999)
        else if t < 40 then
          (!b lxor !c lxor !d, 0x6ed9eba1)
        else if t < 60 then
          (!b land !c lor (!b land !d) lor (!c land !d), 0x8f1bbcdc)
        else
          (!b lxor !c lxor !d, 0xca62c1d6)
      in
      let temp = (rotl !a 5 + f + !e + k + w.(t)) land mask32 in
      e := !d;
      d := !c;
      c := rotl !b 30;
      b := !a;
      a := temp
    done;
    h.(0) <- (h.(0) + !a) land mask32;
    h.(1) <- (h.(1) + !b) land mask32;
    h.(2) <- (h.(2) + !c) land mask32;
    h.(3) <- (h.(3) + !d) land mask32;
    h.(4) <- (h.(4) + !e) land mask32
  done;
  let out = Bytes.create 20 in
  Array.iteri (fun i v -> Bytes.set_int32_be out (i * 4) (Int32.of_int v)) h;
  Bytes.to_string out
