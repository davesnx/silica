let mask32 = 0xffffffff
let rotr x n = (x lsr n) lor (x lsl (32 - n)) land mask32

let k =
  [|
    0x428a2f98;
    0x71374491;
    0xb5c0fbcf;
    0xe9b5dba5;
    0x3956c25b;
    0x59f111f1;
    0x923f82a4;
    0xab1c5ed5;
    0xd807aa98;
    0x12835b01;
    0x243185be;
    0x550c7dc3;
    0x72be5d74;
    0x80deb1fe;
    0x9bdc06a7;
    0xc19bf174;
    0xe49b69c1;
    0xefbe4786;
    0x0fc19dc6;
    0x240ca1cc;
    0x2de92c6f;
    0x4a7484aa;
    0x5cb0a9dc;
    0x76f988da;
    0x983e5152;
    0xa831c66d;
    0xb00327c8;
    0xbf597fc7;
    0xc6e00bf3;
    0xd5a79147;
    0x06ca6351;
    0x14292967;
    0x27b70a85;
    0x2e1b2138;
    0x4d2c6dfc;
    0x53380d13;
    0x650a7354;
    0x766a0abb;
    0x81c2c92e;
    0x92722c85;
    0xa2bfe8a1;
    0xa81a664b;
    0xc24b8b70;
    0xc76c51a3;
    0xd192e819;
    0xd6990624;
    0xf40e3585;
    0x106aa070;
    0x19a4c116;
    0x1e376c08;
    0x2748774c;
    0x34b0bcb5;
    0x391c0cb3;
    0x4ed8aa4a;
    0x5b9cca4f;
    0x682e6ff3;
    0x748f82ee;
    0x78a5636f;
    0x84c87814;
    0x8cc70208;
    0x90befffa;
    0xa4506ceb;
    0xbef9a3f7;
    0xc67178f2;
  |]

let pad data =
  let len = String.length data in
  let zeros = (55 - len) land 63 in
  let tail = Bytes.make (1 + zeros + 8) '\000' in
  Bytes.set tail 0 '\x80';
  Bytes.set_int64_be tail (1 + zeros) (Int64.of_int (len * 8));
  data ^ Bytes.to_string tail

let digest data =
  let msg = pad data in
  let h =
    [|
      0x6a09e667;
      0xbb67ae85;
      0x3c6ef372;
      0xa54ff53a;
      0x510e527f;
      0x9b05688c;
      0x1f83d9ab;
      0x5be0cd19;
    |]
  in
  let w = Array.make 64 0 in
  for block = 0 to (String.length msg / 64) - 1 do
    for t = 0 to 15 do
      w.(t) <-
        Int32.to_int (String.get_int32_be msg ((block * 64) + (t * 4)))
        land mask32
    done;
    for t = 16 to 63 do
      let s0 =
        rotr w.(t - 15) 7 lxor rotr w.(t - 15) 18 lxor (w.(t - 15) lsr 3)
      in
      let s1 =
        rotr w.(t - 2) 17 lxor rotr w.(t - 2) 19 lxor (w.(t - 2) lsr 10)
      in
      w.(t) <- (w.(t - 16) + s0 + w.(t - 7) + s1) land mask32
    done;
    let v = Array.copy h in
    for t = 0 to 63 do
      let a = v.(0) and e = v.(4) in
      let s1 = rotr e 6 lxor rotr e 11 lxor rotr e 25 in
      let ch = e land v.(5) lxor (lnot e land mask32 land v.(6)) in
      let t1 = (v.(7) + s1 + ch + k.(t) + w.(t)) land mask32 in
      let s0 = rotr a 2 lxor rotr a 13 lxor rotr a 22 in
      let maj = a land v.(1) lxor (a land v.(2)) lxor (v.(1) land v.(2)) in
      let t2 = (s0 + maj) land mask32 in
      v.(7) <- v.(6);
      v.(6) <- v.(5);
      v.(5) <- e;
      v.(4) <- (v.(3) + t1) land mask32;
      v.(3) <- v.(2);
      v.(2) <- v.(1);
      v.(1) <- a;
      v.(0) <- (t1 + t2) land mask32
    done;
    Array.iteri (fun i x -> h.(i) <- (h.(i) + x) land mask32) v
  done;
  let out = Bytes.create 32 in
  Array.iteri (fun i x -> Bytes.set_int32_be out (i * 4) (Int32.of_int x)) h;
  Bytes.to_string out

let hex data =
  let d = digest data in
  String.concat ""
    (List.init (String.length d) (fun i ->
         Printf.sprintf "%02x" (Char.code d.[i])
     )
    )
