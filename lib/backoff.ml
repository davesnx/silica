let base_ms = 100
let cap_ms = 5000

let delay_ms ~attempt ~jitter =
  let attempt = max 0 attempt in
  let doubled =
    if attempt >= 6 then
      cap_ms
    else
      min cap_ms (base_ms lsl attempt)
  in
  doubled + int_of_float (float_of_int doubled *. 0.25 *. jitter)
