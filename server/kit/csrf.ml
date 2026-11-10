let token ~secret ~session =
  Base64.encode_url (Hmac.sha256 ~key:secret ("silica-csrf:" ^ session))

let valid ~secret ~session given =
  let expected = token ~secret ~session in
  let length_diff = String.length expected lxor String.length given in
  let diff = ref length_diff in
  String.iteri
    (fun i c ->
      let other =
        if i < String.length given then
          Char.code given.[i]
        else
          0
      in
      diff := !diff lor (Char.code c lxor other)
    )
    expected;
  !diff = 0
