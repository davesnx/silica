type message =
  | Text of string
  | Binary of string
  | Close of { code : int option; reason : string }
  | Ping of string
  | Pong of string

type error = Protocol of string | Too_big

let accept_key client_key =
  Base64.encode
    (Sha1.digest (client_key ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))

let ( let* ) = Result.bind
let protocol msg = Error (Protocol msg)

let read_exact read n =
  if n = 0 then
    Ok ""
  else
    match read n with Some s -> Ok s | None -> protocol "truncated"

let message_of_frame opcode payload =
  match opcode with
  | 1 ->
      Ok (Text payload)
  | 2 ->
      Ok (Binary payload)
  | 8 ->
      if payload = "" then
        Ok (Close { code = None; reason = "" })
      else if String.length payload < 2 then
        protocol "close payload too short"
      else
        Ok
          (Close
             {
               code = Some (String.get_uint16_be payload 0);
               reason = String.sub payload 2 (String.length payload - 2);
             }
          )
  | 9 ->
      Ok (Ping payload)
  | _ ->
      Ok (Pong payload)

let unmask mask payload =
  String.mapi
    (fun i c -> Char.chr (Char.code c lxor Char.code mask.[i land 3]))
    payload

let read_message ~max ~read =
  match read 1 with
  | None ->
      Ok None
  | Some first ->
      let b0 = Char.code first.[0] in
      let fin = b0 land 0x80 <> 0 in
      let opcode = b0 land 0x0f in
      let* second = read_exact read 1 in
      let b1 = Char.code second.[0] in
      let masked = b1 land 0x80 <> 0 in
      let* () =
        if b0 land 0x70 <> 0 then
          protocol "reserved bits set"
        else
          Ok ()
      in
      let* () =
        match opcode with
        | 0 ->
            protocol "fragmented messages are not supported"
        | 1 | 2 | 8 | 9 | 10 ->
            Ok ()
        | n ->
            protocol (Printf.sprintf "unknown opcode %d" n)
      in
      let* () =
        if fin then
          Ok ()
        else
          protocol "fragmented messages are not supported"
      in
      let control = opcode >= 8 in
      let* length =
        match b1 land 0x7f with
        | 126 ->
            let* s = read_exact read 2 in
            Ok (String.get_uint16_be s 0)
        | 127 ->
            let* s = read_exact read 8 in
            let n = String.get_int64_be s 0 in
            if Int64.compare n 0L < 0 then
              protocol "invalid payload length"
            else if Int64.compare n (Int64.of_int max_int) > 0 then
              Ok max_int
            else
              Ok (Int64.to_int n)
        | n ->
            Ok n
      in
      let* () =
        if control && length > 125 then
          protocol "control frame too long"
        else
          Ok ()
      in
      let* () =
        if masked then
          Ok ()
        else
          protocol "client frame is not masked"
      in
      if length > max then
        Error Too_big
      else
        let* mask = read_exact read 4 in
        let* payload = read_exact read length in
        let* message = message_of_frame opcode (unmask mask payload) in
        Ok (Some message)

let opcode_and_payload = function
  | Text s ->
      (1, s)
  | Binary s ->
      (2, s)
  | Close { code; reason } ->
      let prefix =
        match code with
        | None ->
            ""
        | Some c ->
            let b = Bytes.create 2 in
            Bytes.set_uint16_be b 0 c;
            Bytes.to_string b
      in
      (8, prefix ^ reason)
  | Ping s ->
      (9, s)
  | Pong s ->
      (10, s)

let frame_header ~mask ~opcode len =
  let mask_bit =
    if mask = None then
      0
    else
      0x80
  in
  let header = Buffer.create 14 in
  Buffer.add_char header (Char.chr (0x80 lor opcode));
  if len <= 125 then
    Buffer.add_char header (Char.chr (mask_bit lor len))
  else if len <= 0xffff then begin
    Buffer.add_char header (Char.chr (mask_bit lor 126));
    Buffer.add_uint16_be header len
  end else begin
    Buffer.add_char header (Char.chr (mask_bit lor 127));
    Buffer.add_int64_be header (Int64.of_int len)
  end;
  Buffer.contents header

let frame ~mask message =
  let opcode, payload = opcode_and_payload message in
  let header = frame_header ~mask ~opcode (String.length payload) in
  match mask with
  | None ->
      header ^ payload
  | Some m ->
      header ^ m ^ unmask m payload

let encode message = frame ~mask:None message

let text_header text =
  frame_header ~mask:None
    ~opcode:(fst (opcode_and_payload (Text "")))
    (String.length text)

let encode_masked ~mask message =
  if String.length mask <> 4 then
    invalid_arg "Ws.encode_masked: mask must be 4 bytes";
  frame ~mask:(Some mask) message
