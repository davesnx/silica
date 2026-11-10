let bytes n =
  let path = "/dev/urandom" in
  let fail reason =
    failwith (Printf.sprintf "Random_source: %s: %s" path reason)
  in
  match open_in_bin path with
  | exception Sys_error reason ->
      fail reason
  | ic ->
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          match In_channel.really_input_string ic n with
          | Some s ->
              s
          | None ->
              fail (Printf.sprintf "could not read %d bytes" n)
          | exception Sys_error reason ->
              fail reason
        )

let hex_id ~bytes:n =
  let raw = bytes n in
  String.concat ""
    (List.init n (fun i -> Printf.sprintf "%02x" (Char.code raw.[i])))
