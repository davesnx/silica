let bytes ?(source = "/dev/urandom") n =
  let fail reason =
    Error (Printf.sprintf "Random_source: %s: %s" source reason)
  in
  match open_in_bin source with
  | exception Sys_error reason ->
      fail reason
  | ic ->
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          match In_channel.really_input_string ic n with
          | Some s ->
              Ok s
          | None ->
              fail (Printf.sprintf "could not read %d bytes" n)
          | exception Sys_error reason ->
              fail reason
        )

let hex_id ~bytes:n =
  Result.map
    (fun raw ->
      String.concat ""
        (List.init n (fun i -> Printf.sprintf "%02x" (Char.code raw.[i])))
    )
    (bytes n)
