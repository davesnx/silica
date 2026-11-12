let () =
  let melange = Sys.argv.(1) in
  let files = List.sort compare (List.tl (List.tl (Array.to_list Sys.argv))) in
  let contents =
    List.map
      (fun file ->
        Filename.basename file ^ "\n"
        ^ In_channel.with_open_bin file In_channel.input_all
      )
      files
  in
  let digest =
    Digest.to_hex (Digest.string (String.concat "\n" (melange :: contents)))
  in
  Printf.printf "let digest = %S\n" digest
