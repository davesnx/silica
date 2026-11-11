let () =
  print_string "let all = [\n";
  Array.iteri
    (fun position path ->
      if position > 0 then
        Printf.printf "  (%S, %S);\n"
          (Filename.basename (Filename.dirname (Filename.dirname path))
          ^ "/"
          ^ Filename.remove_extension (Filename.basename path)
          )
          (In_channel.with_open_text path In_channel.input_all)
    )
    Sys.argv;
  print_string "]\n"
