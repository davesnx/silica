let read path = In_channel.with_open_bin path In_channel.input_all

let rec lit s =
  if String.for_all (fun c -> Char.code c < 128) s then
    Printf.sprintf "%S" s
  else
    match Str.bounded_split_delim (Str.regexp_string "|js}") s 2 with
    | [ before; after ] ->
        Printf.sprintf "(%s ^ \"|js}\" ^ %s)" (lit before) (lit after)
    | _ ->
        "{js|" ^ s ^ "|js}"

let () =
  print_string "[@@@ocamlformat \"disable\"]\n\n";
  Array.iteri
    (fun i path ->
      if i > 0 then
        Printf.printf "let %s = %s\n"
          (Filename.remove_extension (Filename.basename path))
          (lit (read path))
    )
    Sys.argv
