let read path = In_channel.with_open_bin path In_channel.input_all

let rec walk root relative =
  let directory = Filename.concat root relative in
  Sys.readdir directory |> Array.to_list |> List.sort String.compare
  |> List.concat_map (fun name ->
      let path =
        if relative = "" then
          name
        else
          relative ^ "/" ^ name
      in
      if Sys.is_directory (Filename.concat root path) then
        walk root path
      else
        [ path ]
  )

let () =
  let root = Sys.argv.(1) in
  print_string "[@@@ocamlformat \"disable\"]\n\nlet files = [\n";
  List.iter
    (fun path ->
      Printf.printf "  (%S, %S);\n" path (read (Filename.concat root path))
    )
    (walk root "");
  print_string "]\n"
