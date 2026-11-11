let read path = In_channel.with_open_bin path In_channel.input_all

let valid_name name =
  name <> ""
  && (match name.[0] with 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false)
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true | _ -> false
         )
       name

let project_name directory =
  if Sys.file_exists directory then
    Filename.basename (Unix.realpath directory)
  else
    Filename.basename directory

let replace_all ~pattern ~by text =
  let pattern_length = String.length pattern in
  let buffer = Buffer.create (String.length text) in
  let rec scan index =
    if index > String.length text - pattern_length then
      Buffer.add_substring buffer text index (String.length text - index)
    else if String.sub text index pattern_length = pattern then (
      Buffer.add_string buffer by;
      scan (index + pattern_length)
    ) else (
      Buffer.add_char buffer text.[index];
      scan (index + 1)
    )
  in
  scan 0;
  Buffer.contents buffer

let rec make_directories directory =
  if not (Sys.file_exists directory) then (
    make_directories (Filename.dirname directory);
    Sys.mkdir directory 0o755
  )

type file_status = Missing | Same | Different

let status target contents =
  if not (Sys.file_exists target) then
    Missing
  else if Sys.is_directory target then
    Different
  else if read target = contents then
    Same
  else
    Different

let next_steps directory =
  print_string "\nNext:\n";
  Printf.printf "  cd %s\n" directory;
  print_string "  npm --prefix client install\n";
  print_string "  silica dev\n"

let write directory files =
  let missing =
    List.filter
      (fun (path, contents) ->
        status (Filename.concat directory path) contents = Missing
      )
      files
  in
  List.iter
    (fun (path, contents) ->
      let target = Filename.concat directory path in
      make_directories (Filename.dirname target);
      Out_channel.with_open_bin target (fun channel ->
          Out_channel.output_string channel contents
      );
      Printf.printf "created %s\n" path
    )
    missing;
  if missing = [] then
    Printf.printf "%s already holds the template; nothing changed.\n" directory;
  next_steps directory;
  0

let run directory =
  let name = project_name directory in
  if not (valid_name name) then (
    Printf.eprintf
      "silica new: %S is not a valid project name. Use letters, digits, '_' \
       and '-', and start with a letter.\n"
      name;
    1
  ) else
    let files =
      List.map
        (fun (path, contents) ->
          (path, replace_all ~pattern:"{{name}}" ~by:name contents)
        )
        Template_files.files
    in
    let different =
      List.filter
        (fun (path, contents) ->
          status (Filename.concat directory path) contents = Different
        )
        files
    in
    match different with
    | [] -> (
        try write directory files
        with Sys_error message ->
          Printf.eprintf "silica new: %s\n" message;
          1
      )
    | _ ->
        prerr_string "silica new: these files differ from the template:\n";
        List.iter (fun (path, _) -> Printf.eprintf "  %s\n" path) different;
        prerr_string
          "Nothing was written. Move these files away, or choose another \
           directory.\n";
        1
