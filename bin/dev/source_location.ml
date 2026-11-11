let read_lines path =
  match In_channel.with_open_bin path In_channel.input_all with
  | text ->
      Some (String.split_on_char '\n' text)
  | exception Sys_error _ ->
      None

let excerpt ~path ~line =
  match read_lines path with
  | None ->
      []
  | Some lines ->
      List.filteri
        (fun index _ -> index + 1 >= line - 2 && index + 1 <= line + 2)
        lines
      |> List.mapi (fun index text -> (max 1 (line - 2) + index, text))

let relative ~root path =
  let prefix = root ^ Filename.dir_sep in
  if String.starts_with ~prefix path then
    String.sub path (String.length prefix)
      (String.length path - String.length prefix)
  else
    path

let skipped = [ "_build"; "_opam"; "node_modules" ]

let rec files_named name directory =
  match Sys.readdir directory with
  | exception Sys_error _ ->
      []
  | entries ->
      Array.to_list entries
      |> List.concat_map (fun entry ->
          let path = Filename.concat directory entry in
          if entry.[0] = '.' || List.mem entry skipped then
            []
          else if Sys.is_directory path then
            files_named name path
          else if entry = name then
            [ path ]
          else
            []
      )

let without_line_directive text =
  if String.starts_with ~prefix:"# 1 \"" text then
    match String.index_opt text '\n' with
    | Some index ->
        String.sub text (index + 1) (String.length text - index - 1)
    | None ->
        text
  else
    text

let read path = In_channel.with_open_bin path In_channel.input_all

let source ~root file =
  let file = relative ~root file in
  if Sys.file_exists (Filename.concat root file) then
    file
  else
    let copy = Filename.concat root (Filename.concat "_build/default" file) in
    match without_line_directive (read copy) with
    | exception Sys_error _ ->
        file
    | content -> (
        match
          List.find_opt
            (fun candidate -> read candidate = content)
            (files_named (Filename.basename file) root)
        with
        | Some found ->
            relative ~root found
        | None ->
            file
      )
