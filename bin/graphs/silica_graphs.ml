let suffix = ".spec.json"

let arguments = function None -> [] | Some dir -> [ "--specs"; dir ]

let read_spec path =
  match
    Result.bind
      (Silica_json.Json.of_string
         (In_channel.with_open_bin path In_channel.input_all)
      )
      Silica.Spec.of_json
  with
  | Ok spec ->
      Ok spec
  | Error message ->
      Error (path ^ ": " ^ message)

let markdown (spec : Silica.Spec.t) =
  "# " ^ spec.id ^ "\n\n```mermaid\n" ^ Silica.Spec.to_mermaid spec ^ "```\n"

let svg spec =
  Silica.Render.to_string
    (Silica.Statechart_view.view (Silica.Statechart_layout.layout spec))
  ^ "\n"

let write_file path text =
  let same =
    Sys.file_exists path
    && In_channel.with_open_bin path In_channel.input_all = text
  in
  if not same then
    Out_channel.with_open_bin path (fun channel ->
        Out_channel.output_string channel text
    )

let write dir =
  let names =
    List.sort compare
      (List.filter (String.ends_with ~suffix) (Array.to_list (Sys.readdir dir)))
  in
  let rec each = function
    | [] ->
        Ok (List.length names)
    | name :: rest -> (
        match read_spec (Filename.concat dir name) with
        | Error message ->
            Error message
        | Ok spec ->
            let base = Filename.concat dir (Filename.chop_suffix name suffix) in
            write_file (base ^ ".svg") (svg spec);
            write_file (base ^ ".txt") (Silica.Spec.to_text spec);
            write_file (base ^ ".md") (markdown spec);
            each rest
      )
  in
  each names

let finish graphs code =
  match graphs with
  | None ->
      code
  | Some _ when code = 3 ->
      code
  | Some dir -> (
      match write dir with
      | Ok count ->
          Printf.eprintf
            "silica check: wrote %d graph%s into %s, one for each machine of \
             each entry\n"
            count
            ( if count = 1 then
                ""
              else
                "s"
            )
            dir;
          code
      | Error message ->
          prerr_endline ("silica check: cannot read a spec: " ^ message);
          3
    )
