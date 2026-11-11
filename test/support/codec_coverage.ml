let read file = In_channel.with_open_text file In_channel.input_all

let find text pattern from =
  let length = String.length pattern in
  let rec go index =
    if index + length > String.length text then
      None
    else if String.sub text index length = pattern then
      Some index
    else
      go (index + 1)
  in
  go from

let event_block text =
  Option.map
    (fun start ->
      let stop =
        Option.value (find text "\n\n" start) ~default:(String.length text)
      in
      String.sub text start (stop - start)
    )
    (find text "type event =" 0)

let machine_id text =
  Option.bind (find text "Machine.make" 0) (fun start ->
      Option.bind (find text "~id:\"" start) (fun quote ->
          let first = quote + 5 in
          Option.map
            (fun last -> String.sub text first (last - first))
            (String.index_from_opt text first '"')
      )
  )

let derives_json block = Option.is_some (find block "[@@deriving json" 0)

let required directory =
  Sys.readdir directory |> Array.to_list
  |> List.filter (fun file ->
      Filename.check_suffix file ".mlx" || Filename.check_suffix file ".ml"
  )
  |> List.sort compare
  |> List.filter_map (fun file ->
      let text = read (Filename.concat directory file) in
      match event_block text with
      | Some block when derives_json block ->
          Some (file, machine_id text)
      | Some _ | None ->
          None
  )

let missing directory =
  List.filter_map
    (fun (file, id) ->
      match id with
      | None ->
          Some
            (file ^ ": its events derive JSON, but no Machine.make ~id:\"...\"")
      | Some id when not (List.mem id (Silica.Machine.registered_codecs ())) ->
          Some
            (file ^ ": the machine \"" ^ id
           ^ "\" has events that derive JSON and is not in the codec module"
            )
      | Some _ ->
          None
    )
    (required directory)
