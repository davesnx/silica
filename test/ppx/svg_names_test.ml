let () =
  let attributes = Silica_ppx.svg_attributes () in
  let failures =
    List.concat_map
      (fun (label, name) ->
        let rule =
          match Silica_ppx.Svg_names.violation name with
          | Some reason ->
              [ Printf.sprintf "%s (~%s): %s" name label reason ]
          | None ->
              []
        in
        let expected = Silica_ppx.Svg_names.printed_name label in
        let mismatch =
          if expected <> name then
            [ Printf.sprintf "~%s prints %s, rule says %s" label name expected ]
          else
            []
        in
        rule @ mismatch
      )
      attributes
  in
  List.iter print_endline failures;
  Printf.printf "checked %d SVG attributes, %d failures\n"
    (List.length attributes) (List.length failures);
  if failures <> [] then exit 1
