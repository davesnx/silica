let theme = function
  | "light" ->
      Silica_ui.Theme.light
  | "dark" ->
      Silica_ui.Theme.dark
  | name ->
      prerr_endline ("readme_chart: unknown theme " ^ name);
      exit 2

let style theme =
  let tokens = Silica_ui.Theme.tokens theme in
  Printf.sprintf
    "svg { --ui-text: %s; --ui-muted: %s; --ui-border: %s; --ui-surface: %s; \
     --ui-accent: %s; --ui-accent-text: %s; --ui-danger: %s }"
    tokens.text tokens.muted tokens.border tokens.surface tokens.accent
    tokens.accent_text tokens.danger

let () =
  match Sys.argv with
  | [| _; name; path |] -> (
      match
        Result.bind
          (Silica_json.Json.of_string
             (In_channel.with_open_bin path In_channel.input_all)
          )
          Silica.Spec.of_json
      with
      | Error message ->
          prerr_endline ("readme_chart: " ^ path ^ ": " ^ message);
          exit 1
      | Ok spec ->
          let layout = Silica.Statechart_layout.layout spec in
          let size value = Printf.sprintf "%.0f" (Float.ceil value) in
          print_endline
            (Silica.Render.to_string
               (Silica.Node.element "svg"
                  [
                    Silica.Attr.string "xmlns" "http://www.w3.org/2000/svg";
                    Silica.Attr.string "width" (size layout.width);
                    Silica.Attr.string "height" (size layout.height);
                  ]
                  [
                    Silica.Node.element "style" []
                      [ Silica.Node.text (style (theme name)) ];
                    Silica.Statechart_view.view layout;
                  ]
               )
            )
    )
  | _ ->
      prerr_endline "usage: readme_chart.exe light|dark SPEC.json";
      exit 2
