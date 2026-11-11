let () =
  let folder = Sys.argv.(1) in
  Sys.mkdir folder 0o755;
  List.iter
    (fun (name, text) ->
      let spec, labels = Statechart_goldens.Layouts.read text in
      let file =
        Filename.concat folder
          (String.map (function '/' -> '-' | character -> character) name
          ^ ".svg"
          )
      in
      Out_channel.with_open_text file (fun channel ->
          Out_channel.output_string channel
            (Silica.Render.to_string
               (Silica.Statechart_view.view
                  (Silica.Statechart_layout.layout ~labels spec)
               )
            )
      )
    )
    Statechart_goldens.Golden_specs.all
