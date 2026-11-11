let run ~command arguments =
  if not (Sys.file_exists (Filename.concat "check" "main.ml")) then (
    prerr_endline
      ("silica " ^ command
     ^ ": no check/main.ml here. Run the command from the root of the app, \
        which lists its checks in check/main.ml."
      );
    3
  ) else if
      Sys.command
        (Filename.quote_command "dune"
           [ "build"; "--root"; "."; "./check/main.exe" ]
        )
      <> 0
    then (
    prerr_endline ("silica " ^ command ^ ": check/main.exe does not build.");
    3
  ) else
    Sys.command
      (Filename.quote_command
         (Filename.concat "_build"
            (Filename.concat "default" (Filename.concat "check" "main.exe"))
         )
         arguments
      )
