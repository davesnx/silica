let path =
  Filename.concat "_build"
    (Filename.concat "default" (Filename.concat "check" "main.exe"))

let read_file file = In_channel.with_open_bin file In_channel.input_all

let build ~command =
  if not (Sys.file_exists (Filename.concat "check" "main.ml")) then
    Error
      ( "silica " ^ command
        ^ ": no check/main.ml here. Run the command from the root of the app, \
           which lists its checks in check/main.ml.",
        ""
      )
  else
    let output = Filename.temp_file "silica-build" ".txt" in
    let code =
      Sys.command
        (Filename.quote_command "dune"
           [ "build"; "--root"; "."; "./check/main.exe" ]
           ~stderr:output
        )
    in
    let errors = read_file output in
    Sys.remove output;
    if code = 0 then
      Ok ()
    else (
      prerr_string errors;
      Error ("silica " ^ command ^ ": check/main.exe does not build.", errors)
    )

let run ~command arguments =
  match build ~command with
  | Error (message, _) ->
      prerr_endline message;
      3
  | Ok () ->
      Sys.command (Filename.quote_command path arguments)
