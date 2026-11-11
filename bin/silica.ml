open Cmdliner

let new_command =
  let directory =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"DIR"
          ~doc:
            "The directory of the new app. Its name is the name of the dune \
             project."
    )
  in
  Cmd.v
    (Cmd.info "new" ~doc:"Make a new Silica app."
       ~man:
         [
           `S Manpage.s_description;
           `P
             "Writes a server, a client, a counter component with a Remote \
              call, its tests and an AGENTS.md into $(i,DIR). A file that \
              already holds the same content is skipped. When a file differs, \
              the command lists it and writes nothing.";
         ]
    )
    Term.(const New_project.run $ directory)

let check json graphs =
  match graphs with
  | Some _ ->
      prerr_endline
        "silica check: --graphs is not available yet; run 'silica check' \
         without it.";
      3
  | None ->
      if not (Sys.file_exists (Filename.concat "check" "main.ml")) then (
        prerr_endline
          "silica check: no check/main.ml here. Run the command from the root \
           of the app, which lists its checks in check/main.ml.";
        3
      ) else if
          Sys.command
            (Filename.quote_command "dune"
               [ "build"; "--root"; "."; "./check/main.exe" ]
            )
          <> 0
        then (
        prerr_endline "silica check: check/main.exe does not build.";
        3
      ) else
        Sys.command
          (Filename.quote_command
             (Filename.concat "_build"
                (Filename.concat "default" (Filename.concat "check" "main.exe"))
             )
             ( if json then
                 [ "--json" ]
               else
                 []
             )
          )

let check_command =
  let json =
    Arg.(value & flag & info [ "json" ] ~doc:"Print the report as JSON.")
  in
  let graphs =
    Arg.(
      value
      & opt (some string) None
      & info [ "graphs" ] ~docv:"DIR"
          ~doc:"Write a graph of each checked machine into $(docv)."
    )
  in
  Cmd.v
    (Cmd.info "check"
       ~doc:"Check the machines of the app in the current directory."
       ~man:
         [
           `S Manpage.s_description;
           `P
             "Builds $(b,check/main.exe) of the app with dune and runs it: the \
              model checker runs each check that $(b,check/main.ml) lists. \
              With $(b,--json), the report is JSON (see the section Check a \
              project of the checker documentation).";
           `S Manpage.s_exit_status;
           `P
             "0: every check passed. 1: a finding (a violation, an unreached \
              state, a component that no check covers). 2: a check stopped at \
              its state limit, with no finding. 3: a setup error, bad \
              arguments, or check/main.exe does not build.";
         ]
       ~exits:[]
    )
    Term.(const check $ json $ graphs)

let () =
  exit
    (Cmd.eval'
       (Cmd.group
          (Cmd.info "silica" ~doc:"Make, test and check Silica apps.")
          [ new_command; check_command; Silica_dev.Dev_command.command ]
       )
    )
