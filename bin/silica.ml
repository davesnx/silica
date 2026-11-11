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
  match (json, graphs) with
  | false, None ->
      Sys.command
        (Filename.quote_command "dune"
           [ "test"; "--force"; "--root"; "."; "test" ]
        )
  | _ ->
      prerr_endline
        "silica check: --json and --graphs are not available yet; run 'silica \
         check' with no options.";
      2

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
             "Runs the tests in $(b,test/) with dune, from the root of the \
              app. The exit code is the exit code of dune.";
         ]
    )
    Term.(const check $ json $ graphs)

let () =
  exit
    (Cmd.eval'
       (Cmd.group
          (Cmd.info "silica" ~doc:"Make, test and check Silica apps.")
          [ new_command; check_command ]
       )
    )
