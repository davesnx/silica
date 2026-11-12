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

let setup_error_json message =
  let open Silica_json.Json in
  let string text = declassify (`String text) in
  let empty = declassify (`List []) in
  declassify
    (`Assoc
       [
         ("format", string "silica-check");
         ("version", declassify (`Int 1));
         ("command", string "check");
         ("exit_code", declassify (`Int 3));
         ("outcome", string "setup error");
         ("passed", declassify (`Bool false));
         ("entries", empty);
         ("unchecked", empty);
         ("elsewhere", empty);
         ("golden", declassify `Null);
         ( "errors",
           declassify
             (`List
                [
                  declassify
                    (`Assoc
                       [ ("id", declassify `Null); ("message", string message) ]
                    );
                ]
             )
         );
       ]
    )

let check json graphs traces no_golden =
  match Check_executable.build ~command:"check" with
  | Error (message, details) ->
      prerr_endline message;
      if json then
        print_endline
          (Silica_json.Json.to_string
             (setup_error_json
                ( if String.trim details = "" then
                    message
                  else
                    message ^ "\n" ^ String.trim details
                )
             )
          );
      3
  | Ok () ->
      Silica_graphs.finish graphs
        (Sys.command
           (Filename.quote_command Check_executable.path
              (( if json then
                   [ "--json" ]
                 else
                   []
               )
              @ ( match traces with
                | Some dir ->
                    [ "--traces"; dir ]
                | None ->
                    []
                )
              @ ( if no_golden then
                    []
                  else
                    [ "--golden"; Filename.concat "check" "spec" ]
                )
              @ Silica_graphs.arguments graphs
              )
           )
        )

let check_command =
  let json =
    Arg.(
      value & flag
      & info [ "json" ]
          ~doc:
            "Print the report as JSON on standard output. To keep it, redirect \
             it to a file such as $(b,report.json), which the $(b,.gitignore) \
             of $(b,silica new) leaves out."
    )
  in
  let graphs =
    Arg.(
      value
      & opt (some string) None
      & info [ "graphs" ] ~docv:"DIR"
          ~doc:
            "Also write the graphs of each checked machine into $(docv): \
             $(i,ID).spec.json (the spec), $(i,ID).svg (the drawing), \
             $(i,ID).txt (the text form, one fact per line) and $(i,ID).md (a \
             Mermaid block, which GitHub shows as a chart)."
    )
  in
  let no_golden =
    Arg.(
      value & flag
      & info [ "no-golden" ]
          ~doc:
            "Do not compare the golden files in $(b,check/spec). By default a \
             missing or changed golden file is a finding."
    )
  in
  let traces =
    Arg.(
      value
      & opt (some string) None
      & info [ "traces" ] ~docv:"DIR"
          ~doc:
            "Write each violation as a trace file \
             $(docv)/<id>_<rule>_<state>.jsonl, for $(b,silica replay) and \
             $(b,silica script); the trace files of a check that this run no \
             longer writes are removed. $(docv) is relative to the app; the \
             $(b,.gitignore) of $(b,silica new) leaves out $(b,traces)."
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
              model checker runs each check that $(b,check/main.ml) lists, and \
              compares the golden files in $(b,check/spec) (unless \
              $(b,--no-golden)): a missing or changed golden file is a \
              finding, with the command that writes them. With $(b,--json), \
              the report is JSON (see the section Check a project of the \
              checker documentation).";
           `S Manpage.s_exit_status;
           `P
             "0: every check passed. 1: a finding (a violation, an unreached \
              state, a component that no check covers, a missing or changed \
              golden file). 2: a check stopped at its state limit, with no \
              finding. 3: a setup error, bad arguments, or check/main.exe does \
              not build.";
         ]
       ~exits:[]
    )
    Term.(const check $ json $ graphs $ traces $ no_golden)

let () =
  exit
    (Cmd.eval'
       (Cmd.group
          (Cmd.info "silica" ~doc:"Make, test and check Silica apps.")
          [
            new_command;
            check_command;
            Replay_command.command;
            Script_command.command;
            Conform_command.command;
            Silica_dev.Dev_command.command;
            Silica_dev.Inspect_command.command;
          ]
       )
    )
