open Cmdliner

let trace_files =
  Arg.(
    non_empty & pos_all string []
    & info [] ~docv:"TRACE_FILE"
        ~doc:
          "A trace file that the app recorded, in development or in \
           production, with $(b,Silica.Trace.Recorder)."
  )

let spec =
  Arg.(
    value
    & opt string (Filename.concat "check" "spec")
    & info [ "spec" ] ~docv:"DIR" ~doc:"The directory of the golden specs."
  )

let only =
  Arg.(
    value & opt_all string []
    & info [ "only" ] ~docv:"ID"
        ~doc:
          "Compare with the golden specs and the check of this entry only, \
           such as a joint check $(i,router+issue-list). Repeatable."
  )

let json = Arg.(value & flag & info [ "json" ] ~doc:"Print the result as JSON.")

let conform trace_files spec only json =
  Check_executable.run ~command:"conform"
    ("conform" :: "--spec" :: spec
     :: List.concat_map (fun id -> [ "--only"; id ]) only
    @ ( if json then
          [ "--json" ]
        else
          []
      )
    @ trace_files
    )

let command =
  Cmd.v
    (Cmd.info "conform"
       ~doc:"Compare traces of the app with the specs and the code."
       ~man:
         [
           `S Manpage.s_description;
           `P
             "Builds $(b,check/main.exe) and compares each $(i,TRACE_FILE) \
              with the golden specs in $(b,--spec) by names (each step is a \
              state and an edge of a spec), and with the code of each check by \
              data when the trace holds data (each step gives the same state \
              and snapshot, and every $(b,always) property holds on each \
              snapshot). Each deviation names the transition, the machine, the \
              instance, the step and the difference, with a hint.";
           `P
             "It does not prove that the app is right: only the steps in the \
              trace are compared, a trace with names only says nothing about \
              data, and a step that the spec allows can still be wrong.";
           `S Manpage.s_exit_status;
           `P
             "0: every trace conforms. 1: a deviation. 3: a trace or a spec \
              does not read, no entry has an $(b,--only) id, or check/main.exe \
              does not build.";
         ]
       ~exits:[]
    )
    Term.(const conform $ trace_files $ spec $ only $ json)
