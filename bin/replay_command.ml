open Cmdliner

let id =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"ID" ~doc:"The id of the machine that the trace runs."
  )

let trace_file =
  Arg.(
    required
    & pos 1 (some string) None
    & info [] ~docv:"TRACE_FILE"
        ~doc:"A trace file, such as one that $(b,silica check --traces) wrote."
  )

let json = Arg.(value & flag & info [ "json" ] ~doc:"Print the replay as JSON.")

let replay id trace_file json =
  Check_executable.run ~command:"replay"
    ([ "replay"; id; trace_file ]
    @
    if json then
      [ "--json" ]
    else
      []
    )

let command =
  Cmd.v
    (Cmd.info "replay"
       ~doc:"Replay a trace in the check of a machine of the app."
       ~man:
         [
           `S Manpage.s_description;
           `P
             "Builds $(b,check/main.exe) and runs the steps of $(i,TRACE_FILE) \
              through the check of the machine $(i,ID), with no search. It \
              prints each step, its cause, the rules that fail there, and the \
              last view.";
           `S Manpage.s_exit_status;
           `P
             "0: no rule fails at the last step (after a fix). 1: a rule fails \
              at the last step, or a step of the trace is not enabled. 3: the \
              trace does not read, no entry checks $(i,ID), or check/main.exe \
              does not build.";
         ]
       ~exits:[]
    )
    Term.(const replay $ id $ trace_file $ json)
