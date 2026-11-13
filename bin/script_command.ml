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
        ~doc:"A trace file that $(b,silica check --traces) wrote."
  )

let component =
  Arg.(
    value
    & opt (some string) None
    & info [ "component" ] ~docv:"EXPR"
        ~doc:
          "The OCaml expression of the component. Default: the module named \
           after the machine id, such as $(b,Issue_list.component) for \
           $(b,issue-list)."
  )

let check =
  Arg.(
    value
    & opt (some string) None
    & info [ "check" ] ~docv:"EXPR"
        ~doc:
          "The OCaml expression of the check, which the test evaluates the \
           rule with. Default: the value named after the machine id in the \
           module $(b,Checks) of $(b,check/checks.ml), such as \
           $(b,Checks.issue_list) for $(b,issue-list)."
  )

let output =
  Arg.(
    value
    & opt (some string) None
    & info [ "output"; "o" ] ~docv:"FILE"
        ~doc:
          "Write the test to $(docv), only when it is written, and say so. \
           Without it, the test is printed on standard output."
  )

let script id trace_file component check output =
  let option name = function Some value -> [ name; value ] | None -> [] in
  Check_executable.run ~command:"script"
    ([ "script"; id; trace_file ]
    @ option "--component" component
    @ option "--check" check @ option "--output" output
    )

let command =
  Cmd.v
    (Cmd.info "script"
       ~doc:"Print a Silica_memory test that reproduces a counterexample."
       ~man:
         [
           `S Manpage.s_description;
           `P
             "Builds $(b,check/main.exe) and prints an OCaml test that drives \
              the component in Silica_memory through the steps of \
              $(i,TRACE_FILE), with role queries (press the button \"Save\"), \
              answers to pending calls and clock moves, and ends with a check \
              of the rule that the counterexample breaks. The test is \
              formatted by $(b,ocamlformat) when it is installed. Standard \
              error names the file to save it in: \
              $(b,test/)$(i,NAME)$(b,_replay.ml), where $(i,NAME) is the name \
              of the trace file (machine, rule and state). Add its name to the \
              tests of $(b,test/dune), and $(b,dune test) fails until the bug \
              is fixed.";
           `P
             "Some steps have no input in Silica_memory: a timer that fires \
              before a timer that is due earlier (the checker lets running \
              timers fire in any order), a held source, an event from the \
              inputs of the check, the result of an action. A Live update is \
              given with $(b,Memory.live_update). Then the command prints why \
              and writes no test; $(b,silica replay) checks the trace instead.";
           `S Manpage.s_exit_status;
           `P
             "0: the test is printed. 1: the trace has a step with no input in \
              Silica_memory, or no longer breaks its rule. 3: the trace does \
              not read, no entry checks $(i,ID), or check/main.exe does not \
              build.";
         ]
       ~exits:[]
    )
    Term.(const script $ id $ trace_file $ component $ check $ output)
