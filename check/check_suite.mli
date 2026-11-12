(** The checks of a project in one executable, with one report that a person or
    a program reads, a fixed exit code, and golden files of each machine.

    A project lists its checks in [check/main.ml]:
    {[
      let () =
        Check_suite.main
          [
            Check_suite.entry (Check.component Counter.component);
            Check_suite.entry (Check.app ~routes ... Router.app);
          ]
    ]}
    and runs them with [dune exec ./check/main.exe -- --json]. *)

type entry
(** One check of the suite, named by the id of its root machine. *)

val entry :
  ?dependents:(('s, 'ev) Check.report -> entry list) ->
  ('s, 'ev) Check.t ->
  entry
(** [entry ?dependents check]: run [check]. [dependents report] gives more
    entries made from its report, such as the check of a child component with
    the props that the parent rendered:
    [~dependents:(fun report -> [ entry (Check.component ~props:(Check.props
     report.children Item.component) Item.component) ])]. *)

val elsewhere : machine:string -> reason:string -> entry
(** A component that another suite checks, such as a component of a library. The
    suite does not report it as [unchecked-component], and lists it with the
    reason. Raises [Invalid_argument] when [reason] is blank. *)

type result
(** The reports of the entries that ran, what the suite found besides them, and
    the setup errors. *)

val run : ?only:string list -> entry list -> result
(** Runs the entries in order, and the dependents of each after it. With [only],
    runs the entries with these ids, and an entry with dependents when an id is
    not found yet, to make its dependents; an id that no entry has is a setup
    error. Without [only], a component that a checked view renders
    (["component <id>"] in [not_explored]) and that no entry, no joint child and
    no {!elsewhere} covers is an [unchecked-component] finding.
    [Check.Invalid_check] and any other exception of a check are setup errors.
*)

val golden : ?write:bool -> string -> result -> result
(** [golden ?write dir result] compares the golden files in [dir] with the
    reports of [result]. For each spec of each report: [<id>.spec.txt]
    ({!Silica.Spec.to_text}), [<id>.qnt] and [<id>.dot]; for each entry:
    [<id>.views.txt], the text view of the first snapshot of each state, with
    the path that reaches it. A file that differs, is missing, or that no entry
    makes (only when all entries ran) is a [golden-drift] finding. A changed
    [.spec.txt] is described by {!Silica.Spec.diff}; other files by their
    changed lines. With [write], the files are written and stale ones are
    removed, and the changes are not findings. Nothing is written when the
    result has a setup error. *)

val exit_code : result -> int
(** [3] when there is a setup error; else [1] when there is a finding (a
    violation, an unreached state, an unchecked component, golden drift); else
    [2] when a check is not complete (it stopped at [max_states]); else [0]. *)

val to_json : result -> Silica_json.Json.t
(** The result as JSON. The schema is in the documentation, section "Check a
    project" of the checker page. *)

val pp : Format.formatter -> result -> unit
(** The result as text: {!Check.pp_report} of each entry, the unchecked
    components, the golden changes, the setup errors, and a last line with the
    outcome and the exit code. Prints nothing for a [golden] run that passes and
    finds no change. *)

val main : ?argv:string array -> entry list -> unit
(** Reads the command from [argv] (default [Sys.argv]), runs it, prints the
    result on standard output, and exits with {!exit_code}:
    {v
    main.exe [check] [--json] [--only ID]... [--traces DIR]
    main.exe golden DIR [--write] [--json]
    main.exe replay ID TRACE_FILE [--json]
    main.exe script ID TRACE_FILE [--component EXPR]
    main.exe rules [--json]
    main.exe conform [--spec DIR] [--only ID]... [--json] TRACE_FILE...
    v}
    [check] is the default. With [--traces DIR], each violation is also written
    as a trace file [DIR/<id>_<rule>_<state>.jsonl] ({!Check.export}; the state
    is cut to 20 characters, and a second violation with the same name gets
    [_2], and so on), and the JSON gives its path in [trace_file] and the name
    of its [script] test in [test_file] ([test/<id>_<rule>_<state>_replay.ml]).
    It removes the [.jsonl] files in [DIR] whose name starts with the id of a
    check that ran and that this run does not write, so a stale trace cannot be
    replayed by mistake. [golden DIR] runs every entry and compares the golden
    files in [DIR] (see {!golden}). [replay ID TRACE_FILE] replays the trace in
    the check of the machine [ID] ({!Check.replay_trace}) and prints each step
    with the rules that fail there: exit [0] when no rule fails at the last
    step, [1] when one does or a step is not enabled, [3] when the trace does
    not read or cannot be replayed faithfully. [script ID TRACE_FILE] prints a
    [Silica_memory] test that reproduces the counterexample ({!Check.script}),
    formatted by [ocamlformat] with the configuration of the project when
    [ocamlformat] is on the path, and says on standard error which file to save
    it in ([test/<trace name>_replay.ml]); when the steps cannot run in
    [Silica_memory], it prints why on standard error and exits with [1]. [rules]
    lists {!Check.val-rules}. [conform TRACE_FILE...] compares each trace, such
    as one that the app recorded in development or in production, with the
    golden specs in [--spec DIR] (default [spec]) by names
    ({!Silica.Spec.conforms}) and with the code of each entry by data
    ({!Check.conforms}); with [--only ID], only with the golden specs and the
    check of that entry, such as a joint check of a parent with its children
    ([router+issue-list]). Without [--only], dependents are not run, and
    machines named by {!elsewhere} or with no spec in the selected files are
    listed as not checked. Each deviation names the transition, the machine, the
    instance, the step and the difference, with a hint: the spec is out of date,
    the check does not explore the behaviour, or the trace is from another
    build. Exit [0] when every trace conforms, [1] on a deviation, [3] when a
    trace or a spec does not read. Bad arguments print the usage on standard
    error and exit with [3]. *)
