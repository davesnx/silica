A joint check of mini Linear writes its counterexample with --traces. The
seeded property fails after a submit inside the create dialog, a child of
the create page, which is a child of the router:

  $ ./joint_traces.exe --traces traces > /dev/null
  [1]
  $ ls traces
  router+create-issue+create-issue-dialog-1.jsonl

The checker replays the trace, and so does Silica_memory, on the real tree:

  $ ./joint_traces.exe replay traces/router+create-issue+create-issue-dialog-1.jsonl
  checker: start
  checker: router at 0: /?new&status&assignee&priority&label&group&order -> /?new&status&assignee&priority&label&group&order
  checker: create-issue-dialog at router/0.2:create-issue/1:create-issue-dialog: open -> open, fails no title error
  memory: replayed

The script refuses: the check leaves the user menu, the theme switcher and
the issue list out, and Silica_memory paints them:

  $ ./joint_traces.exe script traces/router+create-issue+create-issue-dialog-1.jsonl
  script: compares the view of the last step, which holds what the check does not run (component issue-list, component me, component theme); Silica_memory paints them, so the views cannot match. List those components with Check.child in ~children or ~pages, or check the property on a check that runs them
