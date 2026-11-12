A joint check of mini Linear writes its counterexample with --traces. The
seeded property fails after a submit inside the create dialog, a child of
the create page, which is a child of the router:

  $ ./joint_traces.exe --traces traces > /dev/null
  [1]
  $ ls traces
  router_create_issue_create_issue_dialog_no_title_error_new_status_assignee.jsonl

The checker replays the trace, and so does Silica_memory, on the real tree:

  $ ./joint_traces.exe replay traces/router_create_issue_create_issue_dialog_no_title_error_new_status_assignee.jsonl
  checker: start
  checker: router at 0: /?new&status&assignee&priority&label&group&order -> /?new&status&assignee&priority&label&group&order
  checker: create-issue-dialog at router/0.2:create-issue/1:create-issue-dialog: open -> open, fails no title error
  memory: replayed

The script works on this joint check too, although the check leaves the user
menu, the theme switcher and the issue list out and Silica_memory paints them:
its last line evaluates the property in the check, along the run of the page,
and does not compare views:

  $ ./joint_traces.exe script traces/router_create_issue_create_issue_dialog_no_title_error_new_status_assignee.jsonl
  open Silica
  module Memory = Silica_memory
  
  let broken =
    Check_expect.rule "no title error" ~state:"/?new&status&assignee&priority&label&group&order"
      ~message:"The property \"no title error\" does not hold in state \
                /?new&status&assignee&priority&label&group&order."
      ~hint:"Change the update or the view so that \"no title error\" holds in \
             state /?new&status&assignee&priority&label&group&order, or correct \
             the property."
  
  let () =
    let page = Memory.mount_app ~url:"/" Linear_app.app in
    Check_expect.holds broken Checks.router_create_issue_create_issue_dialog page (fun () ->
        Memory.press (Memory.by_role page Accessibility.Link ~name:"New issue");
        Memory.submit (Memory.by_role page Accessibility.Form)
    )
