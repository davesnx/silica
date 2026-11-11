let () =
  let case name f = Alcotest.test_case name `Quick f in
  Alcotest.run "spike"
    [
      ( "landing save",
        [
          case "same check" Spike_checks.Landing.check_both;
          case "same html and commands" Spike_checks.Landing.same_html;
          case "same server render" Spike_checks.Landing.render;
          case "spec without running" Spike_checks.Landing.spec_without_running;
        ]
      );
      ( "dialog",
        [
          case "same check" Spike_checks.Dialogs.check_both;
          case "same html and commands" Spike_checks.Dialogs.same_html;
          case "same server render" Spike_checks.Dialogs.render;
        ]
      );
      ( "issue list",
        [
          case "same check" Spike_checks.Issue_list.check_both;
          case "same html and commands" Spike_checks.Issue_list.same_html;
          case "same server render" Spike_checks.Issue_list.render;
        ]
      );
    ]
