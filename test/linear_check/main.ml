let () =
  let open Linear_checks in
  Check_suite.main
    [
      Check_suite.entry (router_check ()) ~dependents:(fun router ->
          [
            Check_suite.entry
              (detail_check
                 ~props:(Check.props router.children Linear_detail.component)
                 ()
              );
            Check_suite.entry
              (create_check
                 ~props:(Check.props router.children Linear_create.component)
                 ()
              )
              ~dependents:(fun create ->
                Check_suite.entry
                  (create_dialog_check
                     ~props:
                       (Check.props create.children
                          Linear_create.dialog_component.component
                       )
                     ()
                  )
                :: []
              );
            Check_suite.entry
              (create_with_dialog
                 ~props:(Check.props router.children Linear_create.component)
                 ()
              );
          ]
      );
      Check_suite.entry (router_with_list ());
      Check_suite.entry (router_with_detail ());
      Check_suite.entry (router_with_create ());
      Check_suite.entry (comments_with_rows ());
      Check_suite.entry (list_check ());
      Check_suite.entry (unsaved_check ());
      Check_suite.entry (me_check ());
      Check_suite.entry (comments_check ());
      Check_suite.entry (composer_check ());
      Check_suite.entry (comment_check ());
      Check_suite.elsewhere ~machine:"command-menu"
        ~reason:"a Silica_ui component, checked in test/ui_check";
      Check_suite.elsewhere ~machine:"menu"
        ~reason:"a Silica_ui component, checked in test/ui_check";
      Check_suite.elsewhere ~machine:"select"
        ~reason:"a Silica_ui component, checked in test/ui_check";
      Check_suite.elsewhere ~machine:"theme"
        ~reason:"a Silica_ui component, checked in test/ui_check";
      Check_suite.elsewhere ~machine:"toast"
        ~reason:"a Silica_ui component, checked in test/ui_check";
      Check_suite.elsewhere ~machine:"issue-filter-content"
        ~reason:
          "a Silica_ui.Popover with another id, checked as filter-popover in \
           test/ui_check";
      Check_suite.elsewhere ~machine:"labels-picker-content"
        ~reason:
          "a Silica_ui.Popover with another id, checked as filter-popover in \
           test/ui_check";
    ]
