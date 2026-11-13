let () =
  Example_codecs.register ();
  let open Example_checks in
  Check_suite.main
    ([
       Check_suite.entry (router ()) ~dependents:(fun report ->
           [
             Check_suite.entry
               (item
                  ~props:(Check.props report.children Example_item.component)
                  ()
               );
           ]
       );
       Check_suite.entry (home ());
       Check_suite.entry (todo ());
       Check_suite.entry (about ());
       Check_suite.entry (edit ());
       Check_suite.entry (not_found ());
       Check_suite.entry (votes ());
       Check_suite.entry (vote ());
       Check_suite.entry (counter_bounded ());
     ]
    @ List.map
        (fun (_, page) -> Check_suite.entry (joint_router page))
        joint_pages
    )
