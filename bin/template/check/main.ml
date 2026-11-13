let () =
  Codecs.register ();
  Check_suite.main [ Check_suite.entry Checks.counter ]
