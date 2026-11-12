let () =
  Linear_codecs.register ();
  Check_suite.main (Linear_checks.suite ())
