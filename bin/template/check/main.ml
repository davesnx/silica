let () =
  Check_suite.main
    [
      Check_suite.entry
        (Check.component
           ~within:(fun state -> Counter.count state <= 3)
           ~samples:[ Check.answers Api.save ~rejected:[ "too many" ] [ () ] ]
           Counter.component
        );
    ]
