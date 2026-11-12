let counter =
  Check.component
    ~within:(fun state -> Counter.count state <= 3)
    ~samples:[ Check.answers Api.save ~rejected:[ "too many" ] [ () ] ]
    Counter.component
