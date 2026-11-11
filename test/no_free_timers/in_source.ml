type state = A

let _ : (state, unit) Silica.Machine.next =
  Silica.Machine.Stay
    [
      Silica.Action.Start_source
        { id = "s"; source = Silica.Action.Scroll_lock };
    ]
