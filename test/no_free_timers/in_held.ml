type state = A

let _ : (state, unit) Silica.Machine.next =
  Silica.Machine.Stay
    [ Silica.Action.Start_held { id = "s"; held = Silica.Action.Scroll_lock } ]
