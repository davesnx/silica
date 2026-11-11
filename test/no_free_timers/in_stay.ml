type state = A

let _ : (state, unit) Silica.Machine.next =
  Silica.Machine.Stay [ Silica.Action.Stop "t" ]
