type state = A

let _ : (state, unit) Silica.Machine.next =
  Silica.Machine.Go_with
    (A, [ Silica.Action.Start_timer { id = "t"; ms = 5; event = () } ])
