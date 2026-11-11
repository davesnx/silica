module States = struct
  type _ t = A : unit t
end
module Timer_machine = Silica.Machine.Make (States)
let _ =
  Timer_machine.stay
    [ Silica.Cmd.Start_source { id = "s"; source = Silica.Cmd.Scroll_lock } ]
