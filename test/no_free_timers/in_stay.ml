module States = struct
  type _ t = A : unit t
end
module Timer_machine = Silica.Machine.Make (States)
let _ = Timer_machine.stay [ Silica.Cmd.Stop "t" ]
