module States = struct
  type _ t = A : unit t
end
module Timer_machine = Silica.Machine.Make (States)
let _ =
  Timer_machine.goto
    ~cmds:[ Silica.Cmd.Start_timer { id = "t"; ms = 5; event = () } ]
    States.A ()
