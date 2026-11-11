module States = struct
  type _ t = A : unit t
end
module Timer_machine = Silica.Machine.Make (States)
let _ =
  Timer_machine.goto
    ~actions:[ Silica.Action.Start_timer { id = "t"; ms = 5; event = () } ]
    States.A ()
