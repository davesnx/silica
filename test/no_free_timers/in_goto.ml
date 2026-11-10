module S = struct
  type _ t = A : unit t
end
module M = Silica.Machine.Make (S)
let _ =
  M.goto
    ~cmds:[ Silica.Cmd.Start_timer { id = "t"; ms = 5; event = () } ]
    S.A ()
