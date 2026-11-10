module S = struct
  type _ t = A : unit t
end
module M = Silica.Machine.Make (S)
let _ =
  M.state S.A "a"
    ~entry:(fun () ->
      [ Silica.Cmd.Start_timer { id = "t"; ms = 5; event = () } ]
    )
    M.ignore
