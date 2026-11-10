module S = struct
  type _ t = A : unit t
end
module M = Silica.Machine.Make (S)
let _ =
  M.stay
    [ Silica.Cmd.Start_source { id = "s"; source = Silica.Cmd.Scroll_lock } ]
