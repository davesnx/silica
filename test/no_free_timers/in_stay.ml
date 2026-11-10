module S = struct
  type _ t = A : unit t
end
module M = Silica.Machine.Make (S)
let _ = M.stay [ Silica.Cmd.Stop "t" ]
