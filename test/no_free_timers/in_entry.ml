type state = A

let _ =
  Silica.Machine.make ~id:"timer" ~init:A
    ~name:(fun A -> "a")
    ~entry:(fun A ->
      [ Silica.Action.Start_timer { id = "t"; ms = 5; event = () } ]
    )
    (fun A () -> Silica.Machine.Ignore)
