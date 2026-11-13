type t = Idle | Building | Built | Broken
type report = Started | Succeeded | Failed

let next phase (progress : Build_watch.progress) =
  let start =
    if phase = Building then
      []
    else
      [ Started ]
  in
  match progress with
  | Waiting | Interrupted ->
      (phase, [])
  | Running _ ->
      (Building, start)
  | Success ->
      (Built, start @ [ Succeeded ])
  | Failed ->
      (Broken, start @ [ Failed ])
