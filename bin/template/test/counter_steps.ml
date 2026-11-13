open Silica

let describe state =
  Printf.sprintf "%s %d"
    (Machine.name Counter.machine state)
    (Counter.count state)

let step state event =
  let state, _actions, _how = Machine.step Counter.machine state event in
  state

let calls state =
  List.filter_map
    (function Machine.Call { path; _ } -> Some path | _ -> None)
    (Machine.active Counter.machine state)

let () =
  let state, _actions = Machine.start Counter.machine in
  Expect.string "start" "counting 0" (describe state);
  let state = step state Counter.Increment in
  Expect.string "increment" "counting 1" (describe state);
  let state = step state Counter.Save in
  Expect.string "save" "saving 1" (describe state);
  Expect.equal ~show:(String.concat ", ") "calls while saving" [ "/api/save" ]
    (calls state);
  let state = step state (Counter.Answered (Ok ())) in
  Expect.string "answer" "saved 1" (describe state);
  Expect.equal ~show:(String.concat ", ") "calls after the answer" []
    (calls state)
