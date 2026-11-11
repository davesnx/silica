open Silica

type event = Step | Pick of int

let rows = 1000

let component =
  Component.make
    (Machine.make ~id:"rows" ~init:0
       ~shape:(Machine.shape ~name:(fun _ -> "shown") ~names:[ "shown" ] ())
       ~events:
         (Machine.shape ~name:(function Step -> "step" | Pick _ -> "pick") ())
       (fun count -> function
         | Step -> Machine.Go (count + 1) | Pick _ -> Machine.Ignore
         )
    )
    (fun count ->
      Node.element "main" []
        [
          Node.element "p" [] [ Node.text (string_of_int count) ];
          Node.element "ul" []
            (List.init rows (fun index ->
                 Node.element "li" []
                   [
                     Node.element "button"
                       [ Attr.on_press (Pick index) ]
                       [ Node.text ("Row " ^ string_of_int index) ];
                   ]
             )
            );
          Node.element "button" [ Attr.on_press Step ] [ Node.text "Step" ];
        ]
    )

let press_step view =
  match Node.view view with
  | Node.Element (_, _, children) -> (
      let button = List.nth children (List.length children - 1) in
      match Node.view button with
      | Node.Element (_, attrs, _) ->
          List.find_map
            (function[@warning "-4"]
              | Node.On_press message -> Some message | _ -> None
              )
            attrs
      | _ ->
          None
    )
  | _ ->
      None

let run ~press ?observe steps =
  let tree = Instance.mount ?observe component in
  let view = ref (fst (Instance.update tree None)) in
  let start = Unix.gettimeofday () in
  for _ = 1 to steps do
    let message =
      if press then
        match press_step !view with
        | Some message ->
            message
        | None ->
            failwith "no Step button"
      else
        Instance.message tree Step
    in
    view := fst (Instance.update tree (Some message))
  done;
  (Unix.gettimeofday () -. start) *. 1000. /. float_of_int steps

let median values =
  let sorted = List.sort compare values in
  List.nth sorted (List.length sorted / 2)

let measure ~press =
  let steps = 200 and rounds = 9 in
  ignore (run ~press steps);
  let plain = ref [] and recorded = ref [] in
  for _ = 1 to rounds do
    plain := run ~press steps :: !plain;
    let recorder = Trace.Recorder.create ~build:"bench" () in
    recorded :=
      run ~press ~observe:(Trace.Recorder.observer recorder) steps :: !recorded
  done;
  let plain = median !plain and recorded = median !recorded in
  (plain, recorded, (recorded -. plain) /. plain *. 100.)

let check_press () =
  let recorder = Trace.Recorder.create ~build:"bench" () in
  ignore (run ~press:true ~observe:(Trace.Recorder.observer recorder) 1);
  let pressed (record : Transition.t) =
    match[@warning "-4"] record.cause with
    | Transition.Element
        { handler = "press"; role = Some "button"; name = "Step"; element; _ }
      ->
        element = "main > button"
    | _ ->
        false
  in
  if not (List.exists pressed (Trace.Recorder.transitions recorder)) then
    failwith "the press record does not name the Step button"

let () =
  check_press ();
  let print name (plain, recorded, overhead) =
    Printf.printf "| %s, no hook | %.4f ms |\n" name plain;
    Printf.printf "| %s, recorder with no data | %.4f ms |\n" name recorded;
    Printf.printf "| trace recorder %s overhead | %.1f %% |\n" name overhead
  in
  print "step" (measure ~press:false);
  print "press" (measure ~press:true)
