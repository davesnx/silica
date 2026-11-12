open Silica
module Memory = Silica_memory

type broken = { rule : string; state : string; message : string; hint : string }

let rule rule ~state ~message ~hint = { rule; state; message; hint }

let fail broken seen =
  prerr_endline
    (broken.rule ^ ": " ^ broken.message ^ "\nhint: " ^ broken.hint ^ "\n"
   ^ seen
    );
  exit 1

let ignored page input =
  let before = List.length (Memory.trace page).transitions in
  input ();
  let added =
    List.filteri
      (fun index _ -> index >= before)
      (Memory.trace page).transitions
  in
  List.exists (fun (record : Transition.t) -> record.change = Unhandled) added

let handles_input broken page input =
  if ignored page input then fail broken "The update still ignores the step."

let handles broken ?(position = 0) ?name page role input =
  match List.nth_opt (Memory.all_by_role ?name page role) position with
  | None ->
      ()
  | Some element -> (
      match ignored page (fun () -> input element) with
      | true ->
          fail broken "The update still ignores the input."
      | false ->
          ()
      | exception Memory.Not_reachable _ ->
          ()
    )

let on_error broken error _ =
  fail broken ("The step raised " ^ Printexc.to_string error ^ ".")

let view_passes broken page =
  match
    Check.view_violations ~rule:broken.rule ~state:broken.state
      (Memory.view page)
  with
  | [] ->
      ()
  | messages ->
      fail broken ("The view still breaks it:\n" ^ String.concat "\n" messages)

let holds broken ?cycle_from ?start check page steps =
  let stopped =
    match steps () with
    | () ->
        None
    | exception Memory.Not_reachable { message; _ } ->
        Some message
    | exception Failure message ->
        Some message
  in
  match Check.replay_run ?cycle_from ?start check (Memory.trace page) with
  | Error reason ->
      fail broken
        ("The run in memory cannot be replayed in the check: " ^ reason)
  | Ok (Not_enabled { at; _ }) ->
      fail broken
        (Printf.sprintf
           "Step %d of the run in memory is not a step that the check offers: \
            the check and the component disagree; check that they are the same \
            component and samples."
           at
        )
  | Ok (Replayed points) -> (
      match
        List.find_map
          (fun (point : (_, _) Check.replayed_step) ->
            List.find_opt
              (fun (failure : Check.failure) -> failure.rule = broken.rule)
              point.failing
            |> Option.map (fun (failure : Check.failure) -> (point, failure))
          )
          points
      with
      | Some (point, failure) ->
          fail broken
            (Printf.sprintf "It still fails in state %s: %s%s" point.state_name
               failure.message
               ( match stopped with
               | Some reason ->
                   "\n(the steps stopped early: " ^ reason ^ ")"
               | None ->
                   ""
               )
            )
      | None ->
          ()
    )
