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

let not_view broken page view =
  if Memory.view_text page = view then
    fail broken ("The view is still the view that broke it:\n" ^ view)

let does_not_wait broken page view =
  if
    Memory.pending page = []
    && Memory.timers page = []
    && Memory.view_text page = view
  then
    fail broken
      ("The page still waits for the user, with no request and no timer, in \
        the view:\n" ^ view
      )

let leaves_cycle broken page view =
  if Memory.view_text page = view then
    fail broken ("The page is back at the view where the cycle starts:\n" ^ view)

let props (component : (_, _, _) Component.t) json =
  match
    Result.bind (Machine.props_codec component.machine) (fun codec ->
        Codec.of_string codec json
    )
  with
  | Ok props ->
      props
  | Error error ->
      prerr_endline ("the props " ^ json ^ " do not decode: " ^ error);
      exit 1
