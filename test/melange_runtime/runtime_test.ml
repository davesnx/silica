open Silica

exception Hook_failed
exception View_broke

type event = Break

let broken_view broken : event Node.t =
  if broken then
    raise View_broke
  else
    Node.element "button" [ Attr.on_press Break ] [ Node.text "break" ]

let breaking ~init =
  Component.make
    (Machine.make ~id:"breaking" ~init
       ~name:(fun _ -> "running")
       (fun _ Break -> Machine.Go true)
    )
    broken_view

let guarded =
  let child = breaking ~init:false in
  Component.make
    (Machine.make ~id:"guard" ~init:()
       ~name:(fun _ -> "guarding")
       (fun () () -> Machine.Ignore)
    )
    (fun () ->
      Node.boundary
        ~fallback:(fun (error : Node.error) ~reset:_ ->
          Node.text ("fallback " ^ Option.value error.detail ~default:"")
        )
        (Component.node child)
    )

let dispatch = ref (fun (_ : Instance.message) -> ())
let painted = ref None

let platform : Instance.message Platform.t =
  {
    renderer =
      (fun ~on_route:_ ~dispatch:send ->
        dispatch := send;
        {
          paint = (fun node -> painted := Some node);
          run = (fun _ -> ());
          start_source = (fun ~id:_ _ -> ());
          stop_source = (fun ~id:_ -> ());
        }
      );
    location = (fun () -> Url.of_string "/");
    io =
      {
        schedule = (fun _ _ () -> ());
        post = (fun ~path:_ ~body:_ _ -> ());
        connect =
          (fun _ -> { Platform.send = (fun _ -> ()); close = (fun _ -> ()) });
        csrf = (fun () -> "");
        session_gone = (fun () -> ());
        log = (fun _ -> ());
      };
  }

let rec find_press node =
  match Node.view node with
  | Node.Element (_, attributes, children) -> (
      match
        List.find_map
          (function Node.On_press message -> Some message | _ -> None)
          attributes
      with
      | Some message ->
          Some message
      | None ->
          List.find_map find_press children
    )
  | Node.List children | Node.Portal (_, children) ->
      List.find_map find_press children
  | Node.Keyed (_, child) ->
      find_press child
  | Node.Boundary { body; _ } ->
      find_press body
  | Node.Text _ | Node.Component _ | Node.Chunk _ ->
      None

let rec text node =
  match Node.view node with
  | Node.Text value ->
      value
  | Node.Element (_, _, children)
  | Node.List children
  | Node.Portal (_, children) ->
      String.concat "" (List.map text children)
  | Node.Keyed (_, child) ->
      text child
  | Node.Boundary { body; _ } ->
      text body
  | Node.Component _ | Node.Chunk _ ->
      ""

let contains text wanted =
  let length = String.length wanted in
  let rec from index =
    index + length <= String.length text
    && (String.sub text index length = wanted || from (index + 1))
  in
  from 0

let start ?(dev = false) ~on_error component =
  Runtime.start ~dev ~on_error ~platform
    ~create:(fun ~dev ~on_error -> Instance.mount ~dev ?on_error component)
    ()

let press () =
  match Option.bind !painted find_press with
  | Some message ->
      !dispatch message
  | None ->
      failwith "no button to press"

let expect_view_broke name = function
  | View_broke ->
      ()
  | other ->
      failwith (name ^ ": on_error got " ^ Printexc.to_string other)

let expect_hook_failed name run =
  match run () with
  | () ->
      failwith (name ^ ": the exception of on_error did not leave")
  | exception Hook_failed ->
      ()
  | exception other ->
      failwith (name ^ ": got " ^ Printexc.to_string other ^ ", not Hook_failed")

let () =
  start (breaking ~init:true) ~on_error:(fun error _ ->
      expect_view_broke "first paint" error
  );
  expect_hook_failed "first paint, on_error raises" (fun () ->
      start (breaking ~init:true) ~on_error:(fun _ _ -> raise Hook_failed)
  );
  start (breaking ~init:false) ~on_error:(fun error _ ->
      expect_view_broke "a step" error
  );
  press ();
  start (breaking ~init:false) ~on_error:(fun _ _ -> raise Hook_failed);
  expect_hook_failed "a step, on_error raises" press;
  let heard = ref None in
  start ~dev:true guarded ~on_error:(fun error _ -> heard := Some error);
  press ();
  Option.iter (expect_view_broke "a boundary") !heard;
  if Option.is_none !heard then failwith "a boundary: on_error got nothing";
  let shown = Option.fold ~none:"" ~some:text !painted in
  if not (contains shown "fallback" && contains shown "View_broke") then
    failwith ("a boundary: the fallback shows " ^ shown);
  print_endline "runtime: errors keep their exception under Melange"
