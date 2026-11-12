open Silica

let counter id label =
  Component.make
    (Machine.make ~id ~init:0
       ~name:(fun _ -> "counting")
       (fun count () -> Machine.Go (count + 1))
    )
    (fun count ->
      Node.element "button"
        [ Attr.on_press () ]
        [ Node.text (label ^ " " ^ string_of_int count) ]
    )

let same_count = counter "same" "count"
let other = counter "other" "other"

let same_text =
  Component.make
    (Machine.make ~id:"same" ~init:"text"
       ~name:(fun _ -> "text")
       (fun text () -> Machine.Go text)
    )
    (fun text -> Node.text text)

type shown = Count | Text | Other
type event = Show of shown | Redraw

let root =
  Component.make
    (Machine.make ~id:"root" ~init:(Count, 0)
       ~name:(fun _ -> "showing")
       (fun (shown, draws) -> function
         | Show next ->
             Machine.Go (next, draws)
         | Redraw ->
             Machine.Go (shown, draws + 1)
         )
    )
    (fun (shown, _) ->
      Node.element "div" []
        [
          ( match shown with
          | Count ->
              Component.node same_count
          | Text ->
              Component.node same_text
          | Other ->
              Component.node other
          );
        ]
    )

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

let rec press node =
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
          List.find_map press children
    )
  | Node.List children | Node.Portal (_, children) ->
      List.find_map press children
  | Node.Keyed (_, child) ->
      press child
  | Node.Boundary { body; _ } ->
      press body
  | Node.Text _ | Node.Component _ | Node.Chunk _ ->
      None

let expect name wanted node =
  let shown = text node in
  if shown <> wanted then
    failwith (name ^ ": expected \"" ^ wanted ^ "\", shown \"" ^ shown ^ "\"")

let () =
  let tree = Instance.mount root in
  let view, _ = Instance.update tree None in
  expect "start" "count 0" view;
  let view, _ = Instance.update tree (press view) in
  expect "a press steps the child" "count 1" view;
  let view, _ = Instance.update tree (Some (Instance.message tree Redraw)) in
  expect "the same machine keeps its state across a render" "count 1" view;
  let view, _ =
    Instance.update tree (Some (Instance.message tree (Show Other)))
  in
  expect "a different machine at the same place is a new child" "other 0" view;
  let view, _ =
    Instance.update tree (Some (Instance.message tree (Show Count)))
  in
  expect "the first machine starts again" "count 0" view;
  match Instance.update tree (Some (Instance.message tree (Show Text))) with
  | _ ->
      failwith "two machine values under the id \"same\" did not raise"
  | exception Invalid_argument message ->
      let wanted = "\"same\"" in
      let rec names index =
        index + String.length wanted <= String.length message
        && (String.sub message index (String.length wanted) = wanted
           || names (index + 1)
           )
      in
      if not (names 0) then
        failwith ("the error does not name the id: " ^ message);
      print_endline "instance: machine matching holds under Melange"
