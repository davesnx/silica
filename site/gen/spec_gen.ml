open Silica

let list print items = "[" ^ String.concat "; " (List.map print items) ^ "]"
let quoted = Printf.sprintf "%S"
let float value = Printf.sprintf "%.0f." (Float.round value)

let label (label : Statechart_layout.label) =
  Printf.sprintf "{ lines = %s; center = %s; bottom = %s; width = %s }"
    (list quoted label.lines) (float label.center) (float label.bottom)
    (float label.width)

let option print = function
  | None ->
      "None"
  | Some value ->
      "Some " ^ print value

let node (node : Statechart_layout.node) =
  Printf.sprintf
    "{ name = %S; x = %s; y = %s; width = %s; height = %s; final = %b; initial \
     = %b; details = %s; drift = %b }"
    node.name (float node.x) (float node.y) (float node.width)
    (float node.height) node.final node.initial (list quoted node.details)
    node.drift

let edge (edge : Statechart_layout.edge) =
  Printf.sprintf
    "{ from = %S; target = %S; points = %s; back = %b; label = %s; drift = %b }"
    edge.from edge.target
    (list (fun (x, y) -> "(" ^ float x ^ ", " ^ float y ^ ")") edge.points)
    edge.back (option label edge.label) edge.drift

let layout (layout : Statechart_layout.t) =
  Printf.sprintf
    "{ machine = %S; width = %s; height = %s; nodes = %s; edges = %s; note = \
     %s }"
    layout.machine (float layout.width) (float layout.height)
    (list node layout.nodes) (list edge layout.edges) (option label layout.note)

let name path =
  String.map
    (function '-' -> '_' | character -> character)
    (Filename.chop_suffix (Filename.basename path) ".spec.json")

let () =
  print_string "[@@@ocamlformat \"disable\"]\n\nopen Silica.Statechart_layout\n";
  Array.iteri
    (fun position path ->
      if position > 0 then
        match
          Result.bind
            (Silica_json.Json.of_string
               (In_channel.with_open_bin path In_channel.input_all)
            )
            Spec.of_json
        with
        | Ok spec ->
            Printf.printf "\nlet %s : Silica.Statechart_layout.t =\n  %s\n"
              (name path)
              (layout (Statechart_layout.layout spec))
        | Error message ->
            prerr_endline ("spec_gen: " ^ path ^ ": " ^ message);
            exit 1
    )
    Sys.argv
