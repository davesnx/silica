open Silica

let goldens =
  List.map
    (fun (name, text) -> (name, text, Statechart_goldens.Layouts.read text))
    Statechart_goldens.Golden_specs.all

let state ?(final = false) ?(timers = []) ?(network = []) ?(held = []) name =
  { Spec.name; final; timers; network; held }

let odd_spec : Spec.t =
  {
    id = "odd machine";
    init = "a b";
    states =
      [
        state "a b" ~timers:[ 300 ] ~network:[ "/api/save"; "live:x y" ];
        state "quote\"" ~held:[ "focus trap" ];
        state "back\\slash";
        state "line\nbreak";
        state "->";
        state "final" ~final:true;
        state "semi;colon#";
      ];
    edges =
      List.sort_uniq compare
        [
          {
            Spec.from = "a b";
            target = "quote\"";
            event = "url changed";
            kept = false;
          };
          { from = "a b"; target = "quote\""; event = ""; kept = false };
          { from = "a b"; target = "a b"; event = "tick"; kept = false };
          { from = "quote\""; target = "->"; event = ""; kept = false };
          { from = "->"; target = "final"; event = "\"quoted\""; kept = false };
          {
            from = "back\\slash";
            target = "line\nbreak";
            event = "";
            kept = false;
          };
          { from = "quote\""; target = "quote\""; event = ""; kept = true };
          {
            from = "quote\"";
            target = "quote\"";
            event = "hover true";
            kept = true;
          };
          { from = "semi;colon#"; target = "a b"; event = "a;b"; kept = false };
        ];
  }

let check_read_back message expected actual =
  match actual with
  | Error error ->
      Alcotest.failf "%s: %s" message error
  | Ok actual ->
      if actual <> expected then
        Alcotest.failf "%s: reads back differently" message

let text_fixpoint () =
  Alcotest.(check bool) "there are goldens" true (List.length goldens >= 30);
  List.iter
    (fun (name, text, spec) ->
      Alcotest.(check string) name text (Spec.to_text spec)
    )
    goldens

let text_round_trip () =
  List.iter
    (fun (name, _, spec) ->
      check_read_back name spec (Spec.of_text (Spec.to_text spec))
    )
    goldens;
  check_read_back "names that need quotes" odd_spec
    (Spec.of_text (Spec.to_text odd_spec))

let text_errors () =
  let error text =
    match Spec.of_text text with Ok _ -> "read" | Error message -> message
  in
  Alcotest.(check string)
    "unknown line" "line 4: cannot read \"bogus line\""
    (error "machine m\ninitial a\nstate a\nbogus line\n");
  Alcotest.(check string)
    "detail first" "line 3: a detail before any state"
    (error "machine m\ninitial a\n  after 3ms\n");
  Alcotest.(check string)
    "open quote" "line 3: a quoted word has no closing quote"
    (error "machine m\ninitial a\nstate \"a\n");
  Alcotest.(check string) "no machine" "no machine line" (error "initial a\n");
  Alcotest.(check string) "no initial" "no initial line" (error "machine m\n")

let json_round_trip () =
  let through_text spec =
    Result.bind
      (Silica_json.Json.of_string
         (Silica_json.Json.to_string (Spec.to_json spec))
      )
      Spec.of_json
  in
  List.iter
    (fun (name, _, spec) -> check_read_back name spec (through_text spec))
    goldens;
  check_read_back "names that need quotes" odd_spec (through_text odd_spec)

let json_errors () =
  let error text =
    match Result.bind (Silica_json.Json.of_string text) Spec.of_json with
    | Ok _ ->
        "read"
    | Error message ->
        message
  in
  Alcotest.(check string) "missing" "no field \"init\"" (error {|{"id":"m"}|});
  Alcotest.(check string)
    "wrong type" "id: expected a string" (error {|{"id":1}|});
  Alcotest.(check string)
    "bad edge" "edges: expected an object with the field \"from\""
    (error {|{"id":"m","init":"a","states":[],"edges":[["a"]]}|})

let mermaid () =
  Alcotest.(check string)
    "odd spec"
    "stateDiagram-v2\n\
    \  direction LR\n\
    \  state \"a b (after 300ms, invoke /api/save, invoke live:x y)\" as s0\n\
    \  state \"quote#quot; (hold focus trap)\" as s1\n\
    \  state \"back\\slash\" as s2\n\
    \  state \"line break\" as s3\n\
    \  state \"->\" as s4\n\
    \  state \"final\" as s5\n\
    \  state \"semi#59;colon#35;\" as s6\n\
    \  [*] --> s0\n\
    \  s4 --> s5 : #quot;quoted#quot;\n\
    \  s0 --> s0 : tick\n\
    \  s0 --> s1 : url changed\n\
    \  s2 --> s3\n\
    \  s1 --> s4\n\
    \  s6 --> s0 : a#59;b\n\
    \  s5 --> [*]\n"
    (Spec.to_mermaid odd_spec)

type box = { left : float; top : float; right : float; bottom : float }

let overlaps first second =
  Float.max first.left second.left < Float.min first.right second.right
  && Float.max first.top second.top < Float.min first.bottom second.bottom

let node_box (node : Statechart_layout.node) =
  {
    left = node.x;
    top = node.y;
    right = node.x +. node.width;
    bottom = node.y +. node.height;
  }

let shrink amount box =
  {
    left = box.left +. amount;
    top = box.top +. amount;
    right = box.right -. amount;
    bottom = box.bottom -. amount;
  }

let label_box (label : Statechart_layout.label) =
  let widest =
    List.fold_left
      (fun widest line -> max widest (String.length line))
      0 label.lines
  in
  let half = float_of_int widest *. 6.6 /. 2. in
  {
    left = label.center -. half;
    right = label.center +. half;
    top = label.bottom -. (14. *. float_of_int (List.length label.lines));
    bottom = label.bottom;
  }

let segment_boxes (edge : Statechart_layout.edge) =
  let rec boxes = function
    | (first_x, first_y) :: ((second_x, second_y) :: _ as rest) ->
        {
          left = Float.min first_x second_x;
          right = Float.max first_x second_x;
          top = Float.min first_y second_y;
          bottom = Float.max first_y second_y;
        }
        :: boxes rest
    | [ _ ] | [] ->
        []
  in
  boxes edge.points

let find_node (layout : Statechart_layout.t) name =
  List.find
    (fun (node : Statechart_layout.node) -> node.name = name)
    layout.nodes

let on_border box (x, y) =
  let near first second = Float.abs (first -. second) < 0.01 in
  let between low value high = value >= low -. 0.01 && value <= high +. 0.01 in
  ((near x box.left || near x box.right) && between box.top y box.bottom)
  || ((near y box.top || near y box.bottom) && between box.left x box.right)

let layouts () =
  List.map
    (fun (name, _, spec) -> (name, spec, Statechart_layout.layout spec))
    goldens

let pairs list =
  let rec go = function
    | first :: rest ->
        List.map (fun second -> (first, second)) rest @ go rest
    | [] ->
        []
  in
  go list

let every_state_and_edge () =
  List.iter
    (fun (name, (spec : Spec.t), (layout : Statechart_layout.t)) ->
      Alcotest.(check (list string))
        (name ^ " nodes")
        (List.map (fun (state : Spec.state) -> state.name) spec.states)
        (List.map
           (fun (node : Statechart_layout.node) -> node.name)
           layout.nodes
        );
      Alcotest.(check (list (pair string string)))
        (name ^ " edges") (Spec.arrows spec)
        (List.map
           (fun (edge : Statechart_layout.edge) -> (edge.from, edge.target))
           layout.edges
        )
    )
    (layouts ())

let no_overlap () =
  List.iter
    (fun (name, _, (layout : Statechart_layout.t)) ->
      List.iter
        (fun ( (first : Statechart_layout.node),
               (second : Statechart_layout.node)
             ) ->
          if overlaps (node_box first) (node_box second) then
            Alcotest.failf "%s: %s overlaps %s" name first.name second.name
        )
        (pairs layout.nodes);
      List.iter
        (fun (node : Statechart_layout.node) ->
          if
            node.x < 0. || node.y < 0.
            || node.x +. node.width > layout.width
            || node.y +. node.height > layout.height
          then
            Alcotest.failf "%s: %s is outside the drawing" name node.name
        )
        layout.nodes
    )
    (layouts ())

let edges_on_borders () =
  List.iter
    (fun (name, _, (layout : Statechart_layout.t)) ->
      List.iter
        (fun (edge : Statechart_layout.edge) ->
          let first = List.hd edge.points
          and last = List.nth edge.points (List.length edge.points - 1) in
          if not (on_border (node_box (find_node layout edge.from)) first) then
            Alcotest.failf "%s: %s -> %s does not start on %s" name edge.from
              edge.target edge.from;
          if not (on_border (node_box (find_node layout edge.target)) last) then
            Alcotest.failf "%s: %s -> %s does not end on %s" name edge.from
              edge.target edge.target
        )
        layout.edges
    )
    (layouts ())

let edges_avoid_nodes () =
  List.iter
    (fun (name, _, (layout : Statechart_layout.t)) ->
      List.iter
        (fun (edge : Statechart_layout.edge) ->
          List.iter
            (fun (node : Statechart_layout.node) ->
              if
                List.exists
                  (overlaps (shrink 0.5 (node_box node)))
                  (segment_boxes edge)
              then
                Alcotest.failf "%s: %s -> %s crosses %s" name edge.from
                  edge.target node.name
            )
            layout.nodes
        )
        layout.edges
    )
    (layouts ())

let labels_are_clear () =
  List.iter
    (fun (name, (spec : Spec.t), (layout : Statechart_layout.t)) ->
      let labelled =
        List.filter_map
          (fun (edge : Statechart_layout.edge) ->
            Option.map (fun label -> (edge, label_box label)) edge.label
          )
          layout.edges
      in
      List.iter
        (fun (edge : Statechart_layout.edge) ->
          let named =
            List.exists
              (fun (label : Spec.label) ->
                label.from = edge.from && label.target = edge.target
                && (not label.kept) && label.event <> ""
              )
              spec.edges
          in
          if named && edge.label = None && layout.note = None then
            Alcotest.failf "%s: %s -> %s has no label" name edge.from
              edge.target
        )
        layout.edges;
      List.iter
        (fun ((edge : Statechart_layout.edge), box) ->
          List.iter
            (fun (node : Statechart_layout.node) ->
              if overlaps box (node_box node) then
                Alcotest.failf "%s: the label of %s -> %s covers %s" name
                  edge.from edge.target node.name
            )
            layout.nodes;
          List.iter
            (fun (other : Statechart_layout.edge) ->
              if
                other != edge && List.exists (overlaps box) (segment_boxes other)
              then
                Alcotest.failf "%s: %s -> %s crosses the label of %s -> %s" name
                  other.from other.target edge.from edge.target
            )
            layout.edges
        )
        labelled;
      List.iter
        (fun ( ((first : Statechart_layout.edge), first_box),
               ((second : Statechart_layout.edge), second_box)
             ) ->
          if overlaps first_box second_box then
            Alcotest.failf "%s: the labels of %s -> %s and %s -> %s overlap"
              name first.from first.target second.from second.target
        )
        (pairs labelled)
    )
    (layouts ())

let occurrences pattern text =
  let length = String.length pattern in
  let rec count from total =
    if from + length > String.length text then
      total
    else
      count (from + 1)
        ( if String.sub text from length = pattern then
            total + 1
          else
            total
        )
  in
  count 0 0

let label_middle (label : Statechart_layout.label) =
  label.bottom -. (7. *. float_of_int (List.length label.lines))

let labels_on_their_edge () =
  let seen = ref 0 in
  List.iter
    (fun (name, _, (layout : Statechart_layout.t)) ->
      List.iter
        (fun (edge : Statechart_layout.edge) ->
          match edge.label with
          | Some label when edge.from <> edge.target ->
              incr seen;
              let left = label.center -. (label.width /. 2.)
              and right = label.center +. (label.width /. 2.) in
              let rec carries = function
                | (first_x, first_y) :: ((second_x, second_y) :: _ as rest) ->
                    Float.abs (first_y -. second_y) < 0.01
                    && Float.abs (first_y -. label_middle label) < 0.5
                    && Float.min first_x second_x <= left +. 0.01
                    && Float.max first_x second_x >= right -. 0.01
                    || carries rest
                | [ _ ] | [] ->
                    false
              in
              if not (carries edge.points) then
                Alcotest.failf "%s: the label of %s -> %s is not on its line"
                  name edge.from edge.target
          | _ ->
              ()
        )
        layout.edges
    )
    (layouts ());
  Alcotest.(check bool) "the goldens have labels" true (!seen > 50)

let labels_in_the_middle () =
  let seen = ref 0 in
  List.iter
    (fun (name, _, (layout : Statechart_layout.t)) ->
      List.iter
        (fun (edge : Statechart_layout.edge) ->
          match edge.label with
          | Some label when edge.from <> edge.target ->
              let xs = List.map fst edge.points in
              let low = List.fold_left Float.min Float.infinity xs
              and high = List.fold_left Float.max Float.neg_infinity xs in
              let columns =
                List.sort_uniq Float.compare
                  (List.filter_map
                     (fun (node : Statechart_layout.node) ->
                       let middle = node.x +. (node.width /. 2.) in
                       if middle > low && middle < high then
                         Some middle
                       else
                         None
                     )
                     layout.nodes
                  )
              in
              if List.length columns >= 2 then (
                incr seen;
                let quarter = (high -. low) /. 4. in
                if
                  label.center < low +. quarter
                  || label.center > high -. quarter
                then
                  Alcotest.failf
                    "%s: the label of %s -> %s is not in the middle of its edge"
                    name edge.from edge.target
              )
          | _ ->
              ()
        )
        layout.edges
    )
    (layouts ());
  Alcotest.(check bool) "the goldens have long labelled edges" true (!seen > 0)

let shared_events_once () =
  let routers =
    List.filter
      (fun (name, _, _) ->
        name = "example_check/router" || name = "linear_check/router"
      )
      (layouts ())
  in
  Alcotest.(check int) "two routers" 2 (List.length routers);
  List.iter
    (fun (name, _, (layout : Statechart_layout.t)) ->
      ( match layout.note with
      | Some note ->
          Alcotest.(check (list string))
            name
            [ "every edge: url changed" ]
            note.lines;
          if note.bottom > layout.height || note.center < 0. then
            Alcotest.failf "%s: the note is outside the drawing" name
      | None ->
          Alcotest.failf "%s: no note" name
      );
      List.iter
        (fun (edge : Statechart_layout.edge) ->
          if edge.label <> None then
            Alcotest.failf "%s: %s -> %s has a label" name edge.from edge.target
        )
        layout.edges
    )
    routers;
  let _, _, toast =
    List.find (fun (name, _, _) -> name = "ui_check/toast") (layouts ())
  in
  Alcotest.(check bool) "toast has no note" true (toast.note = None)

let drift () =
  let _, _, spec =
    List.find (fun (name, _, _) -> name = "ui_check/toast") goldens
  in
  let arrows = Spec.arrows spec in
  let kept = List.hd arrows in
  let missing = ("paused", "expired") in
  Alcotest.(check bool) "not an arrow" false (List.mem missing arrows);
  let observed = [ ("ghost", "empty"); missing; kept; missing ] in
  let layout = Statechart_layout.layout ~observed spec in
  Alcotest.(check (list (pair string string)))
    "the arrows, then the drift edges, sorted"
    (arrows @ [ ("ghost", "empty"); missing ])
    (List.map
       (fun (edge : Statechart_layout.edge) -> (edge.from, edge.target))
       layout.edges
    );
  Alcotest.(check (list string))
    "drift edges"
    [ "ghost -> empty"; "paused -> expired" ]
    (List.filter_map
       (fun (edge : Statechart_layout.edge) ->
         if edge.drift then
           Some (edge.from ^ " -> " ^ edge.target)
         else
           None
       )
       layout.edges
    );
  Alcotest.(check (list string))
    "drift nodes" [ "ghost" ]
    (List.filter_map
       (fun (node : Statechart_layout.node) ->
         if node.drift then
           Some node.name
         else
           None
       )
       layout.nodes
    );
  List.iter
    (fun (edge : Statechart_layout.edge) ->
      if edge.drift && edge.label <> None then
        Alcotest.failf "%s -> %s: a drift edge has a label" edge.from
          edge.target
    )
    layout.edges;
  List.iter
    (fun ((first : Statechart_layout.node), (second : Statechart_layout.node))
       ->
      if overlaps (node_box first) (node_box second) then
        Alcotest.failf "%s overlaps %s" first.name second.name
    )
    (pairs layout.nodes);
  let plain = Statechart_layout.layout spec in
  Alcotest.(check string)
    "no observed steps, no change"
    (Statechart_layout.to_string plain)
    (Statechart_layout.to_string (Statechart_layout.layout ~observed:[] spec));
  Alcotest.(check string)
    "observed arrows change nothing"
    (Statechart_layout.to_string plain)
    (Statechart_layout.to_string
       (Statechart_layout.layout ~observed:arrows spec)
    );
  let svg = Render.to_string (Statechart_view.view layout) in
  Alcotest.(check int) "marked in the view" 3 (occurrences "is-drift" svg);
  Alcotest.(check int)
    "the ghost state" 1
    (occurrences "class=\"chart-node is-drift\" data-state=\"ghost\"" svg);
  Alcotest.(check int) "dashed" 3 (occurrences "stroke-dasharray" svg);
  Alcotest.(check int)
    "nothing marked without observed steps" 0
    (occurrences "is-drift" (Render.to_string (Statechart_view.view plain)))

let inner points =
  match points with
  | [] | [ _ ] ->
      []
  | _ :: rest ->
      List.rev (List.tl (List.rev rest))

let two_way_pairs_apart () =
  let seen = ref 0 in
  List.iter
    (fun (name, _, (layout : Statechart_layout.t)) ->
      List.iter
        (fun ( (first : Statechart_layout.edge),
               (second : Statechart_layout.edge)
             ) ->
          if
            first.from = second.target && first.target = second.from
            && first.from <> first.target
          then (
            incr seen;
            let shared = ref 0 in
            List.iter
              (fun (first_x, first_y) ->
                List.iter
                  (fun (second_x, second_y) ->
                    if Float.abs (first_x -. second_x) < 0.01 then (
                      incr shared;
                      if Float.abs (first_y -. second_y) < 8. then
                        Alcotest.failf "%s: %s <-> %s are drawn together" name
                          first.from first.target
                    )
                  )
                  (inner second.points)
              )
              (inner first.points);
            if !shared = 0 then
              Alcotest.failf "%s: %s <-> %s share no column" name first.from
                first.target
          )
        )
        (pairs layout.edges)
    )
    (layouts ());
  Alcotest.(check bool) "the goldens have two way pairs" true (!seen > 10)

let self_edges_apart () =
  let seen = ref 0 in
  List.iter
    (fun (name, _, (layout : Statechart_layout.t)) ->
      List.iter
        (fun (edge : Statechart_layout.edge) ->
          if edge.from = edge.target then (
            incr seen;
            let node = find_node layout edge.from in
            Alcotest.(check int)
              (name ^ " loop points") 4 (List.length edge.points);
            List.iter
              (fun (_, y) ->
                if y > node.y +. 0.01 then
                  Alcotest.failf "%s: the loop on %s enters the node" name
                    node.name
              )
              edge.points;
            match edge.points with
            | (_, start_y) :: (_, control_y) :: _ ->
                if start_y -. control_y < 20. then
                  Alcotest.failf "%s: the loop on %s is flat" name node.name
            | _ ->
                ()
          )
        )
        layout.edges
    )
    (layouts ());
  Alcotest.(check bool) "the goldens have self edges" true (!seen > 0)

let deterministic () =
  List.iter
    (fun (name, _, spec) ->
      Alcotest.(check string)
        name
        (Statechart_layout.to_string (Statechart_layout.layout spec))
        (Statechart_layout.to_string (Statechart_layout.layout spec))
    )
    goldens

let view () =
  let _, _, spec =
    List.find (fun (name, _, _) -> name = "ui_check/toast") goldens
  in
  let layout = Statechart_layout.layout spec in
  let marked =
    Render.to_string
      (Statechart_view.view ~current:"running" ~taken:("running", "expired")
         layout
      )
  in
  let plain = Render.to_string (Statechart_view.view layout) in
  Alcotest.(check int)
    "a group per state" 4
    (occurrences "class=\"chart-node" marked);
  Alcotest.(check int)
    "a group per edge" 11
    (occurrences "class=\"chart-edge" marked);
  Alcotest.(check int) "one active" 1 (occurrences "is-active" marked);
  Alcotest.(check int)
    "active state" 1
    (occurrences "class=\"chart-node is-active\" data-state=\"running\"" marked);
  Alcotest.(check int) "one taken" 1 (occurrences "is-taken" marked);
  Alcotest.(check int)
    "taken edge" 1
    (occurrences "is-taken\" data-from=\"running\" data-target=\"expired\""
       marked
    );
  Alcotest.(check int)
    "nothing marked" 0
    (occurrences "is-active" plain + occurrences "is-taken" plain);
  Alcotest.(check int) "one start mark" 1 (occurrences "chart-start" plain);
  Alcotest.(check int) "labels" 1 (occurrences ">focus true, hover true<" plain);
  Alcotest.(check int)
    "a box under each label of an edge between two states" 10
    (occurrences "rx=\"3\"" plain);
  Alcotest.(check int) "no note" 0 (occurrences "chart-note" plain);
  let _, _, router =
    List.find (fun (name, _, _) -> name = "linear_check/router") goldens
  in
  let router =
    Render.to_string (Statechart_view.view (Statechart_layout.layout router))
  in
  Alcotest.(check int)
    "the shared events once" 1
    (occurrences "class=\"chart-note\"" router
    * occurrences ">every edge: url changed<" router
    );
  Alcotest.(check int) "no edge label" 0 (occurrences ">url changed<" router);
  Alcotest.(check int) "details" 1 (occurrences ">after 100ms<" plain);
  Alcotest.(check int)
    "accessible name" 1
    (occurrences
       "aria-label=\"Statechart of the machine toast. Current state: running\""
       marked
    )

let kept_steps () =
  let _, _, spec =
    List.find (fun (name, _, _) -> name = "ui_check/note-dialog") goldens
  in
  let layout = Statechart_layout.layout spec in
  Alcotest.(check (list string))
    "the open node lists its kept steps"
    [
      "hold focus_trap";
      "hold scroll_lock";
      "hold dismiss";
      "stay on close_request escape-key,";
      "  close_request outside-press,";
      "  edit";
    ]
    (find_node layout "open").details;
  Alcotest.(check int)
    "the drawing shows that Escape keeps the dialog open" 1
    (occurrences ">stay on close_request escape-key,<"
       (Render.to_string (Statechart_view.view layout))
    )

let () =
  Alcotest.run "statechart"
    [
      ( "spec",
        [
          Alcotest.test_case "text of the goldens" `Quick text_fixpoint;
          Alcotest.test_case "text round trip" `Quick text_round_trip;
          Alcotest.test_case "text errors" `Quick text_errors;
          Alcotest.test_case "json round trip" `Quick json_round_trip;
          Alcotest.test_case "json errors" `Quick json_errors;
          Alcotest.test_case "mermaid" `Quick mermaid;
        ]
      );
      ( "layout",
        [
          Alcotest.test_case "every state and edge" `Quick every_state_and_edge;
          Alcotest.test_case "no overlap" `Quick no_overlap;
          Alcotest.test_case "edges on borders" `Quick edges_on_borders;
          Alcotest.test_case "edges avoid nodes" `Quick edges_avoid_nodes;
          Alcotest.test_case "labels are clear" `Quick labels_are_clear;
          Alcotest.test_case "labels on their edge" `Quick labels_on_their_edge;
          Alcotest.test_case "labels in the middle" `Quick labels_in_the_middle;
          Alcotest.test_case "shared events once" `Quick shared_events_once;
          Alcotest.test_case "drift" `Quick drift;
          Alcotest.test_case "two way pairs apart" `Quick two_way_pairs_apart;
          Alcotest.test_case "self edges apart" `Quick self_edges_apart;
          Alcotest.test_case "deterministic" `Quick deterministic;
        ]
      );
      ( "view",
        [
          Alcotest.test_case "marks" `Quick view;
          Alcotest.test_case "kept steps" `Quick kept_steps;
        ]
      );
    ]
