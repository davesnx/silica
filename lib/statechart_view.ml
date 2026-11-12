let number value =
  let tenths = int_of_float (Float.round (value *. 10.)) in
  if tenths mod 10 = 0 then
    string_of_int (tenths / 10)
  else
    Printf.sprintf "%s%d.%d"
      ( if tenths < 0 then
          "-"
        else
          ""
      )
      (abs tenths / 10)
      (abs tenths mod 10)

let pair (x, y) = number x ^ " " ^ number y

let path points =
  match points with
  | [] ->
      ""
  | first :: rest ->
      let _, parts =
        List.fold_left
          (fun ((last_x, last_y), parts) (x, y) ->
            let part =
              if y = last_y then
                "L " ^ pair (x, y)
              else
                let middle = (last_x +. x) /. 2. in
                Printf.sprintf "C %s, %s, %s"
                  (pair (middle, last_y))
                  (pair (middle, y))
                  (pair (x, y))
            in
            ((x, y), part :: parts)
          )
          (first, []) rest
      in
      String.concat " " (("M " ^ pair first) :: List.rev parts)

let loop_path = function
  | [ start; first; second; finish ] ->
      Printf.sprintf "M %s C %s, %s, %s" (pair start) (pair first) (pair second)
        (pair finish)
  | points ->
      path points

let color name fallback = Printf.sprintf "var(--ui-%s, %s)" name fallback
let text_color = color "text" "#111827"
let muted = color "muted" "#6b7280"
let border = color "border" "#d1d5db"
let surface = color "surface" "#ffffff"
let accent = color "accent" "#4f46e5"
let accent_text = color "accent-text" "#ffffff"
let danger = color "danger" "#dc2626"
let mono = "font-family: ui-monospace, SFMono-Regular, Menlo, monospace"

let classes names =
  String.concat " "
    (List.filter_map
       (fun (name, on) ->
         if on then
           Some name
         else
           None
       )
       names
    )

let text ~x ~y ~size ~fill ?(weight = "normal") content =
  Node.element "text"
    [
      Attr.string "x" (number x);
      Attr.string "y" (number y);
      Attr.string "text-anchor" "middle";
      Attr.string "style"
        (Printf.sprintf "%s; font-size: %dpx; font-weight: %s; fill: %s" mono
           size weight fill
        );
    ]
    [ Node.text content ]

let marker id fill =
  Node.element "marker"
    [
      Attr.string "id" id;
      Attr.string "viewBox" "0 0 10 10";
      Attr.string "refX" "9";
      Attr.string "refY" "5";
      Attr.string "markerWidth" "7";
      Attr.string "markerHeight" "7";
      Attr.string "orient" "auto-start-reverse";
    ]
    [
      Node.element "path"
        [
          Attr.string "d" "M 0 0 L 10 5 L 0 10 z";
          Attr.string "style" ("fill: " ^ fill);
        ]
        [];
    ]

let label_lines ~fill (label : Statechart_layout.label) =
  let count = List.length label.lines in
  List.mapi
    (fun line content ->
      text ~x:label.center
        ~y:(label.bottom -. 3. -. (float_of_int (count - 1 - line) *. 14.))
        ~size:11 ~fill content
    )
    label.lines

let label_chip (label : Statechart_layout.label) =
  let height = 14. *. float_of_int (List.length label.lines) in
  Node.element "rect"
    [
      Attr.string "x" (number (label.center -. (label.width /. 2.)));
      Attr.string "y" (number (label.bottom -. height));
      Attr.string "width" (number label.width);
      Attr.string "height" (number height);
      Attr.string "rx" "3";
      Attr.string "style" ("fill: " ^ surface);
    ]
    []

let edge ~taken (edge : Statechart_layout.edge) =
  let is_taken = taken = Some (edge.from, edge.target) in
  let stroke =
    if is_taken then
      accent
    else if edge.drift then
      danger
    else
      muted
  in
  let label =
    match edge.label with
    | None ->
        []
    | Some label when edge.from = edge.target ->
        label_lines ~fill:stroke label
    | Some label ->
        label_chip label :: label_lines ~fill:stroke label
  in
  Node.element "g"
    [
      Attr.string "class"
        (classes
           [
             ("chart-edge", true);
             ("is-taken", is_taken);
             ("is-back", edge.back);
             ("is-drift", edge.drift);
           ]
        );
      Attr.string "data-from" edge.from;
      Attr.string "data-target" edge.target;
    ]
    (Node.element "path"
       [
         Attr.string "d"
           ( if edge.from = edge.target then
               loop_path edge.points
             else
               path edge.points
           );
         Attr.string "fill" "none";
         Attr.string "style"
           (Printf.sprintf "stroke: %s; stroke-width: %s%s" stroke
              ( if is_taken then
                  "2.5"
                else
                  "1.5"
              )
              ( if edge.drift then
                  "; stroke-dasharray: 5 4"
                else
                  ""
              )
           );
         Attr.string "marker-end"
           ( if is_taken then
               "url(#chart-arrow-taken)"
             else if edge.drift then
               "url(#chart-arrow-drift)"
             else
               "url(#chart-arrow)"
           );
       ]
       []
    :: label
    )

let start (node : Statechart_layout.node) =
  let middle = node.y +. (node.height /. 2.) in
  Node.element "g"
    [ Attr.string "class" "chart-start" ]
    [
      Node.element "circle"
        [
          Attr.string "cx" (number (node.x -. 24.));
          Attr.string "cy" (number middle);
          Attr.string "r" "4";
          Attr.string "style" ("fill: " ^ text_color);
        ]
        [];
      Node.element "path"
        [
          Attr.string "d" (path [ (node.x -. 20., middle); (node.x, middle) ]);
          Attr.string "style" ("stroke: " ^ text_color ^ "; stroke-width: 1.5");
          Attr.string "marker-end" "url(#chart-arrow)";
        ]
        [];
    ]

let node ~current (node : Statechart_layout.node) =
  let active = current = Some node.name in
  let rect ~inset =
    Node.element "rect"
      [
        Attr.string "x" (number (node.x +. inset));
        Attr.string "y" (number (node.y +. inset));
        Attr.string "width" (number (node.width -. (2. *. inset)));
        Attr.string "height" (number (node.height -. (2. *. inset)));
        Attr.string "rx" (number (Float.max 2. (8. -. inset)));
        Attr.string "style"
          (Printf.sprintf "fill: %s; stroke: %s; stroke-width: 1.5%s"
             ( if active && inset = 0. then
                 accent
               else if inset = 0. then
                 surface
               else
                 "none"
             )
             ( if active && inset > 0. then
                 accent_text
               else if active then
                 accent
               else if node.drift then
                 danger
               else
                 border
             )
             ( if node.drift then
                 "; stroke-dasharray: 5 4"
               else
                 ""
             )
          );
      ]
      []
  in
  let middle = node.x +. (node.width /. 2.) in
  let content = 36. +. (14. *. float_of_int (List.length node.details)) in
  let top = node.y +. ((node.height -. content) /. 2.) in
  Node.element "g"
    [
      Attr.string "class"
        (classes
           [
             ("chart-node", true);
             ("is-active", active);
             ("is-final", node.final);
             ("is-initial", node.initial);
             ("is-drift", node.drift);
           ]
        );
      Attr.string "data-state" node.name;
    ]
    (rect ~inset:0.
     ::
     ( if node.final then
         [ rect ~inset:3. ]
       else
         []
     )
    @ text ~x:middle ~y:(top +. 23.) ~size:13
        ~fill:
          ( if active then
              accent_text
            else
              text_color
          )
        ~weight:
          ( if active then
              "600"
            else
              "normal"
          )
        node.name
      :: List.mapi
           (fun line detail ->
             text ~x:middle
               ~y:(top +. 39. +. (float_of_int line *. 14.))
               ~size:11
               ~fill:
                 ( if active then
                     accent_text
                   else
                     muted
                 )
               detail
           )
           node.details
    )

let view ?current ?taken (layout : Statechart_layout.t) =
  let description =
    match current with
    | None ->
        Printf.sprintf "Statechart of the machine %s" layout.machine
    | Some state ->
        Printf.sprintf "Statechart of the machine %s. Current state: %s"
          layout.machine state
  in
  Node.element "svg"
    [
      Attr.string "xmlns" "http://www.w3.org/2000/svg";
      Attr.string "viewBox"
        (Printf.sprintf "0 0 %s %s" (number layout.width) (number layout.height));
      Attr.string "width" (number layout.width);
      Attr.string "height" (number layout.height);
      Attr.string "role" "img";
      Attr.string "aria-label" description;
      Attr.string "class" "chart";
      Attr.string "data-machine" layout.machine;
    ]
    [
      Node.element "defs" []
        [
          marker "chart-arrow" muted;
          marker "chart-arrow-taken" accent;
          marker "chart-arrow-drift" danger;
        ];
      Node.list (List.map (edge ~taken) layout.edges);
      Node.list
        (List.filter_map
           (fun (node : Statechart_layout.node) ->
             if node.initial then
               Some (start node)
             else
               None
           )
           layout.nodes
        );
      Node.list (List.map (node ~current) layout.nodes);
      ( match layout.note with
      | None ->
          Node.list []
      | Some note ->
          Node.element "g"
            [ Attr.string "class" "chart-note" ]
            (label_lines ~fill:muted note)
      );
    ]
