type node = {
  name : string;
  x : float;
  y : float;
  width : float;
  height : float;
  final : bool;
  initial : bool;
  details : string list;
  drift : bool;
}

type label = {
  lines : string list;
  center : float;
  bottom : float;
  width : float;
}

type edge = {
  from : string;
  target : string;
  points : (float * float) list;
  back : bool;
  label : label option;
  drift : bool;
}

type t = {
  machine : string;
  width : float;
  height : float;
  nodes : node list;
  edges : edge list;
  note : label option;
}

let name_character = 7.8
let small_character = 6.6
let line_height = 14.
let padding_x = 12.
let padding_y = 10.
let name_height = 16.
let minimum_width = 72.
let spacing = 48.
let gap = 16.
let margin = 16.
let start_margin = 40.
let label_room = 4.
let loop_foot = 14.
let loop_reach = 30.
let loop_rise = 30.
let loop_label = 32.
let wrap_limit = 24
let port_inset = 4.
let port_step = 9.

let characters text =
  let count = ref 0 in
  String.iter
    (fun character -> if Char.code character land 0xC0 <> 0x80 then incr count)
    text;
  !count

let wrap events =
  List.rev
    (List.fold_left
       (fun lines event ->
         match lines with
         | current :: rest
           when characters current + 2 + characters event <= wrap_limit ->
             (current ^ ", " ^ event) :: rest
         | current :: rest ->
             event :: (current ^ ",") :: rest
         | [] ->
             [ event ]
       )
       [] events
    )

let caption_half lines =
  (float_of_int (List.length lines) *. line_height /. 2.) +. label_room

let text_width lines =
  List.fold_left
    (fun widest line ->
      Float.max widest (float_of_int (characters line) *. small_character)
    )
    0. lines
  +. 8.

type kind = State of int | Caption of int | Bend

type item = {
  kind : kind;
  rank : int;
  width : float;
  above : float;
  below : float;
}

type route = {
  position : int;
  source : int;
  destination : int;
  events : string list;
  back : bool;
  drift : bool;
}

let median values =
  let sorted = Array.of_list (List.sort Float.compare values) in
  let count = Array.length sorted in
  if count mod 2 = 1 then
    sorted.(count / 2)
  else
    (sorted.((count / 2) - 1) +. sorted.(count / 2)) /. 2.

let isotonic values =
  let count = Array.length values in
  let means = Array.make count 0. in
  let sizes = Array.make count 0 in
  let blocks = ref 0 in
  Array.iter
    (fun value ->
      means.(!blocks) <- value;
      sizes.(!blocks) <- 1;
      incr blocks;
      while !blocks > 1 && means.(!blocks - 2) > means.(!blocks - 1) do
        let left = !blocks - 2 and right = !blocks - 1 in
        let total = sizes.(left) + sizes.(right) in
        means.(left) <-
          ((means.(left) *. float_of_int sizes.(left))
          +. (means.(right) *. float_of_int sizes.(right))
          )
          /. float_of_int total;
        sizes.(left) <- total;
        decr blocks
      done
    )
    values;
  let result = Array.make count 0. in
  let next = ref 0 in
  for block = 0 to !blocks - 1 do
    for _ = 1 to sizes.(block) do
      result.(!next) <- means.(block);
      incr next
    done
  done;
  result

let layout ?(observed = []) (spec : Spec.t) =
  let in_spec name =
    List.exists (fun (state : Spec.state) -> state.name = name) spec.states
  in
  let unknown =
    List.fold_left
      (fun found name ->
        if in_spec name || List.mem name found then
          found
        else
          found @ [ name ]
      )
      []
      (List.concat_map (fun (source, target) -> [ source; target ]) observed)
  in
  let states =
    Array.of_list
      (spec.states
      @ List.map
          (fun name ->
            { Spec.name; final = false; timers = []; sources = []; held = [] }
          )
          unknown
      )
  in
  let checked = List.length spec.states in
  let count = Array.length states in
  let index name =
    let rec find position =
      if position >= count then
        None
      else if states.(position).name = name then
        Some position
      else
        find (position + 1)
    in
    find 0
  in
  let events_of (source, destination) =
    List.sort_uniq compare
      (List.filter_map
         (fun (label : Spec.label) ->
           if
             label.from = source && label.target = destination
             && label.event <> "" && not label.kept
           then
             Some label.event
           else
             None
         )
         spec.edges
      )
  in
  let arrows = Spec.arrows spec in
  let drifted =
    List.filter
      (fun pair -> not (List.mem pair arrows))
      (List.sort_uniq compare observed)
  in
  let known =
    List.filter_map
      (fun ((source, destination), drift) ->
        match (index source, index destination) with
        | Some source_index, Some destination_index ->
            Some
              ( source_index,
                destination_index,
                events_of (source, destination),
                drift
              )
        | _ ->
            None
      )
      (List.map (fun pair -> (pair, false)) arrows
      @ List.map (fun pair -> (pair, true)) drifted
      )
  in
  let every_edge =
    match
      List.filter_map
        (fun (_, _, events, drift) ->
          if drift then
            None
          else
            Some events
        )
        known
    with
    | first :: _ :: _ :: _ as all
      when first <> [] && List.for_all (( = ) first) all ->
        first
    | _ ->
        []
  in
  let known =
    if every_edge = [] then
      known
    else
      List.map
        (fun (source, destination, _, drift) -> (source, destination, [], drift))
        known
  in
  let numbered = List.mapi (fun position edge -> (position, edge)) known in
  let loops =
    List.filter_map
      (fun (position, (source, destination, events, drift)) ->
        if source = destination then
          Some (position, source, events, drift)
        else
          None
      )
      numbered
  in
  let links =
    List.filter
      (fun (_, (source, destination, _, _)) -> source <> destination)
      numbered
  in
  let visit_state = Array.make count 0 in
  let discovery = Array.make count 0 in
  let discovered = ref 0 in
  let backs = ref [] in
  let rec visit state =
    visit_state.(state) <- 1;
    discovery.(state) <- !discovered;
    incr discovered;
    List.iter
      (fun (position, (source, destination, _, _)) ->
        if source = state then
          match visit_state.(destination) with
          | 0 ->
              visit destination
          | 1 ->
              backs := position :: !backs
          | _ ->
              ()
      )
      links;
    visit_state.(state) <- 2
  in
  Option.iter visit (index spec.init);
  for state = 0 to count - 1 do
    if visit_state.(state) = 0 then visit state
  done;
  let routes =
    List.map
      (fun (position, (source, destination, events, drift)) ->
        {
          position;
          source;
          destination;
          events;
          back = List.mem position !backs;
          drift;
        }
      )
      links
  in
  let lower route =
    if route.back then
      route.destination
    else
      route.source
  in
  let upper route =
    if route.back then
      route.source
    else
      route.destination
  in
  let layer = Array.make count 0 in
  for _ = 1 to count do
    List.iter
      (fun route ->
        layer.(upper route) <- max layer.(upper route) (layer.(lower route) + 1)
      )
      routes
  done;
  let loop_lines state =
    List.concat_map
      (fun (_, source, events, _) ->
        if source = state then
          wrap events
        else
          []
      )
      loops
  in
  let has_loop state =
    List.exists (fun (_, source, _, _) -> source = state) loops
  in
  let node_size state =
    let details = Spec.details states.(state) in
    let width =
      Float.max minimum_width
        (Float.max
           (float_of_int (characters states.(state).name) *. name_character)
           (List.fold_left
              (fun widest line ->
                Float.max widest
                  (float_of_int (characters line) *. small_character)
              )
              0. details
           )
        +. (2. *. padding_x)
        )
    in
    let ports side =
      List.length (List.filter (fun route -> side route = state) routes)
    in
    let height =
      Float.max
        ((2. *. padding_y) +. name_height
        +. (float_of_int (List.length details) *. line_height)
        )
        ((2. *. port_inset)
        +. (port_step *. float_of_int (max (ports lower) (ports upper)))
        )
    in
    (width, height)
  in
  let items = ref [] in
  let item_count = ref 0 in
  let add item =
    items := item :: !items;
    incr item_count;
    !item_count - 1
  in
  for state = 0 to count - 1 do
    let width, height = node_size state in
    let loop_extra, loop_width =
      if has_loop state then
        let lines = loop_lines state in
        ( loop_label +. (float_of_int (List.length lines) *. line_height),
          Float.max ((2. *. loop_reach) +. 8.) (text_width lines)
        )
      else
        (0., 0.)
    in
    ignore
      (add
         {
           kind = State state;
           rank = 2 * layer.(state);
           width = Float.max width loop_width;
           above = (height /. 2.) +. loop_extra;
           below = height /. 2.;
         }
      )
  done;
  let chains =
    List.map
      (fun route ->
        let first = 2 * layer.(lower route)
        and last = 2 * layer.(upper route) in
        let gaps = (last - first) / 2 in
        let caption_rank =
          first + 1
          + 2
            *
            if route.back then
              (gaps - 1) / 2
            else
              gaps / 2
        in
        let middle =
          List.init
            (last - first - 1)
            (fun offset ->
              let rank = first + 1 + offset in
              if rank = caption_rank && route.events <> [] then
                let lines = wrap route.events in
                add
                  {
                    kind = Caption route.position;
                    rank;
                    width = text_width lines;
                    above = caption_half lines;
                    below = caption_half lines;
                  }
              else
                add { kind = Bend; rank; width = 0.; above = 2.; below = 2. }
            )
        in
        (route, (lower route :: middle) @ [ upper route ])
      )
      routes
  in
  let items = Array.of_list (List.rev !items) in
  let total = Array.length items in
  let left_neighbors = Array.make total [] in
  let right_neighbors = Array.make total [] in
  let segments = ref [] in
  List.iter
    (fun (_, chain) ->
      let rec link = function
        | first :: (second :: _ as rest) ->
            left_neighbors.(second) <- first :: left_neighbors.(second);
            right_neighbors.(first) <- second :: right_neighbors.(first);
            segments := (first, second) :: !segments;
            link rest
        | [ _ ] | [] ->
            ()
      in
      link chain
    )
    chains;
  let segments = List.rev !segments in
  let ranks =
    1 + Array.fold_left (fun highest item -> max highest item.rank) 0 items
  in
  let place = Array.make total 0 in
  let order = Array.make ranks [||] in
  let set_rank rank members =
    order.(rank) <- Array.of_list members;
    Array.iteri (fun position item -> place.(item) <- position) order.(rank)
  in
  let members rank =
    List.filter
      (fun item -> items.(item).rank = rank)
      (List.init total (fun item -> item))
  in
  let first_key item =
    match items.(item).kind with
    | State state ->
        discovery.(state)
    | Caption _ | Bend ->
        count + item
  in
  let barycenter neighbors item =
    match neighbors.(item) with
    | [] ->
        float_of_int place.(item)
    | list ->
        List.fold_left
          (fun sum other -> sum +. float_of_int place.(other))
          0. list
        /. float_of_int (List.length list)
  in
  let sort_rank neighbors rank members =
    set_rank rank
      (List.stable_sort
         (fun first second ->
           Float.compare
             (barycenter neighbors first)
             (barycenter neighbors second)
         )
         members
      )
  in
  for rank = 0 to ranks - 1 do
    let initial =
      List.sort
        (fun first second -> compare (first_key first) (first_key second))
        (members rank)
    in
    set_rank rank initial;
    if rank > 0 then sort_rank left_neighbors rank initial
  done;
  (* ponytail: crossings are counted over all segment pairs and again after
     each trial swap, O(segments^2) per count. Fine for the 12 states and about
     40 edges of today's machines; past a few hundred edges, count with the
     bilayer method of Barth, Juenger and Mutzel and update it per swap. *)
  let crossings () =
    let total = ref 0 in
    List.iter
      (fun (first_left, first_right) ->
        List.iter
          (fun (second_left, second_right) ->
            if
              items.(first_left).rank = items.(second_left).rank
              && place.(first_left) < place.(second_left)
              && place.(first_right) > place.(second_right)
            then
              incr total
          )
          segments
      )
      segments;
    !total
  in
  let snapshot () = Array.map Array.copy order in
  let restore saved =
    Array.iteri
      (fun rank members -> set_rank rank (Array.to_list members))
      saved
  in
  let best = ref (snapshot ()) and fewest = ref (crossings ()) in
  for _ = 1 to 8 do
    for rank = 1 to ranks - 1 do
      sort_rank left_neighbors rank (Array.to_list order.(rank))
    done;
    for rank = ranks - 2 downto 0 do
      sort_rank right_neighbors rank (Array.to_list order.(rank))
    done;
    let now = crossings () in
    if now < !fewest then (
      fewest := now;
      best := snapshot ()
    )
  done;
  restore !best;
  let improved = ref true and rounds = ref 0 in
  while !improved && !rounds < 20 do
    improved := false;
    incr rounds;
    for rank = 0 to ranks - 1 do
      for position = 0 to Array.length order.(rank) - 2 do
        let members = order.(rank) in
        let first = members.(position) and second = members.(position + 1) in
        let before = !fewest in
        members.(position) <- second;
        members.(position + 1) <- first;
        place.(second) <- position;
        place.(first) <- position + 1;
        let after = crossings () in
        if after < before then (
          fewest := after;
          improved := true
        ) else (
          members.(position) <- first;
          members.(position + 1) <- second;
          place.(first) <- position;
          place.(second) <- position + 1
        )
      done
    done
  done;
  let level = Array.make total 0. in
  let arrange rank desired =
    let members = order.(rank) in
    let size = Array.length members in
    if size > 0 then (
      let offsets = Array.make size 0. in
      for position = 1 to size - 1 do
        offsets.(position) <-
          offsets.(position - 1)
          +. items.(members.(position - 1)).below +. gap
          +. items.(members.(position)).above
      done;
      let fitted =
        isotonic
          (Array.mapi
             (fun position value -> value -. offsets.(position))
             desired
          )
      in
      Array.iteri
        (fun position item ->
          level.(item) <- fitted.(position) +. offsets.(position)
        )
        members
    )
  in
  for rank = 0 to ranks - 1 do
    arrange rank (Array.map (fun _ -> 0.) order.(rank))
  done;
  let pull rank =
    arrange rank
      (Array.map
         (fun item ->
           match left_neighbors.(item) @ right_neighbors.(item) with
           | [] ->
               level.(item)
           | neighbors ->
               median (List.map (fun other -> level.(other)) neighbors)
         )
         order.(rank)
      )
  in
  for iteration = 1 to 12 do
    if iteration mod 2 = 1 then
      for rank = 0 to ranks - 1 do
        pull rank
      done
    else
      for rank = ranks - 1 downto 0 do
        pull rank
      done
  done;
  let top =
    Array.fold_left Float.min Float.infinity
      (Array.mapi (fun item value -> value -. items.(item).above) level)
  in
  let bottom =
    Array.fold_left Float.max Float.neg_infinity
      (Array.mapi (fun item value -> value +. items.(item).below) level)
  in
  let shift = margin -. top in
  Array.iteri (fun item value -> level.(item) <- value +. shift) level;
  let column_width =
    Array.map
      (fun members ->
        Array.fold_left
          (fun widest item -> Float.max widest items.(item).width)
          0. members
      )
      order
  in
  let column_left = Array.make ranks start_margin in
  for rank = 1 to ranks - 1 do
    column_left.(rank) <-
      column_left.(rank - 1) +. column_width.(rank - 1) +. spacing
  done;
  let column_right rank = column_left.(rank) +. column_width.(rank) in
  let center item =
    column_left.(items.(item).rank) +. (column_width.(items.(item).rank) /. 2.)
  in
  let sizes = Array.init count node_size in
  let middle_of chain =
    List.filter
      (fun item ->
        match items.(item).kind with
        | State _ ->
            false
        | Caption _ | Bend ->
            true
      )
      chain
  in
  let attachments =
    List.concat_map
      (fun (route, chain) ->
        let middle = middle_of chain in
        [
          (lower route, true, level.(List.hd middle), route.position);
          ( upper route,
            false,
            level.(List.nth middle (List.length middle - 1)),
            route.position
          );
        ]
      )
      chains
  in
  let port state ~right position =
    let mine =
      List.stable_sort compare
        (List.filter_map
           (fun (owner, side, toward, key) ->
             if owner = state && side = right then
               Some (toward, key)
             else
               None
           )
           attachments
        )
    in
    let rec find place = function
      | (_, key) :: _ when key = position ->
          place
      | _ :: rest ->
          find (place + 1) rest
      | [] ->
          0
    in
    let width, height = sizes.(state) in
    let top = level.(state) -. (height /. 2.) +. port_inset in
    let room = height -. (2. *. port_inset) in
    ( ( if right then
          center state +. (width /. 2.)
        else
          center state -. (width /. 2.)
      ),
      top
      +. room
         *. float_of_int (find 0 mine + 1)
         /. float_of_int (List.length mine + 1)
    )
  in
  let rec dedupe = function
    | ((first_x, first_y) as first) :: ((second_x, second_y) as second) :: rest
      when Float.abs (first_x -. second_x) < 0.01
           && Float.abs (first_y -. second_y) < 0.01 ->
        dedupe
          (( if rest = [] then
               second
             else
               first
           )
          :: rest
          )
    | first :: rest ->
        first :: dedupe rest
    | [] ->
        []
  in
  let route_edges =
    List.map
      (fun (route, chain) ->
        let first = List.hd chain in
        let last = List.nth chain (List.length chain - 1) in
        let middle = middle_of chain in
        let middle_points =
          List.concat_map
            (fun item ->
              let rank = items.(item).rank in
              [
                (column_left.(rank), level.(item));
                (column_right rank, level.(item));
              ]
            )
            middle
        in
        let start_x, start_y = port first ~right:true route.position in
        let end_x, end_y = port last ~right:false route.position in
        let points =
          dedupe
            ([ (start_x, start_y); (column_right items.(first).rank, start_y) ]
            @ middle_points
            @ [ (column_left.(items.(last).rank), end_y); (end_x, end_y) ]
            )
        in
        let label =
          List.find_map
            (fun item ->
              match items.(item).kind with
              | Caption _ ->
                  let lines = wrap route.events in
                  Some
                    {
                      lines;
                      center = center item;
                      bottom = level.(item) +. caption_half lines -. label_room;
                      width = text_width lines;
                    }
              | State _ | Bend ->
                  None
            )
            middle
        in
        ( route.position,
          {
            from = states.(route.source).name;
            target = states.(route.destination).name;
            points =
              ( if route.back then
                  List.rev points
                else
                  points
              );
            back = route.back;
            label;
            drift = route.drift;
          }
        )
      )
      chains
  in
  let loop_edges =
    List.map
      (fun (position, state, events, drift) ->
        let _, height = sizes.(state) in
        let middle = center state and top = level.(state) -. (height /. 2.) in
        ( position,
          {
            from = states.(state).name;
            target = states.(state).name;
            points =
              [
                (middle -. loop_foot, top);
                (middle -. loop_reach, top -. loop_rise);
                (middle +. loop_reach, top -. loop_rise);
                (middle +. loop_foot, top);
              ];
            back = false;
            label =
              ( match events with
              | [] ->
                  None
              | events ->
                  Some
                    {
                      lines = wrap events;
                      center = middle;
                      bottom = top -. loop_label;
                      width = text_width (wrap events);
                    }
              );
            drift;
          }
        )
      )
      loops
  in
  let edges =
    List.map snd
      (List.sort
         (fun (first, _) (second, _) -> compare first second)
         (route_edges @ loop_edges)
      )
  in
  let nodes =
    List.init count (fun state ->
        let width, height = sizes.(state) in
        let source = states.(state) in
        {
          name = source.name;
          x = center state -. (width /. 2.);
          y = level.(state) -. (height /. 2.);
          width;
          height;
          final = source.final;
          initial = source.name = spec.init;
          details = Spec.details source;
          drift = state >= checked;
        }
    )
  in
  let drawing_width =
    if ranks = 0 then
      start_margin +. margin
    else
      column_right (ranks - 1) +. margin
  in
  let drawing_height = bottom -. top +. (2. *. margin) in
  let note =
    match every_edge with
    | [] ->
        None
    | events ->
        let lines = [ "every edge: " ^ String.concat ", " events ] in
        let width = text_width lines in
        Some
          {
            lines;
            center = margin +. (width /. 2.);
            bottom = drawing_height -. margin +. line_height +. 8.;
            width;
          }
  in
  {
    machine = spec.id;
    width =
      ( match note with
      | None ->
          drawing_width
      | Some note ->
          Float.max drawing_width (note.width +. (2. *. margin))
      );
    height =
      ( match note with
      | None ->
          drawing_height
      | Some note ->
          note.bottom +. margin
      );
    nodes;
    edges;
    note;
  }

let round value = string_of_int (int_of_float (Float.round value))
let point (x, y) = round x ^ "," ^ round y

let to_string t =
  let buffer = Buffer.create 1024 in
  Printf.bprintf buffer "machine %s %s %s\n" t.machine (round t.width)
    (round t.height);
  List.iter
    (fun node ->
      Printf.bprintf buffer "node %s %s %s %s%s%s%s\n" node.name
        (point (node.x, node.y))
        (round node.width) (round node.height)
        ( if node.initial then
            " initial"
          else
            ""
        )
        ( if node.final then
            " final"
          else
            ""
        )
        ( if node.drift then
            " drift"
          else
            ""
        )
    )
    t.nodes;
  List.iter
    (fun edge ->
      Printf.bprintf buffer "edge %s -> %s%s%s %s%s\n" edge.from edge.target
        ( if edge.back then
            " back"
          else
            ""
        )
        ( if edge.drift then
            " drift"
          else
            ""
        )
        (String.concat " " (List.map point edge.points))
        ( match edge.label with
        | None ->
            ""
        | Some label ->
            Printf.sprintf " label %s %s"
              (point (label.center, label.bottom))
              (String.concat " | " label.lines)
        )
    )
    t.edges;
  Option.iter
    (fun note ->
      Printf.bprintf buffer "note %s %s\n"
        (point (note.center, note.bottom))
        (String.concat " | " note.lines)
    )
    t.note;
  Buffer.contents buffer
