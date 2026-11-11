type link = { source : int; edge : int }
type labels = { at_start : bool array; at_edge : Bytes.t }

type counterexample =
  | Path of link list
  | Waits of link list
  | Loop of { stem : link list; cycle : link list }

type finding = { node : int; counterexample : counterexample }

let on labels edge = Bytes.get labels.at_edge edge = '\001'

let resource (via : Check_model.via) =
  match via with
  | By_timer id ->
      Some ("timer " ^ id)
  | By_answer id ->
      Some ("call " ^ id)
  | By_result _ ->
      Some "action result"
  | By_view | By_input | By_feed _ ->
      None

let edges (graph : (_, _) Check_search.graph) node =
  if node < graph.expanded then
    (graph.first.(node), graph.first.(node + 1) - 1)
  else
    (0, -1)

let running (graph : (_, _) Check_search.graph) node =
  let low, high = edges graph node in
  let rec go edge acc =
    if edge > high then
      List.sort_uniq String.compare acc
    else
      go (edge + 1)
        ( match resource graph.moves.(edge).via with
        | Some id ->
            id :: acc
        | None ->
            acc
        )
  in
  go low []

let may_stop graph node =
  node < graph.Check_search.expanded && running graph node = []

type product = {
  parent : int array;
  through : int array;
  seen : Bytes.t;
  order : int Dynarray.t;
}

let product count =
  {
    parent = Array.make (2 * count) (-1);
    through = Array.make (2 * count) (-1);
    seen = Bytes.make (2 * count) '\000';
    order = Dynarray.create ();
  }

let visited product state = Bytes.get product.seen state = '\001'

let visit product queue state ~from ~edge =
  if not (visited product state) then (
    Bytes.set product.seen state '\001';
    product.parent.(state) <- from;
    product.through.(state) <- edge;
    Dynarray.add_last product.order state;
    Queue.add state queue
  )

let links product state =
  let rec go state acc =
    match product.parent.(state) with
    | -1 ->
        acc
    | from ->
        go from ({ source = from / 2; edge = product.through.(state) } :: acc)
  in
  go state []

let explore (graph : (_, _) Check_search.graph) ~count ~start ~next ~violation =
  let product = product count in
  let queue = Queue.create () in
  List.iteri
    (fun position ({ node; _ } : _ Check_search.graph_start) ->
      let bit = start position in
      violation ~state:None ~edge:None ~node ~bit;
      visit product queue ((2 * node) + Bool.to_int bit) ~from:(-1) ~edge:(-1)
    )
    graph.starts;
  while not (Queue.is_empty queue) do
    let state = Queue.pop queue in
    let low, high = edges graph (state / 2) in
    for edge = low to high do
      let target = graph.targets.(edge) in
      if target >= 0 then (
        let bit = next (state land 1 = 1) edge in
        violation ~state:(Some state) ~edge:(Some edge) ~node:target ~bit;
        visit product queue ((2 * target) + Bool.to_int bit) ~from:state ~edge
      )
    done
  done;
  product

let never_after graph ~count ~name ~after ~never =
  let found = ref [] in
  let names = Hashtbl.create 8 in
  let violation ~state ~edge ~node ~bit =
    let breaks =
      bit && match edge with Some edge -> on never edge | None -> false
    in
    if breaks && not (Hashtbl.mem names (name node)) then (
      Hashtbl.add names (name node) ();
      found := (node, state, edge) :: !found
    )
  in
  let start_breaks = ref [] in
  List.iteri
    (fun position ({ node; _ } : _ Check_search.graph_start) ->
      if after.at_start.(position) && never.at_start.(position) then
        start_breaks := node :: !start_breaks
    )
    graph.Check_search.starts;
  List.iter
    (fun node ->
      if not (Hashtbl.mem names (name node)) then (
        Hashtbl.add names (name node) ();
        found := (node, None, None) :: !found
      )
    )
    (List.rev !start_breaks);
  let explored =
    explore graph ~count
      ~start:(fun position -> after.at_start.(position))
      ~next:(fun armed edge -> armed || on after edge)
      ~violation
  in
  List.rev_map
    (fun (node, state, edge) ->
      let links =
        match (state, edge) with
        | Some state, Some edge ->
            links explored state @ [ { source = state / 2; edge } ]
        | _ ->
            []
      in
      { node; counterexample = Path links }
    )
    !found

let route (graph : (_, _) Check_search.graph) ~inside ~from ~goal =
  let parent = Hashtbl.create 64 in
  let queue = Queue.create () in
  Queue.add from queue;
  Hashtbl.replace parent from None;
  let rec back node acc =
    match Hashtbl.find parent node with
    | None ->
        acc
    | Some link ->
        back link.source (link :: acc)
  in
  let rec search () =
    if Queue.is_empty queue then
      None
    else
      let node = Queue.pop queue in
      let low, high = edges graph node in
      let rec scan edge =
        if edge > high then
          search ()
        else if inside edge then (
          let link = { source = node; edge } in
          if goal edge then
            Some (back node [ link ])
          else
            let target = graph.targets.(edge) in
            if not (Hashtbl.mem parent target) then (
              Hashtbl.replace parent target (Some link);
              Queue.add target queue
            );
            scan (edge + 1)
        ) else
          scan (edge + 1)
      in
      scan low
  in
  match search () with
  | Some path ->
      path
  | None ->
      invalid_arg
        "Check_temporal.route: the component is not strongly connected"

let components (graph : (_, _) Check_search.graph) ~count ~member ~inside =
  let index = Array.make count (-1) in
  let low = Array.make count 0 in
  let on_stack = Bytes.make count '\000' in
  let counter = ref 0 in
  let stack = ref [] in
  let found = ref [] in
  let work = Stack.create () in
  let push node =
    index.(node) <- !counter;
    low.(node) <- !counter;
    incr counter;
    stack := node :: !stack;
    Bytes.set on_stack node '\001';
    Stack.push (node, ref (fst (edges graph node))) work
  in
  for root = 0 to count - 1 do
    if member root && index.(root) = -1 then (
      push root;
      while not (Stack.is_empty work) do
        let node, next = Stack.top work in
        let _, high = edges graph node in
        if !next <= high then (
          let edge = !next in
          incr next;
          if inside edge then
            let target = graph.targets.(edge) in
            if index.(target) = -1 then
              push target
            else if Bytes.get on_stack target = '\001' then
              low.(node) <- min low.(node) index.(target)
        ) else (
          ignore (Stack.pop work);
          ( match Stack.top_opt work with
          | Some (parent, _) ->
              low.(parent) <- min low.(parent) low.(node)
          | None ->
              ()
          );
          if low.(node) = index.(node) then
            let rec pop acc =
              match !stack with
              | top :: rest ->
                  stack := rest;
                  Bytes.set on_stack top '\000';
                  if top = node then
                    top :: acc
                  else
                    pop (top :: acc)
              | [] ->
                  acc
            in
            found := pop [] :: !found
        )
      done
    )
  done;
  !found

let cycle graph ~inside ~entry ~fair_set =
  let current = ref entry in
  let taken = ref [] in
  let delivered = ref [] in
  let passed = ref [ entry ] in
  let walk path =
    List.iter
      (fun link ->
        let target = graph.Check_search.targets.(link.edge) in
        Option.iter
          (fun id -> delivered := id :: !delivered)
          (resource graph.moves.(link.edge).via);
        passed := target :: !passed;
        current := target
      )
      path;
    taken := !taken @ path
  in
  List.iter
    (fun id ->
      if List.mem id fair_set then (
        if not (List.mem id !delivered) then
          walk
            (route graph ~inside ~from:!current ~goal:(fun edge ->
                 resource graph.moves.(edge).via = Some id
             )
            )
      ) else if
          List.for_all (fun node -> List.mem id (running graph node)) !passed
        then
        walk
          (route graph ~inside ~from:!current ~goal:(fun edge ->
               not (List.mem id (running graph graph.targets.(edge)))
           )
          )
    )
    (running graph entry);
  if !current <> entry || !taken = [] then
    walk
      (route graph ~inside ~from:!current ~goal:(fun edge ->
           graph.targets.(edge) = entry
       )
      );
  !taken

let leads_to graph ~count ~name ~from ~to_ =
  let explored =
    explore graph ~count
      ~start:(fun position ->
        from.at_start.(position) && not to_.at_start.(position)
      )
      ~next:(fun waiting edge -> (waiting || on from edge) && not (on to_ edge))
      ~violation:(fun ~state:_ ~edge:_ ~node:_ ~bit:_ -> ())
  in
  let waiting node = visited explored ((2 * node) + 1) in
  let rank = Hashtbl.create 64 in
  Dynarray.iteri
    (fun position state ->
      if state land 1 = 1 then Hashtbl.replace rank (state / 2) position
    )
    explored.order;
  let names = Hashtbl.create 8 in
  let fresh kind node =
    let key = (kind, name node) in
    if Hashtbl.mem names key then
      false
    else (
      Hashtbl.add names key ();
      true
    )
  in
  let waits =
    Dynarray.fold_left
      (fun acc state ->
        let node = state / 2 in
        if state land 1 = 1 && may_stop graph node && fresh `Waits node then
          { node; counterexample = Waits (links explored state) } :: acc
        else
          acc
      )
      [] explored.order
  in
  let inside_waiting edge =
    graph.Check_search.targets.(edge) >= 0 && not (on to_ edge)
  in
  let loops =
    List.filter_map
      (fun members ->
        let set = Hashtbl.create (List.length members) in
        List.iter (fun node -> Hashtbl.replace set node ()) members;
        let inside edge =
          inside_waiting edge && Hashtbl.mem set graph.targets.(edge)
        in
        let internal =
          List.concat_map
            (fun node ->
              let low, high = edges graph node in
              List.filter inside (List.init (high - low + 1) (( + ) low))
            )
            members
        in
        if internal = [] || List.exists (may_stop graph) members then
          None
        else
          let always =
            List.fold_left
              (fun acc node ->
                List.filter (fun id -> List.mem id (running graph node)) acc
              )
              (running graph (List.hd members))
              members
          in
          let delivered =
            List.filter_map
              (fun edge -> resource graph.moves.(edge).via)
              internal
          in
          if List.for_all (fun id -> List.mem id delivered) always then
            let entry =
              List.fold_left
                (fun best node ->
                  if Hashtbl.find rank node < Hashtbl.find rank best then
                    node
                  else
                    best
                )
                (List.hd members) members
            in
            Some (Hashtbl.find rank entry, entry, inside, always)
          else
            None
      )
      (components graph ~count ~member:waiting ~inside:inside_waiting)
  in
  let loops =
    List.filter_map
      (fun (_, entry, inside, always) ->
        if fresh `Loop entry then
          Some
            {
              node = entry;
              counterexample =
                Loop
                  {
                    stem = links explored ((2 * entry) + 1);
                    cycle = cycle graph ~inside ~entry ~fair_set:always;
                  };
            }
        else
          None
      )
      (List.sort
         (fun (first, _, _, _) (second, _, _, _) -> compare first second)
         loops
      )
  in
  List.rev waits @ loops

let never_after_message =
  "The property \"{property}\" does not hold: state {state} matches its never \
   condition, and this step or an earlier one of the run matched its after \
   condition."

let never_after_hint =
  "Change the steps so that no run reaches state {state} with what the never \
   condition forbids once the after condition held, or correct the property."

let waits_message =
  "The property \"{property}\" does not hold: after its from condition held, \
   the run reaches state {state}, where no timer and no call runs, so the app \
   waits for the user, who may do nothing; its to_ condition did not hold on \
   the way."

let waits_hint =
  "Make state {state} own a timer or a call that leads to the to_ condition \
   (for example a retry with a limit, or a failure state that the to_ \
   condition accepts), or correct the property."

let loop_message =
  "The property \"{property}\" does not hold: after its from condition held, \
   the run can repeat {cycle} forever; each timer and call that runs all along \
   is delivered ({delivered}), so this run is fair, and its to_ condition \
   never holds."

let loop_hint =
  "Make a step of the cycle leave it toward the to_ condition: count the \
   repeats and stop after a limit, or handle the answer or timer that repeats \
   ({delivered}) by going on instead of starting it again, or correct the \
   property."
