open Silica

type segment = Index of int | Key of string

type running = {
  instance : string;
  listed : int;
  world : int;
  final_sent : bool;
}

type 'n joint = { root : 'n; running : running list }

type ('p, 's, 'e) runtime = {
  index : int;
  options : ('p, 's, 'e) Check_model.listed;
  machine : ('p, 's, 'e) Machine.t;
  witness : ('p * 's * 'e) Type.Id.t;
  host : 'e Check_model.host;
  recorder : Check_model.recorder;
  worlds : ('p, 's, 'e) Check_model.world Check_model.table;
  mutable drawn : int;
}

type packed = Runtime : ('p, 's, 'e) runtime -> packed

type 'ev binding =
  | Binding : {
      runtime : ('p, 's, 'e) runtime;
      output : 'e -> 's -> 'ev list;
      on_final : 's -> 'ev list;
      subtree : 'ev Node.t;
    }
      -> 'ev binding

type ('ev, 'e) resolution = {
  view : 'ev Node.t;
  own : 'e Node.t;
  running : running list;
  bindings : (string * 'ev binding) list;
}

type ('ev, 'e) effects = {
  mutable pending : 'ev list;
  mutable actions : 'ev Action.host list;
  mutable navigation : 'e Action.host list;
  mutable root_actions : 'e Action.host list;
  mutable remounts : string list;
}

let effects () =
  {
    pending = [];
    actions = [];
    navigation = [];
    remounts = [];
    root_actions = [];
  }

let rec restrict_attr : type a b.
    (a -> b option) -> a Node.attr -> b Node.attr option =
 fun keep attr ->
  let event make value = Option.map make (keep value) in
  let handler (handler : (_, a) Node.handler) : (_, b) Node.handler =
    {
      handler with
      run = (fun payload -> Option.bind (handler.run payload) keep);
    }
  in
  match attr with
  | Node.String (name, value) ->
      Some (Node.String (name, value))
  | Node.Bool (name, value) ->
      Some (Node.Bool (name, value))
  | Node.Ref reference ->
      Some (Node.Ref reference)
  | Node.On_press value ->
      event (fun value -> Node.On_press value) value
  | Node.On_double_press value ->
      event (fun value -> Node.On_double_press value) value
  | Node.On_submit value ->
      event (fun value -> Node.On_submit value) value
  | Node.On_focus value ->
      event (fun value -> Node.On_focus value) value
  | Node.On_blur value ->
      event (fun value -> Node.On_blur value) value
  | Node.On_input input -> (
      match keep (input "") with
      | Some _ ->
          Some
            (Node.On_input
               (fun text ->
                 match keep (input text) with
                 | Some value ->
                     value
                 | None ->
                     invalid_arg
                       "Check_tree: an input handler changed its owner"
               )
            )
      | None ->
          None
    )
  | Node.On_key_down key ->
      Some (Node.On_key_down (handler key))
  | Node.On_key_up key ->
      Some (Node.On_key_up (handler key))
  | Node.On_pointer_enter pointer ->
      Some (Node.On_pointer_enter (handler pointer))
  | Node.On_pointer_leave pointer ->
      Some (Node.On_pointer_leave (handler pointer))
  | Node.On_reset ->
      Some Node.On_reset
  | Node.Rated (rate, inner) ->
      Option.map
        (fun inner -> Node.Rated (rate, inner))
        (restrict_attr keep inner)

let rec restrict : type a b. (a -> b option) -> a Node.t -> b Node.t =
 fun keep node ->
  match Node.view node with
  | Node.Text text ->
      Node.text text
  | Node.Element (tag, attrs, children) ->
      Node.element tag
        (List.filter_map (restrict_attr keep) attrs)
        (List.map (restrict keep) children)
  | Node.List children ->
      Node.list (List.map (restrict keep) children)
  | Node.Keyed (key, child) ->
      Node.keyed key (restrict keep child)
  | Node.Portal (target, children) ->
      Node.portal ?target (List.map (restrict keep) children)
  | Node.Boundary { fallback; body } ->
      Node.boundary
        ~fallback:(fun error ~reset:_ ->
          restrict keep (fallback error ~reset:Node.On_reset)
        )
        (restrict keep body)
  | Node.Component child ->
      Node.component_with_props ?key:child.key ~props:child.props
        ~output:(fun event snapshot ->
          List.filter_map keep (child.output event snapshot)
        )
        ~on_final:(fun snapshot ->
          List.filter_map keep (child.on_final snapshot)
        )
        child.component
  | Node.Chunk (Node.Chunk_view { chunk; loading; render }) ->
      Node.chunk chunk ~loading:(restrict keep loading) (fun value ->
          restrict keep (render value)
      )

let strip node = restrict (fun _ -> None) node

let percent_encode key =
  let buffer = Buffer.create (String.length key) in
  String.iter
    (function
      | ('%' | '/' | ':' | '#' | '.') as character ->
          Buffer.add_string buffer
            (Printf.sprintf "%%%02X" (Char.code character))
      | character ->
          Buffer.add_char buffer character
      )
    key;
  Buffer.contents buffer

let segment_name = function
  | Index index ->
      string_of_int index
  | Key key ->
      "#" ^ percent_encode key

let instance_name ~owner slot machine =
  owner ^ "/"
  ^ String.concat "." (List.rev_map segment_name slot)
  ^ ":" ^ machine

let descends ~from instance =
  let prefix = from ^ "/" in
  String.length instance > String.length prefix
  && String.sub instance 0 (String.length prefix) = prefix

let typed_event : type p s e.
    (p, s, e) runtime -> Check_model.child_event -> e option =
 fun runtime (Child_event { witness; event; _ }) ->
  match Type.Id.provably_equal witness runtime.witness with
  | Some Type.Equal ->
      Some event
  | None ->
      None

let in_child ~instance ~machine ~state (finding : Check_rules.finding) =
  let message, hint = Check_rules.render finding ~state in
  {
    finding with
    custom =
      Some
        ( Printf.sprintf "In the child %s at instance %s: %s" machine instance
            message,
          hint
        );
    values = [];
  }

let joint (type n s ev e)
    ~(own_actions : (ev Action.host list -> e Action.host list) ref) ~keys
    ~texts ~arrivals ~(own : e -> ev) ~(own_of : ev -> e option)
    ~(into : Check_model.child_event -> ev)
    ~(out_of : ev -> Check_model.child_event option)
    ?(link : (string -> ev) option) ~(children : Check_model.child list)
    (model : (n, s, ev, e) Check_model.model) :
    (n joint, s, ev, ev) Check_model.model =
  let all_texts =
    texts
    @ List.concat_map (fun (Check_model.Child listed) -> listed.texts) children
  in
  let runtimes =
    Array.of_list
      (List.mapi
         (fun index (Check_model.Child listed) ->
           let machine = listed.component.machine in
           Runtime
             {
               index;
               options = listed;
               machine;
               witness = Machine.type_id machine;
               host = Check_model.host ~samples:listed.samples ~arrivals;
               recorder = Check_model.recorder ();
               worlds = Check_model.table ();
               drawn = 0;
             }
         )
         children
      )
  in
  let runtime_of : type p r c. (p, r, c) Machine.t -> (p, r, c) runtime option =
   fun machine ->
    let id = Machine.type_id machine in
    Array.fold_left
      (fun (found : (p, r, c) runtime option) (Runtime runtime) ->
        match found with
        | Some _ ->
            found
        | None -> (
            match Type.Id.provably_equal runtime.witness id with
            | Some Type.Equal ->
                Some runtime
            | None ->
                if Machine.id runtime.machine = Machine.id machine then
                  invalid_arg
                    ("Silica.Instance: two machines with the id \""
                   ^ Machine.id machine
                   ^ "\" in one tree. Make each machine once, at the top \
                      level, and give two machines two ids."
                    );
                None
          )
      )
      None runtimes
  in
  let world_of : type p r c.
      (p, r, c) runtime -> running -> (p, r, c) Check_model.world =
   fun runtime entry -> Dynarray.get runtime.worlds.values entry.world
  in
  let lift_child : type p r c. (p, r, c) runtime -> string -> c -> ev =
   fun runtime instance event ->
    into
      (Child_event
         {
           instance;
           machine = Machine.id runtime.machine;
           witness = runtime.witness;
           event;
         }
      )
  in
  let navigation_of : type c. c Action.host list -> e Action.host list =
   fun actions ->
    List.filter_map
      (function
        | Action.Run (Action.Navigate url) ->
            Some (Action.Run (Action.Navigate url))
        | Action.Run (Action.Replace url) ->
            Some (Action.Run (Action.Replace url))
        | Action.Run Action.Back ->
            Some (Action.Run Action.Back)
        | Action.Run Action.Forward ->
            Some (Action.Run Action.Forward)
        | Action.Run Action.Reload ->
            Some (Action.Run Action.Reload)
        | _ ->
            None
        )
      actions
  in
  let remounts actions =
    List.exists
      (function Action.Run Action.Remount -> true | _ -> false)
      actions
  in
  let emit_child : type p r c.
      (ev, e) effects ->
      (p, r, c) runtime ->
      string ->
      c Action.host list ->
      unit =
   fun effects runtime instance actions ->
    effects.actions <-
      List.rev_append
        (List.map (Action.map_host (lift_child runtime instance)) actions)
        effects.actions;
    effects.navigation <-
      List.rev_append (navigation_of actions) effects.navigation;
    if remounts actions then effects.remounts <- instance :: effects.remounts
  in
  let emit_root effects actions =
    effects.actions <-
      List.rev_append (List.map (Action.map_host own) actions) effects.actions;
    effects.root_actions <- List.rev_append actions effects.root_actions;
    if remounts actions then
      effects.remounts <- model.machine :: effects.remounts
  in
  let queue effects events = effects.pending <- effects.pending @ events in
  let resolve (effects : (ev, e) effects) (node : n joint) : (ev, e) resolution
      =
    let removed =
      List.filter
        (fun entry ->
          List.exists
            (fun from -> descends ~from entry.instance)
            effects.remounts
        )
        node.running
    in
    effects.remounts <- [];
    let start =
      List.filter (fun entry -> not (List.memq entry removed)) node.running
    in
    let count = Array.length runtimes in
    let pass old =
      let claimed = ref [] in
      let seen = ref [] in
      let bindings = ref [] in
      let frozen = Array.make count 0 in
      let running index =
        List.length (List.filter (fun entry -> entry.listed = index) !claimed)
      in
      let rec walk : type x.
          lift:(x -> ev) ->
          owner:string ->
          frozen:bool ->
          x Node.t ->
          segment list ->
          ev Node.t =
       fun ~lift ~owner ~frozen:inert node path ->
        let walk_all children =
          List.mapi
            (fun index child ->
              walk ~lift ~owner ~frozen:inert child (Index index :: path)
            )
            children
        in
        match Node.view node with
        | Node.Text text ->
            Node.text text
        | Node.Element (tag, attrs, children) ->
            Node.element tag
              (List.map (Node.map_attr lift) attrs)
              (walk_all children)
        | Node.List children ->
            Node.list (walk_all children)
        | Node.Keyed (key, child) ->
            Node.keyed key
              (walk ~lift ~owner ~frozen:inert child (Key key :: path))
        | Node.Chunk (Node.Chunk_view { chunk; loading; render }) -> (
            match Chunk.get chunk with
            | Some value ->
                walk ~lift ~owner ~frozen:inert (render value) (Index 0 :: path)
            | None ->
                walk ~lift ~owner ~frozen:inert loading (Index 1 :: path)
          )
        | Node.Portal (target, children) ->
            Node.portal ?target (walk_all children)
        | Node.Boundary { fallback; body } ->
            Node.boundary
              ~fallback:(fun error ~reset:_ ->
                Node.map lift (fallback error ~reset:Node.On_reset)
              )
              (walk ~lift ~owner ~frozen:inert body (Index 0 :: path))
        | Node.Component child ->
            place ~lift ~owner ~frozen:inert node child path
      and place : type x p r c.
          lift:(x -> ev) ->
          owner:string ->
          frozen:bool ->
          x Node.t ->
          (p, r, c, x) Node.child ->
          segment list ->
          ev Node.t =
       fun ~lift ~owner ~frozen:inert node child path ->
        let slot =
          match (child.key, path) with
          | Some key, _ :: parent ->
              Key key :: parent
          | Some key, [] ->
              [ Key key ]
          | None, _ ->
              path
        in
        let machine = child.component.machine in
        let instance = instance_name ~owner slot (Machine.id machine) in
        if List.mem instance !seen then
          invalid_arg
            "Silica.Instance: two components have the same position and key";
        seen := instance :: !seen;
        let keyed resolved =
          match child.key with
          | Some key ->
              Node.keyed key resolved
          | None ->
              resolved
        in
        match
          if inert then
            None
          else
            runtime_of machine
        with
        | None ->
            Node.map lift node
        | Some runtime ->
            let existing =
              List.find_opt
                (fun entry ->
                  entry.instance = instance && entry.listed = runtime.index
                )
                old
            in
            let waiting =
              List.length
                (List.filter
                   (fun entry ->
                     entry.listed = runtime.index
                     && (not
                           (List.exists
                              (fun (taken : running) ->
                                taken.instance = entry.instance
                              )
                              !claimed
                           )
                        )
                     && entry.instance <> instance
                   )
                   old
                )
            in
            let room =
              runtime.options.max_instances - running runtime.index - waiting
            in
            if existing = None && room <= 0 then (
              frozen.(runtime.index) <- frozen.(runtime.index) + 1;
              let snapshot, _ = Machine.start machine child.props in
              keyed
                (walk
                   ~lift:(lift_child runtime instance)
                   ~owner:instance ~frozen:true
                   (child.component.view child.props snapshot)
                   []
                )
            ) else
              let world, final_sent =
                match existing with
                | Some entry ->
                    let world = world_of runtime entry in
                    if world.props <> child.props then (
                      let snapshot, actions, _ =
                        Check_model.change_props runtime.recorder machine world
                          child.props
                      in
                      emit_child effects runtime instance actions;
                      ( List.fold_left
                          (Check_model.host_action runtime.host)
                          { world with s = snapshot; props = child.props }
                          actions,
                        entry.final_sent
                      )
                    ) else
                      (world, entry.final_sent)
                | None ->
                    let snapshot, actions = Machine.start machine child.props in
                    Check_model.started runtime.recorder machine snapshot;
                    Check_model.see runtime.recorder machine child.props
                      snapshot;
                    emit_child effects runtime instance actions;
                    ( List.fold_left
                        (Check_model.host_action runtime.host)
                        (Check_model.empty_world child.props snapshot)
                        actions,
                      false
                    )
              in
              let final_sent =
                if (not final_sent) && Machine.is_final machine world.s then (
                  queue effects (List.map lift (child.on_final world.s));
                  true
                ) else
                  final_sent
              in
              claimed :=
                {
                  instance;
                  listed = runtime.index;
                  world = Check_model.intern runtime.worlds world;
                  final_sent;
                }
                :: !claimed;
              let subtree =
                walk
                  ~lift:(lift_child runtime instance)
                  ~owner:instance ~frozen:false
                  (child.component.view world.props world.s)
                  []
              in
              bindings :=
                ( instance,
                  Binding
                    {
                      runtime;
                      output =
                        (fun event snapshot ->
                          List.map lift (child.output event snapshot)
                        );
                      on_final =
                        (fun snapshot -> List.map lift (child.on_final snapshot));
                      subtree;
                    }
                )
                :: !bindings;
              keyed subtree
      in
      let own_view = model.view node.root in
      let view =
        walk ~lift:own ~owner:model.machine ~frozen:false own_view []
      in
      (own_view, view, List.rev !claimed, !bindings, frozen)
    in
    let rec settle old round =
      let own, view, claimed, bindings, frozen = pass old in
      let retry =
        Array.exists
          (fun (Runtime runtime) ->
            frozen.(runtime.index) > 0
            && List.length
                 (List.filter
                    (fun (entry : running) -> entry.listed = runtime.index)
                    claimed
                 )
               < runtime.options.max_instances
          )
          runtimes
      in
      if retry && round < count + 2 then
        settle claimed (round + 1)
      else (
        Array.iter
          (fun (Runtime runtime) ->
            runtime.drawn <-
              max runtime.drawn
                (frozen.(runtime.index)
                + List.length
                    (List.filter
                       (fun (entry : running) -> entry.listed = runtime.index)
                       claimed
                    )
                )
          )
          runtimes;
        List.iter
          (fun (entry : running) ->
            if
              not
                (List.exists
                   (fun (taken : running) -> taken.instance = entry.instance)
                   claimed
                )
            then
              let (Runtime runtime) = runtimes.(entry.listed) in
              let world = world_of runtime entry in
              emit_child effects runtime entry.instance
                (Machine.exit runtime.machine world.props world.s)
          )
          node.running;
        {
          view;
          own;
          running =
            List.sort
              (fun (first : running) (second : running) ->
                String.compare first.instance second.instance
              )
              claimed;
          bindings;
        }
      )
    in
    settle start 0
  in
  let step_child effects (node : n joint) bindings child_event via =
    let (Check_model.Child_event { instance; _ }) = child_event in
    match
      ( List.find_opt
          (fun (entry : running) -> entry.instance = instance)
          node.running,
        List.assoc_opt instance bindings
      )
    with
    | Some entry, Some (Binding binding) -> (
        match typed_event binding.runtime child_event with
        | None ->
            (node, Machine.Unhandled)
        | Some event ->
            let runtime = binding.runtime in
            let world = Check_model.consume (world_of runtime entry) via in
            let snapshot, actions, how =
              Check_model.send ?finished:(Check_model.finished via)
                runtime.recorder runtime.machine world event
            in
            let world =
              List.fold_left
                (Check_model.host_action runtime.host)
                { world with s = snapshot }
                actions
            in
            emit_child effects runtime instance actions;
            let final_sent =
              match how with
              | Machine.Unhandled ->
                  entry.final_sent
              | Machine.Kept | Machine.Entered ->
                  queue effects (binding.output event snapshot);
                  if
                    (not entry.final_sent)
                    && Machine.is_final runtime.machine snapshot
                  then (
                    queue effects (binding.on_final snapshot);
                    true
                  ) else
                    entry.final_sent
            in
            let entry =
              {
                entry with
                world = Check_model.intern runtime.worlds world;
                final_sent;
              }
            in
            ( {
                node with
                running =
                  List.map
                    (fun (other : running) ->
                      if other.instance = instance then
                        entry
                      else
                        other
                    )
                    node.running;
              },
              how
            )
      )
    | _ ->
        (node, Machine.Unhandled)
  in
  let deliver effects (node : n joint) bindings event =
    match out_of event with
    | Some child_event ->
        fst (step_child effects node bindings child_event Check_model.By_input)
    | None ->
        let root, actions, _ =
          model.step node.root { event; via = Check_model.By_input }
        in
        emit_root effects actions;
        { node with root }
  in
  let settle effects (node : n joint) bindings =
    let rec round count node bindings =
      let rec drain node =
        match effects.pending with
        | [] ->
            node
        | event :: rest ->
            effects.pending <- rest;
            drain (deliver effects node bindings event)
      in
      let node = drain node in
      let resolution = resolve effects node in
      let node = { node with running = resolution.running } in
      if effects.pending = [] then
        (node, resolution)
      else if count >= 100 then
        failwith
          "Silica.Instance: components keep raising events for each other"
      else
        round (count + 1) node resolution.bindings
    in
    let rec navigate count (node, resolution) =
      match effects.navigation with
      | [] ->
          (node, resolution)
      | navigation ->
          if count > 50 then failwith "navigation loop: actions keep navigating";
          effects.navigation <- [];
          let root, actions =
            model.tree.navigate node.root (List.rev navigation)
          in
          emit_root effects actions;
          navigate (count + 1) (round 0 { node with root } resolution.bindings)
    in
    navigate 0 (round 0 node bindings)
  in
  let cache = ref [] in
  let remember node resolution =
    cache :=
      (node, resolution)
      :: (match !cache with first :: _ -> [ first ] | [] -> [])
  in
  let resolved node =
    match List.find_opt (fun (known, _) -> known == node) !cache with
    | Some (_, resolution) ->
        resolution
    | None ->
        let resolution = resolve (effects ()) node in
        remember node resolution;
        resolution
  in
  let roots = Hashtbl.create 1024 in
  let started_actions = ref [] and stepped_actions = ref [] in
  let returned keep effects =
    let actions = List.rev effects.actions in
    let entry = (actions, List.rev effects.root_actions) in
    ( match keep with
    | `Start ->
        started_actions := entry :: !started_actions
    | `Step ->
        stepped_actions :=
          entry :: List.filteri (fun index _ -> index < 15) !stepped_actions
    );
    actions
  in
  (own_actions :=
     fun actions ->
       match
         List.find_opt
           (fun (known, _) -> known == actions)
           (!stepped_actions @ !started_actions)
       with
       | Some (_, root) ->
           root
       | None ->
           if actions = [] then
             []
           else
             invalid_arg "Check_tree: the actions of a step that is not recent"
  );
  let start () =
    List.map
      (fun (root, actions) ->
        let effects = effects () in
        emit_root effects actions;
        let node, resolution = settle effects { root; running = [] } [] in
        remember node resolution;
        (node, returned `Start effects)
      )
      (model.start ())
  in
  let step node (move : ev Check_model.move) =
    let bindings = (resolved node).bindings in
    let effects = effects () in
    let node, how =
      match out_of move.event with
      | Some child_event ->
          step_child effects node bindings child_event move.via
      | None ->
          let root, actions, how = model.step node.root move in
          emit_root effects actions;
          ({ node with root }, how)
    in
    let node, resolution = settle effects node bindings in
    remember node resolution;
    (node, returned `Step effects, how)
  in
  let entry_of (node : n joint) instance =
    List.find_opt
      (fun (entry : running) -> entry.instance = instance)
      node.running
  in
  let child_moves (node : n joint) (resolution : (ev, e) resolution) view =
    let running instance = List.mem_assoc instance resolution.bindings in
    List.filter_map
      (fun event ->
        match out_of event with
        | Some (Child_event { instance; _ }) when running instance ->
            Some { Check_model.event; via = Check_model.By_view }
        | Some _ | None ->
            None
      )
      (Check_rules.handlers ~keys view)
    @ List.concat_map
        (fun (Runtime runtime) ->
          let machine = Machine.id runtime.machine in
          List.filter_map
            (fun event ->
              match out_of event with
              | Some (Child_event { instance; machine = owner; _ })
                when owner = machine && running instance ->
                  Some { Check_model.event; via = Check_model.By_typing }
              | Some _ | None ->
                  None
            )
            (Check_rules.typed ~texts:runtime.options.texts view)
        )
        (Array.to_list runtimes)
    @ List.concat_map
        (fun (entry : running) ->
          match List.assoc_opt entry.instance resolution.bindings with
          | None ->
              []
          | Some (Binding { runtime; _ }) ->
              let world = world_of runtime entry in
              let lift = lift_child runtime entry.instance in
              List.map
                (fun event ->
                  { Check_model.event = lift event; via = Check_model.By_input }
                )
                (Check_model.source_events
                   (Machine.held runtime.machine world.props world.s)
                @ runtime.options.inputs world.s
                )
              @ List.map
                  (fun (move : _ Check_model.move) ->
                    { move with event = lift move.event }
                  )
                  (Check_model.resource_moves runtime.host world)
        )
        node.running
  in
  let enabled node view =
    let resolution = resolved node in
    Hashtbl.replace roots node.root ();
    Check_rules.dedup
      (model.enabled node.root resolution.own
      @ ( match link with
        | Some link ->
            List.map
              (fun href ->
                { Check_model.event = link href; via = Check_model.By_view }
              )
              (Check_rules.reachable_links view)
        | None ->
            []
        )
      @ child_moves node resolution view
      )
  in
  let binding_of node instance =
    match
      (entry_of node instance, List.assoc_opt instance (resolved node).bindings)
    with
    | Some entry, Some binding ->
        Some (entry, binding)
    | _ ->
        None
  in
  let cause node view (move : ev Check_model.move) =
    match out_of move.event with
    | Some (Child_event { instance; _ } as child_event) -> (
        match move.via with
        | Check_model.By_view | Check_model.By_typing ->
            Check_model.view_cause ~keys ~texts:all_texts view move.event
        | via -> (
            match binding_of node instance with
            | Some (entry, Binding { runtime; _ }) -> (
                match typed_event runtime child_event with
                | Some event ->
                    Check_model.resource_cause runtime.machine
                      (world_of runtime entry) via event
                | None ->
                    Check_model.Input
              )
            | None ->
                Check_model.Input
          )
      )
    | None -> (
        match (move.via, own_of move.event) with
        | (Check_model.By_view | Check_model.By_typing), Some _ ->
            Check_model.view_cause ~keys ~texts:all_texts view move.event
        | _ ->
            model.cause node.root (strip view) move
      )
  in
  let state_of node instance =
    match binding_of node instance with
    | Some (entry, Binding { runtime; _ }) ->
        Some (Machine.name runtime.machine (world_of runtime entry).s)
    | None ->
        None
  in
  let child_rules (node : n joint) _ event =
    let resolution = resolved node in
    List.concat_map
      (fun (entry : running) ->
        match List.assoc_opt entry.instance resolution.bindings with
        | None ->
            []
        | Some (Binding { runtime; subtree; _ }) ->
            let world = world_of runtime entry in
            let state = Machine.name runtime.machine world.s in
            let mine event =
              match out_of event with
              | Some (Child_event { instance; _ } as child_event)
                when instance = entry.instance ->
                  typed_event runtime child_event
              | Some _ | None ->
                  None
            in
            List.map
              (in_child ~instance:entry.instance
                 ~machine:(Machine.id runtime.machine)
                 ~state
              )
              (Check_model.owner_rules runtime.machine world
              @ runtime.options.check ~snapshot:world.s
                  ~view:(restrict mine subtree) ~event:(Option.bind event mine)
                  ~name:state
              )
      )
      node.running
  in
  let event_name event =
    match out_of event with
    | Some (Child_event { witness; event; _ }) ->
        Array.fold_left
          (fun name (Runtime runtime) ->
            match name with
            | Some _ ->
                name
            | None -> (
                match Type.Id.provably_equal witness runtime.witness with
                | Some Type.Equal ->
                    Some (Machine.event_name runtime.machine event)
                | None ->
                    None
              )
          )
          None runtimes
        |> Option.value ~default:""
    | None ->
        model.event_name event
  in
  let started (Runtime runtime) = runtime.recorder.first <> None in
  {
    Check_model.machine = model.machine;
    initial = model.initial;
    initial_name = model.initial_name;
    start;
    view = (fun node -> (resolved node).view);
    enabled;
    step;
    final = (fun node -> model.final node.root);
    name = (fun node -> model.name node.root);
    snapshot = (fun node -> model.snapshot node.root);
    event_name;
    event_of = (fun event -> Some event);
    cause;
    state_rules =
      (fun node view ->
        let children_held =
          List.concat_map
            (fun (entry : running) ->
              let (Runtime runtime) = runtimes.(entry.listed) in
              let world = world_of runtime entry in
              List.map
                (Action.map_source (lift_child runtime entry.instance))
                (Machine.held runtime.machine world.props world.s)
            )
            node.running
        in
        let untrapped =
          Check_accessibility.dialog_focus ~held:children_held view
        in
        List.filter
          (fun (finding : Check_rules.finding) ->
            finding.rule <> "dialog-focus" || List.mem finding untrapped
          )
          (model.state_rules node.root (strip view))
      );
    action_rules =
      (fun ~values view actions -> Check_rules.focus_target ~values view actions);
    setup_findings = model.setup_findings;
    specs =
      (fun () ->
        model.specs ()
        @ List.filter_map
            (fun (Runtime runtime as packed) ->
              if started packed then
                Some (Check_model.spec_of runtime.recorder runtime.machine)
              else
                None
            )
            (Array.to_list runtimes)
      );
    unreached =
      (fun () ->
        model.unreached ()
        @ List.concat_map
            (fun (Runtime runtime as packed) ->
              if started packed then
                List.map
                  (fun state -> Machine.id runtime.machine ^ ": " ^ state)
                  (Check_model.unreached_of runtime.recorder runtime.machine)
              else
                []
            )
            (Array.to_list runtimes)
      );
    answer =
      (fun node (move : ev Check_model.move) ->
        match out_of move.event with
        | Some (Child_event { instance; _ } as child_event) -> (
            match binding_of node instance with
            | Some (entry, Binding { runtime; _ }) ->
                Option.bind (typed_event runtime child_event) (fun event ->
                    Check_model.answer_json runtime.host
                      (world_of runtime entry) move.via event
                )
            | None ->
                None
          )
        | None ->
            model.answer node.root move
      );
    props_json = (fun node -> model.props_json node.root);
    tree =
      {
        actor =
          (fun event ->
            match out_of event with
            | Some (Child_event { instance; machine; _ }) ->
                Some (instance, machine)
            | None ->
                None
          );
        state_of;
        rules = child_rules;
        inside =
          (fun node ->
            List.for_all
              (fun (entry : running) ->
                let (Runtime runtime) = runtimes.(entry.listed) in
                runtime.options.within (world_of runtime entry).s
              )
              node.running
          );
        not_run =
          (fun () ->
            List.filter_map
              (fun (Runtime runtime) ->
                if runtime.drawn > runtime.options.max_instances then
                  Some
                    (Printf.sprintf
                       "component %s (up to %d instances were shown at once, \
                        and only the first %d ran, as ~max_instances:%d of its \
                        Check.child says; the others showed their start view \
                        and took no step. Raise ~max_instances to %d to run \
                        them all; the ~within of the parent bounds its own \
                        state, not how many children run)"
                       (Machine.id runtime.machine)
                       runtime.drawn runtime.options.max_instances
                       runtime.options.max_instances runtime.drawn
                    )
                else
                  None
              )
              (Array.to_list runtimes)
          );
        counts =
          (fun () ->
            (model.machine, Hashtbl.length roots)
            :: List.map
                 (fun (Runtime runtime) ->
                   ( Machine.id runtime.machine,
                     Dynarray.length runtime.worlds.values
                   )
                 )
                 (Array.to_list runtimes)
          );
        navigate = (fun node _ -> (node, []));
      };
  }
