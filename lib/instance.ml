type message = { run : unit -> unit }

type type_id = Type_id : 'a Type.Id.t -> type_id

let error_counter = ref 0
let seeded = ref false

let fresh_error_id () =
  if not !seeded then (
    seeded := true;
    Random.self_init ()
  );
  incr error_counter;
  let n = Random.bits () lxor !error_counter land 0xffffff in
  String.init 6 (fun i -> "0123456789abcdef".[(n lsr (4 * (5 - i))) land 15])

type segment = Index of int | Key of string

type boundary_cell = { mutable failure : Node.error option }

type change = Started | Resumed | Unhandled | Kept | Entered | Ended

type context = {
  queue : (unit -> unit) Queue.t;
  mutable actions : message Action.host list;
  mutable next_id : int;
  dev : bool;
  on_error : (exn -> Node.error -> unit) option;
  mutable waiting : int;
  machines : (string, type_id) Hashtbl.t;
  observer : observer option;
  mutable view : message Node.t option;
  mutable current : message option;
  mutable resume : resume option;
}

and resume = {
  take : 'p 's 'e. ('p, 's, 'e) asked -> ('s * 'e Action.host list) option;
}

and ('p, 's, 'e) asked = {
  above : packed option;
  at : segment list;
  starting : ('p, 's, 'e) Node.component;
  given : 'p;
}

and ('p, 's, 'e) cell = {
  id : int;
  slot : segment list;
  context : context;
  component : ('p, 's, 'e) Node.component;
  mutable snapshot : 's;
  mutable entry : int;
  mutable alive : bool;
  mutable remount : bool;
  mutable props : 'p;
  mutable notify : 'e -> 's -> unit;
  mutable notify_final : 's -> unit;
  mutable final_sent : bool;
  mutable children : ((segment list * string) * packed) list;
  mutable boundaries : (segment list * boundary_cell) list;
  mutable catch : (string * string -> exn -> unit) option;
  owned : (string, string) Hashtbl.t;
  mutable parent : packed option;
  mutable stale : bool;
  mutable rendering : bool;
  mutable last : ('s * message Node.t) option;
}

and packed = Packed : ('p, 's, 'e) cell -> packed

and cause =
  | Start
  | Props
  | End
  | Message
  | Handler
  | Element of message Node.attr * string option * Key.t option
  | Timer of string * int
  | Answer :
      string * ('i, 'o, 'x) Endpoint.t * ('o, 'x) Endpoint.answer
      -> cause
  | Live : string * 's Topic.t * 's Topic.update -> cause
  | Source : string * 'm Action.source -> cause
  | Result : 'm Action.t -> cause
  | Child : ('p, 's, 'e) cell -> cause
  | Route of Router.event

and observed =
  | Observed :
      ('p, 's, 'e) cell
      * Machine.how
      * 's
      * 'e option
      * message Action.host list
      * cause
      -> observed
  | Detached : {
      instance : string;
      machine : ('p, 's, 'e) Machine.t;
      props : 'p;
      entry : int;
      from : 's option;
      snapshot : 's;
      change : change;
      event : 'e option;
      actions : 'e Action.host list;
      cause : cause;
    }
      -> observed

and observer = {
  observe : observed -> unit;
  handler : 'e. (cause -> 'e -> message) -> 'e Node.attr -> message Node.attr;
}

type ('p, 's, 'e) t = { context : context; root : ('p, 's, 'e) cell }

let add_action context (action : message Action.host) =
  context.actions <- action :: context.actions

let rec until_physically stop = function
  | actions when actions == stop ->
      []
  | action :: rest ->
      action :: until_physically stop rest
  | [] ->
      []

let observe : type p s e.
    (p, s, e) cell ->
    Machine.how ->
    s ->
    e option ->
    message Action.host list ->
    cause ->
    unit =
 fun cell how from event before cause ->
  match cell.context.observer with
  | None ->
      ()
  | Some observer ->
      observer.observe (Observed (cell, how, from, event, before, cause))

let qualify cell local =
  string_of_int cell.id ^ "." ^ string_of_int cell.entry ^ "/" ^ local

let where : type p s e. (p, s, e) cell -> string * string =
 fun cell ->
  ( Machine.id cell.component.machine,
    Machine.name cell.component.machine cell.snapshot
  )

let make_error context (machine, state) exn =
  let id = fresh_error_id () in
  let error =
    {
      Node.id;
      message = "Something went wrong.";
      detail =
        ( if context.dev then
            Some
              (String.concat "\n"
                 [
                   Printexc.to_string exn;
                   "machine: " ^ machine;
                   "state: " ^ state;
                   Printexc.get_backtrace ();
                 ]
              )
          else
            None
        );
    }
  in
  ( match context.on_error with
  | Some hook ->
      hook exn error
  | None ->
      prerr_endline
        ("silica: error " ^ id ^ " in " ^ machine ^ " (" ^ state ^ "): "
       ^ Printexc.to_string exn
        )
  );
  error

let rec invalidate : type p s e. force:bool -> (p, s, e) cell -> unit =
 fun ~force cell ->
  if force || not (cell.stale || cell.rendering) then (
    cell.stale <- true;
    match cell.parent with
    | Some (Packed parent) ->
        invalidate ~force parent
    | None ->
        ()
  )

let rec step : type p s e.
    ?finished:string -> cause:cause -> (p, s, e) cell -> e -> unit =
 fun ?finished ~cause cell event ->
  if cell.alive then
    guard cell (fun () ->
        let from = cell.snapshot and before = cell.context.actions in
        match
          Machine.step_with_props ?finished cell.component.machine cell.props
            cell.snapshot event
        with
        | _, _, Machine.Unhandled ->
            observe cell Machine.Unhandled from (Some event) before cause
        | snapshot, actions, ((Machine.Kept | Machine.Entered) as how) ->
            apply cell snapshot actions how;
            observe cell how from (Some event) before cause;
            cell.notify event snapshot;
            check_final cell
    )

and guard : type p s e. (p, s, e) cell -> (unit -> unit) -> unit =
 fun cell run ->
  try run ()
  with exn -> (
    match cell.catch with
    | Some catch ->
        catch (where cell) exn
    | None ->
        raise exn
  )

and apply : type p s e.
    (p, s, e) cell -> s -> e Action.host list -> Machine.how -> unit =
 fun cell snapshot actions how ->
  cell.snapshot <- snapshot;
  Option.iter
    (fun (Packed parent) -> invalidate ~force:false parent)
    cell.parent;
  ( match how with
  | Machine.Entered ->
      cell.entry <- cell.entry + 1
  | Machine.Kept | Machine.Unhandled ->
      ()
  );
  emit cell actions

and check_final : type p s e. (p, s, e) cell -> unit =
 fun cell ->
  if
    (not cell.final_sent)
    && Machine.is_final cell.component.machine cell.snapshot
  then (
    cell.final_sent <- true;
    cell.notify_final cell.snapshot
  )

and timed : type p s e.
    ?finished:string -> cause:cause -> (p, s, e) cell -> e -> message =
 fun ?finished ~cause cell event ->
  let entry = cell.entry in
  {
    run = (fun () -> if cell.entry = entry then step ?finished ~cause cell event);
  }

and emit : type p s e. (p, s, e) cell -> e Action.host list -> unit =
 fun cell actions ->
  let add = add_action cell.context in
  List.iter
    (function[@warning "-4"]
      | Action.Start_timer { id; ms; event } ->
          let qualified = qualify cell id in
          Hashtbl.replace cell.owned id qualified;
          add
            (Action.Start_timer
               {
                 id = qualified;
                 ms;
                 event =
                   timed ~finished:id ~cause:(Timer (qualified, ms)) cell event;
               }
            )
      | Action.Start_source { id; source } ->
          let qualified = qualify cell id in
          Hashtbl.replace cell.owned id qualified;
          add
            (Action.Start_source
               {
                 id = qualified;
                 source =
                   Action.map_source
                     (timed ~cause:(Source (qualified, source)) cell)
                     source;
               }
            )
      | Action.Start_call { id; endpoint; input; timeout_ms; on_result } ->
          let qualified = qualify cell id in
          Hashtbl.replace cell.owned id qualified;
          add
            (Action.Start_call
               {
                 id = qualified;
                 endpoint;
                 input;
                 timeout_ms;
                 on_result =
                   (fun result ->
                     timed ~finished:id
                       ~cause:(Answer (qualified, endpoint, result))
                       cell (on_result result)
                   );
               }
            )
      | Action.Start_live { id; topic; key; on_update } ->
          let qualified = qualify cell id in
          Hashtbl.replace cell.owned id qualified;
          add
            (Action.Start_live
               {
                 id = qualified;
                 topic;
                 key;
                 on_update =
                   (fun update ->
                     timed
                       ~cause:(Live (qualified, topic, update))
                       cell (on_update update)
                   );
               }
            )
      | Action.Stop id -> (
          match Hashtbl.find_opt cell.owned id with
          | Some qualified ->
              Hashtbl.remove cell.owned id;
              add (Action.Stop qualified)
          | None ->
              ()
        )
      | Action.Run Action.Remount ->
          cell.remount <- true
      | Action.Run action ->
          add
            (Action.Run
               (Action.map
                  (fun event ->
                    { run = (fun () -> step ~cause:(Result action) cell event) }
                  )
                  action
               )
            )
      )
    actions

let to_message cause cell event = { run = (fun () -> step ~cause cell event) }

let percent_encode key =
  let buffer = Buffer.create (String.length key) in
  String.iter
    (function
      | ('%' | '/' | ':' | '#' | '.') as char ->
          let code = Char.code char in
          Buffer.add_char buffer '%';
          Buffer.add_char buffer "0123456789ABCDEF".[code lsr 4];
          Buffer.add_char buffer "0123456789ABCDEF".[code land 15]
      | char ->
          Buffer.add_char buffer char
      )
    key;
  Buffer.contents buffer

let child_path parent slot id =
  parent ^ "/"
  ^ String.concat "."
      (List.rev_map
         (function
           | Index index ->
               string_of_int index
           | Key key ->
               "#" ^ percent_encode key
           )
         slot
      )
  ^ ":" ^ id

let rec cell_path : type p s e. (p, s, e) cell -> string =
 fun cell ->
  let id = Machine.id cell.component.machine in
  match cell.parent with
  | None ->
      id
  | Some (Packed parent) ->
      child_path (cell_path parent) cell.slot id

let create : type p s e.
    context ->
    slot:segment list ->
    parent:packed option ->
    (p, s, e) Node.component ->
    p ->
    (p, s, e) cell =
 fun context ~slot ~parent component props ->
  let machine = component.machine in
  let id = Machine.id machine and type_id = Machine.type_id machine in
  let other (Type_id known) =
    Option.is_none (Type.Id.provably_equal known type_id)
  in
  ( match Hashtbl.find_opt context.machines id with
  | Some known when other known ->
      invalid_arg
        ("Silica.Instance: two machines with the id \"" ^ id
       ^ "\" in one tree. Make each machine once, at the top level, and give \
          two machines two ids."
        )
  | Some _ | None ->
      Hashtbl.replace context.machines id (Type_id type_id)
  );
  let taken =
    match context.resume with
    | None ->
        None
    | Some resume ->
        resume.take
          { above = parent; at = slot; starting = component; given = props }
  in
  let snapshot, actions =
    match taken with
    | Some started ->
        started
    | None ->
        Machine.start_with_props machine props
  in
  context.next_id <- context.next_id + 1;
  let cell =
    {
      id = context.next_id;
      slot;
      context;
      component;
      snapshot;
      entry = 0;
      alive = true;
      remount = false;
      props;
      notify = (fun _ _ -> ());
      notify_final = (fun _ -> ());
      final_sent = false;
      children = [];
      boundaries = [];
      catch = None;
      owned = Hashtbl.create 4;
      parent;
      stale = false;
      rendering = false;
      last = None;
    }
  in
  let before = context.actions in
  emit cell actions;
  observe cell
    (match taken with Some _ -> Machine.Entered | None -> Machine.Kept)
    snapshot None before Start;
  cell

let rec unmount_cell : type p s e. (p, s, e) cell -> unit =
 fun cell ->
  if cell.alive then (
    List.iter (fun (_, Packed child) -> unmount_cell child) cell.children;
    cell.children <- [];
    let before = cell.context.actions in
    emit cell
      (Machine.exit_with_props cell.component.machine cell.props cell.snapshot);
    cell.alive <- false;
    Hashtbl.iter
      (fun _ qualified -> add_action cell.context (Action.Stop qualified))
      cell.owned;
    Hashtbl.reset cell.owned;
    observe cell Machine.Kept cell.snapshot None before End
  )

let props_changed old_props new_props =
  try old_props <> new_props with Invalid_argument _ -> true

let set_cell_props : type p s e. (p, s, e) cell -> p -> unit =
 fun cell props ->
  if props_changed cell.props props then (
    let previous = cell.props in
    cell.props <- props;
    cell.stale <- true;
    guard cell (fun () ->
        let from = cell.snapshot and before = cell.context.actions in
        match
          Machine.set_props cell.component.machine ~previous props cell.snapshot
        with
        | _, _, Machine.Unhandled ->
            observe cell Machine.Unhandled from None before Props
        | snapshot, actions, ((Machine.Kept | Machine.Entered) as how) ->
            apply cell snapshot actions how;
            observe cell how from None before Props;
            check_final cell
    )
  )

let same_slot (path, machine) (other_path, other_machine) =
  path = other_path && String.equal machine other_machine

let same_cell : type p s e.
    (p, s, e) Machine.t -> packed -> (p, s, e) cell option =
 fun machine (Packed existing) ->
  match
    Type.Id.provably_equal
      (Machine.type_id existing.component.machine)
      (Machine.type_id machine)
  with
  | Some Type.Equal ->
      Some existing
  | None ->
      None

let find_child slot machine children =
  List.find_map
    (fun (taken, packed) ->
      if same_slot taken slot then
        same_cell machine packed
      else
        None
    )
    children

let rec render : type p s e. (p, s, e) cell -> message Node.t =
 fun cell ->
  match cell.last with
  | Some (snapshot, node)
    when snapshot == cell.snapshot && not (cell.stale || cell.remount) ->
      node
  | _ ->
      let snapshot = cell.snapshot in
      cell.stale <- false;
      cell.rendering <- true;
      let node =
        match render_view cell with
        | node ->
            cell.rendering <- false;
            node
        | exception error ->
            cell.rendering <- false;
            raise error
      in
      cell.last <- Some (snapshot, node);
      node

and render_view : type p s e. (p, s, e) cell -> message Node.t =
 fun cell ->
  if cell.remount then (
    cell.remount <- false;
    List.iter (fun (_, Packed child) -> unmount_cell child) cell.children;
    cell.children <- []
  );
  let claimed = ref [] in
  let pending = ref false in
  let before = cell.context.actions in
  let handle : e Node.attr -> message Node.attr =
    match cell.context.observer with
    | None ->
        Node.map_attr (to_message Message cell)
    | Some observer ->
        observer.handler (fun cause event ->
            { run = (fun () -> step ~cause cell event) }
        )
  in
  let resolve_attr reset (attr : e Node.attr) : message Node.attr option =
    match[@warning "-4"] attr with
    | Node.On_reset | Node.Rated (_, Node.On_reset) ->
        Option.map (fun message -> Node.On_press message) reset
    | attr ->
        Some (handle attr)
  in
  let rec walk :
      e Node.t ->
      segment list ->
      (string * string -> exn -> unit) option ->
      message option ->
      message Node.t =
   fun node path catch reset ->
    match Node.view node with
    | Node.Text text ->
        Node.text text
    | Node.Element (tag, attrs, children) ->
        Node.element tag
          (List.filter_map (resolve_attr reset) attrs)
          (List.mapi
             (fun i child -> walk child (Index i :: path) catch reset)
             children
          )
    | Node.List children ->
        Node.list
          (List.mapi
             (fun i child -> walk child (Index i :: path) catch reset)
             children
          )
    | Node.Keyed (key, child) ->
        Node.keyed key (walk child (Key key :: path) catch reset)
    | Node.Chunk (Node.Chunk_view { chunk; loading; render }) -> (
        match (Chunk.get chunk, Chunk.failure chunk) with
        | Some value, _ ->
            walk (render value) (Index 0 :: path) catch reset
        | None, Some message ->
            Chunk.forget_failure chunk;
            raise (Node.Chunk_failed message)
        | None, None ->
            pending := true;
            cell.context.waiting <- cell.context.waiting + 1;
            add_action cell.context
              (Action.Run
                 (Action.Load_chunk
                    {
                      chunk = Chunk.Any chunk;
                      on_ready =
                        Some { run = (fun () -> invalidate ~force:false cell) };
                    }
                 )
              );
            walk loading (Index 1 :: path) catch reset
      )
    | Node.Portal (target, children) ->
        Node.portal ?target
          (List.mapi
             (fun i child -> walk child (Index i :: path) catch reset)
             children
          )
    | Node.Boundary { fallback; body } -> (
        let boundary =
          match List.assoc_opt path cell.boundaries with
          | Some boundary ->
              boundary
          | None ->
              let boundary = { failure = None } in
              cell.boundaries <- (path, boundary) :: cell.boundaries;
              boundary
        in
        let show_fallback error =
          let message =
            {
              run =
                (fun () ->
                  boundary.failure <- None;
                  invalidate ~force:false cell
                );
            }
          in
          walk
            (fallback error ~reset:Node.On_reset)
            (Index 1 :: path) catch (Some message)
        in
        match boundary.failure with
        | Some error ->
            show_fallback error
        | None -> (
            let claimed_before = !claimed in
            let inner =
              Some
                (fun failing exn ->
                  boundary.failure <- Some (make_error cell.context failing exn);
                  invalidate ~force:true cell
                )
            in
            try walk body (Index 0 :: path) inner reset
            with exn ->
              let added = until_physically claimed_before !claimed in
              claimed := claimed_before;
              List.iter
                (fun (_, Packed instance) ->
                  if
                    not
                      (List.exists
                         (fun (_, Packed old) -> old.id = instance.id)
                         cell.children
                      )
                  then
                    unmount_cell instance
                )
                added;
              let error = make_error cell.context (where cell) exn in
              boundary.failure <- Some error;
              show_fallback error
          )
      )
    | Node.Component child -> (
        let slot =
          ( ( match (child.key, path) with
            | Some key, _ :: parent ->
                Key key :: parent
            | Some key, [] ->
                [ Key key ]
            | None, _ ->
                path
            ),
            Machine.id child.component.machine
          )
        in
        if List.exists (fun (taken, _) -> same_slot taken slot) !claimed then
          invalid_arg
            "Silica.Instance: two components have the same position and key";
        let instance =
          match find_child slot child.component.machine cell.children with
          | Some existing ->
              set_cell_props existing child.props;
              existing
          | None ->
              create cell.context ~slot:(fst slot) ~parent:(Some (Packed cell))
                child.component child.props
        in
        instance.catch <- catch;
        let from_child parent_event =
          Queue.add
            (fun () -> step ~cause:(Child instance) cell parent_event)
            cell.context.queue
        in
        instance.notify <-
          (fun event snapshot ->
            List.iter from_child (child.output event snapshot)
          );
        instance.notify_final <-
          (fun snapshot -> List.iter from_child (child.on_final snapshot));
        instance.parent <- Some (Packed cell);
        claimed := (slot, Packed instance) :: !claimed;
        check_final instance;
        let resolved = render instance in
        match child.key with
        | Some key ->
            Node.keyed key resolved
        | None ->
            resolved
      )
  in
  let keep_all_on_failure () =
    let known =
      List.filter
        (fun (_, Packed old) ->
          not
            (List.exists
               (fun (_, Packed current) -> current.id = old.id)
               !claimed
            )
        )
        cell.children
    in
    cell.children <- List.rev !claimed @ known
  in
  let resolved =
    try walk (cell.component.view cell.props cell.snapshot) [] cell.catch None
    with exn ->
      keep_all_on_failure ();
      raise exn
  in
  let still_rendered = List.rev !claimed in
  let during = until_physically before cell.context.actions in
  cell.context.actions <- before;
  List.iter
    (fun (_, Packed old) ->
      if
        not
          (List.exists
             (fun (_, Packed current) -> current.id = old.id)
             still_rendered
          )
      then
        unmount_cell old
    )
    cell.children;
  cell.children <- still_rendered;
  cell.context.actions <- during @ cell.context.actions;
  if !pending then invalidate ~force:true cell;
  resolved

let drain context =
  while not (Queue.is_empty context.queue) do
    (Queue.pop context.queue) ()
  done

let take_actions context =
  let actions = List.rev context.actions in
  context.actions <- [];
  actions

let mount_with_props ?(dev = false) ?on_error ?observe ?resume ~props
    (component : (_, _, _) Node.component) =
  let context =
    {
      queue = Queue.create ();
      actions = [];
      next_id = 0;
      dev;
      on_error;
      waiting = 0;
      machines = Hashtbl.create 16;
      observer = observe;
      view = None;
      current = None;
      resume;
    }
  in
  let root = create context ~slot:[] ~parent:None component props in
  { context; root }

let mount ?dev ?on_error ?observe ?resume component =
  mount_with_props ?dev ?on_error ?observe ?resume ~props:() component

let set_props tree props = set_cell_props tree.root props

let max_rounds = 100

let update tree message =
  if not tree.root.alive then
    (Node.list [], [])
  else (
    tree.context.current <- message;
    Option.iter (fun message -> message.run ()) message;
    let rec settle round =
      drain tree.context;
      tree.context.waiting <- 0;
      let node = render tree.root in
      if Queue.is_empty tree.context.queue then
        node
      else if round >= max_rounds then
        failwith
          "Silica.Instance: components keep raising events for each other"
      else
        settle (round + 1)
    in
    let node = settle 0 in
    tree.context.resume <- None;
    tree.context.view <- Some node;
    (node, take_actions tree.context)
  )

let message tree event = to_message Message tree.root event
let route tree route event = to_message (Route route) tree.root event

let path tree = cell_path tree.root

let asked_component asked = asked.starting
let asked_props asked = asked.given

let asked_path asked =
  let named = Machine.id asked.starting.machine in
  match asked.above with
  | None ->
      named
  | Some (Packed parent) ->
      child_path (cell_path parent) asked.at named

type inspected =
  | Inspected : {
      instance : string;
      number : int;
      entry : int;
      machine : ('p, 's, 'e) Machine.t;
      props : 'p;
      from : 's option;
      snapshot : 's;
      change : change;
      event : 'e option;
      actions : 'm Action.host list;
      cause : cause;
      view : message Node.t option;
      message : message option;
    }
      -> inspected

let change_of_how : Machine.how -> change = function
  | Machine.Unhandled ->
      Unhandled
  | Machine.Kept ->
      Kept
  | Machine.Entered ->
      Entered

let inspect_cell : type p s e.
    (p, s, e) cell ->
    change ->
    s option ->
    e option ->
    message Action.host list ->
    cause ->
    inspected =
 fun cell change from event before cause ->
  Inspected
    {
      instance = cell_path cell;
      number = cell.id;
      entry = cell.entry;
      machine = cell.component.machine;
      props = cell.props;
      from;
      snapshot = cell.snapshot;
      change;
      event;
      actions = List.rev (until_physically before cell.context.actions);
      cause;
      view = cell.context.view;
      message = cell.context.current;
    }

let inspect = function
  | Observed (cell, how, from, event, before, cause) -> (
      match[@warning "-4"] cause with
      | Start ->
          inspect_cell cell
            ( if how = Machine.Entered then
                Resumed
              else
                Started
            )
            None event before cause
      | End ->
          inspect_cell cell Ended (Some from) event before cause
      | _ ->
          inspect_cell cell (change_of_how how) (Some from) event before cause
    )
  | Detached
      {
        instance;
        machine;
        props;
        entry;
        from;
        snapshot;
        change;
        event;
        actions;
        cause;
      } ->
      Inspected
        {
          instance;
          number = 1;
          entry;
          machine;
          props;
          from;
          snapshot;
          change;
          event;
          actions;
          cause;
          view = None;
          message = None;
        }
let waiting tree = tree.context.waiting > 0

let callback run = { run }
let perform message = message.run ()
let report tree exn = make_error tree.context (where tree.root) exn

let failure ?(dev = false) ?on_error exn =
  make_error
    {
      queue = Queue.create ();
      actions = [];
      next_id = 0;
      dev;
      on_error;
      waiting = 0;
      machines = Hashtbl.create 16;
      observer = None;
      view = None;
      current = None;
      resume = None;
    }
    ("(none)", "") exn

let unmount tree =
  unmount_cell tree.root;
  take_actions tree.context

let snapshot tree = tree.root.snapshot

let rec count_cell : type p s e. (p, s, e) cell -> int =
 fun cell ->
  List.fold_left
    (fun total (_, Packed child) -> total + count_cell child)
    1 cell.children

let count tree = count_cell tree.root

type running =
  | Running : {
      path : string;
      machine : ('p, 's, 'e) Machine.t;
      props : 'p;
      snapshot : 's;
    }
      -> running

let running tree =
  let rec collect : type p s e. (p, s, e) cell -> running list -> running list =
   fun cell found ->
    if cell.alive then
      List.fold_left
        (fun found (_, Packed child) -> collect child found)
        (Running
           {
             path = cell_path cell;
             machine = cell.component.machine;
             props = cell.props;
             snapshot = cell.snapshot;
           }
        :: found
        )
        cell.children
    else
      found
  in
  List.rev (collect tree.root [])
