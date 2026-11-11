type message = { run : unit -> unit }

type context = {
  queue : (unit -> unit) Queue.t;
  mutable actions : message Action.host list;
  mutable next_id : int;
  dev : bool;
  on_error : (exn -> Node.error -> unit) option;
  mutable waiting : int;
}

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

type ('s, 'e) cell = {
  id : int;
  context : context;
  component : ('s, 'e) Node.component;
  mutable snapshot : 's;
  mutable entry : int;
  mutable alive : bool;
  mutable remount : bool;
  mutable props : 'e option;
  mutable notify : 'e -> 's -> unit;
  mutable notify_final : 's -> unit;
  mutable final_sent : bool;
  mutable children : ((segment list * Obj.t) * packed) list;
  mutable boundaries : (segment list * boundary_cell) list;
  mutable catch : (string * string -> exn -> unit) option;
  owned : (string, string) Hashtbl.t;
  mutable parent : packed option;
  mutable stale : bool;
  mutable rendering : bool;
  mutable last : ('s * message Node.t) option;
}

and packed = Packed : ('s, 'e) cell -> packed

type ('s, 'e) t = { context : context; root : ('s, 'e) cell }

let add_action context (action : message Action.host) =
  context.actions <- action :: context.actions

let qualify cell local =
  string_of_int cell.id ^ "." ^ string_of_int cell.entry ^ "/" ^ local

let where : type s e. (s, e) cell -> string * string =
 fun cell ->
  ( Machine.id cell.component.machine,
    Machine.key cell.component.machine cell.snapshot
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

let rec invalidate : type s e. force:bool -> (s, e) cell -> unit =
 fun ~force cell ->
  if force || not (cell.stale || cell.rendering) then (
    cell.stale <- true;
    match cell.parent with
    | Some (Packed parent) ->
        invalidate ~force parent
    | None ->
        ()
  )

let rec step : type s e. (s, e) cell -> e -> unit =
 fun cell event ->
  if cell.alive then
    try
      match Machine.step cell.component.machine cell.snapshot event with
      | _, _, `Unhandled ->
          ()
      | snapshot, actions, kind ->
          cell.snapshot <- snapshot;
          Option.iter
            (fun (Packed parent) -> invalidate ~force:false parent)
            cell.parent;
          if kind = `Entered then cell.entry <- cell.entry + 1;
          emit cell actions;
          cell.notify event snapshot;
          check_final cell
    with exn -> (
      match cell.catch with
      | Some catch ->
          catch (where cell) exn
      | None ->
          raise exn
    )

and check_final : type s e. (s, e) cell -> unit =
 fun cell ->
  if
    (not cell.final_sent)
    && Machine.is_final cell.component.machine cell.snapshot
  then (
    cell.final_sent <- true;
    cell.notify_final cell.snapshot
  )

and timed : type s e. (s, e) cell -> e -> message =
 fun cell event ->
  let entry = cell.entry in
  { run = (fun () -> if cell.entry = entry then step cell event) }

and emit : type s e. (s, e) cell -> e Action.host list -> unit =
 fun cell actions ->
  let add = add_action cell.context in
  List.iter
    (function
      | Action.Start_timer { id; ms; event } ->
          let qualified = qualify cell id in
          Hashtbl.replace cell.owned id qualified;
          add
            (Action.Start_timer { id = qualified; ms; event = timed cell event })
      | Action.Start_source { id; source } ->
          let qualified = qualify cell id in
          Hashtbl.replace cell.owned id qualified;
          add
            (Action.Start_source
               {
                 id = qualified;
                 source = Action.map_source (timed cell) source;
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
                 on_result = (fun result -> timed cell (on_result result));
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
                 on_update = (fun update -> timed cell (on_update update));
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
                  (fun event -> { run = (fun () -> step cell event) })
                  action
               )
            )
      )
    actions

let to_message cell event = { run = (fun () -> step cell event) }

let create : type s e. context -> (s, e) Node.component -> (s, e) cell =
 fun context component ->
  let snapshot, actions = Machine.start component.machine in
  context.next_id <- context.next_id + 1;
  let cell =
    {
      id = context.next_id;
      context;
      component;
      snapshot;
      entry = 0;
      alive = true;
      remount = false;
      props = None;
      notify = (fun _ _ -> ());
      notify_final = (fun _ -> ());
      final_sent = false;
      children = [];
      boundaries = [];
      catch = None;
      owned = Hashtbl.create 4;
      parent = None;
      stale = false;
      rendering = false;
      last = None;
    }
  in
  emit cell actions;
  cell

let rec unmount_cell : type s e. (s, e) cell -> unit =
 fun cell ->
  if cell.alive then (
    List.iter (fun (_, Packed child) -> unmount_cell child) cell.children;
    cell.children <- [];
    emit cell (Machine.exit cell.component.machine cell.snapshot);
    cell.alive <- false;
    Hashtbl.iter
      (fun _ qualified -> add_action cell.context (Action.Stop qualified))
      cell.owned;
    Hashtbl.reset cell.owned
  )

let props_changed old_props new_props =
  match old_props with
  | None ->
      true
  | Some old_props -> (
      try old_props <> new_props with Invalid_argument _ -> true
    )

let same_slot (path, machine) (other_path, other_machine) =
  path = other_path && machine == other_machine

let rec until_physically stop = function
  | actions when actions == stop ->
      []
  | action :: rest ->
      action :: until_physically stop rest
  | [] ->
      []

let rec render : type s e. (s, e) cell -> message Node.t =
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
        Fun.protect
          ~finally:(fun () -> cell.rendering <- false)
          (fun () -> render_view cell)
      in
      cell.last <- Some (snapshot, node);
      node

and render_view : type s e. (s, e) cell -> message Node.t =
 fun cell ->
  if cell.remount then (
    cell.remount <- false;
    List.iter (fun (_, Packed child) -> unmount_cell child) cell.children;
    cell.children <- []
  );
  let claimed = ref [] in
  let pending = ref false in
  let before = cell.context.actions in
  let resolve_attr reset (attr : e Node.attr) : message Node.attr option =
    match attr with
    | Node.On_reset | Node.Rated (_, Node.On_reset) ->
        Option.map (fun message -> Node.On_press message) reset
    | attr ->
        Some (Node.map_attr (to_message cell) attr)
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
                         (fun (_, Packed old) ->
                           Obj.repr old == Obj.repr instance
                         )
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
            Obj.repr child.component.machine
          )
        in
        if List.exists (fun (taken, _) -> same_slot taken slot) !claimed then
          invalid_arg
            "Silica.Instance: two components have the same position and key";
        let instance =
          match
            List.find_opt (fun (taken, _) -> same_slot taken slot) cell.children
          with
          | Some (_, Packed existing) ->
              (* The machines are physically equal, so both cells have the same
                 type. *)
              (Obj.magic existing : (_, _) cell)
          | None ->
              create cell.context child.component
        in
        instance.catch <- catch;
        instance.notify <-
          (fun event snapshot ->
            List.iter
              (fun parent_event ->
                Queue.add (fun () -> step cell parent_event) cell.context.queue
              )
              (child.output event snapshot)
          );
        instance.notify_final <-
          (fun snapshot ->
            List.iter
              (fun parent_event ->
                Queue.add (fun () -> step cell parent_event) cell.context.queue
              )
              (child.on_final snapshot)
          );
        instance.parent <- Some (Packed cell);
        claimed := (slot, Packed instance) :: !claimed;
        ( match child.props with
        | Some props when props_changed instance.props props ->
            instance.props <- Some props;
            step instance props
        | _ ->
            ()
        );
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
               (fun (_, Packed current) -> Obj.repr current == Obj.repr old)
               !claimed
            )
        )
        cell.children
    in
    cell.children <- List.rev !claimed @ known
  in
  let resolved =
    try walk (cell.component.view cell.snapshot) [] cell.catch None
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
             (fun (_, Packed current) -> Obj.repr current == Obj.repr old)
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

let mount ?(dev = false) ?on_error ?props (component : (_, _) Node.component) =
  let context =
    {
      queue = Queue.create ();
      actions = [];
      next_id = 0;
      dev;
      on_error;
      waiting = 0;
    }
  in
  let root = create context component in
  Option.iter
    (fun props ->
      root.props <- Some props;
      step root props
    )
    props;
  { context; root }

let max_rounds = 100

let update tree message =
  if not tree.root.alive then
    (Node.list [], [])
  else (
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
    (node, take_actions tree.context)
  )

let message tree event = to_message tree.root event
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
    }
    ("(none)", "") exn

let unmount tree =
  unmount_cell tree.root;
  take_actions tree.context

let snapshot tree = tree.root.snapshot

let rec count_cell : type s e. (s, e) cell -> int =
 fun cell ->
  List.fold_left
    (fun total (_, Packed child) -> total + count_cell child)
    1 cell.children

let count tree = count_cell tree.root
