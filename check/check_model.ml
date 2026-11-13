open Silica

exception Invalid_check of string

type sample =
  | Answers : {
      endpoint : ('i, 'o, 'x) Remote.endpoint;
      values : 'o list;
      rejected : 'x list;
      decode : bool;
    }
      -> sample
  | States : { topic : 's Topic.t; values : 's list } -> sample

let answers ?(decode = false) ?(rejected = []) endpoint values =
  Answers { endpoint; values; rejected; decode }

let states topic values = States { topic; values }

type seen = Seen : ('p, 's, 'e) Component.t * 'p -> seen
type children = seen list

let props (type p s e) children (component : (p, s, e) Component.t) : p list =
  List.fold_left
    (fun (acc : p list) (Seen (child, props)) ->
      match
        Type.Id.provably_equal
          (Machine.type_id child.Component.machine)
          (Machine.type_id component.machine)
      with
      | Some Type.Equal ->
          if List.mem props acc then
            acc
          else
            props :: acc
      | None ->
          acc
    )
    [] children
  |> List.rev

let same_seen (Seen (first, first_props)) (Seen (second, second_props)) =
  match
    Type.Id.provably_equal
      (Machine.type_id first.Component.machine)
      (Machine.type_id second.Component.machine)
  with
  | Some Type.Equal -> (
      match first_props = second_props with
      | same ->
          same
      | exception Invalid_argument _ ->
          false
    )
  | None ->
      false

let seen_machine (Seen (component, _)) = Machine.id component.Component.machine

let children_of_view node =
  Check_rules.collect
    (function
      | Node.Component { component; props; _ } ->
          [ Seen (component, props) ]
      | _ ->
          []
      )
    node

type cause =
  | View of {
      handler : string;
      element : string;
      role : string option;
      name : string;
    }
  | Input
  | Timer of { id : string; ms : int }
  | Answer of { id : string; path : string }
  | Live of { id : string; topic : string }
  | Held of { id : string; name : string }
  | Action_result of string
  | Props
  | Child of { instance : string; machine : string }
  | Navigation
  | Address of string

type child_event =
  | Child_event : {
      instance : string;
      machine : string;
      witness : ('p * 's * 'e) Type.Id.t;
      event : 'e;
    }
      -> child_event

type phase = Connecting | Up | Down | Ended
type sub = { phase : phase; arrivals : int; last : int option; dropped : bool }

type via =
  | By_view
  | By_typing
  | By_input
  | By_timer of string
  | By_answer of string
  | By_feed of string * sub
  | By_result of int

type 'ev move = { event : 'ev; via : via }

let ignorable = function
  | By_input | By_typing | By_result _ ->
      true
  | By_view | By_timer _ | By_answer _ | By_feed _ ->
      false

type 'e feed = {
  loading : 'e;
  ready : 'e array array;
  stale : 'e array;
  failed_empty : 'e;
  failed_last : 'e array;
}

type ('p, 's, 'e) world = {
  props : 'p;
  s : 's;
  timers : (string * 'e) list;
  calls : (string * int) list;
  subs : (string * sub) list;
  feeds : (string * int) list;
  results : ('e * string) list;
  hist : Url.t list;
  idx : int;
}

type 'a table = {
  buckets : (int, ('a * int) list) Hashtbl.t;
  values : 'a Dynarray.t;
}

let table () = { buckets = Hashtbl.create 64; values = Dynarray.create () }

let intern table value =
  let hash = Hashtbl.hash_param 100 200 value in
  let bucket = Option.value ~default:[] (Hashtbl.find_opt table.buckets hash) in
  match List.find_opt (fun (other, _) -> other = value) bucket with
  | Some (_, index) ->
      index
  | None ->
      let index = Dynarray.length table.values in
      Dynarray.add_last table.values value;
      Hashtbl.replace table.buckets hash ((value, index) :: bucket);
      index

type 'e host = {
  samples : sample list;
  arrivals : int;
  answer_lists : ('e * Silica_json.Json.t) list table;
  feed_table : ('e feed * Silica_json.Json.t feed) table;
}

let host ~samples ~arrivals =
  { samples; arrivals; answer_lists = table (); feed_table = table () }

let answers_for (type o x e) samples (endpoint : (_, o, x) Remote.endpoint)
    (on_result : (o, x) Remote.answer -> e) =
  let given =
    List.filter_map
      (function
        | Answers a -> (
            match
              Type.Id.provably_equal
                (Remote.answer_id a.endpoint)
                (Remote.answer_id endpoint)
            with
            | Some Type.Equal ->
                Some ((a.values : o list), (a.rejected : x list), a.decode)
            | None ->
                None
          )
        | States _ ->
            None
        )
      samples
  in
  let codec =
    Remote.answer_codec (Remote.output endpoint) (Remote.rejected endpoint)
  in
  List.map
    (fun answer -> (on_result answer, codec.encode answer))
    (List.concat_map
       (fun (values, rejected, _) ->
         List.map (fun value -> Remote.Ok value) values
         @ List.map (fun value -> Remote.Rejected value) rejected
       )
       given
    @ [ Remote.Failed (Remote.Http 500); Remote.Failed (Remote.Network "down") ]
    @
    if List.exists (fun (_, _, decode) -> decode) given then
      [ Remote.Failed (Remote.Decode "sample") ]
    else
      []
    )

let feed_for (type s e) ~arrivals samples (topic : s Topic.t)
    (on_update : s Topic.update -> e) =
  let values =
    Array.of_list
      (List.concat_map
         (function
           | States st -> (
               match
                 Type.Id.provably_equal (Topic.state_id st.topic)
                   (Topic.state_id topic)
               with
               | Some Type.Equal ->
                   (st.values : s list)
               | None ->
                   []
             )
           | Answers _ ->
               []
           )
         samples
      )
  in
  let failed last =
    on_update
      {
        seq = 0;
        status = Status.Failed { error = Remote.Network "down"; last };
      }
  in
  {
    loading = on_update { Topic.seq = 0; status = Status.Loading None };
    ready =
      Array.map
        (fun data ->
          Array.init (arrivals + 1) (fun index ->
              on_update
                {
                  seq = index + 1;
                  status = Status.Ready { data; stale = false };
                }
          )
        )
        values;
    stale =
      Array.map
        (fun data ->
          on_update { seq = 0; status = Status.Ready { data; stale = true } }
        )
        values;
    failed_empty = failed None;
    failed_last = Array.map (fun data -> failed (Some data)) values;
  }

let fresh = { phase = Connecting; arrivals = 0; last = None; dropped = false }

let rec put id value = function
  | [] ->
      [ (id, value) ]
  | (key, _) :: rest when key = id ->
      (id, value) :: rest
  | ((key, _) as item) :: rest when key < id ->
      item :: put id value rest
  | rest ->
      (id, value) :: rest

let host_action host world = function
  | Action.Start_timer { id; event; _ } ->
      { world with timers = put id event world.timers }
  | Action.Start_call { id; endpoint; on_result; _ } ->
      {
        world with
        calls =
          put id
            (intern host.answer_lists
               (answers_for host.samples endpoint on_result)
            )
            world.calls;
      }
  | Action.Start_live { id; topic; on_update; _ } ->
      let update_json = (Topic.update_codec (Topic.state topic)).encode in
      {
        world with
        subs = put id fresh world.subs;
        feeds =
          put id
            (intern host.feed_table
               ( feed_for ~arrivals:host.arrivals host.samples topic on_update,
                 feed_for ~arrivals:host.arrivals host.samples topic update_json
               )
            )
            world.feeds;
      }
  | Action.Stop id ->
      {
        world with
        timers = List.remove_assoc id world.timers;
        calls = List.remove_assoc id world.calls;
        subs = List.remove_assoc id world.subs;
        feeds = List.remove_assoc id world.feeds;
      }
  | Action.Run action ->
      let results =
        match action with
        | Action.Measure { on_rect; _ } ->
            [ (on_rect (Some Rect.zero), "Measure") ]
        | Action.Copy { on_result = Some on_result; _ } ->
            [ (on_result true, "Copy") ]
        | Action.Load_script { on_result; _ } ->
            [ (on_result true, "Load_script") ]
        | Action.Load_chunk { on_ready; _ } ->
            List.map
              (fun event -> (event, "Load_chunk"))
              (Option.to_list on_ready)
        | Action.Copy { on_result = None; _ }
        | Action.Focus _
        | Action.Blur _
        | Action.Scroll_into_view _
        | Action.Scroll_to _
        | Action.Navigate _
        | Action.Replace _
        | Action.Back
        | Action.Forward
        | Action.Reload
        | Action.Set_title _
        | Action.Set_status _
        | Action.Set_document_attribute _
        | Action.Set_cookie _
        | Action.Resume _
        | Action.Revert_navigation
        | Action.Preload _
        | Action.Remount ->
            []
      in
      if results = [] then
        world
      else
        { world with results = List.sort compare (results @ world.results) }
  | Action.Start_held _ ->
      world

let live_moves host world id sub =
  let feed, _ =
    Dynarray.get host.feed_table.values (List.assoc id world.feeds)
  in
  let move event sub = { event; via = By_feed (id, sub) } in
  match sub.phase with
  | Connecting ->
      [ move feed.loading { sub with phase = Up } ]
  | Ended ->
      []
  | Up | Down ->
      let arrivals =
        if sub.arrivals < host.arrivals || sub.phase = Down then
          List.init (Array.length feed.stale) (fun index ->
              move
                feed.ready.(index).(sub.arrivals)
                {
                  sub with
                  phase = Up;
                  arrivals = sub.arrivals + 1;
                  last = Some index;
                }
          )
        else
          []
      in
      let loss =
        match (sub.phase, sub.last) with
        | Up, Some index when not sub.dropped ->
            [
              move feed.stale.(index) { sub with phase = Down; dropped = true };
            ]
        | _ ->
            []
      in
      arrivals @ loss
      @ [
          move
            ( match sub.last with
            | Some index ->
                feed.failed_last.(index)
            | None ->
                feed.failed_empty
            )
            { sub with phase = Ended };
        ]

let resource_moves host world =
  List.map (fun (id, event) -> { event; via = By_timer id }) world.timers
  @ List.concat_map
      (fun (id, answers) ->
        List.map
          (fun (event, _) -> { event; via = By_answer id })
          (Dynarray.get host.answer_lists.values answers)
      )
      world.calls
  @ List.concat_map (fun (id, sub) -> live_moves host world id sub) world.subs
  @ List.mapi
      (fun index (event, _) -> { event; via = By_result index })
      world.results

let feed_entry feed sub =
  match (sub.phase, sub.last) with
  | Ended, Some index ->
      feed.failed_last.(index)
  | Ended, None ->
      feed.failed_empty
  | Down, Some index ->
      feed.stale.(index)
  | Up, Some index ->
      feed.ready.(index).(sub.arrivals - 1)
  | Connecting, _ | (Down | Up), None ->
      feed.loading

let answer_json host world via event =
  match via with
  | By_answer id ->
      Option.bind (List.assoc_opt id world.calls) (fun answers ->
          List.assoc_opt event (Dynarray.get host.answer_lists.values answers)
      )
  | By_feed (id, sub) ->
      Option.map
        (fun index ->
          feed_entry (snd (Dynarray.get host.feed_table.values index)) sub
        )
        (List.assoc_opt id world.feeds)
  | By_view | By_typing | By_input | By_timer _ | By_result _ ->
      None

let consume world = function
  | By_view | By_typing | By_input ->
      world
  | By_timer id ->
      { world with timers = List.remove_assoc id world.timers }
  | By_answer id ->
      { world with calls = List.remove_assoc id world.calls }
  | By_feed (id, sub) ->
      { world with subs = put id sub world.subs }
  | By_result index ->
      {
        world with
        results = List.filteri (fun other _ -> other <> index) world.results;
      }

let finished = function
  | By_timer id | By_answer id ->
      Some id
  | By_view | By_typing | By_input | By_feed _ | By_result _ ->
      None

let source_events (sources : _ Action.held list) =
  List.concat_map
    (function
      | Action.Global_keys { bindings; _ } ->
          List.map snd bindings
      | Action.Outside_press { event; _ } ->
          [ event ]
      | Action.Resize { on_resize; _ } ->
          [ on_resize Rect.zero ]
      | Action.Intersection { on_change; _ } ->
          [ on_change true; on_change false ]
      | Action.Dismiss { on_escape; on_outside; on_focus_out; _ } ->
          Option.to_list on_escape @ Option.to_list on_outside
          @ Option.to_list on_focus_out
      | Action.Scroll { on_scroll; _ } ->
          [ on_scroll Scroll.zero ]
      | Action.Visibility on_change
      | Action.Online on_change
      | Action.Media { on_change; _ } ->
          [ on_change true; on_change false ]
      | Action.Block { on_attempt } ->
          [
            on_attempt (Nav.Push (Url.of_string "/"));
            on_attempt (Nav.Traverse { delta = -1; url = Url.of_string "/" });
          ]
      | Action.Focus_trap _ | Action.Scroll_lock | Action.Anchor _ ->
          []
      )
    sources

let resource_cause machine world via event =
  let active () = Machine.active_with_props machine world.props world.s in
  match via with
  | By_timer id ->
      Timer
        {
          id;
          ms =
            Option.value ~default:0
              (List.find_map
                 (function
                   | Machine.Timer timer when timer.id = id ->
                       Some timer.ms
                   | _ ->
                       None
                   )
                 (active ())
              );
        }
  | By_answer id ->
      Answer
        {
          id;
          path =
            Option.value ~default:""
              (List.find_map
                 (function
                   | Machine.Call call when call.id = id ->
                       Some call.path
                   | _ ->
                       None
                   )
                 (active ())
              );
        }
  | By_feed (id, _) ->
      Live
        {
          id;
          topic =
            Option.value ~default:""
              (List.find_map
                 (function
                   | Machine.Subscription subscription when subscription.id = id
                     ->
                       Some subscription.topic
                   | _ ->
                       None
                   )
                 (active ())
              );
        }
  | By_result index ->
      Action_result
        ( match List.nth_opt world.results index with
        | Some (_, name) ->
            name
        | None ->
            ""
        )
  | By_input -> (
      let held = Machine.held_with_props machine world.props world.s in
      let ids =
        List.filter_map
          (function Machine.Held { id; name } -> Some (id, name) | _ -> None)
          (active ())
      in
      let rec find sources ids =
        match (sources, ids) with
        | source :: sources, (id, name) :: ids ->
            if List.mem event (source_events [ source ]) then
              Some (Held { id; name })
            else
              find sources ids
        | _ ->
            None
      in
      match find held ids with Some cause -> cause | None -> Input
    )
  | By_view | By_typing ->
      Input

type recorder = {
  taken : (Spec.label, unit) Hashtbl.t;
  seen : (string, Spec.state) Hashtbl.t;
  passed : (string, unit) Hashtbl.t;
  mutable first : string option;
}

let recorder () =
  {
    taken = Hashtbl.create 64;
    seen = Hashtbl.create 16;
    passed = Hashtbl.create 4;
    first = None;
  }

let started recorder machine state =
  let name = Machine.name machine state
  and init = Machine.name machine (Machine.init machine) in
  if recorder.first = None then recorder.first <- Some name;
  if name <> init then Hashtbl.replace recorder.passed init ()

let see recorder machine props state =
  let name = Machine.name machine state in
  let active = Machine.active_with_props machine props state in
  let pick f = List.filter_map f active in
  let found =
    {
      Spec.name;
      final = Machine.is_final machine state;
      timers =
        pick (function
          | Machine.Timer { ms; _ } ->
              Some ms
          | Machine.Call _ | Machine.Subscription _ | Machine.Held _ ->
              None
          );
      network =
        pick (function
          | Machine.Call { path; _ } ->
              Some path
          | Machine.Subscription { topic; _ } ->
              Some ("live:" ^ topic)
          | Machine.Timer _ | Machine.Held _ ->
              None
          );
      held =
        pick (function
          | Machine.Held { name; _ } ->
              Some name
          | Machine.Timer _ | Machine.Call _ | Machine.Subscription _ ->
              None
          );
    }
  in
  match Hashtbl.find_opt recorder.seen name with
  | None ->
      Hashtbl.replace recorder.seen name found
  | Some known ->
      let union known found =
        known @ List.filter (fun item -> not (List.mem item known)) found
      in
      let merged =
        {
          known with
          timers = union known.timers found.timers;
          network = union known.network found.network;
          held = union known.held found.held;
        }
      in
      if merged <> known then Hashtbl.replace recorder.seen name merged

let record recorder machine ~from ~props state label (how : Machine.how) =
  see recorder machine props state;
  match how with
  | Entered | Kept ->
      Hashtbl.replace recorder.taken
        {
          Spec.from = Machine.name machine from;
          target = Machine.name machine state;
          event = label ();
          kept =
            how = Kept && Machine.name machine from = Machine.name machine state;
        }
        ()
  | Unhandled ->
      ()

let send ?finished ~label recorder machine world event =
  let state, actions, how =
    Machine.step_with_props ?finished machine world.props world.s event
  in
  record recorder machine ~from:world.s ~props:world.props state
    (fun () ->
      match Machine.event_name machine event with
      | Some name ->
          name
      | None ->
          label ()
    )
    how;
  (state, actions, how)

let change_props recorder machine world props =
  let state, actions, how =
    Machine.set_props machine ~previous:world.props props world.s
  in
  record recorder machine ~from:world.s ~props state (fun () -> "props") how;
  (state, actions, how)

let spec_of recorder machine =
  let init =
    match recorder.first with
    | Some name ->
        name
    | None ->
        Machine.name machine (Machine.init machine)
  in
  {
    Spec.id = Machine.id machine;
    init;
    states =
      Hashtbl.fold (fun _ state acc -> state :: acc) recorder.seen []
      |> List.sort (fun (a : Spec.state) (b : Spec.state) ->
          compare (a.name <> init, a.name) (b.name <> init, b.name)
      );
    edges =
      Hashtbl.fold (fun edge () acc -> edge :: acc) recorder.taken []
      |> List.sort_uniq compare;
  }

let unlisted_of machine =
  match Machine.states machine with
  | Some _ ->
      []
  | None ->
      [ Machine.id machine ]

let unreached_in recorder names =
  List.filter
    (fun name ->
      (not (Hashtbl.mem recorder.seen name))
      && not (Hashtbl.mem recorder.passed name)
    )
    names

let unreached_of recorder machine =
  unreached_in recorder (Option.value ~default:[] (Machine.states machine))

type running_state = { instance : string; machine : string; state : string }

type ('n, 'ev, 'e) tree = {
  actor : 'ev -> (string * string) option;
  states : 'n -> running_state list;
  owner : 'n -> string -> running_state option;
  rules : 'n -> 'e Node.t -> 'ev option -> Check_rules.finding list;
  inside : 'n -> bool;
  not_run : unit -> string list;
  counts : unit -> (string * int) list;
  navigate : 'n -> 'e Action.host list -> 'n * 'e Action.host list;
  attempt : 'n -> 'ev -> Nav.t option;
  attempt_action : 'n -> 'e Action.host -> Nav.t option;
}

let flat () =
  {
    actor = (fun _ -> None);
    states = (fun _ -> []);
    owner = (fun _ _ -> None);
    rules = (fun _ _ _ -> []);
    inside = (fun _ -> true);
    not_run = (fun () -> []);
    counts = (fun () -> []);
    navigate = (fun node _ -> (node, []));
    attempt = (fun _ _ -> None);
    attempt_action = (fun _ _ -> None);
  }

let state_of tree node instance =
  List.find_map
    (fun running ->
      if running.instance = instance then
        Some running.state
      else
        None
    )
    (tree.states node)

let newest_block sources =
  List.fold_left
    (fun found -> function
      | Action.Block { on_attempt } -> Some on_attempt | _ -> found
      )
    None sources

type ('n, 's, 'ev, 'e) model = {
  machine : string;
  initial : 's;
  initial_name : string;
  start : unit -> ('n * 'e Action.host list) list;
  view : 'n -> 'e Node.t;
  enabled : 'n -> 'e Node.t -> 'ev move list;
  step : 'n -> 'ev move -> 'n * 'e Action.host list * Machine.how;
  final : 'n -> bool;
  name : 'n -> string;
  snapshot : 'n -> 's;
  event_name : 'ev -> string option;
  event_of : 'ev -> 'e option;
  delivered : 'n -> 'ev move -> 'ev list;
  cause : 'n -> 'e Node.t -> 'ev move -> cause;
  state_rules : 'n -> 'e Node.t -> Check_rules.finding list;
  action_rules :
    values:(string * string) list ->
    'e Node.t Lazy.t ->
    'e Action.host list ->
    Check_rules.finding list;
  setup_findings : unit -> Check_rules.finding list;
  specs : unit -> Spec.t list;
  unreached : unit -> string list;
  unlisted : unit -> string list;
  answer : 'n -> 'ev move -> Silica_json.Json.t option;
  props_json : 'n -> Silica_json.Json.t option;
  tree : ('n, 'ev, 'e) tree;
}

type ('p, 'e) input = Event of 'e | Props of 'p | In_child of child_event

type ('p, 's, 'e) listed = {
  component : ('p, 's, 'e) Component.t;
  samples : sample list;
  inputs : 's -> 'e list;
  texts : string list;
  within : 's -> bool;
  max_instances : int;
  names : string list;
  check :
    snapshot:'s ->
    view:'e Node.t ->
    event:'e option ->
    name:string ->
    Check_rules.finding list;
}

type child = Child : ('p, 's, 'e) listed -> child

let not_in_child () =
  invalid_arg "Check_model: a flat model got the event of a child instance"

let owner_rules machine world =
  let owned =
    List.filter_map
      (function
        | Machine.Timer { id; _ } ->
            Some (`Timer, id)
        | Machine.Call { id; _ } | Machine.Subscription { id; _ } ->
            Some (`Call, id)
        | Machine.Held _ ->
            None
        )
      (Machine.active_with_props machine world.props world.s)
  in
  List.filter_map
    (fun (id, _) ->
      if List.mem (`Timer, id) owned then
        None
      else
        Some (Check_rules.finding "timer-owner" ~values:[ ("resource", id) ])
    )
    world.timers
  @ List.filter_map
      (fun id ->
        if List.mem (`Call, id) owned then
          None
        else
          Some (Check_rules.finding "call-owner" ~values:[ ("resource", id) ])
      )
      (List.map fst world.calls @ List.map fst world.subs)

let start_props machine = function
  | [] -> (
      match Machine.default_props machine with
      | Some props ->
          [ props ]
      | None ->
          raise
            (Invalid_check
               ("the component " ^ Machine.id machine
              ^ " has props: give them with ~props"
               )
            )
    )
  | props ->
      props

let empty_world props s =
  {
    props;
    s;
    timers = [];
    calls = [];
    subs = [];
    feeds = [];
    results = [];
    hist = [];
    idx = 0;
  }

let describe_cause = function
  | View { handler; element; role; name } ->
      Printf.sprintf "the view offers %s on %s%s at %s"
        ( if String.starts_with ~prefix:"input " handler then
            Printf.sprintf "input %S"
              (String.sub handler 6 (String.length handler - 6))
          else
            handler
        )
        (Option.value role ~default:"the element")
        ( if name = "" then
            ""
          else
            Printf.sprintf " %S" name
        )
        element
  | Input ->
      "an input"
  | Timer { id; ms } ->
      Printf.sprintf "the timer %s (%d ms)" id ms
  | Answer { id; path } ->
      Printf.sprintf "the answer of the call %s to %s" id path
  | Live { id; topic } ->
      Printf.sprintf "an update of the subscription %s to %s" id topic
  | Held { id; name } ->
      Printf.sprintf "the held resource %s (%s)" name id
  | Action_result action ->
      Printf.sprintf "the result of the action %s" action
  | Props ->
      "new props"
  | Child { instance; machine } ->
      Printf.sprintf "the child %s (instance %s)" machine instance
  | Navigation ->
      "the browser history"
  | Address url ->
      "the user opens " ^ url

let transition_cause ~answer ~url : cause -> Transition.cause = function
  | View { handler; element; role; name } ->
      let handler, value =
        match String.index_opt handler ' ' with
        | Some space ->
            ( String.sub handler 0 space,
              Some
                (String.sub handler (space + 1)
                   (String.length handler - space - 1)
                )
            )
        | None ->
            (handler, None)
      in
      Element { handler; role; name; element; value }
  | Input ->
      Message
  | Timer { id; ms } ->
      Timer { id; ms }
  | Answer { id; path } ->
      Answer { id; path; answer }
  | Live { id; topic } ->
      Live { id; topic; update = answer }
  | Held { id; name } ->
      Held { id; name }
  | Action_result action ->
      Action_result { action }
  | Props ->
      Props
  | Child { instance; _ } ->
      Child { instance }
  | Navigation ->
      Url (Option.value url ~default:"")
  | Address url ->
      Url url

let cause_label ~answer cause =
  Transition.cause_label (transition_cause ~answer ~url:None cause)

let move_label model node view move =
  match model.event_name move.event with
  | Some name ->
      name
  | None ->
      cause_label ~answer:(model.answer node move)
        (model.cause node (Lazy.force view) move)

let view_cause ~keys ~texts view event =
  match Check_rules.locate ~keys ~texts view (fun other -> other = event) with
  | Some { handler; element; role; name } ->
      View { handler; element; role; name }
  | None ->
      Input

let component_model ~client_only ~keys ~texts ~inputs ~props ~samples ~arrivals
    (component : (_, _, _) Component.t) =
  let machine = component.machine in
  let all_props = start_props machine props in
  let host = host ~samples ~arrivals in
  let recorder = recorder () in
  let exec world actions = List.fold_left (host_action host) world actions in
  let start () =
    List.map
      (fun props ->
        let s, actions = Machine.start_with_props machine props in
        started recorder machine s;
        see recorder machine props s;
        (exec (empty_world props s) actions, actions)
      )
      all_props
  in
  let rendered = ref None in
  let view world =
    match !rendered with
    | Some (seen, view) when seen == world ->
        view
    | Some _ | None ->
        let view = component.view world.props world.s in
        rendered := Some (world, view);
        view
  in
  let enabled world view =
    Check_rules.dedup
      (List.map
         (fun event -> { event = (Event event : (_, _) input); via = By_view })
         (Check_rules.handlers ~keys view)
      @ List.map
          (fun event ->
            { event = (Event event : (_, _) input); via = By_typing }
          )
          (Check_rules.typed ~texts view)
      @ List.map
          (fun event -> { event = (Event event : (_, _) input); via = By_input })
          (source_events (Machine.held_with_props machine world.props world.s)
          @ inputs world.s
          )
      @ List.filter_map
          (fun props ->
            if props = world.props then
              None
            else
              Some { event = (Props props : (_, _) input); via = By_input }
          )
          all_props
      @ List.map
          (fun move -> { move with event = (Event move.event : (_, _) input) })
          (resource_moves host world)
      )
  in
  let cause world view move =
    match (move.event : (_, _) input) with
    | Props _ ->
        (Props : cause)
    | In_child _ ->
        not_in_child ()
    | Event event -> (
        match move.via with
        | By_view | By_typing ->
            view_cause ~keys ~texts view event
        | via ->
            resource_cause machine world via event
      )
  in
  let answer world move =
    match (move.event : (_, _) input) with
    | Event event ->
        answer_json host world move.via event
    | Props _ | In_child _ ->
        None
  in
  let step world move =
    let label () =
      cause_label ~answer:(answer world move) (cause world (view world) move)
    in
    let world = consume world move.via in
    match (move.event : (_, _) input) with
    | Event event ->
        let s, actions, how =
          send ?finished:(finished move.via) ~label recorder machine world event
        in
        (exec { world with s } actions, actions, how)
    | Props props ->
        let s, actions, how = change_props recorder machine world props in
        (exec { world with s; props } actions, actions, how)
    | In_child _ ->
        not_in_child ()
  in
  {
    machine = Machine.id machine;
    initial = Machine.init machine;
    initial_name = Machine.name machine (Machine.init machine);
    start;
    view;
    enabled;
    step;
    final = (fun world -> Machine.is_final machine world.s);
    name = (fun world -> Machine.name machine world.s);
    snapshot = (fun world -> world.s);
    event_name =
      (function
      | Event event ->
          Machine.event_name machine event
      | Props _ ->
          Some "props"
      | In_child _ ->
          not_in_child ()
      );
    event_of =
      (function Event event -> Some event | Props _ | In_child _ -> None);
    delivered = (fun _ _ -> []);
    cause;
    state_rules =
      (fun world view ->
        owner_rules machine world
        @ Check_accessibility.dialog_focus
            ~held:(Machine.held_with_props machine world.props world.s)
            view
        @
        if client_only then
          List.map
            (fun name ->
              Check_rules.finding "live-uses-client-resource"
                ~values:[ ("resource", name) ]
            )
            (List.filter_map
               (function Machine.Held { name; _ } -> Some name | _ -> None)
               (Machine.active_with_props machine world.props world.s)
            )
        else
          []
      );
    action_rules =
      (fun ~values view actions ->
        Check_rules.focus_target ~values view actions
        @
        if client_only then
          Check_rules.client_actions ~values actions
        else
          []
      );
    setup_findings = (fun () -> []);
    specs = (fun () -> [ spec_of recorder machine ]);
    unreached = (fun () -> unreached_of recorder machine);
    unlisted = (fun () -> unlisted_of machine);
    answer;
    props_json =
      (fun world ->
        Result.to_option
          (Result.map
             (fun (codec : _ Codec.t) -> codec.encode world.props)
             (Machine.props_codec machine)
          )
      );
    tree = flat ();
  }

type 's app_state = { router : 's; history : Url.t list; index : int }

type 'e app_event =
  | Event of 'e
  | Link of Url.t
  | Open of Url.t
  | Browser_back
  | Browser_forward
  | In_child of child_event

let rec nth items index =
  match items with
  | item :: _ when index = 0 ->
      item
  | _ :: rest ->
      nth rest (index - 1)
  | [] ->
      failwith "history index out of range"

let rec take count = function
  | item :: rest when count > 0 ->
      item :: take (count - 1) rest
  | _ ->
      []

let link_problem ?is_not_found ~parse ~print href =
  let url = Url.of_string href in
  match parse url with
  | snapshot ->
      let printed = Url.to_string (print snapshot) in
      if printed <> Url.to_string { url with hash = None } then
        Some
          ("which is not canonical: the router prints that route as " ^ printed)
      else if
        match is_not_found with
        | Some is_not_found ->
            is_not_found snapshot
        | None ->
            false
      then
        Some "which parses to the not-found route"
      else
        None
  | exception Failure message ->
      Some ("which does not parse: " ^ message)

let round_trips ~parse ~print route =
  match parse (print route) with
  | parsed ->
      parsed = route
  | exception Failure _ ->
      false

let app_model ~keys ~texts ~urls ~max_history ~inputs ~samples ~arrivals ~routes
    ~parse ~print ?is_not_found ~url_event root =
  let first =
    match routes with
    | [] ->
        raise (Invalid_check "Check.app needs at least one route in ~routes")
    | first :: _ ->
        first
  in
  let url0 = print first in
  let component : (_, _, _) Component.t = root url0 in
  let machine = component.machine in
  let host = host ~samples ~arrivals in
  let recorder = recorder () in
  let traverse_target world delta =
    let target = world.idx + delta in
    if target >= 0 && target < List.length world.hist then
      Some (Nav.Traverse { delta; url = nth world.hist target })
    else
      None
  in
  let attempt_action world = function
    | Action.Run (Action.Navigate url) ->
        Some (Nav.Push url)
    | Action.Run (Action.Replace url) ->
        Some (Nav.Replace url)
    | Action.Run Action.Back ->
        traverse_target world (-1)
    | Action.Run Action.Forward ->
        traverse_target world 1
    | _ ->
        None
  in
  let rec apply ?previous depth (world, acc) actions =
    List.fold_left
      (fun (world, acc) action ->
        let nav hist idx url =
          if depth > 50 then failwith "navigation loop: actions keep navigating";
          let previous = (world.hist, world.idx) in
          let world = { world with hist; idx } in
          let s, more, _ =
            send
              ~label:(fun () -> "url " ^ Url.to_string url)
              recorder machine world (url_event url)
          in
          apply ~previous (depth + 1) ({ world with s }, acc @ more) more
        in
        let go = function
          | Nav.Push url ->
              let history = take (world.idx + 1) world.hist @ [ url ] in
              if List.length history > max_history then
                nav (List.tl history) world.idx url
              else
                nav history (world.idx + 1) url
          | Nav.Replace url ->
              nav
                (take world.idx world.hist
                @ url
                  :: List.filteri (fun index _ -> index > world.idx) world.hist
                )
                world.idx url
          | Nav.Traverse { delta; url } ->
              nav world.hist (world.idx + delta) url
        in
        match action with
        | Action.Run (Action.Resume navigation) -> (
            match navigation with
            | Nav.Traverse { delta; _ } -> (
                match traverse_target world delta with
                | Some navigation ->
                    go navigation
                | None ->
                    (world, acc)
              )
            | Nav.Push _ | Nav.Replace _ ->
                go navigation
          )
        | Action.Run Action.Revert_navigation -> (
            match previous with
            | Some (hist, idx) ->
                ({ world with hist; idx }, acc)
            | None ->
                (world, acc)
          )
        | Action.Run Action.Reload ->
            nav world.hist world.idx (nth world.hist world.idx)
        | action -> (
            match attempt_action world action with
            | None ->
                (host_action host world action, acc)
            | Some navigation -> (
                match
                  newest_block
                    (Machine.held_with_props machine world.props world.s)
                with
                | Some on_attempt ->
                    let s, more, _ =
                      send
                        ~label:(fun () -> "the block of the navigation")
                        recorder machine world (on_attempt navigation)
                    in
                    apply (depth + 1) ({ world with s }, acc @ more) more
                | None ->
                    go navigation
              )
          )
      )
      (world, acc) actions
  in
  let rendered = ref None in
  let view world =
    match !rendered with
    | Some (seen, view) when seen == world ->
        view
    | Some _ | None ->
        let view = component.view world.props world.s in
        rendered := Some (world, view);
        view
  in
  let to_state world =
    { router = world.s; history = world.hist; index = world.idx }
  in
  let user event = { event; via = By_view } in
  let start () =
    let s0, actions0 = Machine.start machine in
    started recorder machine s0;
    see recorder machine () s0;
    [
      apply 0 ({ (empty_world () s0) with hist = [ url0 ] }, actions0) actions0;
    ]
  in
  let enabled world view =
    Check_rules.dedup
      (List.map
         (fun event -> user (Event event))
         (Check_rules.handlers ~keys view)
      @ List.map
          (fun event -> { event = Event event; via = By_typing })
          (Check_rules.typed ~texts view)
      @ List.map
          (fun event -> { event = Event event; via = By_input })
          (source_events (Machine.held_with_props machine world.props world.s)
          @ inputs (to_state world)
          )
      @ List.map
          (fun move -> { move with event = Event move.event })
          (resource_moves host world)
      @ List.map
          (fun href -> user (Link (Url.of_string href)))
          (Check_rules.reachable_links view)
      @ ( if world.idx > 0 then
            [ user Browser_back ]
          else
            []
        )
      @ ( if world.idx < List.length world.hist - 1 then
            [ user Browser_forward ]
          else
            []
        )
      @ List.map (fun url -> user (Open (Url.of_string url))) urls
      )
  in
  let cause world view move =
    match move.event with
    | Event event -> (
        match move.via with
        | By_view | By_typing ->
            view_cause ~keys ~texts view event
        | via ->
            resource_cause machine world via event
      )
    | Link url -> (
        match Check_rules.locate_link view (Url.to_string url) with
        | Some { handler; element; role; name } ->
            View { handler; element; role; name }
        | None ->
            Navigation
      )
    | Open url ->
        Address (Url.to_string url)
    | Browser_back | Browser_forward ->
        Navigation
    | In_child _ ->
        not_in_child ()
  in
  let step world move =
    let navigate actions =
      let world, actions = apply 0 (world, []) actions in
      (world, actions, Machine.Kept)
    in
    match move.event with
    | Event event ->
        let label () =
          cause_label
            ~answer:(answer_json host world move.via event)
            (cause world (view world) move)
        in
        let world = consume world move.via in
        let s, actions, how =
          send ?finished:(finished move.via) ~label recorder machine world event
        in
        let world, actions = apply 0 ({ world with s }, actions) actions in
        (world, actions, how)
    | Link url | Open url ->
        navigate [ Action.Run (Action.Navigate url) ]
    | Browser_back ->
        navigate [ Action.Run Action.Back ]
    | Browser_forward ->
        navigate [ Action.Run Action.Forward ]
    | In_child _ ->
        not_in_child ()
  in
  let attempt world = function
    | Link url | Open url ->
        Some (Nav.Push url)
    | Browser_back ->
        traverse_target world (-1)
    | Browser_forward ->
        traverse_target world 1
    | Event _ | In_child _ ->
        None
  in
  let route_findings world =
    if round_trips ~parse ~print world.s then
      []
    else
      [
        Check_rules.finding "route-round-trip"
          ~values:[ ("url", Url.to_string (print world.s)) ];
      ]
  in
  let link_findings view =
    List.filter_map
      (fun href ->
        Option.map
          (fun problem ->
            let element =
              Option.map
                (fun (located : Check_rules.located) -> located.element)
                (Check_rules.locate_link view href)
            in
            Check_rules.finding "link-parses" ?element
              ~values:[ ("href", href); ("problem", problem) ]
          )
          (link_problem ?is_not_found ~parse ~print href)
      )
      (Check_rules.links view)
  in
  {
    machine = Machine.id machine;
    initial = { router = Machine.init machine; history = [ url0 ]; index = 0 };
    initial_name = Machine.name machine (Machine.init machine);
    start;
    view;
    enabled;
    step;
    final = (fun world -> Machine.is_final machine world.s);
    name = (fun world -> Machine.name machine world.s);
    snapshot = to_state;
    event_name =
      (function
      | Event event ->
          Machine.event_name machine event
      | Link url ->
          Some ("link " ^ Url.to_string url)
      | Open url ->
          Some ("open " ^ Url.to_string url)
      | Browser_back ->
          Some "back"
      | Browser_forward ->
          Some "forward"
      | In_child _ ->
          not_in_child ()
      );
    event_of = (function Event event -> Some event | _ -> None);
    delivered = (fun _ _ -> []);
    cause;
    state_rules =
      (fun world view ->
        owner_rules machine world
        @ Check_accessibility.dialog_focus
            ~held:(Machine.held_with_props machine world.props world.s)
            view
        @ route_findings world @ link_findings view
      );
    action_rules =
      (fun ~values view actions -> Check_rules.focus_target ~values view actions);
    setup_findings =
      (fun () ->
        List.filter_map
          (fun route ->
            if round_trips ~parse ~print route then
              None
            else
              Some
                (Check_rules.finding "route-round-trip"
                   ~values:[ ("url", Url.to_string (print route)) ]
                )
          )
          routes
      );
    specs = (fun () -> [ spec_of recorder machine ]);
    unreached =
      (fun () ->
        unreached_in recorder
          ( match Machine.states machine with
          | Some states ->
              states
          | None ->
              List.sort_uniq String.compare
                (List.map (Machine.name machine) routes)
          )
      );
    unlisted = (fun () -> []);
    answer =
      (fun world move ->
        match move.event with
        | Event event ->
            answer_json host world move.via event
        | Link _ | Open _ | Browser_back | Browser_forward | In_child _ ->
            None
      );
    props_json = (fun _ -> None);
    tree =
      {
        (flat ()) with
        navigate =
          (fun world actions ->
            let world, actions = apply 0 (world, []) actions in
            (world, actions)
          );
        attempt;
        attempt_action;
      };
  }
