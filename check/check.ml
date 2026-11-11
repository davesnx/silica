open Silica

type ('s, 'e) invariant = { name : string; holds : 's -> 'e Node.t -> bool }

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

let props (type p s e) children (c : (p, s, e) Component.t) : p list =
  List.fold_left
    (fun (acc : p list) (Seen (child, props)) ->
      match
        Type.Id.provably_equal
          (Machine.type_id child.Component.machine)
          (Machine.type_id c.machine)
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

type ('s, 'ev) report =
  | Ok of {
      states : int;
      transitions : int;
      not_explored : string list;
      edges : Spec.label list;
      children : children;
      spec : Spec.t;
      paths : (string * 'ev list) list;
    }
  | Violation of { invariant : string; trace : 'ev list; state : 's }
  | Unreached of { states : string list }
  | Limit of { states : int }
  | Error of string

type ('p, 'e) input = Event of 'e | Props of 'p

type phase = Connecting | Up | Down | Ended

type sub = { phase : phase; arrivals : int; last : int option; dropped : bool }

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
  calls : (string * 'e list) list;
  subs : (string * sub) list;
  feeds : (string * 'e feed) list;
  results : 'e list;
  hist : Url.t list;
  idx : int;
}

type via =
  | View
  | Input
  | Fire of string
  | Answer of string
  | Feed of string * sub
  | Result of int
type 'ev move = { event : 'ev; via : via }

type 's app_state = { router : 's; history : Url.t list; index : int }

type 'e app_event =
  | Event of 'e
  | Link of Url.t
  | Browser_back
  | Browser_forward

type ('n, 'ev, 'e) model = {
  view : 'n -> 'e Node.t;
  enabled : 'n -> 'ev move list;
  step : 'n -> 'ev move -> 'n * 'e Action.host list * Machine.how;
  final : 'n -> bool;
  start : ('n * 'e Action.host list) list;
  name : 'n -> string;
}

let rec iter f n =
  let sh = Node.view n in
  f sh;
  match sh with
  | Node.Element (_, _, cs) | Node.List cs ->
      List.iter (iter f) cs
  | Node.Keyed (_, c) ->
      iter f c
  | Node.Portal (_, cs) ->
      List.iter (iter f) cs
  | Node.Boundary { body; _ } ->
      iter f body
  | Node.Chunk chunk ->
      iter f (Node.chunk_view chunk)
  | Node.Text _ | Node.Component _ ->
      ()

let collect f n =
  let acc = ref [] in
  iter (fun sh -> acc := List.rev_append (f sh) !acc) n;
  List.rev !acc

let dedup l =
  List.rev
    (List.fold_left
       (fun acc x ->
         if List.mem x acc then
           acc
         else
           x :: acc
       )
       [] l
    )

let default_keys =
  [ Key.escape; Key.enter; Key.tab; Key.arrow_up; Key.arrow_down ]

let rec attr_events ~keys = function
  | Node.On_press e
  | Node.On_double_press e
  | Node.On_submit e
  | Node.On_focus e
  | Node.On_blur e ->
      [ e ]
  | Node.On_key_down handler | Node.On_key_up handler ->
      List.filter_map handler.run keys
  | Node.On_pointer_enter handler | Node.On_pointer_leave handler ->
      Option.to_list (handler.run Pointer.origin)
  | Node.Rated (_, attr) ->
      attr_events ~keys attr
  | Node.String _ | Node.Bool _ | Node.Ref _ | Node.On_input _ | Node.On_reset
    ->
      []

let source_events (sources : _ Action.source list) =
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

let handlers ~keys n =
  dedup
    (collect
       (function
         | Node.Element (_, attrs, _)
           when not (List.mem (Node.Bool ("disabled", true)) attrs) ->
             List.concat_map (attr_events ~keys) attrs
         | _ ->
             []
         )
       n
    )

let refs n =
  collect
    (function
      | Node.Element (_, attrs, _) ->
          List.filter_map
            (function Node.Ref r -> Some (Ref.name r) | _ -> None)
            attrs
      | _ ->
          []
      )
    n

let rec text n =
  match Node.view n with
  | Node.Text t ->
      t
  | Node.Element (_, _, cs) | Node.List cs ->
      String.concat "" (List.map text cs)
  | Node.Keyed (_, c) ->
      text c
  | Node.Portal (_, cs) ->
      String.concat "" (List.map text cs)
  | Node.Boundary { body; _ } ->
      text body
  | Node.Chunk chunk ->
      text (Node.chunk_view chunk)
  | Node.Component _ ->
      ""

let rec has_reset n =
  match Node.view n with
  | Node.Element (_, attrs, children) ->
      List.exists
        (function
          | Node.On_reset | Node.Rated (_, Node.On_reset) -> true | _ -> false
          )
        attrs
      || List.exists has_reset children
  | Node.List children | Node.Portal (_, children) ->
      List.exists has_reset children
  | Node.Keyed (_, child) ->
      has_reset child
  | Node.Boundary { body; _ } ->
      has_reset body
  | Node.Chunk chunk ->
      has_reset (Node.chunk_view chunk)
  | Node.Text _ | Node.Component _ ->
      false

let sample_error = { Node.id = "000000"; message = "error"; detail = None }

let boundaries_resettable n =
  List.for_all Fun.id
    (collect
       (function
         | Node.Boundary { fallback; _ } ->
             [ has_reset (fallback sample_error ~reset:Node.On_reset) ]
         | _ ->
             []
         )
       n
    )

let blank s = String.trim s = ""

let named attrs children =
  List.exists
    (function
      | Node.String (("aria-label" | "title"), v) -> not (blank v) | _ -> false
      )
    attrs
  || not (blank (String.concat "" (List.map text children)))

let pressable tag attrs =
  tag = "button" || tag = "a"
  || List.exists
       (function Node.On_press _ | Node.On_double_press _ -> true | _ -> false)
       attrs

let all_named n =
  List.for_all Fun.id
    (collect
       (function
         | Node.Element (tag, attrs, cs) when pressable tag attrs ->
             [ named attrs cs ]
         | _ ->
             []
         )
       n
    )

let refs_unique n =
  let rs = refs n in
  List.length (List.sort_uniq compare rs) = List.length rs

let links n =
  collect
    (function
      | Node.Element ("a", attrs, _)
        when List.exists
               (function
                 | Node.String ("data-silica-link", _)
                 | Node.Bool ("data-silica-link", true) ->
                     true
                 | _ ->
                     false
                 )
               attrs ->
          List.filter_map
            (function Node.String ("href", h) -> Some h | _ -> None)
            attrs
      | _ ->
          []
      )
    n

let not_explored_of_view n =
  collect
    (function
      | Node.Component child ->
          [ "component " ^ Machine.id child.component.machine ]
      | Node.Element (_, attrs, _) ->
          List.filter_map
            (function
              | Node.String ("data-silica-live", channel) ->
                  Some ("island " ^ channel)
              | _ ->
                  None
              )
            attrs
      | _ ->
          []
      )
    n

let focus_targets_present view actions =
  let present = refs view in
  List.for_all
    (function
      | Action.Run
          ( Action.Focus r
          | Action.Blur r
          | Action.Scroll_into_view r
          | Action.Scroll_to { target = Some r; _ } ) ->
          List.mem (Ref.name r) present
      | _ ->
          true
      )
    actions

let functional msg =
  let sub = "functional value" in
  let n = String.length sub in
  let rec go i =
    i + n <= String.length msg && (String.sub msg i n = sub || go (i + 1))
  in
  go 0

let children_of_view n =
  collect
    (function
      | Node.Component { component; props; _ } ->
          [ Seen (component, props) ]
      | _ ->
          []
      )
    n

let ignorable = function
  | Input | Result _ ->
      true
  | View | Fire _ | Answer _ | Feed _ ->
      false

let search ~max_states ~within ~model ~checks ~action_checks ~to_state =
  let parents = Dynarray.create () in
  let seen = Hashtbl.create 1024 in
  let queue = Queue.create () in
  let result = ref None in
  let transitions = ref 0 in
  let not_explored = ref [] in
  let children = ref [] in
  let paths = Hashtbl.create 16 in
  let path_order = ref [] in
  let trace i =
    let rec go i acc =
      match Dynarray.get parents i with
      | None ->
          acc
      | Some (p, e) ->
          go p (e :: acc)
    in
    go i []
  in
  let fail name tr n =
    if Option.is_none !result then
      result :=
        Some
          (Violation
             {
               invariant = name;
               trace = List.map (fun mv -> mv.event) tr;
               state = to_state n;
             }
          )
  in
  let add parent n =
    ignore (n = n);
    let h = Hashtbl.hash_param 100 200 n in
    let bucket = Option.value ~default:[] (Hashtbl.find_opt seen h) in
    if within (to_state n) && not (List.exists (fun m -> m = n) bucket) then (
      if Dynarray.length parents >= max_states then (
        if Option.is_none !result then
          result := Some (Limit { states = Dynarray.length parents })
      ) else
        let i = Dynarray.length parents in
        Dynarray.add_last parents parent;
        let name = model.name n in
        if not (Hashtbl.mem paths name) then (
          Hashtbl.replace paths name i;
          path_order := name :: !path_order
        );
        let view = model.view n in
        not_explored :=
          List.rev_append (not_explored_of_view view) !not_explored;
        children := List.rev_append (children_of_view view) !children;
        Hashtbl.replace seen h (n :: bucket);
        Queue.add (i, n) queue;
        List.iter
          (fun (name, f) -> if not (f n) then fail name (trace i) n)
          checks
    )
  in
  let failed_action n actions =
    List.find_map
      (fun (name, holds) ->
        if holds n actions then
          None
        else
          Some name
      )
      action_checks
  in
  List.iter
    (fun (n0, actions0) ->
      if Option.is_none !result then
        match failed_action n0 actions0 with
        | None ->
            add None n0
        | Some name ->
            fail name [] n0
    )
    model.start;
  while Option.is_none !result && not (Queue.is_empty queue) do
    let i, n = Queue.pop queue in
    List.iter
      (fun mv ->
        incr transitions;
        if Option.is_none !result then
          match model.step n mv with
          | _, _, Machine.Unhandled when not (ignorable mv.via) ->
              fail "ignored-event" (trace i @ [ mv ]) n
          | n', actions, _ -> (
              match failed_action n' actions with
              | None ->
                  add (Some (i, mv)) n'
              | Some name ->
                  fail name (trace i @ [ mv ]) n'
            )
      )
      (model.enabled n)
  done;
  match !result with
  | Some r ->
      r
  | None ->
      Ok
        {
          states = Dynarray.length parents;
          transitions = !transitions;
          not_explored = List.sort_uniq String.compare !not_explored;
          edges = [];
          children = List.rev !children;
          spec = { Spec.id = ""; init = ""; states = []; edges = [] };
          paths =
            List.rev_map
              (fun name ->
                ( name,
                  List.map (fun mv -> mv.event) (trace (Hashtbl.find paths name))
                )
              )
              !path_order;
        }

let run ?(extra_actions = []) ~max_states ~within ~model ~invariants ~extra
    ~to_state () =
  let builtin =
    [
      ("accessible-name", fun n -> all_named (model.view n));
      ("unique-refs", fun n -> refs_unique (model.view n));
      ("boundary-reset", fun n -> boundaries_resettable (model.view n));
      ( "dead-end",
        fun n ->
          model.final n
          || not_explored_of_view (model.view n) <> []
          || match model.enabled n with [] -> false | _ :: _ -> true
      );
    ]
  in
  let user =
    List.map
      (fun (inv : (_, _) invariant) ->
        (inv.name, fun n -> inv.holds (to_state n) (model.view n))
      )
      invariants
  in
  try
    search ~max_states ~within ~model
      ~checks:(builtin @ extra @ user)
      ~action_checks:
        (( "focus-target",
           fun n actions -> focus_targets_present (model.view n) actions
         )
        :: extra_actions
        )
      ~to_state
  with
  | Invalid_argument m when functional m ->
      Error
        "state or event holds a functional value; states and events must be \
         first-order"
  | Failure m ->
      Error m

type host = { samples : sample list; arrivals : int }

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
  List.concat_map
    (fun (values, rejected, _) ->
      List.map (fun v -> on_result (Remote.Ok v)) values
      @ List.map (fun x -> on_result (Remote.Rejected x)) rejected
    )
    given
  @ [
      on_result (Remote.Failed (Remote.Http 500));
      on_result (Remote.Failed (Remote.Network "down"));
    ]
  @
  if List.exists (fun (_, _, decode) -> decode) given then
    [ on_result (Remote.Failed (Remote.Decode "sample")) ]
  else
    []

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
  | (k, _) :: rest when k = id ->
      (id, value) :: rest
  | ((k, _) as x) :: rest when k < id ->
      x :: put id value rest
  | rest ->
      (id, value) :: rest

let host_action host w = function
  | Action.Start_timer { id; event; _ } ->
      { w with timers = put id event w.timers }
  | Action.Start_call { id; endpoint; on_result; _ } ->
      {
        w with
        calls = put id (answers_for host.samples endpoint on_result) w.calls;
      }
  | Action.Start_live { id; topic; on_update; _ } ->
      {
        w with
        subs = put id fresh w.subs;
        feeds =
          put id
            (feed_for ~arrivals:host.arrivals host.samples topic on_update)
            w.feeds;
      }
  | Action.Stop id ->
      {
        w with
        timers = List.remove_assoc id w.timers;
        calls = List.remove_assoc id w.calls;
        subs = List.remove_assoc id w.subs;
        feeds = List.remove_assoc id w.feeds;
      }
  | Action.Run action ->
      let results =
        match action with
        | Action.Measure { on_rect; _ } ->
            [ on_rect (Some Rect.zero) ]
        | Action.Copy { on_result = Some on_result; _ } ->
            [ on_result true ]
        | Action.Load_script { on_result; _ } ->
            [ on_result true ]
        | Action.Load_chunk { on_ready; _ } ->
            Option.to_list on_ready
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
        w
      else
        { w with results = List.sort compare (results @ w.results) }
  | Action.Start_source _ ->
      w

let live_moves host w id sub =
  let feed = List.assoc id w.feeds in
  let move event sub = { event; via = Feed (id, sub) } in
  match sub.phase with
  | Connecting ->
      [ move feed.loading { sub with phase = Up } ]
  | Ended ->
      []
  | Up | Down ->
      let arrivals =
        if sub.arrivals < host.arrivals || sub.phase = Down then
          List.init (Array.length feed.stale) (fun i ->
              move
                feed.ready.(i).(sub.arrivals)
                {
                  sub with
                  phase = Up;
                  arrivals = sub.arrivals + 1;
                  last = Some i;
                }
          )
        else
          []
      in
      let loss =
        match (sub.phase, sub.last) with
        | Up, Some i when not sub.dropped ->
            [ move feed.stale.(i) { sub with phase = Down; dropped = true } ]
        | _ ->
            []
      in
      arrivals @ loss
      @ [
          move
            ( match sub.last with
            | Some i ->
                feed.failed_last.(i)
            | None ->
                feed.failed_empty
            )
            { sub with phase = Ended };
        ]

let resource_moves host w =
  List.map (fun (id, event) -> { event; via = Fire id }) w.timers
  @ List.concat_map
      (fun (id, answers) ->
        List.map (fun event -> { event; via = Answer id }) answers
      )
      w.calls
  @ List.concat_map (fun (id, sub) -> live_moves host w id sub) w.subs
  @ List.mapi (fun i event -> { event; via = Result i }) w.results

let consume w = function
  | View | Input ->
      w
  | Fire id ->
      { w with timers = List.remove_assoc id w.timers }
  | Answer id ->
      { w with calls = List.remove_assoc id w.calls }
  | Feed (id, sub) ->
      { w with subs = put id sub w.subs }
  | Result i ->
      { w with results = List.filteri (fun j _ -> j <> i) w.results }

let owner_checks m =
  let ids w =
    List.filter_map
      (function
        | Machine.Timer { id; _ } ->
            Some (`Timer, id)
        | Machine.Call { id; _ } | Machine.Subscription { id; _ } ->
            Some (`Call, id)
        | Machine.Held _ ->
            None
        )
      (Machine.active m w.props w.s)
  in
  [
    ( "timer-owner",
      fun w ->
        let owned = ids w in
        List.for_all (fun (id, _) -> List.mem (`Timer, id) owned) w.timers
    );
    ( "call-owner",
      fun w ->
        let owned = ids w in
        List.for_all
          (fun id -> List.mem (`Call, id) owned)
          (List.map fst w.calls @ List.map fst w.subs)
    );
  ]

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

let started recorder m s =
  let name = Machine.name m s and init = Machine.name m (Machine.init m) in
  if recorder.first = None then recorder.first <- Some name;
  if name <> init then Hashtbl.replace recorder.passed init ()

let see recorder m props s =
  let name = Machine.name m s in
  if not (Hashtbl.mem recorder.seen name) then
    let active = Machine.active m props s in
    let pick f = List.filter_map f active in
    Hashtbl.replace recorder.seen name
      {
        Spec.name;
        final = Machine.is_final m s;
        timers =
          pick (function
            | Machine.Timer { ms; _ } ->
                Some ms
            | Machine.Call _ | Machine.Subscription _ | Machine.Held _ ->
                None
            );
        sources =
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

let record recorder m ~from ~props s label (how : Machine.how) =
  see recorder m props s;
  match how with
  | Entered | Kept ->
      Hashtbl.replace recorder.taken
        {
          Spec.from = Machine.name m from;
          target = Machine.name m s;
          event = label;
          kept = how = Kept;
        }
        ()
  | Unhandled ->
      ()

let finished = function
  | Fire id | Answer id ->
      Some id
  | View | Input | Feed _ | Result _ ->
      None

let send ?finished recorder m w e =
  let s, actions, how = Machine.step ?finished m w.props w.s e in
  record recorder m ~from:w.s ~props:w.props s (Machine.event_name m e) how;
  (s, actions, how)

let set_props recorder m w props =
  let s, actions, how = Machine.set_props m ~previous:w.props props w.s in
  record recorder m ~from:w.s ~props s "props" how;
  (s, actions, how)

let explored recorder m taken =
  let init =
    match recorder.first with
    | Some name ->
        name
    | None ->
        Machine.name m (Machine.init m)
  in
  {
    Spec.id = Machine.id m;
    init;
    states =
      Hashtbl.fold (fun _ state acc -> state :: acc) recorder.seen []
      |> List.sort (fun (a : Spec.state) (b : Spec.state) ->
          compare (a.name <> init, a.name) (b.name <> init, b.name)
      );
    edges =
      List.filter_map
        (fun (l : Spec.label) ->
          if l.kept then
            None
          else
            Some (l.from, l.target)
        )
        taken
      |> List.sort_uniq compare;
  }

let finish ~recorder ~allow_unreached m = function
  | Ok ok ->
      let taken =
        Hashtbl.fold (fun edge () acc -> edge :: acc) recorder.taken []
        |> List.sort_uniq compare
      in
      let unreached =
        List.filter
          (fun name ->
            (not (Hashtbl.mem recorder.seen name))
            && (not (Hashtbl.mem recorder.passed name))
            && not (List.mem name allow_unreached)
          )
          (Option.value ~default:[] (Machine.states m))
      in
      if unreached <> [] then
        Unreached { states = unreached }
      else
        Ok { ok with edges = taken; spec = explored recorder m taken }
  | (Violation _ | Unreached _ | Limit _ | Error _) as r ->
      r

let server_actions actions =
  List.for_all
    (function
      | Action.Run
          ( Action.Measure _
          | Action.Copy _
          | Action.Load_script _
          | Action.Resume _
          | Action.Revert_navigation
          | Action.Preload _
          | Action.Load_chunk _
          | Action.Set_status _
          | Action.Set_document_attribute _
          | Action.Set_cookie _ ) ->
          false
      | _ ->
          true
      )
    actions

let start_props m = function
  | [] -> (
      match Machine.default_props m with
      | Some props ->
          Stdlib.Ok [ props ]
      | None ->
          Stdlib.Error
            ("the component " ^ Machine.id m
           ^ " has props: give them with ~props"
            )
    )
  | props ->
      Stdlib.Ok props

let component_with ~client_only_sources ~max_states ~within ~inputs ~props
    ~samples ~arrivals ~allow_unreached ~keys ~invariants
    (c : (_, _, _) Component.t) =
  let m = c.machine in
  match start_props m props with
  | Stdlib.Error message ->
      Error message
  | Stdlib.Ok all_props ->
      let host = { samples; arrivals } in
      let recorder = recorder () in
      let exec w actions = List.fold_left (host_action host) w actions in
      let start props =
        let s, actions = Machine.start m props in
        started recorder m s;
        see recorder m props s;
        ( exec
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
            actions,
          actions
        )
      in
      let view w = c.view w.props w.s in
      let model =
        {
          view;
          enabled =
            (fun w ->
              dedup
                (List.map
                   (fun event ->
                     { event = (Event event : (_, _) input); via = View }
                   )
                   (handlers ~keys (view w))
                @ List.map
                    (fun event ->
                      { event = (Event event : (_, _) input); via = Input }
                    )
                    (source_events (Machine.held m w.props w.s) @ inputs w.s)
                @ List.filter_map
                    (fun props ->
                      if props = w.props then
                        None
                      else
                        Some
                          { event = (Props props : (_, _) input); via = Input }
                    )
                    all_props
                @ List.map
                    (fun mv ->
                      { mv with event = (Event mv.event : (_, _) input) }
                    )
                    (resource_moves host w)
                )
            );
          step =
            (fun w mv ->
              let w = consume w mv.via in
              match (mv.event : (_, _) input) with
              | Event event ->
                  let s, actions, how =
                    send ?finished:(finished mv.via) recorder m w event
                  in
                  (exec { w with s } actions, actions, how)
              | Props props ->
                  let s, actions, how = set_props recorder m w props in
                  (exec { w with s; props } actions, actions, how)
            );
          final = (fun w -> Machine.is_final m w.s);
          start = List.map start all_props;
          name = (fun w -> Machine.name m w.s);
        }
      in
      let extra =
        owner_checks m
        @
        if client_only_sources then
          [
            ("live-uses-client-source", fun w -> Machine.held m w.props w.s = []);
          ]
        else
          []
      in
      let extra_actions =
        if client_only_sources then
          [
            ("live-uses-client-action", fun _ actions -> server_actions actions);
          ]
        else
          []
      in
      run ~extra_actions ~max_states ~within ~model ~invariants ~extra
        ~to_state:(fun w -> w.s)
        ()
      |> finish ~recorder ~allow_unreached m

let component ?(max_states = 100_000) ?(within = fun _ -> true)
    ?(inputs = fun _ -> []) ?(props = []) ?(samples = []) ?(arrivals = 2)
    ?(allow_unreached = []) ?(keys = default_keys) ?(invariants = []) c =
  component_with ~client_only_sources:false ~max_states ~within ~inputs ~props
    ~samples ~arrivals ~allow_unreached ~keys ~invariants c

let live ?(max_states = 100_000) ?(within = fun _ -> true)
    ?(inputs = fun _ -> []) ?(samples = []) ?(arrivals = 2)
    ?(allow_unreached = []) ?(keys = default_keys) ?(invariants = [])
    (live : (_, _) Live.t) =
  component_with ~client_only_sources:true ~max_states ~within ~inputs ~props:[]
    ~samples ~arrivals ~allow_unreached ~keys ~invariants live.component

let rec nth l i =
  match l with
  | x :: _ when i = 0 ->
      x
  | _ :: t ->
      nth t (i - 1)
  | [] ->
      failwith "history index out of range"
let rec take n = function x :: t when n > 0 -> x :: take (n - 1) t | _ -> []

let canonical_link ?is_not_found ~parse ~print href =
  let url = Url.of_string href in
  match parse url with
  | snapshot ->
      Url.to_string (print snapshot) = Url.to_string url
      && not
           ( match is_not_found with
           | Some is_not_found ->
               is_not_found snapshot
           | None ->
               false
           )
  | exception Failure _ ->
      false

let app ?(max_states = 100_000) ?(within = fun _ -> true) ?(max_history = 4)
    ?(inputs = fun _ -> []) ?(samples = []) ?(arrivals = 2)
    ?(allow_unreached = []) ?(keys = default_keys) ?(invariants = []) ~routes
    ~parse ~print ?is_not_found ~url_event root =
  match routes with
  | [] ->
      Error "no routes given"
  | first :: _ -> (
      let url0 = print first in
      let component : (_, _, _) Component.t = root url0 in
      let m = component.machine in
      let host = { samples; arrivals } in
      let recorder = recorder () in
      let rec apply depth (w, acc) actions =
        List.fold_left
          (fun (w, acc) c ->
            let nav hist idx u =
              if depth > 50 then
                failwith "navigation loop: actions keep navigating";
              let w = { w with hist; idx } in
              let s, cs, _ = send recorder m w (url_event u) in
              apply (depth + 1) ({ w with s }, acc @ cs) cs
            in
            match c with
            | Action.Run (Action.Navigate u) ->
                let h = take (w.idx + 1) w.hist @ [ u ] in
                if List.length h > max_history then
                  nav (List.tl h) w.idx u
                else
                  nav h (w.idx + 1) u
            | Action.Run (Action.Replace u) ->
                nav
                  (take w.idx w.hist
                  @ (u :: List.filteri (fun i _ -> i > w.idx) w.hist)
                  )
                  w.idx u
            | Action.Run Action.Back when w.idx > 0 ->
                nav w.hist (w.idx - 1) (nth w.hist (w.idx - 1))
            | Action.Run Action.Forward when w.idx < List.length w.hist - 1 ->
                nav w.hist (w.idx + 1) (nth w.hist (w.idx + 1))
            | Action.Run Action.Reload ->
                nav w.hist w.idx (nth w.hist w.idx)
            | c ->
                (host_action host w c, acc)
          )
          (w, acc) actions
      in
      let view w = component.view w.props w.s in
      let s0, actions0 = Machine.start m () in
      started recorder m s0;
      see recorder m () s0;
      let to_state w = { router = w.s; history = w.hist; index = w.idx } in
      let user event = { event; via = View } in
      let model =
        {
          view;
          enabled =
            (fun w ->
              dedup
                (List.map (fun e -> user (Event e)) (handlers ~keys (view w))
                @ List.map
                    (fun e -> { event = Event e; via = Input })
                    (source_events (Machine.held m w.props w.s)
                    @ inputs (to_state w)
                    )
                @ List.map
                    (fun mv -> { mv with event = Event mv.event })
                    (resource_moves host w)
                @ List.map
                    (fun h -> user (Link (Url.of_string h)))
                    (links (view w))
                @ ( if w.idx > 0 then
                      [ user Browser_back ]
                    else
                      []
                  )
                @
                if w.idx < List.length w.hist - 1 then
                  [ user Browser_forward ]
                else
                  []
                )
            );
          step =
            (fun w mv ->
              let navigate actions =
                let w, actions = apply 0 (w, []) actions in
                (w, actions, Machine.Kept)
              in
              match mv.event with
              | Event e ->
                  let w = consume w mv.via in
                  let s, actions, how =
                    send ?finished:(finished mv.via) recorder m w e
                  in
                  let w, actions = apply 0 ({ w with s }, actions) actions in
                  (w, actions, how)
              | Link u ->
                  navigate [ Action.Run (Action.Navigate u) ]
              | Browser_back ->
                  navigate [ Action.Run Action.Back ]
              | Browser_forward ->
                  navigate [ Action.Run Action.Forward ]
            );
          final = (fun w -> Machine.is_final m w.s);
          name = (fun w -> Machine.name m w.s);
          start =
            [
              apply 0
                ( {
                    props = ();
                    s = s0;
                    timers = [];
                    calls = [];
                    subs = [];
                    feeds = [];
                    results = [];
                    hist = [ url0 ];
                    idx = 0;
                  },
                  actions0
                )
                actions0;
            ];
        }
      in
      match
        List.find_opt
          (fun route ->
            match parse (print route) with
            | parsed ->
                parsed <> route
            | exception Failure _ ->
                true
          )
          routes
      with
      | Some _ ->
          Violation
            {
              invariant = "route-round-trip";
              trace = [];
              state = to_state (fst (List.hd model.start));
            }
      | None ->
          let extra =
            owner_checks m
            @ [
                ( "route-round-trip",
                  fun w ->
                    match parse (print w.s) with
                    | parsed ->
                        parsed = w.s
                    | exception Failure _ ->
                        false
                );
                ( "link-parses",
                  fun w ->
                    List.for_all
                      (canonical_link ?is_not_found ~parse ~print)
                      (links (view w))
                );
              ]
          in
          run ~max_states ~within ~model ~invariants ~extra ~to_state ()
          |> finish ~recorder ~allow_unreached m
    )

let pp_app_event pp ppf = function
  | Event e ->
      pp ppf e
  | Link u ->
      Format.fprintf ppf "link %s" (Url.to_string u)
  | Browser_back ->
      Format.pp_print_string ppf "back"
  | Browser_forward ->
      Format.pp_print_string ppf "forward"

let pp_input pp ppf : (_, _) input -> unit = function
  | Event e ->
      pp ppf e
  | Props _ ->
      Format.pp_print_string ppf "new props"

let pp_report ~pp_event ?pp_state ppf = function
  | Ok { states; transitions; not_explored; _ } ->
      Format.fprintf ppf "ok: %d states, %d transitions" states transitions;
      List.iter
        (fun entry -> Format.fprintf ppf "@\nnot explored in page: %s" entry)
        not_explored
  | Unreached { states } ->
      Format.fprintf ppf "states never reached: %s" (String.concat ", " states)
  | Limit { states } ->
      Format.fprintf ppf "limit reached after %d states" states
  | Error m ->
      Format.fprintf ppf "error: %s" m
  | Violation { invariant; trace; state } ->
      Format.fprintf ppf "@[<v>invariant %S violated after %d steps:" invariant
        (List.length trace);
      List.iteri
        (fun i e -> Format.fprintf ppf "@,  %d. %a" (i + 1) pp_event e)
        trace;
      Option.iter (fun pp -> Format.fprintf ppf "@,state: %a" pp state) pp_state;
      Format.fprintf ppf "@]"
