open Silica

type ('s, 'e) invariant = { name : string; holds : 's -> 'e Node.t -> bool }

type sample =
  | Answers : {
      endpoint : ('i, 'o) Remote.endpoint;
      values : 'o list;
      decode : bool;
    }
      -> sample
  | States : { topic : 's Topic.t; values : 's list } -> sample

let answers ?(decode = false) endpoint values =
  Answers { endpoint; values; decode }

let states topic values = States { topic; values }

type seen = Seen : ('s, 'e) Component.t * 'e -> seen
type children = seen list

let props (type s e) children (c : (s, e) Component.t) : e list =
  List.fold_left
    (fun (acc : e list) (Seen (child, event)) ->
      match
        Type.Id.provably_equal
          (Machine.type_id child.Component.machine)
          (Machine.type_id c.machine)
      with
      | Some Type.Equal ->
          if List.mem event acc then
            acc
          else
            event :: acc
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
    }
  | Violation of { invariant : string; trace : 'ev list; state : 's }
  | Unreached of { edges : (string * string) list }
  | Limit of { states : int }
  | Error of string

type phase = Connecting | Up | Down | Ended

type sub = { phase : phase; arrivals : int; last : int option; dropped : bool }

type ('s, 'e) world = {
  s : 's;
  timers : (string * 'e) list;
  calls : string list;
  subs : (string * sub) list;
  results : 'e list;
  hist : Url.t list;
  idx : int;
}

type via =
  | User
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
  step : 'n -> 'ev move -> 'n * 'e Cmd.host list;
  final : 'n -> bool;
  start : 'n * 'e Cmd.host list;
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

let source_events (sources : _ Cmd.source list) =
  List.concat_map
    (function
      | Cmd.Global_keys { bindings; _ } ->
          List.map snd bindings
      | Cmd.Outside_press { event; _ } ->
          [ event ]
      | Cmd.Resize { on_resize; _ } ->
          [ on_resize Rect.zero ]
      | Cmd.Intersection { on_change; _ } ->
          [ on_change true; on_change false ]
      | Cmd.Dismiss { on_escape; on_outside; on_focus_out; _ } ->
          Option.to_list on_escape @ Option.to_list on_outside
          @ Option.to_list on_focus_out
      | Cmd.Scroll { on_scroll; _ } ->
          [ on_scroll Scroll.zero ]
      | Cmd.Visibility on_change
      | Cmd.Online on_change
      | Cmd.Media { on_change; _ } ->
          [ on_change true; on_change false ]
      | Cmd.Block { on_attempt } ->
          [
            on_attempt (Nav.Push (Url.of_string "/"));
            on_attempt (Nav.Traverse { delta = -1; url = Url.of_string "/" });
          ]
      | Cmd.Focus_trap _ | Cmd.Scroll_lock | Cmd.Anchor _ ->
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

let focus_targets_present view cmds =
  let present = refs view in
  List.for_all
    (function
      | Cmd.Run
          ( Cmd.Focus r
          | Cmd.Blur r
          | Cmd.Scroll_into_view r
          | Cmd.Scroll_to { target = Some r; _ } ) ->
          List.mem (Ref.name r) present
      | _ ->
          true
      )
    cmds

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
      | Node.Component { component; props = Some props; _ } ->
          [ Seen (component, props) ]
      | _ ->
          []
      )
    n

let search ~max_states ~within ~model ~checks ~command_checks ~to_state =
  let parents = Dynarray.create () in
  let seen = Hashtbl.create 1024 in
  let queue = Queue.create () in
  let result = ref None in
  let transitions = ref 0 in
  let not_explored = ref [] in
  let children = ref [] in
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
  let failed_command n cmds =
    List.find_map
      (fun (name, holds) ->
        if holds n cmds then
          None
        else
          Some name
      )
      command_checks
  in
  let n0, cmds0 = model.start in
  ( match failed_command n0 cmds0 with
  | None ->
      add None n0
  | Some name ->
      fail name [] n0
  );
  while Option.is_none !result && not (Queue.is_empty queue) do
    let i, n = Queue.pop queue in
    List.iter
      (fun mv ->
        incr transitions;
        if Option.is_none !result then
          match model.step n mv with
          | n', cmds -> (
              match failed_command n' cmds with
              | None ->
                  add (Some (i, mv)) n'
              | Some name ->
                  fail name (trace i @ [ mv ]) n'
            )
          | exception Machine.Undeclared_target _ ->
              fail "undeclared-target" (trace i @ [ mv ]) n
          | exception Machine.Missing_case _ ->
              fail "missing-case" (trace i @ [ mv ]) n
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
        }

let run ?(extra_commands = []) ~max_states ~within ~model ~invariants ~extra
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
          || match model.enabled n with [] -> false | _ -> true
      );
    ]
  in
  let user =
    List.map
      (fun inv -> (inv.name, fun n -> inv.holds (to_state n) (model.view n)))
      invariants
  in
  try
    search ~max_states ~within ~model
      ~checks:(builtin @ extra @ user)
      ~command_checks:
        (( "focus-target",
           fun n cmds -> focus_targets_present (model.view n) cmds
         )
        :: extra_commands
        )
      ~to_state
  with
  | Invalid_argument m when functional m ->
      Error
        "state or event holds a functional value; states and events must be \
         first-order"
  | Failure m ->
      Error m

type 'e feed = {
  count : int;
  loading : 'e;
  ready : int -> int -> 'e;
  stale : int -> 'e;
  failed : int option -> 'e;
}

type 'e host = {
  samples : sample list;
  arrivals : int;
  answers_of : (string, 'e list) Hashtbl.t;
  feeds : (string, 'e feed) Hashtbl.t;
}

let answers_for (type o e) samples (endpoint : (_, o) Remote.endpoint)
    (on_result : (o, Remote.error) result -> e) =
  let given =
    List.filter_map
      (function
        | Answers a -> (
            match
              Type.Id.provably_equal
                (Remote.output_id a.endpoint)
                (Remote.output_id endpoint)
            with
            | Some Type.Equal ->
                Some ((a.values : o list), a.decode)
            | None ->
                None
          )
        | States _ ->
            None
        )
      samples
  in
  List.concat_map
    (fun (values, _) -> List.map (fun v -> on_result (Ok v)) values)
    given
  @ [
      on_result (Error (Remote.Http 500));
      on_result (Error (Remote.Network "down"));
    ]
  @
  if List.exists snd given then
    [ on_result (Error (Remote.Decode "sample")) ]
  else
    []

let feed_for (type s e) samples (topic : s Topic.t)
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
  {
    count = Array.length values;
    loading = on_update { Topic.seq = 0; status = Status.Loading None };
    ready =
      (fun i seq ->
        on_update
          { seq; status = Status.Ready { data = values.(i); stale = false } }
      );
    stale =
      (fun i ->
        on_update
          { seq = 0; status = Status.Ready { data = values.(i); stale = true } }
      );
    failed =
      (fun last ->
        on_update
          {
            seq = 0;
            status =
              Status.Failed
                {
                  error = Remote.Network "down";
                  last = Option.map (Array.get values) last;
                };
          }
      );
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

let host_cmd host w = function
  | Cmd.Start_timer { id; event; _ } ->
      { w with timers = put id event w.timers }
  | Cmd.Start_call { id; endpoint; on_result; _ } ->
      if not (Hashtbl.mem host.answers_of id) then
        Hashtbl.replace host.answers_of id
          (answers_for host.samples endpoint on_result);
      { w with calls = List.sort_uniq compare (id :: w.calls) }
  | Cmd.Start_live { id; topic; on_update; _ } ->
      if not (Hashtbl.mem host.feeds id) then
        Hashtbl.replace host.feeds id (feed_for host.samples topic on_update);
      { w with subs = put id fresh w.subs }
  | Cmd.Stop id ->
      {
        w with
        timers = List.remove_assoc id w.timers;
        calls = List.filter (fun c -> c <> id) w.calls;
        subs = List.remove_assoc id w.subs;
      }
  | Cmd.Run command ->
      let results =
        match command with
        | Cmd.Measure { on_rect; _ } ->
            [ on_rect (Some Rect.zero) ]
        | Cmd.Copy { on_result = Some on_result; _ } ->
            [ on_result true ]
        | Cmd.Load_script { on_result; _ } ->
            [ on_result true ]
        | Cmd.Load_chunk { on_ready; _ } ->
            Option.to_list on_ready
        | Cmd.Copy { on_result = None; _ }
        | Cmd.Focus _
        | Cmd.Blur _
        | Cmd.Scroll_into_view _
        | Cmd.Scroll_to _
        | Cmd.Navigate _
        | Cmd.Replace _
        | Cmd.Back
        | Cmd.Forward
        | Cmd.Reload
        | Cmd.Set_title _
        | Cmd.Set_status _
        | Cmd.Set_document_attribute _
        | Cmd.Set_cookie _
        | Cmd.Resume _
        | Cmd.Revert_navigation
        | Cmd.Preload _
        | Cmd.Remount ->
            []
      in
      if results = [] then
        w
      else
        { w with results = List.sort compare (results @ w.results) }
  | Cmd.Start_source _ ->
      w

let live_moves host id sub =
  let feed = Hashtbl.find host.feeds id in
  let move event sub = { event; via = Feed (id, sub) } in
  match sub.phase with
  | Connecting ->
      [ move feed.loading { sub with phase = Up } ]
  | Ended ->
      []
  | Up | Down ->
      let arrivals =
        if sub.arrivals < host.arrivals || sub.phase = Down then
          List.init feed.count (fun i ->
              move
                (feed.ready i (sub.arrivals + 1))
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
            [ move (feed.stale i) { sub with phase = Down; dropped = true } ]
        | _ ->
            []
      in
      arrivals @ loss
      @ [ move (feed.failed sub.last) { sub with phase = Ended } ]

let resource_moves host w =
  List.map (fun (id, event) -> { event; via = Fire id }) w.timers
  @ List.concat_map
      (fun id ->
        List.map
          (fun event -> { event; via = Answer id })
          (Hashtbl.find host.answers_of id)
      )
      w.calls
  @ List.concat_map (fun (id, sub) -> live_moves host id sub) w.subs
  @ List.mapi (fun i event -> { event; via = Result i }) w.results

let consume w = function
  | User ->
      w
  | Fire id ->
      { w with timers = List.remove_assoc id w.timers }
  | Answer id ->
      { w with calls = List.filter (fun c -> c <> id) w.calls }
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
      (Machine.active m w.s)
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
          (w.calls @ List.map fst w.subs)
    );
  ]

type 'e recorder = {
  taken : (Spec.label, unit) Hashtbl.t;
  label : 'e -> string;
}

let send recorder m w e =
  let s, cmds, how = Machine.step m w.s e in
  ( match how with
  | `Entered | `Kept ->
      Hashtbl.replace recorder.taken
        {
          Spec.from = Machine.key m w.s;
          target = Machine.key m s;
          event = recorder.label e;
          kept = how = `Kept;
        }
        ()
  | `Unhandled ->
      ()
  );
  (s, cmds)

let finish ~recorder ~allow_unreached m = function
  | Ok ok ->
      let taken =
        Hashtbl.fold (fun edge () acc -> edge :: acc) recorder.taken []
        |> List.sort_uniq compare
      in
      let unreached =
        List.filter
          (fun (a, b) ->
            a <> b
            && (not
                  (List.exists
                     (fun (l : Spec.label) ->
                       l.from = a && l.target = b && not l.kept
                     )
                     taken
                  )
               )
            && not (List.mem (a, b) allow_unreached)
          )
          (Machine.spec m).Spec.edges
      in
      if unreached <> [] then
        Unreached { edges = unreached }
      else
        Ok { ok with edges = taken }
  | (Violation _ | Unreached _ | Limit _ | Error _) as r ->
      r

let server_commands cmds =
  List.for_all
    (function
      | Cmd.Run
          ( Cmd.Measure _
          | Cmd.Copy _
          | Cmd.Load_script _
          | Cmd.Resume _
          | Cmd.Revert_navigation
          | Cmd.Preload _
          | Cmd.Load_chunk _
          | Cmd.Set_status _
          | Cmd.Set_document_attribute _
          | Cmd.Set_cookie _ ) ->
          false
      | _ ->
          true
      )
    cmds

let component_with ~client_only_sources ~max_states ~within ~inputs ~props
    ~samples ~arrivals ~allow_unreached ~label ~keys ~invariants
    (c : (_, _) Component.t) =
  let m = c.machine in
  let host =
    {
      samples;
      arrivals;
      answers_of = Hashtbl.create 16;
      feeds = Hashtbl.create 16;
    }
  in
  let recorder = { taken = Hashtbl.create 64; label } in
  let exec w cmds = List.fold_left (host_cmd host) w cmds in
  let w0 s =
    { s; timers = []; calls = []; subs = []; results = []; hist = []; idx = 0 }
  in
  let s0, cmds0 = Machine.start m in
  let model =
    {
      view = (fun w -> c.view w.s);
      enabled =
        (fun w ->
          dedup
            (List.map
               (fun event -> { event; via = User })
               (handlers ~keys (c.view w.s)
               @ source_events (Machine.held m w.s)
               @ inputs w.s @ props
               )
            @ resource_moves host w
            )
        );
      step =
        (fun w mv ->
          let w = consume w mv.via in
          let s, cmds = send recorder m w mv.event in
          (exec { w with s } cmds, cmds)
        );
      final = (fun w -> Machine.is_final m w.s);
      start = (exec (w0 s0) cmds0, cmds0);
    }
  in
  let extra =
    owner_checks m
    @
    if client_only_sources then
      [ ("live-uses-client-source", fun w -> Machine.held m w.s = []) ]
    else
      []
  in
  let extra_commands =
    if client_only_sources then
      [ ("live-uses-client-command", fun _ cmds -> server_commands cmds) ]
    else
      []
  in
  run ~extra_commands ~max_states ~within ~model ~invariants ~extra
    ~to_state:(fun w -> w.s)
    ()
  |> finish ~recorder ~allow_unreached m

let component ?(max_states = 100_000) ?(within = fun _ -> true)
    ?(inputs = fun _ -> []) ?(props = []) ?(samples = []) ?(arrivals = 2)
    ?(allow_unreached = []) ?(label = fun _ -> "") ?(keys = default_keys)
    ?(invariants = []) c =
  component_with ~client_only_sources:false ~max_states ~within ~inputs ~props
    ~samples ~arrivals ~allow_unreached ~label ~keys ~invariants c

let live ?(max_states = 100_000) ?(within = fun _ -> true)
    ?(inputs = fun _ -> []) ?(props = []) ?(samples = []) ?(arrivals = 2)
    ?(allow_unreached = []) ?(label = fun _ -> "") ?(keys = default_keys)
    ?(invariants = []) (live : (_, _) Live.t) =
  component_with ~client_only_sources:true ~max_states ~within ~inputs ~props
    ~samples ~arrivals ~allow_unreached ~label ~keys ~invariants live.component

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
    ?(allow_unreached = []) ?(label = fun _ -> "") ?(keys = default_keys)
    ?(invariants = []) ~routes ~parse ~print ?is_not_found ~url_event root =
  match routes with
  | [] ->
      Error "no routes given"
  | first :: _ -> (
      let url0 = print first in
      let component : (_, _) Component.t = root url0 in
      let m = component.machine in
      let host =
        {
          samples;
          arrivals;
          answers_of = Hashtbl.create 16;
          feeds = Hashtbl.create 16;
        }
      in
      let recorder = { taken = Hashtbl.create 64; label } in
      let rec apply depth (w, acc) cmds =
        List.fold_left
          (fun (w, acc) c ->
            let nav hist idx u =
              if depth > 50 then
                failwith "navigation loop: commands keep navigating";
              let w = { w with hist; idx } in
              let s, cs = send recorder m w (url_event u) in
              apply (depth + 1) ({ w with s }, acc @ cs) cs
            in
            match c with
            | Cmd.Run (Cmd.Navigate u) ->
                let h = take (w.idx + 1) w.hist @ [ u ] in
                if List.length h > max_history then
                  nav (List.tl h) w.idx u
                else
                  nav h (w.idx + 1) u
            | Cmd.Run (Cmd.Replace u) ->
                nav
                  (take w.idx w.hist
                  @ (u :: List.filteri (fun i _ -> i > w.idx) w.hist)
                  )
                  w.idx u
            | Cmd.Run Cmd.Back when w.idx > 0 ->
                nav w.hist (w.idx - 1) (nth w.hist (w.idx - 1))
            | Cmd.Run Cmd.Forward when w.idx < List.length w.hist - 1 ->
                nav w.hist (w.idx + 1) (nth w.hist (w.idx + 1))
            | Cmd.Run Cmd.Reload ->
                nav w.hist w.idx (nth w.hist w.idx)
            | c ->
                (host_cmd host w c, acc)
          )
          (w, acc) cmds
      in
      let view w = component.view w.s in
      let s0, cmds0 = Machine.start m in
      let to_state w = { router = w.s; history = w.hist; index = w.idx } in
      let user event = { event; via = User } in
      let model =
        {
          view;
          enabled =
            (fun w ->
              dedup
                (List.map
                   (fun e -> user (Event e))
                   (handlers ~keys (view w)
                   @ source_events (Machine.held m w.s)
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
              match mv.event with
              | Event e ->
                  let w = consume w mv.via in
                  let s, cmds = send recorder m w e in
                  apply 0 ({ w with s }, cmds) cmds
              | Link u ->
                  apply 0 (w, []) [ Cmd.Run (Cmd.Navigate u) ]
              | Browser_back ->
                  apply 0 (w, []) [ Cmd.Run Cmd.Back ]
              | Browser_forward ->
                  apply 0 (w, []) [ Cmd.Run Cmd.Forward ]
            );
          final = (fun w -> Machine.is_final m w.s);
          start =
            apply 0
              ( {
                  s = s0;
                  timers = [];
                  calls = [];
                  subs = [];
                  results = [];
                  hist = [ url0 ];
                  idx = 0;
                },
                cmds0
              )
              cmds0;
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
              state = to_state (fst model.start);
            }
      | None ->
          let extra =
            owner_checks m
            @ [
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

let pp_report ~pp_event ?pp_state ppf = function
  | Ok { states; transitions; not_explored; _ } ->
      Format.fprintf ppf "ok: %d states, %d transitions" states transitions;
      List.iter
        (fun entry -> Format.fprintf ppf "@\nnot explored in page: %s" entry)
        not_explored
  | Unreached { edges } ->
      Format.fprintf ppf "declared edges never taken: %s"
        (String.concat ", " (List.map (fun (a, b) -> a ^ " -> " ^ b) edges))
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
