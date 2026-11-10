type state = {
  name : string;
  final : bool;
  timers : int list;
  sources : string list;
  held : string list;
}

type t = {
  id : string;
  init : string;
  states : state list;
  edges : (string * string) list;
}

type kind = Unreachable | Dead_end
type issue = { machine : string; state : string; kind : kind }

let leaves t name =
  List.filter_map
    (fun (a, b) ->
      if a = name && b <> name then
        Some b
      else
        None
    )
    t.edges

let reachable t =
  let rec go seen = function
    | [] ->
        seen
    | name :: rest when List.mem name seen ->
        go seen rest
    | name :: rest ->
        go (name :: seen) (leaves t name @ rest)
  in
  go [] [ t.init ]

let lint t =
  let seen = reachable t in
  let several = List.length t.states > 1 in
  List.concat_map
    (fun s ->
      ( if List.mem s.name seen then
          []
        else
          [ { machine = t.id; state = s.name; kind = Unreachable } ]
      )
      @
      if several && (not s.final) && leaves t s.name = [] then
        [ { machine = t.id; state = s.name; kind = Dead_end } ]
      else
        []
    )
    t.states

let issue_to_string i =
  Printf.sprintf "%s.%s: %s" i.machine i.state
    ( match i.kind with
    | Unreachable ->
        "unreachable"
    | Dead_end ->
        "non-final dead end"
    )

type label = { from : string; target : string; event : string; kept : bool }

let words name =
  String.split_on_char ' '
    (String.map
       (fun c ->
         match c with 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> c | _ -> ' '
       )
       name
    )
  |> List.filter (fun w -> w <> "")

let camel name =
  String.concat ""
    (List.map
       (fun w -> String.capitalize_ascii (String.lowercase_ascii w))
       (words name)
    )

let snake name = String.concat "_" (List.map String.lowercase_ascii (words name))

let edge_events labels (a, b) =
  List.sort_uniq compare
    (List.filter_map
       (fun l ->
         if l.from = a && l.target = b && l.event <> "" && not l.kept then
           Some l.event
         else
           None
       )
       labels
    )

let constructors t =
  List.rev
    (snd
       (List.fold_left
          (fun (i, acc) s ->
            let base = camel s.name in
            let name =
              if
                base = ""
                || (base.[0] >= '0' && base.[0] <= '9')
                || List.exists (fun (_, c) -> c = base) acc
              then
                Printf.sprintf "S%d%s" i base
              else
                base
            in
            (i + 1, (s.name, name) :: acc)
          )
          (1, []) t.states
       )
    )

let lower_snake constructor =
  let b = Buffer.create (String.length constructor + 4) in
  String.iteri
    (fun i c ->
      if c >= 'A' && c <= 'Z' && i > 0 then Buffer.add_char b '_';
      Buffer.add_char b (Char.lowercase_ascii c)
    )
    constructor;
  Buffer.contents b

let to_quint ?(labels = []) t =
  let b = Buffer.create 2048 in
  let names = constructors t in
  let state name = List.assoc name names in
  let event e =
    if e = "" then
      "OnAny"
    else
      "On" ^ camel e
  in
  let module_name =
    match snake t.id with
    | "" ->
        "m"
    | name when name.[0] >= '0' && name.[0] <= '9' ->
        "m_" ^ name
    | name ->
        name
  in
  let edges =
    List.sort_uniq compare
      (List.concat_map
         (fun (a, c) ->
           match edge_events labels (a, c) with
           | [] ->
               [ (a, c, "") ]
           | events ->
               List.map (fun e -> (a, c, e)) events
         )
         (List.sort_uniq compare
            (t.edges
            @ List.filter_map
                (fun l ->
                  if l.kept then
                    None
                  else
                    Some (l.from, l.target)
                )
                labels
            )
         )
      )
  in
  let stays =
    List.sort_uniq compare
      (List.filter_map
         (fun l ->
           if l.kept then
             Some (l.from, l.event)
           else
             None
         )
         labels
      )
  in
  let events =
    List.sort_uniq compare
      (List.filter
         (fun e -> e <> "")
         (List.map (fun (_, _, e) -> e) edges @ List.map snd stays)
      )
  in
  let set items = "Set(" ^ String.concat ", " items ^ ")" in
  let owned s =
    List.map (Printf.sprintf "\"%s/after %dms\"" s.name) s.timers
    @ List.map (Printf.sprintf "\"%s/invoke %s\"" s.name) s.sources
    @ List.map (Printf.sprintf "\"%s/hold %s\"" s.name) s.held
  in
  let on e =
    if e = "" then
      "any"
    else
      snake e
  in
  let actions =
    List.map
      (fun (a, c, e) ->
        ( a,
          Printf.sprintf "%s_to_%s_on_%s"
            (lower_snake (state a))
            (lower_snake (state c))
            (on e),
          Printf.sprintf "all { state == %s, enter(%s, %s) }" (state a)
            (state c) (event e)
        )
      )
      edges
    @ List.map
        (fun (a, e) ->
          ( a,
            Printf.sprintf "%s_stays_on_%s" (lower_snake (state a)) (on e),
            Printf.sprintf
              "all { state == %s, state' = state, running' = running, last' = \
               %s }"
              (state a) (event e)
          )
        )
        stays
  in
  let pr fmt = Printf.bprintf b fmt in
  pr "// Generated from the spec of the machine %S. Do not edit.\n" t.id;
  pr "module %s {\n" module_name;
  pr "  type State = | %s\n"
    (String.concat " | " (List.map (fun s -> state s.name) t.states));
  pr "  type Event = %s\n\n"
    (String.concat " | " ("OnStart" :: "OnAny" :: List.map event events));
  pr "  var state: State\n  var running: Set[str]\n  var last: Event\n\n";
  pr "  pure def owned(s: State): Set[str] = match s {\n";
  List.iter
    (fun s -> pr "    | %s => %s\n" (state s.name) (set (owned s)))
    t.states;
  pr "  }\n\n";
  pr "  pure val finals: Set[State] = %s\n"
    (set
       (List.filter_map
          (fun s ->
            if s.final then
              Some (state s.name)
            else
              None
          )
          t.states
       )
    );
  pr "  pure val movable: Set[State] = %s\n\n"
    (set
       (List.filter_map
          (fun s ->
            if List.exists (fun (a, _, _) -> a = s.name) actions then
              Some (state s.name)
            else
              None
          )
          t.states
       )
    );
  pr
    "  action enter(s: State, e: Event): bool = all {\n\
    \    state' = s,\n\
    \    running' = owned(s),\n\
    \    last' = e,\n\
    \  }\n\n";
  pr
    "  action init = all {\n\
    \    state' = %s,\n\
    \    running' = owned(%s),\n\
    \    last' = OnStart,\n\
    \  }\n\n"
    (state t.init) (state t.init);
  List.iter (fun (_, name, body) -> pr "  action %s = %s\n" name body) actions;
  ( match actions with
  | [] ->
      pr
        "  action step = all {\n\
        \    state' = state,\n\
        \    running' = running,\n\
        \    last' = last,\n\
        \  }\n\n"
  | _ ->
      pr "\n  action step = any {\n";
      List.iter (fun (_, name, _) -> pr "    %s,\n" name) actions;
      pr "  }\n\n"
  );
  pr "  val resources_owned = running.subseteq(owned(state))\n";
  pr "  val can_move = finals.contains(state) or movable.contains(state)\n";
  pr "  val inv = resources_owned and can_move\n";
  pr "}\n";
  Buffer.contents b

let to_dot ?(labels = []) t =
  let b = Buffer.create 512 in
  let quote name = Printf.sprintf "\"%s.%s\"" t.id name in
  Printf.bprintf b "digraph \"%s\" {\nrankdir=LR;\n" t.id;
  Printf.bprintf b "\"%s.__init\" [shape=point];\n\"%s.__init\" -> %s;\n" t.id
    t.id (quote t.init);
  List.iter
    (fun s ->
      let details =
        List.map (Printf.sprintf "after %dms") s.timers
        @ List.map (fun path -> "invoke " ^ path) s.sources
        @ List.map (fun name -> "hold " ^ name) s.held
      in
      Printf.bprintf b "%s [label=\"%s\" shape=%s];\n" (quote s.name)
        (String.concat "\\n" (s.name :: details))
        ( if s.final then
            "doublecircle"
          else
            "box"
        )
    )
    t.states;
  List.iter
    (fun (a, c) ->
      match edge_events labels (a, c) with
      | [] ->
          Printf.bprintf b "%s -> %s;\n" (quote a) (quote c)
      | events ->
          Printf.bprintf b "%s -> %s [label=\"%s\"];\n" (quote a) (quote c)
            (String.concat ", " events)
    )
    t.edges;
  Buffer.add_string b "}\n";
  Buffer.contents b
