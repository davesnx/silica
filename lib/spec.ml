module Json = Silica_json.Json

type state = {
  name : string;
  final : bool;
  timers : int list;
  sources : string list;
  held : string list;
}

type label = { from : string; target : string; event : string; kept : bool }
type t = { id : string; init : string; states : state list; edges : label list }

let arrows t =
  List.sort_uniq compare
    (List.filter_map
       (fun label ->
         if label.kept then
           None
         else
           Some (label.from, label.target)
       )
       t.edges
    )

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
    (arrows t)

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

let edge_events t (a, b) =
  List.sort_uniq compare
    (List.filter_map
       (fun l ->
         if l.from = a && l.target = b && l.event <> "" && not l.kept then
           Some l.event
         else
           None
       )
       t.edges
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

let to_quint t =
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
           match edge_events t (a, c) with
           | [] ->
               [ (a, c, "") ]
           | events ->
               List.map (fun e -> (a, c, e)) events
         )
         (arrows t)
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
         t.edges
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
  let transitions =
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
            if List.exists (fun (a, _, _) -> a = s.name) transitions then
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
  List.iter
    (fun (_, name, body) -> pr "  action %s = %s\n" name body)
    transitions;
  ( match transitions with
  | [] ->
      pr
        "  action step = all {\n\
        \    state' = state,\n\
        \    running' = running,\n\
        \    last' = last,\n\
        \  }\n\n"
  | _ ->
      pr "\n  action step = any {\n";
      List.iter (fun (_, name, _) -> pr "    %s,\n" name) transitions;
      pr "  }\n\n"
  );
  pr "  val resources_owned = running.subseteq(owned(state))\n";
  pr "  val can_move = finals.contains(state) or movable.contains(state)\n";
  pr "  val inv = resources_owned and can_move\n";
  pr "}\n";
  Buffer.contents b

let details s =
  List.map (Printf.sprintf "after %dms") s.timers
  @ List.map (fun path -> "invoke " ^ path) s.sources
  @ List.map (fun name -> "hold " ^ name) s.held

let to_dot t =
  let b = Buffer.create 512 in
  let quote name = Printf.sprintf "\"%s.%s\"" t.id name in
  Printf.bprintf b "digraph \"%s\" {\nrankdir=LR;\n" t.id;
  Printf.bprintf b "\"%s.__init\" [shape=point];\n\"%s.__init\" -> %s;\n" t.id
    t.id (quote t.init);
  List.iter
    (fun s ->
      Printf.bprintf b "%s [label=\"%s\" shape=%s];\n" (quote s.name)
        (String.concat "\\n" (s.name :: details s))
        ( if s.final then
            "doublecircle"
          else
            "box"
        )
    )
    t.states;
  List.iter
    (fun (a, c) ->
      match edge_events t (a, c) with
      | [] ->
          Printf.bprintf b "%s -> %s;\n" (quote a) (quote c)
      | events ->
          Printf.bprintf b "%s -> %s [label=\"%s\"];\n" (quote a) (quote c)
            (String.concat ", " events)
    )
    (arrows t);
  Buffer.add_string b "}\n";
  Buffer.contents b

let bare word =
  word <> ""
  && String.for_all
       (fun c -> c > ' ' && c <> '"' && c <> '\\' && c <> '\127')
       word

let token word =
  if bare word then
    word
  else
    let b = Buffer.create (String.length word + 2) in
    Buffer.add_char b '"';
    String.iter
      (function
        | '"' ->
            Buffer.add_string b "\\\""
        | '\\' ->
            Buffer.add_string b "\\\\"
        | '\n' ->
            Buffer.add_string b "\\n"
        | '\t' ->
            Buffer.add_string b "\\t"
        | '\r' ->
            Buffer.add_string b "\\r"
        | c ->
            Buffer.add_char b c
        )
      word;
    Buffer.add_char b '"';
    Buffer.contents b

let label_events t ~kept (a, c) =
  List.sort_uniq compare
    (List.filter_map
       (fun l ->
         if l.from = a && l.target = c && l.kept = kept then
           Some l.event
         else
           None
       )
       t.edges
    )

let to_text t =
  let b = Buffer.create 512 in
  let line words =
    Buffer.add_string b (String.concat " " words);
    Buffer.add_char b '\n'
  in
  let on = function
    | [] | [ "" ] ->
        []
    | events ->
        "on" :: List.map token events
  in
  line [ "machine"; token t.id ];
  line [ "initial"; token t.init ];
  List.iter
    (fun s ->
      line
        ("state" :: token s.name
        ::
        ( if s.final then
            [ "final" ]
          else
            []
        )
        );
      List.iter (fun ms -> line [ "  after"; string_of_int ms ^ "ms" ]) s.timers;
      List.iter (fun path -> line [ "  invoke"; token path ]) s.sources;
      List.iter (fun name -> line [ "  hold"; token name ]) s.held
    )
    t.states;
  List.iter
    (fun (a, c) ->
      line
        ([ "edge"; token a; "->"; token c ]
        @ on (label_events t ~kept:false (a, c))
        )
    )
    (arrows t);
  List.iter
    (fun s ->
      if
        List.exists
          (fun l -> l.kept && l.from = s.name && l.target = s.name)
          t.edges
      then
        line
          ([ "stay"; token s.name ]
          @ on (label_events t ~kept:true (s.name, s.name))
          )
    )
    t.states;
  Buffer.contents b

let is_blank c = c = ' ' || c = '\t' || c = '\r'

let tokens text =
  let length = String.length text in
  let rec skip i =
    if i < length && is_blank text.[i] then
      skip (i + 1)
    else
      i
  in
  let rec words i acc =
    let i = skip i in
    if i >= length then
      Ok (List.rev acc)
    else if text.[i] = '"' then
      quoted (i + 1) (Buffer.create 16) acc
    else
      let rec stop j =
        if j < length && (not (is_blank text.[j])) && text.[j] <> '"' then
          stop (j + 1)
        else
          j
      in
      let j = stop i in
      words j (String.sub text i (j - i) :: acc)
  and quoted i b acc =
    if i >= length then
      Error "a quoted word has no closing quote"
    else
      match text.[i] with
      | '"' ->
          words (i + 1) (Buffer.contents b :: acc)
      | '\\' when i + 1 < length ->
          Buffer.add_char b
            ( match text.[i + 1] with
            | 'n' ->
                '\n'
            | 't' ->
                '\t'
            | 'r' ->
                '\r'
            | c ->
                c
            );
          quoted (i + 2) b acc
      | c ->
          Buffer.add_char b c;
          quoted (i + 1) b acc
  in
  words 0 []

let ( let* ) = Result.bind

let milliseconds word =
  let length = String.length word in
  if length > 2 && String.sub word (length - 2) 2 = "ms" then
    int_of_string_opt (String.sub word 0 (length - 2))
  else
    None

type reading = {
  machine : string option;
  initial : string option;
  read_states : state list;
  read_labels : label list;
}

let of_text text =
  let state name final =
    { name; final; timers = []; sources = []; held = [] }
  in
  let read number (reading : reading) words =
    let fail () =
      Error
        (Printf.sprintf "line %d: cannot read %S" number
           (String.concat " " words)
        )
    in
    let detail change =
      match reading.read_states with
      | current :: rest ->
          Ok { reading with read_states = change current :: rest }
      | [] ->
          Error (Printf.sprintf "line %d: a detail before any state" number)
    in
    let label ~kept from target event = { from; target; event; kept } in
    match words with
    | [] ->
        Ok reading
    | [ "machine"; id ] ->
        Ok { reading with machine = Some id }
    | [ "initial"; init ] ->
        Ok { reading with initial = Some init }
    | [ "state"; name ] ->
        Ok
          { reading with read_states = state name false :: reading.read_states }
    | [ "state"; name; "final" ] ->
        Ok { reading with read_states = state name true :: reading.read_states }
    | [ "after"; delay ] -> (
        match milliseconds delay with
        | Some ms ->
            detail (fun s -> { s with timers = s.timers @ [ ms ] })
        | None ->
            fail ()
      )
    | [ "invoke"; path ] ->
        detail (fun s -> { s with sources = s.sources @ [ path ] })
    | [ "hold"; name ] ->
        detail (fun s -> { s with held = s.held @ [ name ] })
    | [ "edge"; from; "->"; target ] ->
        Ok
          {
            reading with
            read_labels =
              label ~kept:false from target "" :: reading.read_labels;
          }
    | "edge" :: from :: "->" :: target :: "on" :: (_ :: _ as events) ->
        Ok
          {
            reading with
            read_labels =
              List.map (label ~kept:false from target) events
              @ reading.read_labels;
          }
    | [ "stay"; name ] ->
        Ok
          {
            reading with
            read_labels = label ~kept:true name name "" :: reading.read_labels;
          }
    | "stay" :: name :: "on" :: (_ :: _ as events) ->
        Ok
          {
            reading with
            read_labels =
              List.map (label ~kept:true name name) events @ reading.read_labels;
          }
    | _ ->
        fail ()
  in
  let* reading, _ =
    List.fold_left
      (fun acc text_line ->
        let* reading, number = acc in
        let* words =
          Result.map_error
            (Printf.sprintf "line %d: %s" number)
            (tokens text_line)
        in
        let* reading = read number reading words in
        Ok (reading, number + 1)
      )
      (Ok
         ( { machine = None; initial = None; read_states = []; read_labels = [] },
           1
         )
      )
      (String.split_on_char '\n' text)
  in
  match (reading.machine, reading.initial) with
  | None, _ ->
      Error "no machine line"
  | _, None ->
      Error "no initial line"
  | Some id, Some init ->
      Ok
        {
          id;
          init;
          states = List.rev reading.read_states;
          edges = List.sort_uniq compare reading.read_labels;
        }

let mermaid_text text =
  let b = Buffer.create (String.length text) in
  String.iter
    (function
      | '"' ->
          Buffer.add_string b "#quot;"
      | '#' ->
          Buffer.add_string b "#35;"
      | ';' ->
          Buffer.add_string b "#59;"
      | '\n' ->
          Buffer.add_char b ' '
      | c ->
          Buffer.add_char b c
      )
    text;
  Buffer.contents b

let to_mermaid t =
  let b = Buffer.create 512 in
  let ids = List.mapi (fun i s -> (s.name, Printf.sprintf "s%d" i)) t.states in
  let id name = List.assoc_opt name ids in
  Buffer.add_string b "stateDiagram-v2\n  direction LR\n";
  List.iter
    (fun s ->
      let title =
        match details s with
        | [] ->
            s.name
        | details ->
            s.name ^ " (" ^ String.concat ", " details ^ ")"
      in
      Printf.bprintf b "  state \"%s\" as %s\n" (mermaid_text title)
        (List.assoc s.name ids)
    )
    t.states;
  Option.iter (Printf.bprintf b "  [*] --> %s\n") (id t.init);
  List.iter
    (fun (a, c) ->
      match (id a, id c, edge_events t (a, c)) with
      | Some from, Some target, [] ->
          Printf.bprintf b "  %s --> %s\n" from target
      | Some from, Some target, events ->
          Printf.bprintf b "  %s --> %s : %s\n" from target
            (mermaid_text (String.concat ", " events))
      | _ ->
          ()
    )
    (arrows t);
  List.iter
    (fun s ->
      if s.final then Printf.bprintf b "  %s --> [*]\n" (List.assoc s.name ids)
    )
    t.states;
  Buffer.contents b

let to_json t =
  let string text = Json.declassify (`String text) in
  let list f items = Json.declassify (`List (List.map f items)) in
  let bool value = Json.declassify (`Bool value) in
  let int value = Json.declassify (`Int value) in
  let record fields = Json.declassify (`Assoc fields) in
  record
    [
      ("id", string t.id);
      ("init", string t.init);
      ( "states",
        list
          (fun s ->
            record
              [
                ("name", string s.name);
                ("final", bool s.final);
                ("timers", list int s.timers);
                ("sources", list string s.sources);
                ("held", list string s.held);
              ]
          )
          t.states
      );
      ( "edges",
        list
          (fun l ->
            record
              [
                ("from", string l.from);
                ("target", string l.target);
                ("event", string l.event);
                ("kept", bool l.kept);
              ]
          )
          t.edges
      );
    ]

let field name json =
  match Json.classify json with
  | `Assoc fields -> (
      match List.assoc_opt name fields with
      | Some value ->
          Ok value
      | None ->
          Error (Printf.sprintf "no field %S" name)
    )
  | _ ->
      Error (Printf.sprintf "expected an object with the field %S" name)

let json_string json =
  match Json.classify json with
  | `String text ->
      Ok text
  | _ ->
      Error "expected a string"

let json_bool json =
  match Json.classify json with
  | `Bool value ->
      Ok value
  | _ ->
      Error "expected a boolean"

let json_int json =
  match Json.classify json with
  | `Int value ->
      Ok value
  | `Float value when Float.is_integer value ->
      Ok (int_of_float value)
  | _ ->
      Error "expected an integer"

let json_list read json =
  match Json.classify json with
  | `List items ->
      List.fold_right
        (fun item acc ->
          let* acc = acc in
          let* value = read item in
          Ok (value :: acc)
        )
        items (Ok [])
  | _ ->
      Error "expected a list"

let json_field name read json =
  let* value = field name json in
  Result.map_error (Printf.sprintf "%s: %s" name) (read value)

let of_json json =
  let* id = json_field "id" json_string json in
  let* init = json_field "init" json_string json in
  let* states =
    json_field "states"
      (json_list (fun json ->
           let* name = json_field "name" json_string json in
           let* final = json_field "final" json_bool json in
           let* timers = json_field "timers" (json_list json_int) json in
           let* sources = json_field "sources" (json_list json_string) json in
           let* held = json_field "held" (json_list json_string) json in
           Ok { name; final; timers; sources; held }
       )
      )
      json
  in
  let* edges =
    json_field "edges"
      (json_list (fun json ->
           let* from = json_field "from" json_string json in
           let* target = json_field "target" json_string json in
           let* event = json_field "event" json_string json in
           let* kept = json_field "kept" json_bool json in
           Ok { from; target; event; kept }
       )
      )
      json
  in
  Ok { id; init; states; edges }

type change =
  | Initial_changed of { before : string; after : string }
  | State_added of state
  | State_removed of state
  | State_changed of { before : state; after : state }
  | Edge_added of label
  | Edge_removed of label

let diff before after =
  let named states name = List.find_opt (fun s -> s.name = name) states in
  let initial =
    if before.init = after.init then
      []
    else
      [ Initial_changed { before = before.init; after = after.init } ]
  in
  let removed =
    List.filter_map
      (fun s ->
        match named after.states s.name with
        | None ->
            Some (State_removed s)
        | Some _ ->
            None
      )
      before.states
  in
  let added_or_changed =
    List.filter_map
      (fun s ->
        match named before.states s.name with
        | None ->
            Some (State_added s)
        | Some old when old <> s ->
            Some (State_changed { before = old; after = s })
        | Some _ ->
            None
      )
      after.states
  in
  let edges_removed =
    List.filter_map
      (fun label ->
        if List.mem label after.edges then
          None
        else
          Some (Edge_removed label)
      )
      before.edges
  in
  let edges_added =
    List.filter_map
      (fun label ->
        if List.mem label before.edges then
          None
        else
          Some (Edge_added label)
      )
      after.edges
  in
  initial @ removed @ added_or_changed @ edges_removed @ edges_added

let label_text label =
  let on = match label.event with "" -> "" | event -> " on " ^ token event in
  if label.kept then
    "stay " ^ token label.from ^ on
  else
    "edge " ^ token label.from ^ " -> " ^ token label.target ^ on

let state_changes before after =
  let compare_lists items_before items_after =
    List.filter_map
      (fun item ->
        if List.mem item items_before then
          None
        else
          Some ("added " ^ item)
      )
      items_after
    @ List.filter_map
        (fun item ->
          if List.mem item items_after then
            None
          else
            Some ("removed " ^ item)
        )
        items_before
  in
  ( match (before.final, after.final) with
    | false, true ->
        [ "now final" ]
    | true, false ->
        [ "no longer final" ]
    | _ ->
        []
    )
  @ compare_lists (details before) (details after)

let change_to_string = function
  | Initial_changed { before; after } ->
      Printf.sprintf "changed initial state from %s to %s" (token before)
        (token after)
  | State_added s ->
      "added state " ^ token s.name
  | State_removed s ->
      "removed state " ^ token s.name
  | State_changed { before; after } ->
      Printf.sprintf "changed state %s: %s" (token after.name)
        (String.concat "; " (state_changes before after))
  | Edge_added label ->
      "added " ^ label_text label
  | Edge_removed label ->
      "removed " ^ label_text label
