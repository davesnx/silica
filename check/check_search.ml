open Silica

type ('s, 'ev) found = {
  rule : string;
  message : string;
  hint : string;
  element : string option;
  state : 's;
  state_name : string;
  view : string;
  prefix : int option;
  last : ('ev Check_model.move * string) option;
}

type ('n, 's, 'ev) result = {
  nodes : 'n Dynarray.t;
  parents : (int * 'ev Check_model.move) option Dynarray.t;
  transitions : int;
  complete : bool;
  truncated : bool;
  violations : ('s, 'ev) found list;
  allowed : (Check_rules.allowance * int) list;
  firsts : (string * int) list;
  views : (string * string) list;
  not_explored : string list;
  children : Check_model.children;
}

type ('n, 'e, 'ev) rule =
  string
  * ('n ->
    'e Node.t ->
    'ev Check_model.move list ->
    'ev option ->
    Check_rules.finding list
    )

let functional message =
  let part = "functional value" in
  let length = String.length part in
  let rec go index =
    index + length <= String.length message
    && (String.sub message index length = part || go (index + 1))
  in
  go 0

let closure_error machine =
  Check_model.Invalid_check
    ("a state, event or props value of the machine " ^ machine
   ^ " holds a function; states, events and props must be plain data that the \
      checker can compare"
    )

let guard machine f =
  match f () with
  | value ->
      Ok value
  | exception Invalid_argument message when functional message ->
      raise (closure_error machine)
  | exception (Check_model.Invalid_check _ as error) ->
      raise error
  | exception Out_of_memory ->
      raise Out_of_memory
  | exception error ->
      Error error

let raised ~where error =
  Check_rules.finding "raised"
    ~values:[ ("where", where); ("exception", Printexc.to_string error) ]

let path parents index =
  let rec go index acc =
    match Dynarray.get parents index with
    | None ->
        acc
    | Some (parent, move) ->
        go parent ((parent, move, index) :: acc)
  in
  go index []

let run ~(model : ('n, 's, 'ev, 'e) Check_model.model)
    ~(rules : ('n, 'e, 'ev) rule list) ~max_states ~max_violations ~within
    ~(allow : Check_rules.allowance list) =
  let machine = model.machine in
  let nodes = Dynarray.create () in
  let parents = Dynarray.create () in
  let seen = Hashtbl.create 1024 in
  let queue = Queue.create () in
  let transitions = ref 0 in
  let complete = ref true in
  let truncated = ref false in
  let keys = Hashtbl.create 16 in
  let found = ref [] in
  let count = ref 0 in
  let allowed_keys = Hashtbl.create 16 in
  let allowed = Array.make (List.length allow) 0 in
  let firsts = Hashtbl.create 16 in
  let first_order = ref [] in
  let views = ref [] in
  let not_explored = ref [] in
  let children = ref [] in
  let report ~state ~state_name ~view ~prefix ~last
      (finding : Check_rules.finding) =
    if not !truncated then
      let message, hint = Check_rules.render finding ~state:state_name in
      let key = (finding.rule, state_name, finding.element, message) in
      let rec allowance index = function
        | [] ->
            None
        | first :: rest ->
            if
              Check_rules.allows first ~rule:finding.rule ~state:state_name
                ~element:finding.element
            then
              Some index
            else
              allowance (index + 1) rest
      in
      match allowance 0 allow with
      | Some index ->
          if not (Hashtbl.mem allowed_keys key) then (
            Hashtbl.add allowed_keys key ();
            allowed.(index) <- allowed.(index) + 1
          )
      | None ->
          if not (Hashtbl.mem keys key) then
            if !count >= max_violations then
              truncated := true
            else (
              Hashtbl.add keys key ();
              incr count;
              found :=
                {
                  rule = finding.rule;
                  message;
                  hint;
                  element = finding.element;
                  state;
                  state_name;
                  view = Lazy.force view;
                  prefix;
                  last;
                }
                :: !found
            )
  in
  let text view = lazy (View_text.to_string view) in
  let no_view = lazy "" in
  let check_node index node view moves event =
    let state = model.snapshot node and state_name = model.name node in
    List.iter
      (fun (where, rule) ->
        let findings =
          match guard machine (fun () -> rule node view moves event) with
          | Ok findings ->
              findings
          | Error error ->
              [ raised ~where error ]
        in
        List.iter
          (report ~state ~state_name ~view:(text view) ~prefix:(Some index)
             ~last:None
          )
          findings
      )
      rules
  in
  let plain node =
    match node = node with
    | _ ->
        ()
    | exception Invalid_argument message when functional message ->
        raise (closure_error machine)
  in
  let add parent node view =
    if within (model.snapshot node) then
      let hash = Hashtbl.hash_param 100 200 node in
      let bucket = Option.value ~default:[] (Hashtbl.find_opt seen hash) in
      let known =
        match List.exists (fun other -> other = node) bucket with
        | known ->
            known
        | exception Invalid_argument message when functional message ->
            raise (closure_error machine)
      in
      if not known then
        let () = plain node in
        if Dynarray.length nodes >= max_states then
          complete := false
        else
          let failed where error =
            report ~state:(model.snapshot node) ~state_name:(model.name node)
              ~view:no_view ~prefix:(Option.map fst parent)
              ~last:
                (Option.map (fun (_, move) -> (move, model.name node)) parent)
              (raised ~where error)
          in
          match guard machine (fun () -> Lazy.force view) with
          | Error error ->
              failed "view" error
          | Ok view -> (
              match guard machine (fun () -> model.enabled node view) with
              | Error error ->
                  failed "handlers or inputs" error
              | Ok moves ->
                  let index = Dynarray.length nodes in
                  Dynarray.add_last nodes node;
                  Dynarray.add_last parents parent;
                  Hashtbl.replace seen hash (node :: bucket);
                  let name = model.name node in
                  if not (Hashtbl.mem firsts name) then (
                    Hashtbl.replace firsts name index;
                    first_order := name :: !first_order;
                    views := (name, View_text.to_string view) :: !views
                  );
                  not_explored :=
                    List.rev_append
                      (Check_rules.not_explored_of_view view)
                      !not_explored;
                  children :=
                    List.rev_append
                      (Check_model.children_of_view view)
                      !children;
                  Queue.add (index, moves) queue;
                  check_node index node view moves
                    (Option.map
                       (fun (_, (move : _ Check_model.move)) -> move.event)
                       parent
                    )
            )
  in
  let starts =
    match guard machine model.start with
    | Ok starts ->
        starts
    | Error error ->
        report ~state:model.initial ~state_name:model.initial_name ~view:no_view
          ~prefix:None ~last:None
          (raised ~where:"start" error);
        []
  in
  let setup_state, setup_name =
    match starts with
    | (node, _) :: _ ->
        (model.snapshot node, model.name node)
    | [] ->
        (model.initial, model.initial_name)
  in
  List.iter
    (report ~state:setup_state ~state_name:setup_name ~view:no_view ~prefix:None
       ~last:None
    )
    (model.setup_findings ());
  List.iter
    (fun (node, actions) ->
      let view = lazy (model.view node) in
      let values = [ ("event", "start"); ("from", model.name node) ] in
      ( match
          guard machine (fun () -> model.action_rules ~values view actions)
        with
      | Ok findings ->
          List.iter
            (report ~state:(model.snapshot node) ~state_name:(model.name node)
               ~view:
                 ( lazy
                   ( match Lazy.force view with
                   | view ->
                       View_text.to_string view
                   | exception _ ->
                       ""
                   )
                   )
               ~prefix:None ~last:None
            )
            findings
      | Error _ ->
          ()
      );
      add None node view
    )
    starts;
  let label event =
    match model.event_name event with "" -> "(unnamed)" | name -> name
  in
  while (not !truncated) && not (Queue.is_empty queue) do
    let index, moves = Queue.pop queue in
    let node = Dynarray.get nodes index in
    let from = model.name node in
    List.iter
      (fun (move : _ Check_model.move) ->
        incr transitions;
        if not !truncated then
          match guard machine (fun () -> model.step node move) with
          | Error error ->
              report ~state:(model.snapshot node) ~state_name:from
                ~view:(lazy (View_text.to_string (model.view node)))
                ~prefix:(Some index)
                ~last:(Some (move, from))
                (raised ~where:"update" error)
          | Ok (_, _, Machine.Unhandled)
            when not (Check_model.ignorable move.via) ->
              let view = model.view node in
              let cause = model.cause node view move in
              let element =
                match cause with
                | Check_model.View { element; _ } ->
                    Some element
                | _ ->
                    None
              in
              report ~state:(model.snapshot node) ~state_name:from
                ~view:(text view) ~prefix:(Some index)
                ~last:(Some (move, from))
                (Check_rules.finding "ignored-event" ?element
                   ~values:
                     [
                       ("event", label move.event);
                       ("cause", Check_model.describe_cause cause);
                     ]
                )
          | Ok (next, actions, _) ->
              let view = lazy (model.view next) in
              let target = model.name next in
              let values = [ ("event", label move.event); ("from", from) ] in
              ( match
                  guard machine (fun () ->
                      model.action_rules ~values view actions
                  )
                with
              | Ok findings ->
                  List.iter
                    (report ~state:(model.snapshot next) ~state_name:target
                       ~view:
                         ( lazy
                           ( match Lazy.force view with
                           | view ->
                               View_text.to_string view
                           | exception _ ->
                               ""
                           )
                           )
                       ~prefix:(Some index)
                       ~last:(Some (move, target))
                    )
                    findings
              | Error _ ->
                  ()
              );
              add (Some (index, move)) next view
      )
      moves
  done;
  {
    nodes;
    parents;
    transitions = !transitions;
    complete = !complete;
    truncated = !truncated;
    violations = List.rev !found;
    allowed =
      List.mapi (fun index allowance -> (allowance, allowed.(index))) allow;
    firsts =
      List.rev_map (fun name -> (name, Hashtbl.find firsts name)) !first_order;
    views = List.rev !views;
    not_explored = List.sort_uniq String.compare !not_explored;
    children = List.rev !children;
  }
