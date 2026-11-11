exception
  Undeclared_target of { machine : string; from : string; target : string }
exception Missing_case of { machine : string; index : int }

type ('s, 'e) outcome =
  | Unhandled
  | Stay of 'e Action.t list
  | Goto of { reenter : bool; target : 's; actions : 'e Action.t list }

type resource =
  | Timer of { id : string; ms : int }
  | Call of { id : string; path : string }
  | Subscription of { id : string; topic : string }
  | Held of { id : string; name : string }

type ('s, 'e) t = {
  id : string;
  init : 's;
  key : 's -> string;
  final : 's -> bool;
  step :
    's -> 'e -> 's * 'e Action.host list * [ `Unhandled | `Kept | `Entered ];
  enter : 's -> 'e Action.host list;
  first : 'e Action.host list;
  exit : 's -> 'e Action.host list;
  active : 's -> resource list;
  held : 's -> 'e Action.source list;
  covered : int list;
  spec : Spec.t;
  type_id : ('s * 'e) Type.Id.t;
}

let id m = m.id
let init m = m.init
let key m = m.key
let is_final m = m.final
let spec m = m.spec
let type_id m = m.type_id
let active m = m.active
let held m = m.held
let exit m = m.exit
let step m = m.step
let run actions = List.map (fun action -> Action.Run action) actions

let send m snapshot event =
  match m.step snapshot event with
  | s, actions, `Unhandled ->
      (s, actions, `Unhandled)
  | s, actions, (`Kept | `Entered) ->
      (s, actions, `Handled)

let start m =
  let s = m.init in
  (s, m.first @ m.enter s)

let map_snapshot m ~get ~set =
  {
    id = m.id;
    init = get m.init;
    key = (fun s -> m.key (set s));
    final = (fun s -> m.final (set s));
    step =
      (fun s e ->
        let s', actions, handled = m.step (set s) e in
        (get s', actions, handled)
      );
    enter = (fun s -> m.enter (set s));
    first = m.first;
    exit = (fun s -> m.exit (set s));
    active = (fun s -> m.active (set s));
    held = (fun s -> m.held (set s));
    covered = m.covered;
    spec = m.spec;
    type_id = Type.Id.make ();
  }

module Make (State : sig
  type 'p t
end) =
struct
  type snapshot = At : 'p State.t * 'p -> snapshot

  module Targets = struct
    type t = [] | ( :: ) : 'p State.t * t -> t
    type any = Any : 'p State.t -> any

    let rec to_list : t -> any list = function
      | [] ->
          []
      | state :: rest ->
          Any state :: to_list rest
  end

  type any = Targets.any = Any : 'p State.t -> any

  type ('p, 'e) state_case = {
    state : 'p State.t;
    name : string;
    to_ : any list;
    entry : 'p -> 'e Action.t list;
    exit : 'p -> 'e Action.t list;
    hooks : bool;
    after : (int * 'e) list;
    invoke : ('p, 'e) Source.t list;
    final : bool;
    on : 'p -> 'e -> (snapshot, 'e) outcome;
  }

  type 'e always_case = {
    always_to : any list;
    always_on : 'e -> (snapshot, 'e) outcome;
  }

  type 'e case =
    | State : ('p, 'e) state_case -> 'e case
    | Always : 'e always_case -> 'e case

  let at state data = At (state, data)

  (* A state is a constant constructor, so it is an immediate integer and two
     states are equal exactly when the integers are equal. *)
  let same : type a b. a State.t -> b State.t -> bool =
   fun a b -> Obj.repr a == Obj.repr b

  let index state =
    let repr = Obj.repr state in
    if Obj.is_int repr then
      (Obj.obj repr : int)
    else
      -1

  let goto ?(actions = []) state data =
    Goto { reenter = false; target = At (state, data); actions }

  let reenter ?(actions = []) state data =
    Goto { reenter = true; target = At (state, data); actions }

  let stay actions = Stay actions
  let unhandled = Unhandled
  let ignore _ _ = Unhandled

  let make (type e) ~id ~init ?(start = []) (cases : e case list) :
      (snapshot, e) t =
    let always_cases =
      List.filter_map (function Always a -> Some a | State _ -> None) cases
    in
    let find : type p. p State.t -> (p, e) state_case =
     fun state ->
      let rec go = function
        | [] ->
            raise (Missing_case { machine = id; index = index state })
        | Always _ :: rest ->
            go rest
        | State c :: rest ->
            if same c.state state then
              (* Equal constructors have equal indices, so the data types agree. *)
              (Obj.magic c : (p, e) state_case)
            else
              go rest
      in
      go cases
    in
    let name_of (Any state) = (find state).name in
    let check : type p. p State.t -> any list -> (snapshot, e) outcome -> unit =
     fun from declared outcome ->
      match outcome with
      | Goto { target = At (target, _); _ }
        when not
               (same from target
               || List.exists (fun (Any d) -> same d target) declared
               ) ->
          raise
            (Undeclared_target
               {
                 machine = id;
                 from = (find from).name;
                 target =
                   ( try (find target).name
                     with Missing_case { index; _ } ->
                       "#" ^ string_of_int index
                   );
               }
            )
      | _ ->
          ()
    in
    let timer_id c i = id ^ "/" ^ c.name ^ "/after" ^ string_of_int i in
    let call_id c i = id ^ "/" ^ c.name ^ "/invoke" ^ string_of_int i in
    let enter (At (state, data)) =
      let c = find state in
      List.map (fun action -> Action.Run action) (c.entry data)
      @ List.mapi
          (fun i (ms, event) ->
            Action.Start_timer { id = timer_id c i; ms; event }
          )
          c.after
      @ List.mapi
          (fun i source -> Source.start (call_id c i) data source)
          c.invoke
    in
    let exit (At (state, data)) =
      let c = find state in
      List.map (fun action -> Action.Run action) (c.exit data)
      @ List.mapi (fun i _ -> Action.Stop (timer_id c i)) c.after
      @ List.mapi (fun i _ -> Action.Stop (call_id c i)) c.invoke
    in
    let active (At (state, _)) =
      let c = find state in
      List.mapi (fun i (ms, _) -> Timer { id = timer_id c i; ms }) c.after
      @ List.mapi
          (fun i source ->
            match Source.topic source with
            | Some topic ->
                Subscription { id = call_id c i; topic }
            | None when Source.is_held source ->
                Held { id = call_id c i; name = Source.name source }
            | None ->
                Call { id = call_id c i; path = Source.name source }
          )
          c.invoke
    in
    let held (At (state, data)) =
      List.filter_map (Source.held data) (find state).invoke
    in
    let handle (At (state, data)) event =
      let c = find state in
      match c.on data event with
      | Unhandled ->
          let rec go = function
            | [] ->
                Unhandled
            | a :: rest -> (
                match a.always_on event with
                | Unhandled ->
                    go rest
                | outcome ->
                    check state a.always_to outcome;
                    outcome
              )
          in
          go always_cases
      | outcome ->
          check state c.to_ outcome;
          outcome
    in
    let step (At (state, _) as current) event =
      if (find state).final then
        (current, [], `Unhandled)
      else
        match handle current event with
        | Unhandled ->
            (current, [], `Unhandled)
        | Stay actions ->
            (current, run actions, `Kept)
        | Goto { reenter; target; actions } ->
            let (At (next, _)) = target in
            if reenter || not (same state next) then
              (target, exit current @ run actions @ enter target, `Entered)
            else
              (target, run actions, `Kept)
    in
    let (At (init_state, _)) = init in
    let spec =
      let always_targets =
        List.concat_map (fun a -> List.map name_of a.always_to) always_cases
      in
      let states =
        List.filter_map
          (function
            | State c ->
                Some
                  {
                    Spec.name = c.name;
                    final = c.final;
                    timers = List.map fst c.after;
                    sources =
                      List.filter_map
                        (fun source ->
                          if Source.is_held source then
                            None
                          else
                            Some (Source.name source)
                        )
                        c.invoke;
                    held =
                      List.filter_map
                        (fun source ->
                          if Source.is_held source then
                            Some (Source.name source)
                          else
                            None
                        )
                        c.invoke;
                  }
            | Always _ ->
                None
            )
          cases
      in
      let restarts c =
        match (c.after, c.invoke) with [], [] -> c.hooks | _ -> true
      in
      let edges =
        List.concat_map
          (function
            | State c when not c.final ->
                List.map (fun t -> (c.name, name_of t)) c.to_
                @ List.map (fun t -> (c.name, t)) always_targets
                @
                if restarts c then
                  [ (c.name, c.name) ]
                else
                  []
            | State _ | Always _ ->
                []
            )
          cases
        |> List.sort_uniq compare
      in
      { Spec.id; init = (find init_state).name; states; edges }
    in
    {
      id;
      init;
      key = (fun (At (state, _)) -> (find state).name);
      final = (fun (At (state, _)) -> (find state).final);
      step;
      enter;
      first = run start;
      exit;
      active;
      held;
      covered =
        List.filter_map
          (function State c -> Some (index c.state) | Always _ -> None)
          cases;
      spec;
      type_id = Type.Id.make ();
    }

  let no_actions _ = []
  let empty_list = []

  type targets = Targets.t = [] | ( :: ) : 'p State.t * targets -> targets

  let state state name ?(to_ = ([] : targets)) ?entry ?exit
      ?(after = empty_list) ?(invoke = empty_list) ?(final = false) on =
    State
      {
        state;
        name;
        to_ = Targets.to_list to_;
        entry = Option.value entry ~default:no_actions;
        exit = Option.value exit ~default:no_actions;
        hooks = Option.is_some entry || Option.is_some exit;
        after;
        invoke;
        final;
        on;
      }

  let always ?(to_ = ([] : targets)) on =
    Always { always_to = Targets.to_list to_; always_on = on }

  let cases_cover machine (states : targets) =
    List.iter
      (fun (Any state) ->
        if not (List.mem (index state) machine.covered) then
          raise (Missing_case { machine = machine.id; index = index state })
      )
      (Targets.to_list states)
end
