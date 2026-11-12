type ('s, 'e) next =
  | Ignore
  | Stay of 'e Action.t list
  | Go of 's
  | Go_with of 's * 'e Action.t list
  | Reenter of 's

let map_next f = function
  | Ignore ->
      Ignore
  | Stay actions ->
      Stay actions
  | Go state ->
      Go (f state)
  | Go_with (state, actions) ->
      Go_with (f state, actions)
  | Reenter state ->
      Reenter (f state)

type resource =
  | Timer of { id : string; ms : int }
  | Call of { id : string; path : string }
  | Subscription of { id : string; topic : string }
  | Held of { id : string; name : string }

type how = Unhandled | Kept | Entered
type 's shape = {
  shape_name : 's -> string;
  shape_names : string list option;
  fingerprint : 's Fingerprint.t option;
}

let shape ~name ?names ?fingerprint () =
  { shape_name = name; shape_names = names; fingerprint }

let shape_name shape = shape.shape_name
let shape_names shape = Option.value shape.shape_names ~default:[]

type ('p, 's, 'e) t = {
  id : string;
  init : 's;
  shape : 's shape;
  events : 'e shape option;
  final : 's -> bool;
  owns : 'p -> 's -> 'e Source.t list;
  entry : 'p -> 's -> 'e Action.t list;
  exit : 'p -> 's -> 'e Action.t list;
  first : 'e Action.t list;
  on_props : 'p option -> 'p -> 's -> ('s, 'e) next;
  update : 'p -> 's -> 'e -> ('s, 'e) next;
  default_props : 'p option;
  type_id : ('p * 's * 'e) Type.Id.t;
}

let id machine = machine.id
let init machine = machine.init
let name machine = machine.shape.shape_name
let states machine = machine.shape.shape_names
let fingerprint machine = machine.shape.fingerprint

let event_name machine event =
  match machine.events with
  | Some events ->
      events.shape_name event
  | None ->
      ""

type codecs =
  | Codecs : {
      id : string;
      type_id : ('p * 's * 'e) Type.Id.t;
      props : 'p Codec.t option;
      state : 's Codec.t option;
      event : 'e Codec.t option;
    }
      -> codecs

let registered : codecs list ref = ref []

let codecs (type p s e) (machine : (p, s, e) t) :
    (p Codec.t option * s Codec.t option * e Codec.t option) option =
  let rec find :
      codecs list ->
      (p Codec.t option * s Codec.t option * e Codec.t option) option = function
    | [] ->
        None
    | Codecs entry :: rest -> (
        match Type.Id.provably_equal entry.type_id machine.type_id with
        | Some Type.Equal ->
            Some (entry.props, entry.state, entry.event)
        | None ->
            find rest
      )
  in
  find !registered

let register_codecs machine ?props ?state ?event () =
  registered :=
    Codecs { id = machine.id; type_id = machine.type_id; props; state; event }
    :: List.filter
         (fun (Codecs { type_id; _ }) ->
           Option.is_none (Type.Id.provably_equal type_id machine.type_id)
         )
         !registered

let codec machine what = function
  | Some (Some codec) ->
      Ok codec
  | Some None ->
      Error
        ("the machine \"" ^ machine.id ^ "\" is registered with no " ^ what
       ^ " codec: give ~" ^ what ^ " to Machine.register_codecs"
        )
  | None ->
      Error
        ("the machine \"" ^ machine.id
       ^ "\" has no registered codecs: call Machine.register_codecs on it in \
          the codec module of the app (for example Linear_codecs.register), \
          and call that module before asking for a codec"
        )

let props_codec machine =
  codec machine "props"
    (Option.map (fun (props, _, _) -> props) (codecs machine))

let state_codec machine =
  codec machine "state"
    (Option.map (fun (_, state, _) -> state) (codecs machine))

let event_codec machine =
  codec machine "event"
    (Option.map (fun (_, _, event) -> event) (codecs machine))

let registered_codecs () = List.map (fun (Codecs { id; _ }) -> id) !registered
let is_final machine = machine.final
let default_props machine = machine.default_props
let type_id machine = machine.type_id
let run actions = List.map (fun action -> Action.Run action) actions

let owned machine props state =
  let prefix = machine.id ^ "/" ^ name machine state ^ "/" in
  let counts = Hashtbl.create 8 in
  List.map
    (fun source ->
      let key = Source.key source in
      let count = 1 + Option.value ~default:0 (Hashtbl.find_opt counts key) in
      Hashtbl.replace counts key count;
      let unique =
        if count = 1 then
          key
        else
          key ^ "#" ^ string_of_int count
      in
      (prefix ^ unique, source)
    )
    (machine.owns props state)

let active machine props state =
  List.map
    (fun (id, source) ->
      match (Source.delay source, Source.topic source) with
      | Some ms, _ ->
          Timer { id; ms }
      | None, Some topic ->
          Subscription { id; topic }
      | None, None when Source.is_held source ->
          Held { id; name = Source.name source }
      | None, None ->
          Call { id; path = Source.name source }
    )
    (owned machine props state)

let held machine props state =
  List.filter_map Source.held (machine.owns props state)

let starts sources =
  List.map (fun (id, source) -> Source.start id source) sources
let stops sources = List.map (fun (id, _) -> Action.Stop id) sources

let enter machine props state =
  run (machine.entry props state) @ starts (owned machine props state)

let exit machine props state =
  run (machine.exit props state) @ stops (owned machine props state)

let without others sources =
  List.filter (fun (id, _) -> not (List.mem_assoc id others)) sources

let move ?finished machine ~from_props ~to_props state next ~reenter actions =
  if reenter || name machine next <> name machine state then
    ( next,
      exit machine from_props state @ run actions @ enter machine to_props next,
      Entered
    )
  else
    let before =
      List.filter
        (fun (id, _) -> Some id <> finished)
        (owned machine from_props state)
    and after = owned machine to_props next in
    let changed =
      List.filter
        (fun (id, source) ->
          match List.assoc_opt id before with
          | Some old ->
              not (Source.same old source)
          | None ->
              false
        )
        after
    in
    ( next,
      stops (without after before @ changed)
      @ run actions
      @ starts (without before after @ changed),
      Kept
    )

let apply ?finished machine ~from_props ~to_props state = function
  | Ignore ->
      move ?finished machine ~from_props ~to_props state state ~reenter:false []
  | Stay actions ->
      move ?finished machine ~from_props ~to_props state state ~reenter:false
        actions
  | Go next ->
      move ?finished machine ~from_props ~to_props state next ~reenter:false []
  | Go_with (next, actions) ->
      move ?finished machine ~from_props ~to_props state next ~reenter:false
        actions
  | Reenter next ->
      move machine ~from_props ~to_props state next ~reenter:true []

let step ?finished machine props state event =
  if machine.final state then
    (state, [], Unhandled)
  else
    match machine.update props state event with
    | Ignore ->
        (state, [], Unhandled)
    | (Stay _ | Go _ | Go_with _ | Reenter _) as next ->
        apply ?finished machine ~from_props:props ~to_props:props state next

let can machine props state event =
  (not (machine.final state))
  &&
  match machine.update props state event with
  | Ignore ->
      false
  | Stay _ | Go _ | Go_with _ | Reenter _ ->
      true

let props_step machine ~previous props state =
  let next =
    if machine.final state then
      Ignore
    else
      machine.on_props previous props state
  in
  let from_props = Option.value previous ~default:props in
  let state, actions, how =
    apply machine ~from_props ~to_props:props state next
  in
  match (next, actions) with
  | Ignore, [] ->
      (state, [], Unhandled)
  | (Ignore | Stay _ | Go _ | Go_with _ | Reenter _), _ ->
      (state, actions, how)

let start machine props =
  let state = machine.init in
  let state, actions, _ = props_step machine ~previous:None props state in
  (state, run machine.first @ enter machine props machine.init @ actions)

let resume machine props state = starts (owned machine props state)

let set_props machine ~previous props state =
  props_step machine ~previous:(Some previous) props state

let no_actions _ _ = []
let owns_nothing _ _ = []
let never _ = false
let ignore_props _ _ _ = Ignore

let make_with_props ~id ~init ~shape ?events ?(owns = owns_nothing)
    ?(entry = no_actions) ?(exit = no_actions) ?(final = never) ?(start = [])
    ?(on_props = ignore_props) update =
  {
    id;
    init;
    shape;
    events;
    final;
    owns;
    entry;
    exit;
    first = start;
    on_props;
    update;
    default_props = None;
    type_id = Type.Id.make ();
  }

let make ~id ~init ~shape ?events ?owns ?entry ?exit ?final ?start update =
  let without_props f = Option.map (fun f () state -> f state) f in
  {
    (make_with_props ~id ~init ~shape ?events ?owns:(without_props owns)
       ?entry:(without_props entry) ?exit:(without_props exit) ?final ?start
       (fun () state event -> update state event
     )
    )
    with
    default_props = Some ();
  }
