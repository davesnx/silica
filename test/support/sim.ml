open Silica

type call = {
  id : string;
  path : string;
  input : string;
  timeout_ms : int option;
  respond : string -> unit;
  reject : string -> unit;
  fail : Remote.error -> unit;
}

type subscription =
  | Subscription : {
      id : string;
      topic : 's Topic.t;
      key : string;
      on_update : 's Topic.update -> Instance.message;
    }
      -> subscription

type ('p, 's, 'e) t = {
  tree_set_props : 'p -> unit;
  update :
    Instance.message option ->
    Instance.message Node.t * Instance.message Action.host list;
  tree_snapshot : unit -> 's;
  tree_count : unit -> int;
  tree_message : 'e -> Instance.message;
  tree_unmount : unit -> Instance.message Action.host list;
  mutable node : Instance.message Node.t;
  mutable timers : (string * Instance.message) list;
  mutable calls : call list;
  mutable sources : (string * Instance.message Action.source) list;
  mutable subscriptions : subscription list;
  mutable ever : (string * (unit -> unit)) list;
  mutable log : string list;
}

let note t line = t.log <- line :: t.log

let nav_text = function
  | Nav.Push u ->
      "push " ^ Url.to_string u
  | Nav.Replace u ->
      "replace " ^ Url.to_string u
  | Nav.Traverse { delta; url } ->
      Printf.sprintf "traverse %d %s" delta (Url.to_string url)

let rec apply t actions =
  List.iter
    (function
      | Action.Start_timer { id; event; _ } ->
          note t ("start " ^ id);
          t.timers <- (id, event) :: List.remove_assoc id t.timers;
          t.ever <- (id, fun () -> deliver t event) :: t.ever
      | Action.Start_source { id; source } ->
          note t ("start " ^ id);
          t.sources <- (id, source) :: List.remove_assoc id t.sources
      | Action.Start_call { id; endpoint; input; timeout_ms; on_result } ->
          note t ("start " ^ id);
          let call =
            {
              id;
              path = Remote.path endpoint;
              input = Codec.to_string (Remote.input endpoint) input;
              timeout_ms;
              respond =
                (fun wire ->
                  match Codec.of_string (Remote.output endpoint) wire with
                  | Ok value ->
                      deliver t (on_result (Remote.Ok value))
                  | Error message ->
                      failwith message
                );
              reject =
                (fun wire ->
                  match Codec.of_string (Remote.rejected endpoint) wire with
                  | Ok value ->
                      deliver t (on_result (Remote.Rejected value))
                  | Error message ->
                      failwith message
                );
              fail = (fun error -> deliver t (on_result (Remote.Failed error)));
            }
          in
          t.calls <- call :: List.filter (fun c -> c.id <> id) t.calls;
          t.ever <- (id, fun () -> call.fail (Remote.Network "stale")) :: t.ever
      | Action.Start_live { id; topic; key; on_update } ->
          note t ("start " ^ id);
          t.subscriptions <-
            Subscription { id; topic; key; on_update }
            :: List.filter (fun (Subscription s) -> s.id <> id) t.subscriptions
      | Action.Stop id ->
          note t ("stop " ^ id);
          t.subscriptions <-
            List.filter (fun (Subscription s) -> s.id <> id) t.subscriptions;
          t.timers <- List.remove_assoc id t.timers;
          t.sources <- List.remove_assoc id t.sources;
          t.calls <- List.filter (fun c -> c.id <> id) t.calls
      | Action.Run (Action.Focus r) ->
          note t ("focus " ^ Ref.name r)
      | Action.Run (Action.Blur r) ->
          note t ("blur " ^ Ref.name r)
      | Action.Run (Action.Scroll_into_view r) ->
          note t ("scroll " ^ Ref.name r)
      | Action.Run (Action.Scroll_to { target; left; top }) ->
          note t
            (Printf.sprintf "scroll_to %s %d %d"
               (Option.fold ~none:"window" ~some:Ref.name target)
               left top
            )
      | Action.Run (Action.Measure { target; _ }) ->
          note t ("measure " ^ Ref.name target)
      | Action.Run (Action.Copy { text; _ }) ->
          note t ("copy " ^ text)
      | Action.Run (Action.Load_script { src; _ }) ->
          note t ("load_script " ^ src)
      | Action.Run (Action.Navigate u) ->
          note t ("navigate " ^ Url.to_string u)
      | Action.Run (Action.Replace u) ->
          note t ("replace " ^ Url.to_string u)
      | Action.Run Action.Back ->
          note t "back"
      | Action.Run Action.Forward ->
          note t "forward"
      | Action.Run Action.Reload ->
          note t "reload"
      | Action.Run (Action.Set_title title) ->
          note t ("title " ^ title)
      | Action.Run Action.Remount ->
          note t "remount"
      | Action.Run (Action.Set_status status) ->
          note t ("status " ^ string_of_int status)
      | Action.Run (Action.Set_document_attribute { name; value }) ->
          note t
            ("document " ^ name ^ " " ^ Option.value value ~default:"(none)")
      | Action.Run (Action.Set_cookie { name; value; _ }) ->
          note t ("cookie " ^ name ^ " " ^ value)
      | Action.Run (Action.Resume nav) ->
          note t ("resume " ^ nav_text nav)
      | Action.Run Action.Revert_navigation ->
          note t "revert"
      | Action.Run (Action.Preload (Remote.Request (endpoint, input))) ->
          note t
            ("preload " ^ Remote.path endpoint ^ " "
            ^ Codec.to_string (Remote.input endpoint) input
            )
      | Action.Run (Action.Load_chunk _) ->
          note t "load chunk"
      )
    actions

and deliver t message =
  let node, actions = t.update (Some message) in
  t.node <- node;
  apply t actions

let mount_with_props ?dev ~props component =
  let tree =
    Instance.mount_with_props ?dev ~on_error:(fun _ _ -> ()) ~props component
  in
  let t =
    {
      tree_set_props = Instance.set_props tree;
      update = Instance.update tree;
      tree_snapshot = (fun () -> Instance.snapshot tree);
      tree_count = (fun () -> Instance.count tree);
      tree_message = Instance.message tree;
      tree_unmount = (fun () -> Instance.unmount tree);
      node = Node.text "";
      timers = [];
      calls = [];
      sources = [];
      subscriptions = [];
      ever = [];
      log = [];
    }
  in
  let node, actions = t.update None in
  t.node <- node;
  apply t actions;
  t

let mount ?dev component = mount_with_props ?dev ~props:() component
let node t = t.node

let html t = Render.to_string t.node

let rec inline_portals node =
  match Node.view node with
  | Node.Element (tag, attrs, children) ->
      Node.element tag attrs (List.map inline_portals children)
  | Node.List children ->
      Node.list (List.map inline_portals children)
  | Node.Keyed (key, child) ->
      Node.keyed key (inline_portals child)
  | Node.Portal (_, children) ->
      Node.element "div"
        [ Attr.string "data-silica-portal" "" ]
        (List.map inline_portals children)
  | Node.Boundary { body; _ } ->
      inline_portals body
  | Node.Text _ | Node.Component _ | Node.Chunk _ ->
      node

let page t = Render.to_string (inline_portals t.node)
let snapshot t = t.tree_snapshot ()
let count t = t.tree_count ()
let send t event = deliver t (t.tree_message event)

let set_props t props =
  t.tree_set_props props;
  let node, actions = t.update None in
  t.node <- node;
  apply t actions

let rec find f node =
  match Node.view node with
  | Node.Element (_, attrs, children) as shape -> (
      match f shape attrs with
      | Some found ->
          Some found
      | None ->
          List.find_map (find f) children
    )
  | Node.List children ->
      List.find_map (find f) children
  | Node.Keyed (_, child) ->
      find f child
  | Node.Portal (_, children) ->
      List.find_map (find f) children
  | Node.Boundary { body; _ } ->
      find f body
  | Node.Text _ | Node.Component _ | Node.Chunk _ ->
      None

let rec text node =
  match Node.view node with
  | Node.Text s ->
      s
  | Node.Element (_, _, children) | Node.List children ->
      String.concat "" (List.map text children)
  | Node.Keyed (_, child) ->
      text child
  | Node.Portal (_, children) ->
      String.concat "" (List.map text children)
  | Node.Boundary { body; _ } ->
      text body
  | Node.Component _ | Node.Chunk _ ->
      ""

let press t wanted =
  let contains hay needle =
    let n = String.length needle and h = String.length hay in
    let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
    go 0
  in
  let found =
    find
      (fun shape attrs ->
        let handler =
          List.find_map (function Node.On_press m -> Some m | _ -> None) attrs
        in
        match (handler, shape) with
        | Some message, Node.Element (_, _, children)
          when contains (String.concat "" (List.map text children)) wanted ->
            Some message
        | _ ->
            None
      )
      t.node
  in
  match found with
  | Some message ->
      deliver t message
  | None ->
      failwith ("Sim.press: nothing pressable with text " ^ wanted)

let submit t =
  let found =
    find
      (fun _ attrs ->
        List.find_map (function Node.On_submit m -> Some m | _ -> None) attrs
      )
      t.node
  in
  match found with
  | Some message ->
      deliver t message
  | None ->
      failwith "Sim.submit: no form with a submit handler"

let input t ~label value =
  let found =
    find
      (fun _ attrs ->
        if List.mem (Node.String ("aria-label", label)) attrs then
          List.find_map
            (function Node.On_input f -> Some (f value) | _ -> None)
            attrs
        else
          None
      )
      t.node
  in
  match found with
  | Some message ->
      deliver t message
  | None ->
      failwith ("Sim.input: no input with label " ^ label)

let running t =
  List.sort compare
    (List.map fst t.timers
    @ List.map (fun c -> c.id) t.calls
    @ List.map fst t.sources
    )

let sources t =
  List.rev_map (fun (id, source) -> (id, Action.source_name source)) t.sources

let press_key t key =
  let delivered = ref false in
  List.iter
    (fun (_, source) ->
      match source with
      | Action.Global_keys { bindings; _ } ->
          List.iter
            (fun (bound, message) ->
              if (not !delivered) && Key.equal bound key then (
                delivered := true;
                deliver t message
              )
            )
            bindings
      | _ ->
          ()
    )
    t.sources;
  !delivered

let outside_press t =
  let events =
    List.filter_map
      (fun (_, source) ->
        match (source : Instance.message Action.source) with
        | Action.Outside_press { event; _ } ->
            Some event
        | Action.Dismiss { on_outside; _ } ->
            on_outside
        | _ ->
            None
      )
      t.sources
  in
  match events with
  | [] ->
      failwith "Sim.outside_press: no outside press source is running"
  | events ->
      List.iter (deliver t) events

let escape t =
  match
    List.find_map
      (fun (_, source) ->
        match (source : Instance.message Action.source) with
        | Action.Dismiss { on_escape; _ } ->
            on_escape
        | _ ->
            None
      )
      t.sources
  with
  | Some message ->
      deliver t message;
      true
  | None ->
      false

let rec path_to id node =
  match Node.view node with
  | Node.Element (_, attrs, children) ->
      if List.mem (Node.String ("id", id)) attrs then
        Some [ attrs ]
      else
        Option.map
          (fun below -> below @ [ attrs ])
          (List.find_map (path_to id) children)
  | Node.List children | Node.Portal (_, children) ->
      List.find_map (path_to id) children
  | Node.Keyed (_, child) ->
      path_to id child
  | Node.Boundary { body; _ } ->
      path_to id body
  | Node.Text _ | Node.Component _ | Node.Chunk _ ->
      None

let chain t id =
  match path_to id t.node with
  | Some chain ->
      chain
  | None ->
      failwith ("Sim: no element with id " ^ id)

let rec unrated = function Node.Rated (_, attr) -> unrated attr | attr -> attr

let first_in_chain t id select =
  List.find_map
    (fun attrs -> List.find_map (fun attr -> select (unrated attr)) attrs)
    (chain t id)

let key_down t ~id key =
  match
    first_in_chain t id (function
      | Node.On_key_down handler ->
          handler.run key
      | _ ->
          None
      )
  with
  | Some message ->
      deliver t message;
      true
  | None ->
      false

let event_at name select t ~id =
  match first_in_chain t id select with
  | Some message ->
      deliver t message
  | None ->
      failwith (Printf.sprintf "Sim.%s: no handler at or above #%s" name id)

let press_id t =
  event_at "press_id" (function Node.On_press m -> Some m | _ -> None) t

let focus t =
  event_at "focus" (function Node.On_focus m -> Some m | _ -> None) t

let blur t = event_at "blur" (function Node.On_blur m -> Some m | _ -> None) t

let pointer_enter t =
  event_at "pointer_enter"
    (function
      | Node.On_pointer_enter handler -> handler.run Pointer.origin | _ -> None
      )
    t

let pointer_leave t =
  event_at "pointer_leave"
    (function
      | Node.On_pointer_leave handler -> handler.run Pointer.origin | _ -> None
      )
    t

let attribute t ~id name =
  match path_to id t.node with
  | Some (attrs :: _) ->
      List.find_map
        (function
          | Node.String (key, value) when key = name ->
              Some value
          | Node.Bool (key, true) when key = name ->
              Some ""
          | _ ->
              None
          )
        attrs
  | _ ->
      None

let exists t ~id = Option.is_some (path_to id t.node)

let fire t id =
  match List.assoc_opt id t.timers with
  | Some message ->
      t.timers <- List.remove_assoc id t.timers;
      deliver t message
  | None ->
      failwith ("Sim.fire: timer not running: " ^ id)

let fire_after_stop t id =
  match List.assoc_opt id t.ever with
  | Some deliver_it ->
      deliver_it ()
  | None ->
      failwith ("Sim.fire_after_stop: never started: " ^ id)

let find_call t endpoint =
  match List.find_opt (fun c -> c.path = Remote.path endpoint) t.calls with
  | Some call ->
      t.calls <- List.filter (fun c -> c.id <> call.id) t.calls;
      call
  | None ->
      failwith ("Sim: no running call to " ^ Remote.path endpoint)

let answer t endpoint value =
  let call = find_call t endpoint in
  call.respond (Codec.to_string (Remote.output endpoint) value)

let reject t endpoint value =
  let call = find_call t endpoint in
  call.reject (Codec.to_string (Remote.rejected endpoint) value)

let answer_error t endpoint error = (find_call t endpoint).fail error

let call_timeouts t = List.rev_map (fun c -> c.timeout_ms) t.calls

let expire_call t endpoint =
  let call = find_call t endpoint in
  match call.timeout_ms with
  | Some _ ->
      call.fail (Remote.Network "timeout")
  | None ->
      failwith ("Sim.expire_call: no timeout on " ^ call.path)
let call_inputs t = List.rev_map (fun c -> c.input) t.calls
let log t = List.rev t.log
let clear_log t = t.log <- []

let unmount t = apply t (t.tree_unmount ())

let subscriptions t =
  List.rev_map
    (fun (Subscription s) -> (Topic.name s.topic, s.key))
    t.subscriptions

let publish t topic ~key (update : _ Topic.update) =
  List.iter
    (fun (Subscription s) ->
      if Topic.name s.topic = Topic.name topic && s.key = key then
        let convert value =
          match
            (Topic.state s.topic).Codec.decode
              ((Topic.state topic).Codec.encode value)
          with
          | Ok value ->
              value
          | Error message ->
              failwith ("Sim.publish: " ^ message)
        in
        deliver t
          (s.on_update
             {
               Topic.seq = update.seq;
               status = Status.map convert update.status;
             }
          )
    )
    t.subscriptions
