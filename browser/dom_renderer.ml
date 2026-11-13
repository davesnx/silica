open Silica

let current_url () =
  Url.of_string
    (Dom_bindings.pathname Dom_bindings.location
    ^ Dom_bindings.search Dom_bindings.location
    ^ Dom_bindings.hash Dom_bindings.location
    )

module Scroll = struct
  let storage_key = "silica-scroll"
  let table : (int, int * int) Hashtbl.t = Hashtbl.create 16
  let counter = ref (int_of_float (Js.Date.now ()))
  let current = ref 0
  let index = ref 0

  type pending =
    | Nothing
    | Once of (unit -> unit)
    | Restore of { x : int; y : int; until : float }

  let pending = ref Nothing
  let placed = ref None
  let restore_ms = 10_000.

  let fresh () =
    incr counter;
    !counter

  let load () =
    Hashtbl.reset table;
    match Dom_bindings.opt (Dom_bindings.session_get storage_key) with
    | None ->
        ()
    | Some text ->
        List.iter
          (fun entry ->
            match
              List.map int_of_string_opt (String.split_on_char ':' entry)
            with
            | [ Some key; Some x; Some y ] ->
                Hashtbl.replace table key (x, y)
            | _ ->
                ()
          )
          (String.split_on_char ';' text)

  let store () =
    Dom_bindings.session_set storage_key
      (String.concat ";"
         (Hashtbl.fold
            (fun key (x, y) acc ->
              String.concat ":" (List.map string_of_int [ key; x; y ]) :: acc
            )
            table []
         )
      )

  let entry ~index_when_new =
    match
      ( Dom_bindings.opt (Dom_bindings.history_key ()),
        Dom_bindings.opt (Dom_bindings.history_index ())
      )
    with
    | Some key, Some i ->
        (key, i)
    | _ ->
        let key = fresh () in
        Dom_bindings.write_history true key index_when_new
          (Dom_bindings.href Dom_bindings.location);
        (key, index_when_new)

  let restore key =
    placed := None;
    match Hashtbl.find_opt table key with
    | Some (x, y) ->
        Restore { x; y; until = Js.Date.now () +. restore_ms }
    | None ->
        Nothing

  let start () =
    Dom_bindings.manual_scroll_restoration ();
    load ();
    let key, i = entry ~index_when_new:0 in
    current := key;
    index := i;
    pending := restore key

  let save () =
    Hashtbl.replace table !current
      (Dom_bindings.scroll_x (), Dom_bindings.scroll_y ());
    store ()

  let to_hash_or_top hash =
    match
      Dom_bindings.opt (Dom_bindings.get_by_id Dom_bindings.document hash)
    with
    | Some target when Dom_bindings.has_scroll target ->
        Dom_bindings.scroll_into_view target
    | _ ->
        Dom_bindings.scroll_to 0 0

  let push ~scroll ~(previous : Url.t) (url : Url.t) =
    save ();
    let key = fresh () in
    incr index;
    Dom_bindings.write_history false key !index (Url.to_string url);
    current := key;
    if scroll then
      pending :=
        Once
          (fun () ->
            match url.hash with
            | Some hash ->
                to_hash_or_top hash
            | None ->
                if url.path <> previous.path then Dom_bindings.scroll_to 0 0
          )

  let replace ~(previous : Url.t) (url : Url.t) =
    if url.path <> previous.path then save ();
    ( match !pending with
    | Restore _ ->
        pending := Nothing
    | Nothing | Once _ ->
        ()
    );
    Dom_bindings.write_history true !current !index (Url.to_string url)

  let delta () =
    match Dom_bindings.opt (Dom_bindings.history_index ()) with
    | Some i ->
        i - !index
    | None ->
        1

  let pop () =
    save ();
    let key, i = entry ~index_when_new:(!index + 1) in
    current := key;
    index := i;
    pending := restore key

  let rejoin ~scroll =
    let key, i = entry ~index_when_new:(!index + 1) in
    current := key;
    index := i;
    pending := Nothing;
    if scroll then
      Option.iter
        (fun (x, y) -> Dom_bindings.scroll_to x y)
        (Hashtbl.find_opt table key)

  let after_paint () =
    match !pending with
    | Nothing ->
        ()
    | Once action ->
        pending := Nothing;
        action ()
    | Restore { x; y; until } ->
        Dom_bindings.scroll_to x y;
        let here = (Dom_bindings.scroll_x (), Dom_bindings.scroll_y ()) in
        if here = (x, y) || Js.Date.now () > until then
          pending := Nothing
        else
          placed := Some here

  let scrolled () =
    match !pending with
    | Restore _
      when !placed <> Some (Dom_bindings.scroll_x (), Dom_bindings.scroll_y ())
      ->
        pending := Nothing
    | Nothing | Once _ | Restore _ ->
        ()
end

let find_ref containers r =
  let name = Ref.name r in
  List.find_map
    (fun container ->
      Array.to_list
        (Dom_bindings.array_from
           (Dom_bindings.query_all container "[data-silica-ref]")
        )
      |> List.find_opt (fun n ->
          Dom_bindings.opt (Dom_bindings.get_attribute n "data-silica-ref")
          = Some name
      )
    )
    containers

let key_of_event ev =
  Key.make ~ctrl:(Dom_bindings.ctrl_key ev) ~meta:(Dom_bindings.meta_key ev)
    ~shift:(Dom_bindings.shift_key ev)
    ~alt:(Dom_bindings.alt_key ev)
    ~repeat:(Dom_bindings.event_repeat ev)
    (Dom_bindings.key ev)

let is_text_field node =
  Dom_bindings.node_type node = 1
  &&
  match String.lowercase_ascii (Dom_bindings.tag_name node) with
  | "input" | "textarea" | "select" ->
      true
  | _ ->
      Dom_bindings.opt
        (Dom_bindings.closest node
           "[contenteditable]:not([contenteditable=false])"
        )
      <> None

let pointer_of_event ev =
  {
    Pointer.x = Dom_bindings.client_x ev;
    y = Dom_bindings.client_y ev;
    button = Dom_bindings.event_button ev;
  }

let unwrap (attr : _ Node.attr) =
  match attr with
  | Node.Rated (rate, inner) ->
      (inner, Some rate)
  | _ ->
      (attr, None)

let handlers_at v target select =
  List.concat_map
    (fun (dom, attrs) ->
      List.filter_map
        (fun attr ->
          let inner, rate = unwrap attr in
          Option.map (fun value -> (dom, value, rate)) (select inner)
        )
        attrs
    )
    (Vdom.elements v target)

let first_handler v target select =
  match handlers_at v target select with [] -> None | first :: _ -> Some first

let emit (type e) ~(dispatch : e -> unit) node name rate (event : e) =
  match rate with
  | None ->
      dispatch event
  | Some rate ->
      let slot = "__silica_rate_" ^ name in
      let limited =
        match
          ( Dom_bindings.opt (Dom_bindings.get_property node slot)
            : (Rate.t * (e -> unit)) option
            )
        with
        | Some (current, limited) when current = rate ->
            limited
        | _ ->
            let limited = Rate.limiter ~schedule:Web.schedule rate dispatch in
            Dom_bindings.set_property node slot (rate, limited);
            limited
      in
      limited event

let anchor_url a =
  Url.of_string
    (Dom_bindings.pathname a ^ Dom_bindings.search a ^ Dom_bindings.hash a)

let preload_intent ~preload ev =
  match preload with
  | None ->
      ()
  | Some preload -> (
      match
        Dom_bindings.opt
          (Dom_bindings.closest (Dom_bindings.target ev)
             "a[data-silica-preload=\"intent\"]"
          )
      with
      | Some a ->
          let entered_from_inside =
            match Dom_bindings.opt (Dom_bindings.related_target ev) with
            | Some from ->
                Dom_bindings.contains a from
            | None ->
                false
          in
          if
            (not entered_from_inside)
            && Dom_bindings.origin a = Dom_bindings.origin Dom_bindings.location
          then
            preload (anchor_url a)
      | None ->
          ()
    )

let on_node listeners node name handler =
  Dom_bindings.add_listener node name handler;
  listeners :=
    (fun () -> Dom_bindings.remove_listener node name handler) :: !listeners

let held_limit = 16
let held_ms = 10_000.

type held = { at : float; run : unit -> unit }

let rec newest count = function
  | held :: rest when count > 0 ->
      held :: newest (count - 1) rest
  | _ ->
      []

let hold (held : held list ref) run =
  held := { at = Js.Date.now (); run } :: newest (held_limit - 1) !held

let release (held : held list ref) =
  let now = Js.Date.now () in
  let all = List.rev !held in
  held := [];
  List.iter (fun held -> if now -. held.at <= held_ms then held.run ()) all

let delegate (type e) (v : e Vdom.t) ~listeners ~(held : held list ref)
    ~(dispatch : e -> unit) ~(link : (replace:bool -> Url.t -> unit) option)
    ~(preload : (Url.t -> unit) option) =
  let root = v.Vdom.root in
  let on name f =
    on_node listeners root name (fun ev ->
        if v.Vdom.adopting then (
          if name = "submit" then Dom_bindings.prevent_default ev;
          hold held (fun () -> f ev)
        ) else
          f ev
    )
  in
  let watch name f = on_node listeners root name f in
  let press_handler t =
    first_handler v t (function Node.On_press e -> Some e | _ -> None)
  in
  let deliver_handler found =
    Option.iter (fun (dom, e, rate) -> emit ~dispatch dom "press" rate e) found
  in
  let keyed select name ev =
    let key = key_of_event ev in
    let rec go = function
      | [] ->
          false
      | (dom, (handler : (Key.t, e) Node.handler), rate) :: rest -> (
          match handler.run key with
          | Some e ->
              if handler.prevent then Dom_bindings.prevent_default ev;
              emit ~dispatch dom name rate e;
              true
          | None ->
              go rest
        )
    in
    go (handlers_at v (Dom_bindings.target ev) select)
  in
  on "click" (fun ev ->
      let t = Dom_bindings.target ev in
      let anchor =
        match link with
        | None ->
            None
        | Some _ ->
            Dom_bindings.opt (Dom_bindings.closest t "a[data-silica-link]")
      in
      match (anchor, link) with
      | Some a, Some go
        when (not (Dom_bindings.default_prevented ev))
             && Dom_bindings.button ev = 0
             && (not
                   (Dom_bindings.ctrl_key ev || Dom_bindings.meta_key ev
                  || Dom_bindings.shift_key ev || Dom_bindings.alt_key ev
                   )
                )
             && Dom_bindings.origin a
                = Dom_bindings.origin Dom_bindings.location
             && Dom_bindings.opt (Dom_bindings.get_attribute a "target") = None
             && Dom_bindings.opt (Dom_bindings.get_attribute a "download")
                = None ->
          Dom_bindings.prevent_default ev;
          go
            ~replace:
              (Dom_bindings.opt
                 (Dom_bindings.get_attribute a "data-silica-replace")
              <> None
              )
            (anchor_url a)
      | _ ->
          deliver_handler (press_handler t)
  );
  on "keydown" (fun ev ->
      let handled =
        keyed (function Node.On_key_down h -> Some h | _ -> None) "keydown" ev
      in
      if not handled then
        let t = Dom_bindings.target ev in
        let k = Dom_bindings.key ev in
        let native =
          List.mem
            (String.lowercase_ascii (Dom_bindings.tag_name t))
            [ "button"; "a"; "input"; "textarea"; "select"; "summary" ]
        in
        if (k = "Enter" || k = " ") && not native then
          match press_handler t with
          | Some _ as found ->
              Dom_bindings.prevent_default ev;
              deliver_handler found
          | None ->
              ()
  );
  on "keyup" (fun ev ->
      ignore
        (keyed (function Node.On_key_up h -> Some h | _ -> None) "keyup" ev)
  );
  let crossing select name ev =
    let related = Dom_bindings.opt (Dom_bindings.related_target ev) in
    List.iter
      (fun (dom, (handler : (Pointer.t, e) Node.handler), rate) ->
        let already_inside =
          match related with
          | Some r ->
              Dom_bindings.contains dom r
          | None ->
              false
        in
        if not already_inside then
          match handler.run (pointer_of_event ev) with
          | Some e ->
              if handler.prevent then Dom_bindings.prevent_default ev;
              emit ~dispatch dom name rate e
          | None ->
              ()
      )
      (handlers_at v (Dom_bindings.target ev) select)
  in
  let enter =
    crossing
      (function Node.On_pointer_enter h -> Some h | _ -> None)
      "pointerenter"
  in
  watch "pointerover" (fun ev ->
      enter ev;
      preload_intent ~preload ev
  );
  watch "pointerout"
    (crossing
       (function Node.On_pointer_leave h -> Some h | _ -> None)
       "pointerleave"
    );
  on "dblclick" (fun ev ->
      deliver_handler
        (first_handler v (Dom_bindings.target ev) (function
          | Node.On_double_press e ->
              Some e
          | _ ->
              None
          ))
  );
  watch "input" (fun ev ->
      let t = Dom_bindings.target ev in
      Option.iter
        (fun (dom, f, rate) ->
          emit ~dispatch dom "input" rate (f (Dom_bindings.value t))
        )
        (first_handler v t (function Node.On_input f -> Some f | _ -> None))
  );
  on "submit" (fun ev ->
      match
        first_handler v (Dom_bindings.target ev) (function
          | Node.On_submit e ->
              Some e
          | _ ->
              None
          )
      with
      | Some _ as found ->
          Dom_bindings.prevent_default ev;
          deliver_handler found
      | None ->
          ()
  );
  watch "focusin" (fun ev ->
      deliver_handler
        (first_handler v (Dom_bindings.target ev) (function
          | Node.On_focus e ->
              Some e
          | _ ->
              None
          ));
      preload_intent ~preload ev
  );
  watch "focusout" (fun ev ->
      deliver_handler
        (first_handler v (Dom_bindings.target ev) (function
          | Node.On_blur e ->
              Some e
          | _ ->
              None
          ))
  )

let focusable_selector =
  "a[href], button:not([disabled]), input:not([disabled]):not([type=hidden]), \
   select:not([disabled]), textarea:not([disabled]), \
   [tabindex]:not([tabindex=\"-1\"])"

let focusables container =
  Array.to_list
    (Dom_bindings.array_from
       (Dom_bindings.query_all container focusable_selector)
    )

let focus_first container =
  match focusables container with
  | first :: _ ->
      Dom_bindings.focus first
  | [] ->
      Dom_bindings.set_attribute container "tabindex" "-1";
      Dom_bindings.focus container

type layer = {
  containers : Ref.t list;
  on_escape : (unit -> unit) option;
  on_outside : (unit -> unit) option;
  on_focus_out : (unit -> unit) option;
}

type scope = {
  top : Dom_bindings.node;
  find : Ref.t -> Dom_bindings.node option;
  document : Document.t;
  mutable layers : layer list;
  mutable layer_listeners : (unit -> unit) list;
}

let on_scope scope ?(capture = false) name handler =
  Dom_bindings.add_listener_capture scope.top name handler capture;
  fun () -> Dom_bindings.remove_listener_capture scope.top name handler capture

let inside scope refs target =
  List.exists
    (fun r ->
      match scope.find r with
      | Some n ->
          Dom_bindings.contains n target
      | None ->
          false
    )
    refs

let rec newer_than layer = function
  | [] ->
      []
  | l :: _ when l == layer ->
      []
  | l :: rest ->
      l :: newer_than layer rest

let in_newer_layer scope layer target =
  List.exists
    (fun l -> inside scope l.containers target)
    (newer_than layer scope.layers)

let on_escape scope ev =
  if Dom_bindings.key ev = "Escape" && not (Dom_bindings.default_prevented ev)
  then
    match List.find_map (fun l -> l.on_escape) scope.layers with
    | Some deliver ->
        Dom_bindings.prevent_default ev;
        deliver ()
    | None ->
        ()

let outside scope select ev =
  let target = Dom_bindings.target ev in
  let rec hit = function
    | [] ->
        []
    | l :: _ when inside scope l.containers target ->
        []
    | l :: rest ->
        l :: hit rest
  in
  List.iter
    (fun deliver -> deliver ())
    (List.filter_map select (hit scope.layers))

let push_layer scope layer =
  if scope.layers = [] then
    scope.layer_listeners <-
      [
        on_scope scope "keydown" (on_escape scope);
        on_scope scope ~capture:true "pointerdown"
          (outside scope (fun l -> l.on_outside));
        on_scope scope "focusin" (outside scope (fun l -> l.on_focus_out));
      ];
  scope.layers <- layer :: scope.layers;
  fun () ->
    scope.layers <- List.filter (fun l -> l != layer) scope.layers;
    if scope.layers = [] then (
      List.iter (fun stop -> stop ()) scope.layer_listeners;
      scope.layer_listeners <- []
    )

let inert_count = "__silica_inert"

let make_others_inert node =
  let body = Dom_bindings.body Dom_bindings.document in
  let rec siblings first skip acc =
    match first with
    | None ->
        acc
    | Some n ->
        siblings
          (Dom_bindings.opt (Dom_bindings.next_sibling n))
          skip
          ( if n == skip || Dom_bindings.node_type n <> 1 then
              acc
            else
              n :: acc
          )
  in
  let rec others n acc =
    if n == body then
      acc
    else
      match Dom_bindings.opt (Dom_bindings.parent_node n) with
      | None ->
          acc
      | Some parent ->
          others parent
            (siblings (Dom_bindings.opt (Dom_bindings.first_child parent)) n acc)
  in
  let count n =
    Option.value
      (Dom_bindings.opt (Dom_bindings.get_property n inert_count))
      ~default:0
  in
  let marked =
    List.filter
      (fun n ->
        let owned = count n > 0 in
        if
          owned
          || Dom_bindings.opt (Dom_bindings.get_attribute n "inert") = None
        then (
          Dom_bindings.set_property n inert_count (count n + 1);
          Dom_bindings.set_attribute n "inert" "";
          true
        ) else
          false
      )
      (others node [])
  in
  fun () ->
    List.iter
      (fun n ->
        let left = count n - 1 in
        Dom_bindings.set_property n inert_count left;
        if left = 0 then Dom_bindings.remove_attribute n "inert"
      )
      marked

let trap_focus scope ~container ~return_focus =
  let previous = Dom_bindings.opt (Dom_bindings.active_in scope.top) in
  let layer =
    {
      containers = [ container ];
      on_escape = None;
      on_outside = None;
      on_focus_out = None;
    }
  in
  let pop = push_layer scope layer in
  let container_node () = scope.find container in
  let restore =
    match container_node () with
    | Some node ->
        make_others_inert node
    | None ->
        fun () -> ()
  in
  Option.iter focus_first (container_node ());
  let stop_keys =
    on_scope scope ~capture:true "keydown" (fun ev ->
        let active = Dom_bindings.opt (Dom_bindings.active_in scope.top) in
        let in_newer =
          match (active, container_node ()) with
          | Some a, Some node ->
              (not (Dom_bindings.contains node a))
              && in_newer_layer scope layer a
          | _ ->
              false
        in
        if Dom_bindings.key ev = "Tab" && not in_newer then
          match container_node () with
          | None ->
              ()
          | Some node -> (
              let items = focusables node in
              match (items, List.rev items) with
              | [], _ | _, [] ->
                  Dom_bindings.prevent_default ev
              | first :: _, last :: _ ->
                  let outside =
                    match active with
                    | Some a ->
                        not (Dom_bindings.contains node a)
                    | None ->
                        true
                  in
                  let at node_ =
                    match active with Some a -> a == node_ | None -> false
                  in
                  if Dom_bindings.shift_key ev && (outside || at first) then (
                    Dom_bindings.prevent_default ev;
                    Dom_bindings.focus last
                  ) else if
                      (not (Dom_bindings.shift_key ev)) && (outside || at last)
                    then (
                    Dom_bindings.prevent_default ev;
                    Dom_bindings.focus first
                  )
            )
    )
  in
  let stop_focus =
    on_scope scope ~capture:true "focusin" (fun ev ->
        let target = Dom_bindings.target ev in
        match container_node () with
        | Some node
          when (not (Dom_bindings.contains node target))
               && not (in_newer_layer scope layer target) ->
            focus_first node
        | _ ->
            ()
    )
  in
  fun () ->
    stop_keys ();
    stop_focus ();
    pop ();
    restore ();
    if return_focus then
      match previous with
      | Some node when Dom_bindings.is_connected node ->
          Dom_bindings.focus node
      | _ ->
          ()

let px value = Js.Float.toString value ^ "px"

let rect_of array =
  { Rect.x = array.(0); y = array.(1); width = array.(2); height = array.(3) }

let place_popup scope ~anchor ~popup place =
  match (scope.find anchor, scope.find popup) with
  | Some anchor_node, Some popup_node ->
      Dom_bindings.style_property popup_node "position" "fixed";
      let size = Dom_bindings.viewport_size () in
      let viewport =
        { Rect.x = 0.; y = 0.; width = size.(0); height = size.(1) }
      in
      let anchor_rect = rect_of (Dom_bindings.bounding_rect anchor_node) in
      let result =
        Place.compute place ~anchor:anchor_rect
          ~popup:(rect_of (Dom_bindings.bounding_rect popup_node))
          ~viewport
      in
      Dom_bindings.style_property popup_node "left" (px result.x);
      Dom_bindings.style_property popup_node "top" (px result.y);
      Dom_bindings.style_property popup_node "--anchor-width"
        (px anchor_rect.width);
      Dom_bindings.style_property popup_node "--anchor-height"
        (px anchor_rect.height);
      Dom_bindings.style_property popup_node "--available-width"
        (px result.available_width);
      Dom_bindings.style_property popup_node "--available-height"
        (px result.available_height);
      Dom_bindings.set_attribute popup_node "data-side"
        (Place.side_name result.side);
      Dom_bindings.set_attribute popup_node "data-align"
        (Place.align_name result.align)
  | _ ->
      ()

let track_anchor scope ~anchor ~popup place =
  let update () = place_popup scope ~anchor ~popup place in
  update ();
  let nodes = Array.of_list (List.filter_map scope.find [ anchor; popup ]) in
  Dom_bindings.on_layout_change nodes update

let listen node name handler =
  Dom_bindings.add_listener node name handler;
  fun () -> Dom_bindings.remove_listener node name handler

let scroll_of array =
  {
    Silica.Scroll.left = array.(0);
    top = array.(1);
    scroll_width = array.(2);
    scroll_height = array.(3);
    client_width = array.(4);
    client_height = array.(5);
  }

let start_held (type e) scope ~(dispatch : e -> unit)
    ~(block : (Nav.t -> e) -> unit -> unit) (source : e Action.held) :
    unit -> unit =
  let limited rate deliver =
    match rate with
    | None ->
        deliver
    | Some rate ->
        Rate.limiter ~schedule:Web.schedule rate deliver
  in
  match source with
  | Action.Focus_trap { container; return_focus } ->
      trap_focus scope ~container ~return_focus
  | Action.Scroll_lock ->
      Document.lock_scroll scope.document
  | Action.Outside_press { container; event } ->
      on_scope scope ~capture:true "pointerdown" (fun ev ->
          let inside =
            match scope.find container with
            | Some node ->
                Dom_bindings.contains node (Dom_bindings.target ev)
            | None ->
                false
          in
          if not inside then dispatch event
      )
  | Action.Global_keys { bindings; prevent; rate } ->
      let deliver = limited rate dispatch in
      on_scope scope "keydown" (fun ev ->
          let key = key_of_event ev in
          let text_entry =
            (not (key.ctrl || key.meta || key.key = "Escape"))
            && is_text_field (Dom_bindings.target ev)
          in
          match
            List.find_opt (fun (bound, _) -> Key.equal bound key) bindings
          with
          | Some _ when Dom_bindings.default_prevented ev || text_entry ->
              ()
          | Some (_, event) ->
              if prevent then Dom_bindings.prevent_default ev;
              deliver event
          | None ->
              ()
      )
  | Action.Resize { target; rate; on_resize } -> (
      let deliver = limited rate (fun rect -> dispatch (on_resize rect)) in
      match scope.find target with
      | Some node ->
          Dom_bindings.observe_resize node (fun rect ->
              deliver
                {
                  Rect.x = rect.(0);
                  y = rect.(1);
                  width = rect.(2);
                  height = rect.(3);
                }
          )
      | None ->
          fun () -> ()
    )
  | Action.Dismiss { containers; on_escape; on_outside; on_focus_out } ->
      let deliver event () = dispatch event in
      push_layer scope
        {
          containers;
          on_escape = Option.map deliver on_escape;
          on_outside = Option.map deliver on_outside;
          on_focus_out = Option.map deliver on_focus_out;
        }
  | Action.Anchor { anchor; popup; place } ->
      track_anchor scope ~anchor ~popup place
  | Action.Intersection { target; rate; on_change } -> (
      let deliver =
        limited rate (fun visible -> dispatch (on_change visible))
      in
      match scope.find target with
      | Some node ->
          Dom_bindings.observe_intersection node deliver
      | None ->
          fun () -> ()
    )
  | Action.Scroll { target; rate; on_scroll } -> (
      let deliver = limited rate (fun s -> dispatch (on_scroll s)) in
      let watch node read =
        let report _ = deliver (scroll_of (read ())) in
        report ();
        listen node "scroll" report
      in
      match target with
      | None ->
          watch Dom_bindings.window Dom_bindings.page_scroll
      | Some r -> (
          match scope.find r with
          | Some node ->
              watch node (fun () -> Dom_bindings.element_scroll node)
          | None ->
              fun () -> ()
        )
    )
  | Action.Visibility on_change ->
      let report _ = dispatch (on_change (Dom_bindings.visible ())) in
      report ();
      listen Dom_bindings.document "visibilitychange" report
  | Action.Block { on_attempt } ->
      block on_attempt
  | Action.Media { query; on_change } ->
      Dom_bindings.watch_media query (fun matches ->
          dispatch (on_change matches)
      )
  | Action.Online on_change ->
      let report _ = dispatch (on_change (Dom_bindings.online ())) in
      report ();
      let stop_online = listen Dom_bindings.window "online" report in
      let stop_offline = listen Dom_bindings.window "offline" report in
      fun () ->
        stop_online ();
        stop_offline ()

type change = Pushed | Replaced of Url.t | Traversed of int
type pop = Report | Pass | Silent of { scroll : bool }

let route_focus root =
  let target =
    match
      Dom_bindings.opt (Dom_bindings.query_one root "[data-silica-route-focus]")
    with
    | Some node ->
        node
    | None ->
        root
  in
  if Dom_bindings.opt (Dom_bindings.get_attribute target "tabindex") = None then
    Dom_bindings.set_attribute target "tabindex" "-1";
  Dom_bindings.focus_without_scroll target

let make (type e) ~dev ~adopt ~hooks ~document ~root
    ~(on_route : (Router.event -> e) option) : e Renderer.make =
 fun ~dispatch ->
  let top = Dom_bindings.root_node root in
  let layer =
    if Dom_bindings.node_type top = 9 then
      Some (Dom_bindings.body top)
    else if top == root then
      None
    else
      Some top
  in
  let announcer = ref None in
  let announce text =
    match (!announcer, layer) with
    | Some region, _ ->
        Dom_bindings.set_text_content region text
    | None, Some parent ->
        let region = Dom_bindings.create_announcer parent in
        announcer := Some region;
        Dom_bindings.set_text_content region text
    | None, None ->
        ()
  in
  let released = ref false in
  let report event = Option.iter (fun f -> dispatch (f event)) on_route in
  let here = ref (current_url ()) in
  let last = ref None in
  let next_pop = ref Report in
  let focus_due = ref false in
  let blocks : (int * (Nav.t -> e)) list ref = ref [] in
  let block_ids = ref 0 in
  let stop_asking = ref None in
  let block on_attempt =
    incr block_ids;
    let id = !block_ids in
    blocks := (id, on_attempt) :: !blocks;
    if !stop_asking = None then
      stop_asking := Some (Dom_bindings.ask_before_unload ());
    fun () ->
      blocks := List.filter (fun (other, _) -> other <> id) !blocks;
      if !blocks = [] then (
        Option.iter (fun stop -> stop ()) !stop_asking;
        stop_asking := None
      )
  in
  let blocked nav =
    match !blocks with
    | (_, on_attempt) :: _ ->
        dispatch (on_attempt nav);
        true
    | [] ->
        false
  in
  let arrivals = ref 0 in
  let arrived (before : Url.t) (url : Url.t) =
    if Option.is_some on_route && before.path <> url.path then (
      incr arrivals;
      let arrival = !arrivals in
      focus_due := true;
      Dom_bindings.after_task (fun () ->
          if arrival = !arrivals && not !released then (
            if !focus_due then (
              focus_due := false;
              route_focus root
            );
            announce (Dom_bindings.title Dom_bindings.document)
          )
      )
    )
  in
  let changed change url =
    last := Some change;
    here := url;
    report (Router.Url_changed url)
  in
  let push url =
    let before = !here in
    Scroll.push ~scroll:(Option.is_some on_route) ~previous:before url;
    arrived before url;
    changed Pushed url
  in
  let replace url =
    let before = !here in
    Scroll.replace ~previous:before url;
    changed (Replaced before) url
  in
  let go delta = if delta <> 0 then Dom_bindings.history_go delta in
  let popped () =
    let url = current_url () in
    match !next_pop with
    | Silent { scroll } ->
        next_pop := Report;
        Scroll.rejoin ~scroll;
        here := url
    | Pass ->
        next_pop := Report;
        let before = !here in
        let delta = Scroll.delta () in
        Scroll.pop ();
        arrived before url;
        changed (Traversed delta) url
    | Report ->
        let delta = Scroll.delta () in
        if not (blocked (Nav.Traverse { delta; url })) then (
          let before = !here in
          Scroll.pop ();
          arrived before url;
          changed (Traversed delta) url
        ) else (
          next_pop := Silent { scroll = false };
          go (-delta)
        )
  in
  let revert () =
    focus_due := false;
    incr arrivals;
    ( match !last with
    | Some Pushed ->
        next_pop := Silent { scroll = true };
        go (-1)
    | Some (Replaced old) ->
        Scroll.replace ~previous:!here old;
        here := old
    | Some (Traversed delta) ->
        next_pop := Silent { scroll = false };
        go (-delta)
    | None ->
        ()
    );
    last := None
  in
  let resume = function
    | Nav.Push url ->
        push url
    | Nav.Replace url ->
        replace url
    | Nav.Traverse { delta; _ } ->
        if delta <> 0 then (
          next_pop := Pass;
          go delta
        )
  in
  let link =
    Option.map
      (fun _ ~replace:replacing url ->
        if replacing then (
          if not (blocked (Nav.Replace url)) then replace url
        ) else if not (blocked (Nav.Push url)) then
          push url
      )
      on_route
  in
  let preload =
    Option.map (fun _ url -> report (Router.Preload url)) on_route
  in
  let hooks =
    match preload with
    | Some preload ->
        {
          hooks with
          Vdom.viewport =
            Dom_bindings.watch_visible (fun a ->
                if
                  Dom_bindings.origin a
                  = Dom_bindings.origin Dom_bindings.location
                then
                  preload (anchor_url a)
            );
        }
    | None ->
        hooks
  in
  let held = ref [] in
  let listeners = ref [] in
  let delegate_portal sub =
    delegate sub ~listeners ~held ~dispatch ~link ~preload
  in
  let portal_parent = function
    | None ->
        layer
    | Some id ->
        Dom_bindings.opt (Dom_bindings.find_by_id_in top id)
  in
  let v =
    Vdom.make ~hooks ~adopt ~dev ~on_portal:delegate_portal ~portal_parent root
  in
  let find_ref r = find_ref (Vdom.containers v) r in
  let scope =
    { top; find = find_ref; document; layers = []; layer_listeners = [] }
  in
  delegate v ~listeners ~held ~dispatch ~link ~preload;
  if Option.is_some on_route then (
    Scroll.start ();
    on_node listeners Dom_bindings.window "popstate" (fun _ -> popped ());
    on_node listeners Dom_bindings.window "pagehide" (fun _ -> Scroll.save ());
    on_node listeners Dom_bindings.window "scroll" (fun _ -> Scroll.scrolled ())
  );
  let sources : (string, unit -> unit) Hashtbl.t = Hashtbl.create 4 in
  let stop_held ~id =
    match Hashtbl.find_opt sources id with
    | Some stop ->
        Hashtbl.remove sources id;
        stop ()
    | None ->
        ()
  in
  let on_ref f r = Option.iter f (find_ref r) in
  let run : e Action.t -> unit = function
    | Action.Focus r ->
        focus_due := false;
        on_ref
          (fun node ->
            if
              Dom_bindings.opt (Dom_bindings.closest node "[data-silica-portal]")
              <> None
            then
              Dom_bindings.focus_without_scroll node
            else
              Dom_bindings.focus node
          )
          r
    | Action.Blur r ->
        on_ref Dom_bindings.blur r
    | Action.Scroll_into_view r ->
        Dom_bindings.microtask (fun () ->
            on_ref
              (fun n ->
                if Dom_bindings.has_scroll n then
                  Dom_bindings.scroll_into_view_nearest n
              )
              r
        )
    | Action.Navigate u ->
        if not (blocked (Nav.Push u)) then push u
    | Action.Replace u ->
        if not (blocked (Nav.Replace u)) then replace u
    | Action.Resume nav ->
        resume nav
    | Action.Revert_navigation ->
        revert ()
    | Action.Remount
    | Action.Preload _
    | Action.Load_chunk _
    | Action.Set_status _ ->
        ()
    | Action.Scroll_to { target = None; left; top } ->
        Dom_bindings.scroll_to left top
    | Action.Scroll_to { target = Some r; left; top } ->
        on_ref (fun node -> Dom_bindings.scroll_element node left top) r
    | Action.Measure { target; on_rect } ->
        dispatch
          (on_rect
             (Option.map
                (fun node -> rect_of (Dom_bindings.bounding_rect node))
                (find_ref target)
             )
          )
    | Action.Copy { text; on_result } ->
        Dom_bindings.copy_text text (fun ok ->
            Option.iter (fun on_result -> dispatch (on_result ok)) on_result
        )
    | Action.Load_script { src; on_result } ->
        Dom_bindings.load_script src (fun ok -> dispatch (on_result ok))
    | Action.Set_title title ->
        Document.send document (Document.Set_title title)
    | Action.Set_document_attribute { name; value } as action ->
        Action.check action;
        Document.send document (Document.Set_document_attribute { name; value })
    | Action.Set_cookie { name; value; max_age } as action ->
        Action.check action;
        Document.send document (Document.Set_cookie { name; value; max_age })
    | Action.Back ->
        Dom_bindings.back Dom_bindings.history
    | Action.Forward ->
        Dom_bindings.forward Dom_bindings.history
    | Action.Reload ->
        Dom_bindings.reload Dom_bindings.location
  in
  {
    Renderer.paint =
      (fun node ->
        Vdom.render v node;
        List.iter Dom_bindings.dispatch_input (Vdom.take_typed v);
        release held;
        Scroll.after_paint ()
      );
    run;
    start_held =
      (fun ~id source ->
        stop_held ~id;
        Hashtbl.replace sources id (start_held scope ~dispatch ~block source)
      );
    stop_held;
    release =
      (fun () ->
        released := true;
        held := [];
        List.iter
          (fun id -> stop_held ~id)
          (Hashtbl.fold (fun id _ all -> id :: all) sources []);
        List.iter (fun stop -> stop ()) !listeners;
        listeners := [];
        Vdom.release v;
        Option.iter
          (fun region ->
            Option.iter
              (fun parent -> Dom_bindings.remove_child parent region)
              (Dom_bindings.opt (Dom_bindings.parent_node region))
          )
          !announcer;
        announcer := None
      );
  }
