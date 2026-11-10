open Silica

let current_url () =
  Url.of_string
    (Dom.pathname Dom.location ^ Dom.search Dom.location ^ Dom.hash Dom.location)

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
    match Dom.opt (Dom.session_get storage_key) with
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
    Dom.session_set storage_key
      (String.concat ";"
         (Hashtbl.fold
            (fun key (x, y) acc ->
              String.concat ":" (List.map string_of_int [ key; x; y ]) :: acc
            )
            table []
         )
      )

  let entry ~index_when_new =
    match (Dom.opt (Dom.history_key ()), Dom.opt (Dom.history_index ())) with
    | Some key, Some i ->
        (key, i)
    | _ ->
        let key = fresh () in
        Dom.write_history true key index_when_new (Dom.href Dom.location);
        (key, index_when_new)

  let restore key =
    placed := None;
    match Hashtbl.find_opt table key with
    | Some (x, y) ->
        Restore { x; y; until = Js.Date.now () +. restore_ms }
    | None ->
        Nothing

  let start () =
    Dom.manual_scroll_restoration ();
    load ();
    let key, i = entry ~index_when_new:0 in
    current := key;
    index := i;
    pending := restore key

  let save () =
    Hashtbl.replace table !current (Dom.scroll_x (), Dom.scroll_y ());
    store ()

  let to_hash_or_top hash =
    match Dom.opt (Dom.get_by_id Dom.document hash) with
    | Some target when Dom.has_scroll target ->
        Dom.scroll_into_view target
    | _ ->
        Dom.scroll_to 0 0

  let push ~scroll ~(previous : Url.t) (url : Url.t) =
    save ();
    let key = fresh () in
    incr index;
    Dom.write_history false key !index (Url.to_string url);
    current := key;
    if scroll then
      pending :=
        Once
          (fun () ->
            match url.hash with
            | Some hash ->
                to_hash_or_top hash
            | None ->
                if url.path <> previous.path then Dom.scroll_to 0 0
          )

  let replace ~(previous : Url.t) (url : Url.t) =
    if url.path <> previous.path then save ();
    ( match !pending with
    | Restore _ ->
        pending := Nothing
    | Nothing | Once _ ->
        ()
    );
    Dom.write_history true !current !index (Url.to_string url)

  let delta () =
    match Dom.opt (Dom.history_index ()) with Some i -> i - !index | None -> 1

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
      Option.iter (fun (x, y) -> Dom.scroll_to x y) (Hashtbl.find_opt table key)

  let after_paint () =
    match !pending with
    | Nothing ->
        ()
    | Once action ->
        pending := Nothing;
        action ()
    | Restore { x; y; until } ->
        Dom.scroll_to x y;
        let here = (Dom.scroll_x (), Dom.scroll_y ()) in
        if here = (x, y) || Js.Date.now () > until then
          pending := Nothing
        else
          placed := Some here

  let scrolled () =
    match !pending with
    | Restore _ when !placed <> Some (Dom.scroll_x (), Dom.scroll_y ()) ->
        pending := Nothing
    | Nothing | Once _ | Restore _ ->
        ()
end

let find_ref r =
  let name = Ref.name r in
  Array.to_list (Dom.array_from (Dom.query_all Dom.document "[data-silica-ref]"))
  |> List.find_opt (fun n ->
      Dom.opt (Dom.get_attribute n "data-silica-ref") = Some name
  )

let key_of_event ev =
  Key.make ~ctrl:(Dom.ctrl_key ev) ~meta:(Dom.meta_key ev)
    ~shift:(Dom.shift_key ev) ~alt:(Dom.alt_key ev)
    ~repeat:(Dom.event_repeat ev) (Dom.key ev)

let is_text_field node =
  Dom.node_type node = 1
  &&
  match String.lowercase_ascii (Dom.tag_name node) with
  | "input" | "textarea" | "select" ->
      true
  | _ ->
      Dom.opt (Dom.closest node "[contenteditable]:not([contenteditable=false])")
      <> None

let pointer_of_event ev =
  {
    Pointer.x = Dom.client_x ev;
    y = Dom.client_y ev;
    button = Dom.event_button ev;
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
          (Dom.opt (Dom.get_property node slot) : (Rate.t * (e -> unit)) option)
        with
        | Some (current, limited) when current = rate ->
            limited
        | _ ->
            let limited = Rate.limiter ~schedule:Web.schedule rate dispatch in
            Dom.set_property node slot (rate, limited);
            limited
      in
      limited event

let anchor_url a = Url.of_string (Dom.pathname a ^ Dom.search a ^ Dom.hash a)

let preload_intent ~preload ev =
  match preload with
  | None ->
      ()
  | Some preload -> (
      match
        Dom.opt (Dom.closest (Dom.target ev) "a[data-silica-preload=\"intent\"]")
      with
      | Some a ->
          let entered_from_inside =
            match Dom.opt (Dom.related_target ev) with
            | Some from ->
                Dom.contains a from
            | None ->
                false
          in
          if (not entered_from_inside) && Dom.origin a = Dom.origin Dom.location
          then
            preload (anchor_url a)
      | None ->
          ()
    )

let delegate (type e) (v : e Vdom.t) ~(dispatch : e -> unit)
    ~(link : (replace:bool -> Url.t -> unit) option)
    ~(preload : (Url.t -> unit) option) =
  let root = v.Vdom.root in
  let on name f = Dom.add_listener root name f in
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
              if handler.prevent then Dom.prevent_default ev;
              emit ~dispatch dom name rate e;
              true
          | None ->
              go rest
        )
    in
    go (handlers_at v (Dom.target ev) select)
  in
  on "click" (fun ev ->
      let t = Dom.target ev in
      let anchor =
        match link with
        | None ->
            None
        | Some _ ->
            Dom.opt (Dom.closest t "a[data-silica-link]")
      in
      match (anchor, link) with
      | Some a, Some go
        when (not (Dom.default_prevented ev))
             && Dom.button ev = 0
             && (not
                   (Dom.ctrl_key ev || Dom.meta_key ev || Dom.shift_key ev
                  || Dom.alt_key ev
                   )
                )
             && Dom.origin a = Dom.origin Dom.location
             && Dom.opt (Dom.get_attribute a "target") = None
             && Dom.opt (Dom.get_attribute a "download") = None ->
          Dom.prevent_default ev;
          go
            ~replace:
              (Dom.opt (Dom.get_attribute a "data-silica-replace") <> None)
            (anchor_url a)
      | _ ->
          deliver_handler (press_handler t)
  );
  on "keydown" (fun ev ->
      let handled =
        keyed (function Node.On_key_down h -> Some h | _ -> None) "keydown" ev
      in
      if not handled then
        let t = Dom.target ev in
        let k = Dom.key ev in
        let native =
          List.mem
            (String.lowercase_ascii (Dom.tag_name t))
            [ "button"; "a"; "input"; "textarea"; "select"; "summary" ]
        in
        if (k = "Enter" || k = " ") && not native then
          match press_handler t with
          | Some _ as found ->
              Dom.prevent_default ev;
              deliver_handler found
          | None ->
              ()
  );
  on "keyup" (fun ev ->
      ignore
        (keyed (function Node.On_key_up h -> Some h | _ -> None) "keyup" ev)
  );
  let crossing select name ev =
    let related = Dom.opt (Dom.related_target ev) in
    List.iter
      (fun (dom, (handler : (Pointer.t, e) Node.handler), rate) ->
        let already_inside =
          match related with Some r -> Dom.contains dom r | None -> false
        in
        if not already_inside then
          match handler.run (pointer_of_event ev) with
          | Some e ->
              if handler.prevent then Dom.prevent_default ev;
              emit ~dispatch dom name rate e
          | None ->
              ()
      )
      (handlers_at v (Dom.target ev) select)
  in
  let enter =
    crossing
      (function Node.On_pointer_enter h -> Some h | _ -> None)
      "pointerenter"
  in
  on "pointerover" (fun ev ->
      enter ev;
      preload_intent ~preload ev
  );
  on "pointerout"
    (crossing
       (function Node.On_pointer_leave h -> Some h | _ -> None)
       "pointerleave"
    );
  on "dblclick" (fun ev ->
      deliver_handler
        (first_handler v (Dom.target ev) (function
          | Node.On_double_press e ->
              Some e
          | _ ->
              None
          ))
  );
  on "input" (fun ev ->
      let t = Dom.target ev in
      Option.iter
        (fun (dom, f, rate) -> emit ~dispatch dom "input" rate (f (Dom.value t)))
        (first_handler v t (function Node.On_input f -> Some f | _ -> None))
  );
  on "submit" (fun ev ->
      match
        first_handler v (Dom.target ev) (function
          | Node.On_submit e ->
              Some e
          | _ ->
              None
          )
      with
      | Some _ as found ->
          Dom.prevent_default ev;
          deliver_handler found
      | None ->
          ()
  );
  on "focusin" (fun ev ->
      deliver_handler
        (first_handler v (Dom.target ev) (function
          | Node.On_focus e ->
              Some e
          | _ ->
              None
          ));
      preload_intent ~preload ev
  );
  on "focusout" (fun ev ->
      deliver_handler
        (first_handler v (Dom.target ev) (function
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
  Array.to_list (Dom.array_from (Dom.query_all container focusable_selector))

let focus_first container =
  match focusables container with
  | first :: _ ->
      Dom.focus first
  | [] ->
      Dom.set_attribute container "tabindex" "-1";
      Dom.focus container

let scroll_locks = ref 0
let saved_scroll_style = ref ("", "")

let lock_scroll () =
  if !scroll_locks = 0 then (
    let root = Dom.document_element Dom.document in
    let width = Dom.scrollbar_width () in
    saved_scroll_style :=
      (Dom.style_get root "overflow", Dom.style_get root "paddingRight");
    Dom.style_set root "overflow" "hidden";
    if width > 0 then
      Dom.style_set root "paddingRight" (string_of_int width ^ "px")
  );
  incr scroll_locks;
  fun () ->
    decr scroll_locks;
    if !scroll_locks = 0 then (
      let root = Dom.document_element Dom.document in
      let overflow, padding = !saved_scroll_style in
      Dom.style_set root "overflow" overflow;
      Dom.style_set root "paddingRight" padding
    )

let on_document ?(capture = false) name handler =
  let document = Dom.document in
  Dom.add_listener_capture document name handler capture;
  fun () -> Dom.remove_listener_capture document name handler capture

type layer = {
  containers : Ref.t list;
  on_escape : (unit -> unit) option;
  on_outside : (unit -> unit) option;
  on_focus_out : (unit -> unit) option;
}

let layers : layer list ref = ref []

let inside refs target =
  List.exists
    (fun r ->
      match find_ref r with Some n -> Dom.contains n target | None -> false
    )
    refs

let rec newer_than layer = function
  | [] ->
      []
  | l :: _ when l == layer ->
      []
  | l :: rest ->
      l :: newer_than layer rest

let in_newer_layer layer target =
  List.exists (fun l -> inside l.containers target) (newer_than layer !layers)

let layer_listeners = ref None

let on_escape ev =
  if Dom.key ev = "Escape" && not (Dom.default_prevented ev) then
    match List.find_map (fun l -> l.on_escape) !layers with
    | Some deliver ->
        Dom.prevent_default ev;
        deliver ()
    | None ->
        ()

let outside select ev =
  let target = Dom.target ev in
  let rec hit = function
    | [] ->
        []
    | l :: _ when inside l.containers target ->
        []
    | l :: rest ->
        l :: hit rest
  in
  List.iter (fun deliver -> deliver ()) (List.filter_map select (hit !layers))

let push_layer layer =
  ( match !layer_listeners with
  | Some (document, _) when document == Dom.document ->
      ()
  | _ ->
      layers := [];
      layer_listeners :=
        Some
          ( Dom.document,
            [
              on_document "keydown" on_escape;
              on_document ~capture:true "pointerdown"
                (outside (fun l -> l.on_outside));
              on_document "focusin" (outside (fun l -> l.on_focus_out));
            ]
          )
  );
  layers := layer :: !layers;
  fun () ->
    layers := List.filter (fun l -> l != layer) !layers;
    if !layers = [] then (
      Option.iter
        (fun (_, stops) -> List.iter (fun stop -> stop ()) stops)
        !layer_listeners;
      layer_listeners := None
    )

let inert_count = "__silica_inert"

let make_others_inert node =
  let body = Dom.body Dom.document in
  let rec siblings first skip acc =
    match first with
    | None ->
        acc
    | Some n ->
        siblings
          (Dom.opt (Dom.next_sibling n))
          skip
          ( if n == skip || Dom.node_type n <> 1 then
              acc
            else
              n :: acc
          )
  in
  let rec others n acc =
    if n == body then
      acc
    else
      match Dom.opt (Dom.parent_node n) with
      | None ->
          acc
      | Some parent ->
          others parent (siblings (Dom.opt (Dom.first_child parent)) n acc)
  in
  let count n =
    Option.value (Dom.opt (Dom.get_property n inert_count)) ~default:0
  in
  let marked =
    List.filter
      (fun n ->
        let owned = count n > 0 in
        if owned || Dom.opt (Dom.get_attribute n "inert") = None then (
          Dom.set_property n inert_count (count n + 1);
          Dom.set_attribute n "inert" "";
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
        Dom.set_property n inert_count left;
        if left = 0 then Dom.remove_attribute n "inert"
      )
      marked

let trap_focus ~container ~return_focus =
  let previous = Dom.opt (Dom.active_element Dom.document) in
  let layer =
    {
      containers = [ container ];
      on_escape = None;
      on_outside = None;
      on_focus_out = None;
    }
  in
  let pop = push_layer layer in
  let container_node () = find_ref container in
  let restore =
    match container_node () with
    | Some node ->
        make_others_inert node
    | None ->
        fun () -> ()
  in
  Option.iter focus_first (container_node ());
  let stop_keys =
    on_document ~capture:true "keydown" (fun ev ->
        let active = Dom.opt (Dom.active_element Dom.document) in
        let in_newer =
          match (active, container_node ()) with
          | Some a, Some node ->
              (not (Dom.contains node a)) && in_newer_layer layer a
          | _ ->
              false
        in
        if Dom.key ev = "Tab" && not in_newer then
          match container_node () with
          | None ->
              ()
          | Some node -> (
              let items = focusables node in
              match (items, List.rev items) with
              | [], _ | _, [] ->
                  Dom.prevent_default ev
              | first :: _, last :: _ ->
                  let outside =
                    match active with
                    | Some a ->
                        not (Dom.contains node a)
                    | None ->
                        true
                  in
                  let at node_ =
                    match active with Some a -> a == node_ | None -> false
                  in
                  if Dom.shift_key ev && (outside || at first) then (
                    Dom.prevent_default ev;
                    Dom.focus last
                  ) else if (not (Dom.shift_key ev)) && (outside || at last)
                    then (
                    Dom.prevent_default ev;
                    Dom.focus first
                  )
            )
    )
  in
  let stop_focus =
    on_document ~capture:true "focusin" (fun ev ->
        let target = Dom.target ev in
        match container_node () with
        | Some node
          when (not (Dom.contains node target))
               && not (in_newer_layer layer target) ->
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
      | Some node when Dom.is_connected node ->
          Dom.focus node
      | _ ->
          ()

let px value = Js.Float.toString value ^ "px"

let rect_of array =
  { Rect.x = array.(0); y = array.(1); width = array.(2); height = array.(3) }

let place_popup ~anchor ~popup place =
  match (find_ref anchor, find_ref popup) with
  | Some anchor_node, Some popup_node ->
      Dom.style_property popup_node "position" "fixed";
      let size = Dom.viewport_size () in
      let viewport =
        { Rect.x = 0.; y = 0.; width = size.(0); height = size.(1) }
      in
      let anchor_rect = rect_of (Dom.bounding_rect anchor_node) in
      let result =
        Place.compute place ~anchor:anchor_rect
          ~popup:(rect_of (Dom.bounding_rect popup_node))
          ~viewport
      in
      Dom.style_property popup_node "left" (px result.x);
      Dom.style_property popup_node "top" (px result.y);
      Dom.style_property popup_node "--anchor-width" (px anchor_rect.width);
      Dom.style_property popup_node "--anchor-height" (px anchor_rect.height);
      Dom.style_property popup_node "--available-width"
        (px result.available_width);
      Dom.style_property popup_node "--available-height"
        (px result.available_height);
      Dom.set_attribute popup_node "data-side" (Place.side_name result.side);
      Dom.set_attribute popup_node "data-align" (Place.align_name result.align)
  | _ ->
      ()

let track_anchor ~anchor ~popup place =
  let update () = place_popup ~anchor ~popup place in
  update ();
  let nodes = Array.of_list (List.filter_map find_ref [ anchor; popup ]) in
  Dom.on_layout_change nodes update

let listen node name handler =
  Dom.add_listener node name handler;
  fun () -> Dom.remove_listener node name handler

let scroll_of array =
  {
    Silica.Scroll.left = array.(0);
    top = array.(1);
    scroll_width = array.(2);
    scroll_height = array.(3);
    client_width = array.(4);
    client_height = array.(5);
  }

let start_source (type e) ~(dispatch : e -> unit)
    ~(block : (Nav.t -> e) -> unit -> unit) (source : e Cmd.source) :
    unit -> unit =
  let limited rate deliver =
    match rate with
    | None ->
        deliver
    | Some rate ->
        Rate.limiter ~schedule:Web.schedule rate deliver
  in
  match source with
  | Cmd.Focus_trap { container; return_focus } ->
      trap_focus ~container ~return_focus
  | Cmd.Scroll_lock ->
      lock_scroll ()
  | Cmd.Outside_press { container; event } ->
      on_document ~capture:true "pointerdown" (fun ev ->
          let inside =
            match find_ref container with
            | Some node ->
                Dom.contains node (Dom.target ev)
            | None ->
                false
          in
          if not inside then dispatch event
      )
  | Cmd.Global_keys { bindings; prevent; rate } ->
      let deliver = limited rate dispatch in
      on_document "keydown" (fun ev ->
          let key = key_of_event ev in
          let text_entry =
            (not (key.ctrl || key.meta || key.key = "Escape"))
            && is_text_field (Dom.target ev)
          in
          match
            List.find_opt (fun (bound, _) -> Key.equal bound key) bindings
          with
          | Some _ when Dom.default_prevented ev || text_entry ->
              ()
          | Some (_, event) ->
              if prevent then Dom.prevent_default ev;
              deliver event
          | None ->
              ()
      )
  | Cmd.Resize { target; rate; on_resize } -> (
      let deliver = limited rate (fun rect -> dispatch (on_resize rect)) in
      match find_ref target with
      | Some node ->
          Dom.observe_resize node (fun rect ->
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
  | Cmd.Dismiss { containers; on_escape; on_outside; on_focus_out } ->
      let deliver event () = dispatch event in
      push_layer
        {
          containers;
          on_escape = Option.map deliver on_escape;
          on_outside = Option.map deliver on_outside;
          on_focus_out = Option.map deliver on_focus_out;
        }
  | Cmd.Anchor { anchor; popup; place } ->
      track_anchor ~anchor ~popup place
  | Cmd.Intersection { target; rate; on_change } -> (
      let deliver =
        limited rate (fun visible -> dispatch (on_change visible))
      in
      match find_ref target with
      | Some node ->
          Dom.observe_intersection node deliver
      | None ->
          fun () -> ()
    )
  | Cmd.Scroll { target; rate; on_scroll } -> (
      let deliver = limited rate (fun s -> dispatch (on_scroll s)) in
      let watch node read =
        let report _ = deliver (scroll_of (read ())) in
        report ();
        listen node "scroll" report
      in
      match target with
      | None ->
          watch Dom.window Dom.page_scroll
      | Some r -> (
          match find_ref r with
          | Some node ->
              watch node (fun () -> Dom.element_scroll node)
          | None ->
              fun () -> ()
        )
    )
  | Cmd.Visibility on_change ->
      let report _ = dispatch (on_change (Dom.visible ())) in
      report ();
      listen Dom.document "visibilitychange" report
  | Cmd.Block { on_attempt } ->
      block on_attempt
  | Cmd.Media { query; on_change } ->
      Dom.watch_media query (fun matches -> dispatch (on_change matches))
  | Cmd.Online on_change ->
      let report _ = dispatch (on_change (Dom.online ())) in
      report ();
      let stop_online = listen Dom.window "online" report in
      let stop_offline = listen Dom.window "offline" report in
      fun () ->
        stop_online ();
        stop_offline ()

type change = Pushed | Replaced of Url.t | Traversed of int
type pop = Report | Pass | Silent of { scroll : bool }

let route_focus root =
  let target =
    match Dom.opt (Dom.query_one root "[data-silica-route-focus]") with
    | Some node ->
        node
    | None ->
        root
  in
  if Dom.opt (Dom.get_attribute target "tabindex") = None then
    Dom.set_attribute target "tabindex" "-1";
  Dom.focus_without_scroll target

let make (type e) ~dev ~adopt ~hooks ~root
    ~(on_route : (Router.event -> e) option) : e Renderer.make =
 fun ~dispatch ->
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
    if !stop_asking = None then stop_asking := Some (Dom.ask_before_unload ());
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
      Dom.after_task (fun () ->
          if arrival = !arrivals then (
            if !focus_due then (
              focus_due := false;
              route_focus root
            );
            Dom.announce (Dom.title Dom.document)
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
  let go delta = if delta <> 0 then Dom.history_go delta in
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
            Dom.watch_visible (fun a ->
                if Dom.origin a = Dom.origin Dom.location then
                  preload (anchor_url a)
            );
        }
    | None ->
        hooks
  in
  let delegate_portal sub = delegate sub ~dispatch ~link ~preload in
  let v = Vdom.make ~hooks ~adopt ~dev ~on_portal:delegate_portal root in
  delegate v ~dispatch ~link ~preload;
  if Option.is_some on_route then (
    Scroll.start ();
    Dom.add_listener Dom.window "popstate" (fun _ -> popped ());
    Dom.add_listener Dom.window "pagehide" (fun _ -> Scroll.save ());
    Dom.add_listener Dom.window "scroll" (fun _ -> Scroll.scrolled ())
  );
  let sources : (string, unit -> unit) Hashtbl.t = Hashtbl.create 4 in
  let stop_source ~id =
    match Hashtbl.find_opt sources id with
    | Some stop ->
        Hashtbl.remove sources id;
        stop ()
    | None ->
        ()
  in
  let on_ref f r = Option.iter f (find_ref r) in
  let run : e Cmd.t -> unit = function
    | Cmd.Focus r ->
        focus_due := false;
        on_ref
          (fun node ->
            if Dom.opt (Dom.closest node "[data-silica-portal]") <> None then
              Dom.focus_without_scroll node
            else
              Dom.focus node
          )
          r
    | Cmd.Blur r ->
        on_ref Dom.blur r
    | Cmd.Scroll_into_view r ->
        Dom.microtask (fun () ->
            on_ref
              (fun n -> if Dom.has_scroll n then Dom.scroll_into_view_nearest n)
              r
        )
    | Cmd.Navigate u ->
        if not (blocked (Nav.Push u)) then push u
    | Cmd.Replace u ->
        if not (blocked (Nav.Replace u)) then replace u
    | Cmd.Resume nav ->
        resume nav
    | Cmd.Revert_navigation ->
        revert ()
    | Cmd.Remount | Cmd.Preload _ | Cmd.Load_chunk _ | Cmd.Set_status _ ->
        ()
    | Cmd.Scroll_to { target = None; left; top } ->
        Dom.scroll_to left top
    | Cmd.Scroll_to { target = Some r; left; top } ->
        on_ref (fun node -> Dom.scroll_element node left top) r
    | Cmd.Measure { target; on_rect } ->
        dispatch
          (on_rect
             (Option.map
                (fun node -> rect_of (Dom.bounding_rect node))
                (find_ref target)
             )
          )
    | Cmd.Copy { text; on_result } ->
        Dom.copy_text text (fun ok ->
            Option.iter (fun on_result -> dispatch (on_result ok)) on_result
        )
    | Cmd.Load_script { src; on_result } ->
        Dom.load_script src (fun ok -> dispatch (on_result ok))
    | Cmd.Set_title title ->
        Dom.set_title_ Dom.document title
    | Cmd.Set_document_attribute { name; value } as command ->
        Cmd.check command;
        Dom.set_document_attribute name (Js.Nullable.fromOption value)
    | Cmd.Set_cookie { name; value; max_age } as command ->
        Cmd.check command;
        Dom.set_cookie name value max_age
    | Cmd.Back ->
        Dom.back Dom.history
    | Cmd.Forward ->
        Dom.forward Dom.history
    | Cmd.Reload ->
        Dom.reload Dom.location
  in
  {
    Renderer.paint =
      (fun node ->
        Vdom.render v node;
        List.iter Dom.dispatch_input (Vdom.take_typed v);
        Scroll.after_paint ()
      );
    run;
    start_source =
      (fun ~id source ->
        stop_source ~id;
        Hashtbl.replace sources id (start_source ~dispatch ~block source)
      );
    stop_source;
  }
