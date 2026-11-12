open Silica

type message = Instance.message

type vnode =
  | Elt of { tag : string; attrs : message Node.attr list; kids : vnode list }
  | Txt of string

type task = {
  due : int;
  order : int;
  ms : int;
  run : unit -> unit;
  mutable live : bool;
}

type request = {
  path : string;
  body : string;
  csrf : string;
  mutable answer : ((Platform.reply, string) result -> unit) option;
}

type socket = {
  events : Platform.socket_events;
  mutable sent : string list;
  mutable closed : int option;
}

type change = Pushed | Replaced of Url.t | Traversed of int

type memory = {
  csrf : string;
  mutable now : int;
  mutable order : int;
  mutable tasks : task list;
  mutable requests : request list;
  mutable sockets : socket list;
  mutable entries : Url.t list;
  mutable index : int;
  mutable actions : message Action.t list;
  mutable sources : (string * message Action.source * (message -> unit)) list;
  mutable title : string;
  mutable document : (string * string) list;
  mutable cookies : (string * string) list;
  mutable media : (string * bool) list;
  mutable logs : string list;
  mutable gone : int;
  mutable view : message Node.t;
  mutable roots : vnode list list;
  mutable accessible : (found * message Accessibility.element) list option;
  mutable paints : int;
  mutable focused : element option;
  mutable dispatch : message -> unit;
  mutable on_route : (Router.event -> message) option;
  mutable last : change option;
  mutable route_focus : bool;
  mutable arrivals : int;
  mutable announced : string list;
  limiters : (int * int list * string, Rate.t * (message -> unit)) Hashtbl.t;
  mutable recorder : Trace.Recorder.t option;
  start_url : string;
}

and element = { owner : memory; root : int; path : int list }

and found = {
  el : element;
  tag : string;
  attrs : message Node.attr list;
  kids : vnode list;
}

let schedule t ms run =
  t.order <- t.order + 1;
  let task = { due = t.now + ms; order = t.order; ms; run; live = true } in
  t.tasks <- t.tasks @ [ task ];
  fun () -> task.live <- false

let now t = t.now

let advance t ms =
  if ms < 0 then invalid_arg "Silica_memory.advance: negative time";
  let target = t.now + ms in
  let rec loop () =
    t.tasks <- List.filter (fun task -> task.live) t.tasks;
    let due =
      List.sort
        (fun a b -> compare (a.due, a.order) (b.due, b.order))
        (List.filter (fun task -> task.due <= target) t.tasks)
    in
    match due with
    | [] ->
        ()
    | task :: _ ->
        task.live <- false;
        t.now <- max t.now task.due;
        task.run ();
        loop ()
  in
  loop ();
  t.now <- target

let timers t =
  List.filter_map
    (fun task ->
      if task.live then
        Some task.ms
      else
        None
    )
    t.tasks

let path (r : request) = r.path
let body (r : request) = r.body
let request_csrf (r : request) = r.csrf
let requests t = List.rev t.requests
let pending t = List.filter (fun r -> r.answer <> None) (requests t)

let answer r result =
  match r.answer with
  | None ->
      invalid_arg
        ("Silica_memory: the request to " ^ r.path ^ " was already answered")
  | Some k ->
      r.answer <- None;
      k result

let reply ?(status = 200) r body = answer r (Ok { Platform.status; body })
let network_error r message = answer r (Error message)
let sockets t = List.rev t.sockets
let sent s = List.rev s.sent
let closed s = s.closed
let socket_open s = s.events.on_open ()
let socket_message s data = s.events.on_message data
let socket_close s code = s.events.on_close code
let socket_error s = s.events.on_error ()

let connect t (events : Platform.socket_events) : Platform.socket =
  let s = { events; sent = []; closed = None } in
  t.sockets <- s :: t.sockets;
  {
    send = (fun m -> s.sent <- m :: s.sent);
    close = (fun code -> s.closed <- Some code);
  }

let normalize (node : message Node.t) =
  let portals = ref [] in
  let rec norm n =
    match Node.view n with
    | Node.Element (tag, attrs, kids) ->
        [ Elt { tag; attrs; kids = norm_list kids } ]
    | Node.Text "" ->
        []
    | Node.Text s ->
        [ Txt s ]
    | Node.List l ->
        norm_list l
    | Node.Keyed (_, n) ->
        norm n
    | Node.Portal (_, kids) ->
        let nodes = norm_list kids in
        portals := nodes :: !portals;
        []
    | Node.Boundary { body; _ } ->
        norm body
    | Node.Component _ | Node.Chunk _ ->
        invalid_arg "Silica_memory: unresolved component node"
  and norm_list l = List.concat_map norm l in
  let main = norm node in
  main :: List.rev !portals

let stale () = failwith "Silica_memory: the element is no longer in the view"

let chain_opt (e : element) =
  let rec go items prefix acc = function
    | [] ->
        Some acc
    | i :: rest -> (
        match List.nth_opt items i with
        | Some (Elt { tag; attrs; kids }) ->
            let path = prefix @ [ i ] in
            go kids path
              ({ el = { e with path }; tag; attrs; kids } :: acc)
              rest
        | Some (Txt _) | None ->
            None
      )
  in
  match List.nth_opt e.owner.roots e.root with
  | None ->
      None
  | Some items -> (
      match go items [] [] e.path with
      | Some (_ :: _ as c) ->
          Some c
      | _ ->
          None
    )

let chain e = match chain_opt e with Some c -> c | None -> stale ()
let self e = List.hd (chain e)

let elements t =
  List.concat
    (List.mapi
       (fun root items ->
         let rec walk prefix items =
           List.concat
             (List.mapi
                (fun i v ->
                  match v with
                  | Txt _ ->
                      []
                  | Elt { tag; attrs; kids } ->
                      let path = prefix @ [ i ] in
                      { el = { owner = t; root; path }; tag; attrs; kids }
                      :: walk path kids
                )
                items
             )
         in
         walk [] items
       )
       t.roots
    )

let attr_value attrs name =
  List.find_map
    (function
      | Node.String (n, v) when n = name ->
          Some v
      | Node.Bool (n, true) when n = name ->
          Some ""
      | Node.Ref r when name = "data-silica-ref" ->
          Some (Ref.name r)
      | _ ->
          None
      )
    attrs

let rec text_of kids =
  String.concat ""
    (List.map (function Txt s -> s | Elt { kids; _ } -> text_of kids) kids)

let below (outer : element) (inner : element) =
  let rec prefix = function
    | [], _ :: _ ->
        true
    | a :: x, b :: y ->
        a = b && prefix (x, y)
    | _ ->
        false
  in
  outer.root = inner.root && prefix (outer.path, inner.path)

let scoped ?within t =
  let all = elements t in
  match within with
  | None ->
      all
  | Some outer ->
      ignore (chain outer);
      List.filter (fun f -> below outer f.el) all

let all ?within t = List.map (fun f -> f.el) (scoped ?within t)

let by_id t id =
  match
    List.find_opt (fun f -> attr_value f.attrs "id" = Some id) (elements t)
  with
  | Some f ->
      f.el
  | None ->
      failwith ("Silica_memory: no element with id " ^ id)

let select ?within t name value =
  List.filter_map
    (fun f ->
      if attr_value f.attrs name = Some value then
        Some f.el
      else
        None
    )
    (scoped ?within t)

let by_text ?within t text =
  let matches =
    List.filter (fun f -> text_of f.kids = text) (scoped ?within t)
  in
  match
    List.find_opt
      (fun f -> not (List.exists (fun g -> below f.el g.el) matches))
      matches
  with
  | Some f ->
      f.el
  | None ->
      failwith ("Silica_memory: no element with the text " ^ text)

let text e = text_of (self e).kids
let attribute e name = attr_value (self e).attrs name
let tag e = (self e).tag

let unwrap (attr : message Node.attr) =
  match attr with
  | Node.Rated (rate, inner) ->
      (inner, Some rate)
  | _ ->
      (attr, None)

let handlers chain select =
  List.concat_map
    (fun f ->
      List.filter_map
        (fun attr ->
          let inner, rate = unwrap attr in
          Option.map (fun value -> (f.el, value, rate)) (select inner)
        )
        f.attrs
    )
    chain

let first chain select =
  match handlers chain select with [] -> None | found :: _ -> Some found

let emit t (el : element) name rate message =
  match rate with
  | None ->
      t.dispatch message
  | Some rate ->
      let key = (el.root, el.path, name) in
      let limited =
        match Hashtbl.find_opt t.limiters key with
        | Some (current, limited) when current = rate ->
            limited
        | _ ->
            let limited =
              Rate.limiter ~schedule:(schedule t) rate (fun m -> t.dispatch m)
            in
            Hashtbl.replace t.limiters key (rate, limited);
            limited
      in
      limited message

let deliver t name = Option.iter (fun (el, e, rate) -> emit t el name rate e)

type layer = {
  containers : Ref.t list;
  on_escape : message option;
  on_outside : message option;
  on_focus_out : message option;
}

let is_layer = function
  | Action.Dismiss _ | Action.Focus_trap _ ->
      true
  | _ ->
      false

let layers t =
  List.rev
    (List.filter_map
       (fun (_, source, _) ->
         match source with
         | Action.Dismiss { containers; on_escape; on_outside; on_focus_out } ->
             Some { containers; on_escape; on_outside; on_focus_out }
         | Action.Focus_trap { container; _ } ->
             Some
               {
                 containers = [ container ];
                 on_escape = None;
                 on_outside = None;
                 on_focus_out = None;
               }
         | _ ->
             None
       )
       t.sources
    )

let inside refs chain =
  List.exists
    (fun f ->
      match attr_value f.attrs "data-silica-ref" with
      | Some name ->
          List.exists (fun r -> Ref.name r = name) refs
      | None ->
          false
    )
    chain

let outside t chain select =
  let rec hit = function
    | [] ->
        []
    | l :: _ when inside l.containers chain ->
        []
    | l :: rest ->
        l :: hit rest
  in
  List.iter t.dispatch (List.filter_map select (hit (layers t)))

let pointer_down t chain =
  let layered = ref false in
  List.iter
    (fun (_, source, _) ->
      match source with
      | Action.Outside_press { container; event } ->
          if not (inside [ container ] chain) then t.dispatch event
      | source when is_layer source && not !layered ->
          layered := true;
          outside t chain (fun l -> l.on_outside)
      | _ ->
          ()
    )
    t.sources

let report t event = Option.iter (fun f -> t.dispatch (f event)) t.on_route
let go t url = report t (Router.Url_changed url)
let current_url t = List.nth t.entries t.index

let push t url =
  t.entries <- List.filteri (fun i _ -> i <= t.index) t.entries @ [ url ];
  t.index <- t.index + 1

let replace t url =
  t.entries <-
    List.mapi
      (fun i u ->
        if i = t.index then
          url
        else
          u
      )
      t.entries

let same_origin href =
  String.length href > 0
  && href.[0] = '/'
  && not (String.length href > 1 && href.[1] = '/')

let newest_block t =
  List.fold_left
    (fun found (_, source, deliver) ->
      match source with
      | Action.Block { on_attempt } ->
          Some (fun nav -> deliver (on_attempt nav))
      | _ ->
          found
    )
    None t.sources

let blocked t nav =
  match newest_block t with
  | Some attempt ->
      attempt nav;
      true
  | None ->
      false

let route_focus_target t =
  List.find_map
    (fun f ->
      if attr_value f.attrs "data-silica-route-focus" <> None then
        Some f.el
      else
        None
    )
    (elements t)

let rec set_focus t target =
  let same =
    match (t.focused, target) with
    | Some a, Some b ->
        a.root = b.root && a.path = b.path
    | None, None ->
        true
    | _ ->
        false
  in
  if not same then (
    let old = Option.bind t.focused chain_opt in
    t.focused <- target;
    Option.iter
      (fun c ->
        deliver t "focusout"
          (first c (function Node.On_blur e -> Some e | _ -> None))
      )
      old;
    Option.iter
      (fun e ->
        let c = chain e in
        deliver t "focusin"
          (first c (function Node.On_focus e -> Some e | _ -> None));
        outside t c (fun l -> l.on_focus_out);
        preload_link t c
      )
      target
  )

and preload_link t chain =
  match
    List.find_opt
      (fun f ->
        f.tag = "a" && attr_value f.attrs "data-silica-preload" = Some "intent"
      )
      chain
  with
  | Some a -> (
      match attr_value a.attrs "href" with
      | Some href when same_origin href ->
          report t (Router.Preload (Url.of_string href))
      | _ ->
          ()
    )
  | None ->
      ()

let arrived t (before : Url.t) =
  let now = current_url t in
  if before.path <> now.path then (
    t.route_focus <- true;
    t.arrivals <- t.arrivals + 1;
    let arrival = t.arrivals in
    ignore
      ( schedule t 0 (fun () ->
            if arrival = t.arrivals then (
              if t.route_focus then (
                t.route_focus <- false;
                set_focus t (route_focus_target t)
              );
              t.announced <- t.title :: t.announced
            )
        )
        : unit -> unit
        )
  )

let do_push t url =
  let before = current_url t in
  push t url;
  t.last <- Some Pushed;
  arrived t before;
  go t url

let do_replace t url =
  let before = current_url t in
  replace t url;
  t.last <- Some (Replaced before);
  go t url

let traverse t delta =
  let target = t.index + delta in
  if target >= 0 && target < List.length t.entries then (
    let before = current_url t in
    t.index <- target;
    t.last <- Some (Traversed delta);
    arrived t before;
    go t (current_url t)
  )

let navigate t url = if not (blocked t (Nav.Push url)) then do_push t url
let replace_url t url =
  if not (blocked t (Nav.Replace url)) then do_replace t url

let step t delta =
  let target = t.index + delta in
  if target >= 0 && target < List.length t.entries then
    if not (blocked t (Nav.Traverse { delta; url = List.nth t.entries target }))
    then
      traverse t delta

let resume t = function
  | Nav.Push url ->
      do_push t url
  | Nav.Replace url ->
      do_replace t url
  | Nav.Traverse { delta; _ } ->
      traverse t delta

let revert t =
  ( match t.last with
  | Some Pushed ->
      t.index <- t.index - 1
  | Some (Replaced old) ->
      replace t old
  | Some (Traversed delta) ->
      t.index <- t.index - delta
  | None ->
      ()
  );
  t.last <- None;
  t.route_focus <- false;
  t.arrivals <- t.arrivals + 1

let click t chain =
  let link =
    List.find_opt
      (fun f -> f.tag = "a" && attr_value f.attrs "data-silica-link" <> None)
      chain
  in
  let href = Option.bind link (fun a -> attr_value a.attrs "href") in
  match (link, href, t.on_route) with
  | Some a, Some href, Some _
    when attr_value a.attrs "target" = None
         && attr_value a.attrs "download" = None
         && same_origin href ->
      let url = Url.of_string href in
      if attr_value a.attrs "data-silica-replace" <> None then
        replace_url t url
      else
        navigate t url
  | _ ->
      deliver t "press"
        (first chain (function Node.On_press e -> Some e | _ -> None))

let accessible memory =
  match memory.accessible with
  | Some pairs ->
      pairs
  | None ->
      let found = elements memory in
      let computed = Accessibility.elements memory.view in
      if
        List.compare_lengths found computed <> 0
        || not
             (List.for_all2
                (fun (found : found) element ->
                  found.tag = Accessibility.tag element
                )
                found computed
             )
      then
        failwith
          "Silica_memory: the accessibility tree does not match the painted \
           view";
      let pairs = List.combine found computed in
      memory.accessible <- Some pairs;
      pairs

let accessible_element (element : element) =
  ignore (chain element);
  snd
    (List.find
       (fun ((found : found), _) ->
         found.el.root = element.root && found.el.path = element.path
       )
       (accessible element.owner)
    )

type unreachable = Disabled | Hidden | Inert

exception Not_reachable of { reason : unreachable; message : string }

let describe_element (element : element) =
  let item = accessible_element element in
  let name = Accessibility.name item in
  Printf.sprintf "%s%s at %s"
    ( match Accessibility.role item with
    | Some role ->
        Accessibility.role_to_string role
    | None ->
        Accessibility.tag item
    )
    ( if name = "" then
        ""
      else
        Printf.sprintf " %S" name
    )
    (Accessibility.path item)

let natively_disabled (found : found) =
  List.mem found.tag
    [
      "button"; "input"; "select"; "textarea"; "option"; "optgroup"; "fieldset";
    ]
  && attr_value found.attrs "disabled" <> None

let reachable action (element : element) =
  let chain = chain element in
  let refuse cause text =
    raise
      (Not_reachable
         {
           reason = cause;
           message =
             Printf.sprintf
               "Silica_memory: the %s %s, so a browser does not deliver %s to \
                it."
               (describe_element element) text action;
         }
      )
  in
  let blocked (found : found) =
    if attr_value found.attrs "inert" <> None then
      Inert
    else
      Hidden
  in
  ( match
      List.find_opt
        (fun (found : found) ->
          attr_value found.attrs "hidden" <> None
          || attr_value found.attrs "inert" <> None
        )
        chain
    with
  | Some found when found.el.path = element.path ->
      refuse (blocked found)
        ( if attr_value found.attrs "inert" <> None then
            "is inert"
          else
            "is hidden"
        )
  | Some found ->
      refuse (blocked found)
        (Printf.sprintf "is inside the %s %s" found.tag
           ( if attr_value found.attrs "inert" <> None then
               "that is inert"
             else
               "that is hidden"
           )
        )
  | None ->
      ()
  );
  match List.find_opt natively_disabled chain with
  | Some found when found.el.path = element.path ->
      refuse Disabled "is disabled"
  | Some found ->
      refuse Disabled (Printf.sprintf "is inside the disabled %s" found.tag)
  | None ->
      ()

let hover e =
  let chain = chain e in
  preload_link e.owner chain;
  deliver e.owner "pointerenter"
    (first chain (function
      | Node.On_pointer_enter handler ->
          handler.run Pointer.origin
      | _ ->
          None
      ))

let leave e =
  deliver e.owner "pointerleave"
    (first (chain e) (function
      | Node.On_pointer_leave handler ->
          handler.run Pointer.origin
      | _ ->
          None
      ))

let submits (control : found) =
  match
    ( control.tag,
      Option.map String.lowercase_ascii (attr_value control.attrs "type")
    )
  with
  | "button", (None | Some "submit") | "input", Some ("submit" | "image") ->
      true
  | _ ->
      false

let rec from_form = function
  | [] ->
      []
  | (found : found) :: rest ->
      if found.tag = "form" then
        found :: rest
      else
        from_form rest

let press e =
  reachable "a press" e;
  let t = e.owner in
  let chain = chain e in
  pointer_down t chain;
  click t chain;
  match chain_opt e with
  | None ->
      ()
  | Some chain -> (
      match
        List.find_opt
          (fun (found : found) -> found.tag = "button" || found.tag = "input")
          chain
      with
      | Some control when submits control ->
          deliver t "submit"
            (first (from_form chain) (function
              | Node.On_submit e ->
                  Some e
              | _ ->
                  None
              ))
      | _ ->
          ()
    )

let double_press e =
  reachable "a double press" e;
  deliver e.owner "dblclick"
    (first (chain e) (function Node.On_double_press e -> Some e | _ -> None))

let input e value =
  reachable "input" e;
  Option.iter
    (fun (el, f, rate) -> emit e.owner el "input" rate (f value))
    (first (chain e) (function Node.On_input f -> Some f | _ -> None))

let submit e =
  reachable "a submit" e;
  deliver e.owner "submit"
    (first (chain e) (function Node.On_submit e -> Some e | _ -> None))

let focus e =
  reachable "focus" e;
  set_focus e.owner (Some e)
let blur t = set_focus t None
let focused t = t.focused

let native = [ "button"; "a"; "input"; "textarea"; "select"; "summary" ]

let key_handled t chain name select (k : Key.t) prevented =
  let rec go = function
    | [] ->
        false
    | (el, (handler : (Key.t, message) Node.handler), rate) :: rest -> (
        match handler.run k with
        | Some e ->
            if handler.prevent then prevented := true;
            emit t el name rate e;
            true
        | None ->
            go rest
      )
  in
  go (handlers chain select)

let focus_chain t = match t.focused with Some e -> chain e | None -> []

let key t (k : Key.t) =
  let chain = focus_chain t in
  let prevented = ref false in
  let handled =
    key_handled t chain "keydown"
      (function Node.On_key_down h -> Some h | _ -> None)
      k prevented
  in
  ( if not handled then
      let is_native =
        match chain with f :: _ -> List.mem f.tag native | [] -> false
      in
      if (k.key = "Enter" || k.key = " ") && not is_native then
        match
          first chain (function Node.On_press e -> Some e | _ -> None)
        with
        | Some _ as found ->
            prevented := true;
            deliver t "press" found
        | None ->
            ()
  );
  let text_field =
    ( match chain with
      | f :: _ ->
          List.mem f.tag [ "input"; "textarea"; "select" ]
      | [] ->
          false
      )
    || List.exists
         (fun f ->
           match attr_value f.attrs "contenteditable" with
           | Some v ->
               v <> "false"
           | None ->
               false
         )
         chain
  in
  let text_entry = (not (k.ctrl || k.meta || k.key = "Escape")) && text_field in
  let layered = ref false in
  List.iter
    (fun (_, source, deliver) ->
      match source with
      | Action.Global_keys { bindings; prevent; _ } -> (
          match
            List.find_opt (fun (bound, _) -> Key.equal bound k) bindings
          with
          | Some _ when !prevented || text_entry ->
              ()
          | Some (_, event) ->
              if prevent then prevented := true;
              deliver event
          | None ->
              ()
        )
      | source when is_layer source && not !layered -> (
          layered := true;
          if k.key = "Escape" && not !prevented then
            match List.find_map (fun l -> l.on_escape) (layers t) with
            | Some event ->
                prevented := true;
                t.dispatch event
            | None ->
                ()
        )
      | _ ->
          ()
    )
    t.sources

let key_up t k =
  ignore
    (key_handled t (focus_chain t) "keyup"
       (function Node.On_key_up h -> Some h | _ -> None)
       k (ref false)
    )

let find_ref t r =
  List.find_map
    (fun f ->
      if attr_value f.attrs "data-silica-ref" = Some (Ref.name r) then
        Some f.el
      else
        None
    )
    (elements t)

let back t = step t (-1)
let forward t = step t 1

let run t (action : message Action.t) =
  t.actions <- action :: t.actions;
  match action with
  | Action.Focus r ->
      t.route_focus <- false;
      Option.iter (fun e -> set_focus t (Some e)) (find_ref t r)
  | Action.Blur r -> (
      match (find_ref t r, t.focused) with
      | Some e, Some f when e.root = f.root && e.path = f.path ->
          set_focus t None
      | _ ->
          ()
    )
  | Action.Navigate url ->
      navigate t url
  | Action.Replace url ->
      replace_url t url
  | Action.Resume nav ->
      resume t nav
  | Action.Revert_navigation ->
      revert t
  | Action.Back ->
      back t
  | Action.Forward ->
      forward t
  | Action.Set_title title ->
      t.title <- title
  | Action.Set_document_attribute { name; value } as action -> (
      Action.check action;
      let others = List.remove_assoc name t.document in
      match value with
      | Some value ->
          t.document <- others @ [ (name, value) ]
      | None ->
          t.document <- others
    )
  | Action.Set_cookie { name; value; _ } as action ->
      Action.check action;
      t.cookies <- List.remove_assoc name t.cookies @ [ (name, value) ]
  | Action.Measure { target; on_rect } ->
      t.dispatch (on_rect (Option.map (fun _ -> Rect.zero) (find_ref t target)))
  | Action.Copy { on_result; _ } ->
      Option.iter (fun on_result -> t.dispatch (on_result true)) on_result
  | Action.Load_script { on_result; _ } ->
      t.dispatch (on_result true)
  | Action.Scroll_into_view _
  | Action.Scroll_to _
  | Action.Reload
  | Action.Remount
  | Action.Set_status _
  | Action.Preload _
  | Action.Load_chunk _ ->
      ()

let paint t node =
  t.view <- node;
  t.roots <- normalize node;
  t.accessible <- None;
  t.paints <- t.paints + 1;
  match t.focused with
  | Some e when chain_opt e = None ->
      t.focused <- None
  | _ ->
      ()

let media_matches t query =
  Option.value (List.assoc_opt query t.media) ~default:false

let stop_source t ~id =
  t.sources <- List.filter (fun (other, _, _) -> other <> id) t.sources

let start_source t ~id (source : message Action.source) =
  stop_source t ~id;
  let deliver =
    match source with
    | Action.Global_keys { rate = Some rate; _ } ->
        Rate.limiter ~schedule:(schedule t) rate (fun m -> t.dispatch m)
    | _ ->
        fun m -> t.dispatch m
  in
  t.sources <- t.sources @ [ (id, source, deliver) ];
  match source with
  | Action.Media { query; on_change } ->
      deliver (on_change (media_matches t query))
  | _ ->
      ()

let platform t : message Platform.t =
  {
    renderer =
      (fun ~on_route ~dispatch ->
        t.dispatch <- dispatch;
        t.on_route <- on_route;
        {
          Renderer.paint = paint t;
          run = run t;
          start_source = start_source t;
          stop_source = stop_source t;
        }
      );
    location = (fun () -> current_url t);
    io =
      {
        schedule = schedule t;
        post =
          (fun ~path ~body k ->
            t.requests <-
              { path; body; csrf = t.csrf; answer = Some k } :: t.requests
          );
        connect = connect t;
        csrf = (fun () -> t.csrf);
        session_gone = (fun () -> t.gone <- t.gone + 1);
        log = (fun line -> t.logs <- line :: t.logs);
      };
  }

let make ~csrf ~url ~media =
  {
    csrf;
    now = 0;
    order = 0;
    tasks = [];
    requests = [];
    sockets = [];
    entries = [ Url.of_string url ];
    index = 0;
    actions = [];
    sources = [];
    title = "";
    document = [];
    cookies = [];
    media;
    logs = [];
    gone = 0;
    view = Node.list [];
    roots = [];
    accessible = None;
    paints = 0;
    focused = None;
    dispatch =
      (fun _ -> failwith "Silica_memory: the runtime has not made the renderer");
    on_route = None;
    last = None;
    route_focus = false;
    arrivals = 0;
    announced = [];
    limiters = Hashtbl.create 8;
    recorder = None;
    start_url = url;
  }

let start (type p s e) ?dev ?on_error ?answers ?live_config ?(csrf = "")
    ?(url = "/") ?(media = []) ~(on_route : (Router.event -> e) option)
    ~(create :
       memory ->
       dev:bool ->
       on_error:_ ->
       observe:Instance.observer ->
       (p, s, e) Instance.t
       ) () =
  let t = make ~csrf ~url ~media in
  let recorder =
    Trace.Recorder.create ~capacity:max_int ~data:true
      ~clock:(fun () -> float_of_int t.now)
      ~platform:"memory" ~build:"memory" ()
  in
  t.recorder <- Some recorder;
  Runtime.start ?dev ?on_error ?answers ?live_config ?on_route
    ~platform:(platform t)
    ~create:(fun ~dev ~on_error ->
      create t ~dev ~on_error ~observe:(Trace.Recorder.observer recorder)
    )
    ();
  t

let mount ?dev ?on_error ?answers ?live_config ?csrf ?url ?media component =
  start ?dev ?on_error ?answers ?live_config ?csrf ?url ?media ~on_route:None
    ~create:(fun _ ~dev ~on_error ~observe ->
      Instance.mount ~dev ?on_error ~observe component
    )
    ()

let mount_app ?dev ?on_error ?answers ?live_config ?csrf ?url ?media app =
  start ?dev ?on_error ?answers ?live_config ?csrf ?url ?media
    ~on_route:(Some Fun.id)
    ~create:(fun t ~dev ~on_error ~observe ->
      Instance.mount ~dev ?on_error ~observe (app (current_url t))
    )
    ()

let view t = t.view
let html t = Render.to_string t.view
let paints t = t.paints
let url t = Url.to_string (current_url t)
let history t = List.map Url.to_string t.entries
let history_index t = t.index
let actions t = List.rev t.actions
let sources t = List.map (fun (id, source, _) -> (id, source)) t.sources
let title t = t.title
let document_attribute t name = List.assoc_opt name t.document
let cookies t = t.cookies

let set_media t query matches =
  t.media <- (query, matches) :: List.remove_assoc query t.media;
  List.iter
    (fun (_, source, deliver) ->
      match source with
      | Action.Media { query = q; on_change } when q = query ->
          deliver (on_change matches)
      | _ ->
          ()
    )
    t.sources
let logs t = List.rev t.logs
let session_gone t = t.gone

let leave_asks t =
  List.exists
    (fun (_, source, _) ->
      match source with Action.Block _ -> true | _ -> false
    )
    t.sources

let announcements t = List.rev t.announced

let disabled element =
  Accessibility.disabled (accessible_element element)
  || List.exists natively_disabled (chain element)

let hidden element = Accessibility.hidden (accessible_element element)
let checked element = Accessibility.checked (accessible_element element)
let selected element = Accessibility.selected (accessible_element element)
let expanded element = Accessibility.expanded (accessible_element element)
let role element = Accessibility.role (accessible_element element)
let accessible_name element = Accessibility.name (accessible_element element)

let in_role ?within memory role =
  ignore (Option.map chain within);
  List.filter
    (fun ((found : found), element) ->
      (match within with Some outer -> below outer found.el | None -> true)
      && (not (Accessibility.hidden element))
      && Accessibility.role element = Some role
    )
    (accessible memory)

let state_words ~level ~checked ~selected ~expanded =
  let flag name = function
    | Some true ->
        [ name ]
    | Some false ->
        [ name ^ "=false" ]
    | None ->
        []
  in
  List.concat
    [
      Option.fold ~none:[]
        ~some:(fun level -> [ "level=" ^ string_of_int level ])
        level;
      flag "checked" checked;
      flag "selected" selected;
      flag "expanded" expanded;
    ]

let describe role name words =
  String.concat " "
    (Accessibility.role_to_string role
     :: Option.fold ~none:[]
          ~some:(fun name -> [ Printf.sprintf "%S" name ])
          name
    @ words
    )

let by_role ?within ?name ?level ?checked ?selected ?expanded memory role =
  let candidates = in_role ?within memory role in
  let wanted expected actual =
    match expected with Some expected -> actual = Some expected | None -> true
  in
  let matches =
    List.filter
      (fun (_, element) ->
        wanted name (Some (Accessibility.name element))
        && wanted level (Accessibility.level element)
        && wanted checked (Accessibility.checked element)
        && wanted selected (Accessibility.selected element)
        && wanted expanded (Accessibility.expanded element)
      )
      candidates
  in
  let query =
    describe role name (state_words ~level ~checked ~selected ~expanded)
  in
  let listing pairs =
    String.concat ""
      (List.map
         (fun (_, element) ->
           Printf.sprintf "\n  %s at %s"
             (describe role
                (Some (Accessibility.name element))
                (state_words
                   ~level:(Accessibility.level element)
                   ~checked:(Accessibility.checked element)
                   ~selected:(Accessibility.selected element)
                   ~expanded:(Accessibility.expanded element)
                )
             )
             (Accessibility.path element)
         )
         pairs
      )
  in
  match matches with
  | [ ((found : found), _) ] ->
      found.el
  | [] ->
      if candidates = [] then
        failwith
          (Printf.sprintf "Silica_memory: no %s; the view has no %s." query
             (Accessibility.role_to_string role)
          )
      else
        failwith
          (Printf.sprintf "Silica_memory: no %s. The elements of role %s are:%s"
             query
             (Accessibility.role_to_string role)
             (listing candidates)
          )
  | several ->
      failwith
        (Printf.sprintf "Silica_memory: %d elements match %s:%s"
           (List.length several) query (listing several)
        )

let all_by_role ?within ?name memory role =
  List.filter_map
    (fun ((found : found), element) ->
      match name with
      | Some name when Accessibility.name element <> name ->
          None
      | Some _ | None ->
          Some found.el
    )
    (in_role ?within memory role)

let all_by_name ?within memory name =
  ignore (Option.map chain within);
  List.filter_map
    (fun ((found : found), element) ->
      if
        (match within with Some outer -> below outer found.el | None -> true)
        && (not (Accessibility.hidden element))
        && Accessibility.role element = None
        && Accessibility.name element = name
      then
        Some found.el
      else
        None
    )
    (accessible memory)

let by_name ?within memory name =
  match all_by_name ?within memory name with
  | [ element ] ->
      element
  | [] ->
      let with_role =
        List.filter_map
          (fun (_, element) ->
            match Accessibility.role element with
            | Some role
              when (not (Accessibility.hidden element))
                   && Accessibility.name element = name ->
                Some (Accessibility.role_to_string role)
            | _ ->
                None
          )
          (accessible memory)
      in
      failwith
        ( match with_role with
        | [] ->
            Printf.sprintf
              "Silica_memory: no element with no role has the name %S." name
        | role :: _ ->
            Printf.sprintf
              "Silica_memory: no element with no role has the name %S; a %s \
               has it: use by_role."
              name role
        )
  | several ->
      failwith
        (Printf.sprintf
           "Silica_memory: %d elements with no role have the name %S; use \
            all_by_name and pick one."
           (List.length several) name
        )

let view_text memory = View_text.to_string memory.view

type 'p t = {
  memory : memory;
  props : 'p -> unit;
  props_of_json : Silica_json.Json.t -> ('p, string) result;
}

let no_props _ = Ok ()

let mount ?dev ?on_error ?answers ?live_config ?csrf ?url ?media component =
  {
    memory =
      mount ?dev ?on_error ?answers ?live_config ?csrf ?url ?media component;
    props = ignore;
    props_of_json = no_props;
  }

let mount_app ?dev ?on_error ?answers ?live_config ?csrf ?url ?media app =
  {
    memory =
      mount_app ?dev ?on_error ?answers ?live_config ?csrf ?url ?media app;
    props = ignore;
    props_of_json = no_props;
  }

let mount_with_props ?dev ?on_error ?answers ?live_config ?csrf ?url ?media
    ~props component =
  let current = ref props in
  let tree = ref None in
  let memory =
    start ?dev ?on_error ?answers ?live_config ?csrf ?url ?media ~on_route:None
      ~create:(fun _ ~dev ~on_error ~observe ->
        let created =
          Instance.mount_with_props ~dev ?on_error ~observe ~props:!current
            component
        in
        tree := Some created;
        created
      )
      ()
  in
  let props next =
    memory.dispatch
      (Instance.callback (fun () ->
           current := next;
           Option.iter (fun created -> Instance.set_props created next) !tree
       )
      )
  in
  let props_of_json json =
    Result.bind (Machine.props_codec component.Component.machine)
      (fun (codec : _ Codec.t) -> codec.decode json
    )
  in
  { memory; props; props_of_json }

let set_props handle props = handle.props props
let view handle = view handle.memory
let html handle = html handle.memory
let paints handle = paints handle.memory
let all ?within handle = all ?within handle.memory
let by_id handle id = by_id handle.memory id
let select ?within handle name value = select ?within handle.memory name value
let by_text ?within handle text = by_text ?within handle.memory text

let by_role ?within ?name ?level ?checked ?selected ?expanded handle role =
  by_role ?within ?name ?level ?checked ?selected ?expanded handle.memory role

let all_by_role ?within ?name handle role =
  all_by_role ?within ?name handle.memory role

let by_name ?within handle name = by_name ?within handle.memory name
let all_by_name ?within handle name = all_by_name ?within handle.memory name
let view_text handle = view_text handle.memory
let blur handle = blur handle.memory
let focused handle = focused handle.memory
let key handle key_value = key handle.memory key_value
let key_up handle key_value = key_up handle.memory key_value
let back handle = back handle.memory
let open_url handle url = navigate handle.memory (Url.of_string url)
let forward handle = forward handle.memory
let now handle = now handle.memory

let trace handle =
  match handle.memory.recorder with
  | Some recorder ->
      Trace.Recorder.trace recorder ~url:(Some handle.memory.start_url)
  | None ->
      invalid_arg "Silica_memory.trace: the runtime has not started"
let advance handle ms = advance handle.memory ms
let timers handle = timers handle.memory
let requests handle = requests handle.memory
let pending handle = pending handle.memory

let pending_request handle path =
  match
    List.find_opt
      (fun (request : request) -> request.path = path)
      (pending handle)
  with
  | Some request ->
      request
  | None ->
      failwith
        (Printf.sprintf "Silica_memory: no pending request to %s; pending: %s"
           path
           ( match pending handle with
           | [] ->
               "none"
           | requests ->
               String.concat ", "
                 (List.map (fun (request : request) -> request.path) requests)
           )
        )
let sockets handle = sockets handle.memory
let url handle = url handle.memory
let history handle = history handle.memory
let history_index handle = history_index handle.memory
let actions handle = actions handle.memory
let sources handle = sources handle.memory
let title handle = title handle.memory
let document_attribute handle name = document_attribute handle.memory name
let cookies handle = cookies handle.memory
let set_media handle query matches = set_media handle.memory query matches
let logs handle = logs handle.memory
let leave_asks handle = leave_asks handle.memory
let announcements handle = announcements handle.memory
let session_gone handle = session_gone handle.memory

type replay_result =
  | Replayed
  | Different of { seq : int; expected : string; got : string }
  | Not_realisable of { seq : int; reason : string }
  | Not_found of { seq : int; error : string }

let without_number id =
  match String.index_opt id '/' with
  | Some slash
    when slash > 0
         && String.for_all
              (fun character ->
                character = '.' || ('0' <= character && character <= '9')
              )
              (String.sub id 0 slash) ->
      String.sub id (slash + 1) (String.length id - slash - 1)
  | _ ->
      id

let drop_prefix ~prefix text =
  if String.starts_with ~prefix text then
    Some
      (String.sub text (String.length prefix)
         (String.length text - String.length prefix)
      )
  else
    None

let started_timer action =
  Option.bind (drop_prefix ~prefix:"start timer " action) (fun rest ->
      match String.rindex_opt rest ' ' with
      | Some space -> (
          let id = String.sub rest 0 space in
          match
            drop_prefix ~prefix:""
              (String.sub rest (space + 1) (String.length rest - space - 1))
          with
          | Some delay when String.ends_with ~suffix:"ms" delay ->
              Option.map
                (fun ms -> (id, ms))
                (int_of_string_opt
                   (String.sub delay 0 (String.length delay - 2))
                )
          | _ ->
              None
        )
      | None ->
          None
  )

let running_timers (records : Transition.t list) =
  List.fold_left
    (fun (running, order) (record : Transition.t) ->
      let running =
        match record.cause with
        | Timer { id; _ } ->
            List.filter (fun (other, _) -> other <> id) running
        | _ ->
            running
      in
      List.fold_left
        (fun (running, order) action ->
          match started_timer action with
          | Some (id, ms) ->
              let start =
                int_of_float (Option.value record.time_ms ~default:0.)
              in
              ( (id, (start + ms, order))
                :: List.filter (fun (other, _) -> other <> id) running,
                order + 1
              )
          | None -> (
              match drop_prefix ~prefix:"stop " action with
              | Some id ->
                  (List.filter (fun (other, _) -> other <> id) running, order)
              | None ->
                  (running, order)
            )
        )
        (running, order) record.actions
    )
    ([], 0) records
  |> fst

let describe_record (record : Transition.t) =
  Printf.sprintf "%s: %s -> %s (%s)" record.machine record.from record.target
    ( match record.change with
    | Started ->
        "started"
    | Resumed ->
        "resumed"
    | Unhandled ->
        "unhandled"
    | Kept ->
        "kept"
    | Entered ->
        "entered"
    | Ended ->
        "ended"
    )

let find_element memory ~role ~name ~path =
  let visible =
    List.filter
      (fun (_, element) -> not (Accessibility.hidden element))
      (accessible memory)
  in
  let role_matches element =
    match role with
    | Some role ->
        Option.map Accessibility.role_to_string (Accessibility.role element)
        = Some role
    | None ->
        true
  in
  let named =
    List.filter
      (fun (_, element) ->
        role_matches element && Accessibility.name element = name
      )
      visible
  in
  let at_path pairs =
    List.filter (fun (_, element) -> Accessibility.path element = path) pairs
  in
  match named with
  | [ ((found : found), _) ] ->
      Ok found.el
  | _ -> (
      match at_path named with
      | [ ((found : found), _) ] ->
          Ok found.el
      | _ ->
          Error
            (Printf.sprintf "no single %s %S at %s; the view is:\n%s"
               (Option.value role ~default:"element")
               name path
               (View_text.to_string memory.view)
            )
    )

let has_focus handle (element : element) =
  match focused handle with
  | Some (other : element) ->
      other.root = element.root && other.path = element.path
  | None ->
      false

let drive handle (record : Transition.t) =
  let memory = handle.memory in
  let not_realisable reason =
    Error (Not_realisable { seq = record.seq; reason })
  in
  match record.cause with
  | Element { handler; role; name; element; value } -> (
      match find_element memory ~role ~name ~path:element with
      | Error error ->
          Error (Not_found { seq = record.seq; error })
      | Ok found -> (
          match (handler, value) with
          | "press", _ ->
              Ok (press found)
          | "double_press", _ ->
              Ok (double_press found)
          | "submit", _ ->
              Ok (submit found)
          | "focus", _ ->
              Ok (focus found)
          | "blur", _ ->
              if not (has_focus handle found) then focus found;
              Ok (blur handle)
          | "pointer_leave", _ ->
              Ok (leave found)
          | "pointer_enter", _ ->
              Ok (hover found)
          | "input", Some text ->
              Ok (input found text)
          | "key_down", Some key_name ->
              if not (has_focus handle found) then focus found;
              Ok (key handle (Key.make key_name))
          | "key_up", Some key_name ->
              if not (has_focus handle found) then focus found;
              Ok (key_up handle (Key.make key_name))
          | _ ->
              not_realisable
                ("the handler " ^ handler ^ " has no input in Silica_memory")
        )
    )
  | Timer { id; ms } -> (
      let running =
        running_timers
          ( match memory.recorder with
          | Some recorder ->
              Trace.Recorder.transitions recorder
          | None ->
              []
          )
      in
      let matching =
        match List.assoc_opt id running with
        | Some timing ->
            Some (id, timing)
        | None ->
            List.find_opt
              (fun (other, _) -> without_number other = without_number id)
              running
      in
      match matching with
      | None ->
          not_realisable ("the timer " ^ id ^ " is not running")
      | Some (found, (due, order)) -> (
          match
            List.find_opt
              (fun (other, (other_due, other_order)) ->
                other <> found
                && (other_due < due || (other_due = due && other_order < order))
              )
              running
          with
          | Some (other, (other_due, _)) ->
              not_realisable
                (Printf.sprintf
                   "the trace fires the timer %s (%d ms) first, but the timer \
                    %s is due %d ms earlier in real time"
                   id ms other (due - other_due)
                )
          | None ->
              Ok (advance handle (max 0 (due - memory.now)))
        )
    )
  | Answer { path; answer; _ } -> (
      match
        ( List.find_opt
            (fun (request : request) -> request.path = path)
            (pending handle),
          answer
        )
      with
      | None, _ ->
          Error
            (Not_found
               { seq = record.seq; error = "no pending request to " ^ path }
            )
      | Some _, None ->
          not_realisable ("the trace has no answer data for " ^ path)
      | Some request, Some answer -> (
          let text json = Silica_json.Json.to_string json in
          match Codec.constructor_of answer with
          | Ok ("Ok", [ value ]) ->
              Ok (reply request (text value))
          | Ok ("Rejected", [ value ]) ->
              Ok (reply ~status:422 request (text value))
          | Ok ("Failed", [ error ]) -> (
              match Codec.constructor_of error with
              | Ok ("Http", [ status ]) -> (
                  match int_of_string_opt (text status) with
                  | Some status ->
                      Ok (reply ~status request "")
                  | None ->
                      not_realisable ("the status " ^ text status)
                )
              | Ok ("Network", [ reason ]) ->
                  Ok
                    (network_error request
                       ( match Silica_json.Json.classify reason with
                       | `String reason ->
                           reason
                       | _ ->
                           text reason
                       )
                    )
              | _ ->
                  Ok (reply request "not json")
            )
          | _ ->
              not_realisable ("the answer " ^ text answer ^ " is not an answer")
        )
    )
  | Props -> (
      match record.props with
      | None ->
          not_realisable "the trace has no props data"
      | Some json -> (
          match handle.props_of_json json with
          | Ok props ->
              Ok (handle.props props)
          | Error error ->
              not_realisable ("the props do not decode: " ^ error)
        )
    )
  | Url url -> (
      let entries = List.map Url.to_string memory.entries in
      if memory.index > 0 && List.nth_opt entries (memory.index - 1) = Some url
      then
        Ok (back handle)
      else if List.nth_opt entries (memory.index + 1) = Some url then
        Ok (forward handle)
      else
        match select handle "href" url with
        | link :: _ ->
            Ok (press link)
        | [] ->
            Ok (open_url handle url)
    )
  | Live { topic; _ } ->
      not_realisable
        ("an update of the Live topic " ^ topic
       ^ ": Silica_memory does not replay Live updates"
        )
  | Held { source; _ } ->
      not_realisable
        ("the held source " ^ source
       ^ ": Silica_memory does not replay held sources"
        )
  | Action_result { action } ->
      not_realisable
        ("the result of the action " ^ action
       ^ ", which Silica_memory gives at once"
        )
  | Message ->
      not_realisable
        "an event that the host or the inputs of the check gave, not an element"
  | Preload url ->
      not_realisable ("a preload of " ^ url)
  | Start | Child _ | Ended ->
      Ok ()

let is_input (record : Transition.t) ~root =
  match record.cause with
  | Element _
  | Timer _
  | Answer _
  | Live _
  | Held _
  | Action_result _
  | Url _
  | Preload _
  | Message ->
      true
  | Props ->
      record.instance = root
  | Start | Child _ | Ended ->
      false

let replay handle (expected : Trace.t) =
  let records () = (trace handle).transitions in
  let root_of transitions =
    List.find_opt
      (fun (record : Transition.t) ->
        record.change = Started && record.machine = expected.header.root
      )
      transitions
  in
  let compare (wanted : Transition.t) (got : Transition.t option) =
    match got with
    | Some got
      when got.target = wanted.target
           && got.change = Unhandled = (wanted.change = Unhandled) ->
        None
    | Some got ->
        Some
          (Different
             {
               seq = wanted.seq;
               expected = describe_record wanted;
               got = describe_record got;
             }
          )
    | None ->
        Some
          (Different
             {
               seq = wanted.seq;
               expected = describe_record wanted;
               got = "no transition of " ^ wanted.instance;
             }
          )
  in
  match root_of expected.transitions with
  | None ->
      Different
        {
          seq = 0;
          expected = "a start of " ^ expected.header.root;
          got = "a trace with no start of its root";
        }
  | Some start -> (
      match compare start (root_of (records ())) with
      | Some different ->
          different
      | None ->
          let rec go = function
            | [] ->
                Replayed
            | (wanted : Transition.t) :: rest -> (
                if not (is_input wanted ~root:start.instance) then
                  go rest
                else
                  let before = List.length (records ()) in
                  match drive handle wanted with
                  | Error result ->
                      result
                  | Ok () -> (
                      let added =
                        List.filteri
                          (fun index _ -> index >= before)
                          (records ())
                      in
                      let mine =
                        List.filter
                          (fun (record : Transition.t) ->
                            record.instance = wanted.instance
                          )
                          added
                      in
                      let own =
                        List.filter
                          (fun (record : Transition.t) ->
                            match record.cause with
                            | Props | Child _ ->
                                record.cause = wanted.cause
                            | _ ->
                                true
                          )
                          mine
                      in
                      match
                        compare wanted
                          (List.nth_opt
                             (List.rev
                                (match own with [] -> mine | own -> own)
                             )
                             0
                          )
                      with
                      | Some different ->
                          different
                      | None ->
                          go rest
                    )
              )
          in
          go expected.transitions
    )

let shows handle text = View_text.shows handle.memory.view text
