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

type t = {
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
  mutable paints : int;
  mutable focused : element option;
  mutable dispatch : message -> unit;
  mutable on_route : (Router.event -> message) option;
  mutable last : change option;
  mutable route_focus : bool;
  mutable arrivals : int;
  mutable announced : string list;
  limiters : (int * int list * string, Rate.t * (message -> unit)) Hashtbl.t;
}

and element = { owner : t; root : int; path : int list }

type found = {
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

let hover e = preload_link e.owner (chain e)

let press e =
  let t = e.owner in
  let chain = chain e in
  pointer_down t chain;
  click t chain

let double_press e =
  deliver e.owner "dblclick"
    (first (chain e) (function Node.On_double_press e -> Some e | _ -> None))

let input e value =
  Option.iter
    (fun (el, f, rate) -> emit e.owner el "input" rate (f value))
    (first (chain e) (function Node.On_input f -> Some f | _ -> None))

let submit e =
  deliver e.owner "submit"
    (first (chain e) (function Node.On_submit e -> Some e | _ -> None))

let focus e = set_focus e.owner (Some e)
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
  }

let start (type s e) ?dev ?on_error ?answers ?live_config ?(csrf = "")
    ?(url = "/") ?(media = []) ~(on_route : (Router.event -> e) option)
    ~(create : t -> dev:bool -> on_error:_ -> (s, e) Instance.t) () =
  let t = make ~csrf ~url ~media in
  Runtime.start ?dev ?on_error ?answers ?live_config ?on_route
    ~platform:(platform t) ~create:(create t) ();
  t

let mount ?dev ?on_error ?answers ?live_config ?csrf ?url ?media component =
  start ?dev ?on_error ?answers ?live_config ?csrf ?url ?media ~on_route:None
    ~create:(fun _ ~dev ~on_error -> Instance.mount ~dev ?on_error component)
    ()

let mount_app ?dev ?on_error ?answers ?live_config ?csrf ?url ?media app =
  start ?dev ?on_error ?answers ?live_config ?csrf ?url ?media
    ~on_route:(Some Fun.id)
    ~create:(fun t ~dev ~on_error ->
      Instance.mount ~dev ?on_error (app (current_url t))
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
