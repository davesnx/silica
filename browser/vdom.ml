open Silica

type 'e vnode = {
  key : string option;
  kind : 'e kind;
  mutable dom : Dom.node option;
}

and 'e kind =
  | Elt of {
      tag : string;
      attrs : 'e Node.attr list;
      children : 'e vnode array;
      channel : string option;
      islands : bool;
    }
  | Txt of string

type segment = Index of int | Key of string

type 'e portal = {
  target : string option;
  theme : string option;
  path : segment list;
  nodes : 'e vnode array;
}

let position parent index key =
  (match key with Some k -> Key k | None -> Index index) :: parent

let theme_of attrs theme =
  match
    List.find_map
      (function Node.String ("data-theme", v) -> Some v | _ -> None)
      attrs
  with
  | None ->
      theme
  | found ->
      found

let island attrs =
  List.find_map
    (function Node.String ("data-silica-live", c) -> Some c | _ -> None)
    attrs

let has_islands v = match v.kind with Elt e -> e.islands | Txt _ -> false

let rec norm ~portals ~theme ~parent ~index ?key acc n =
  match Node.view n with
  | Node.Element (tag, attrs, children) ->
      let children =
        norm_list ~portals ~theme:(theme_of attrs theme)
          ~parent:(position parent index key)
          children
      in
      let channel = island attrs in
      {
        key;
        dom = None;
        kind =
          Elt
            {
              tag;
              attrs;
              children;
              channel;
              islands =
                Option.is_some channel || Array.exists has_islands children;
            };
      }
      :: acc
  | Node.Text "" ->
      acc
  | Node.Text s ->
      { key; kind = Txt s; dom = None } :: acc
  | Node.List l ->
      norm_into ~portals ~theme ~parent:(position parent index key) acc l
  | Node.Keyed (k, n) ->
      norm ~portals ~theme ~parent ~index ~key:k acc n
  | Node.Portal (target, children) ->
      let outer = !portals in
      let path = position parent index key in
      let nodes = norm_list ~portals ~theme ~parent:path children in
      let rec inner = function
        | rest when rest == outer ->
            []
        | portal :: rest ->
            portal :: inner rest
        | [] ->
            []
      in
      let theme =
        if target = None then
          theme
        else
          None
      in
      portals := inner !portals @ ({ target; theme; path; nodes } :: outer);
      acc
  | Node.Boundary { body; _ } ->
      norm ~portals ~theme ~parent ~index ?key acc body
  | Node.Component _ | Node.Chunk _ ->
      invalid_arg "Silica_browser: unresolved component node"

and norm_into ~portals ~theme ~parent acc l =
  let rec go index acc = function
    | [] ->
        acc
    | n :: rest ->
        go (index + 1) (norm ~portals ~theme ~parent ~index acc n) rest
  in
  go 0 acc l

and norm_list ~portals ~theme ~parent l =
  Array.of_list (List.rev (norm_into ~portals ~theme ~parent [] l))

let dom_of v =
  match v.dom with
  | Some dom ->
      dom
  | None ->
      invalid_arg "Silica_browser: a view node has no DOM node"

let svg_namespace = "http://www.w3.org/2000/svg"
let xlink_namespace = "http://www.w3.org/1999/xlink"
let xml_namespace = "http://www.w3.org/XML/1998/namespace"

type namespace = Html | Svg

let own namespace tag =
  if tag = "svg" then
    Svg
  else
    namespace

let inside namespace tag =
  if tag = "foreignObject" then
    Html
  else
    own namespace tag

let same_option a b =
  match (a, b) with
  | None, None ->
      true
  | Some x, Some y ->
      String.equal x y
  | Some _, None | None, Some _ ->
      false

let prefixed prefix name =
  let n = String.length prefix in
  String.length name > n && String.sub name 0 n = prefix

let attribute_namespace name =
  if prefixed "xlink:" name then
    Some (xlink_namespace, String.sub name 6 (String.length name - 6))
  else if prefixed "xml:" name then
    Some (xml_namespace, String.sub name 4 (String.length name - 4))
  else
    None

let read_attribute el name =
  match attribute_namespace name with
  | Some (namespace, local) ->
      Dom.opt (Dom.get_attribute_ns el namespace local)
  | None ->
      Dom.opt (Dom.get_attribute el name)

let write_attribute el name value =
  match attribute_namespace name with
  | Some (namespace, _) ->
      Dom.set_attribute_ns el namespace name value
  | None ->
      Dom.set_attribute el name value

let erase_attribute el name =
  match attribute_namespace name with
  | Some (namespace, local) ->
      Dom.remove_attribute_ns el namespace local
  | None ->
      Dom.remove_attribute el name

let attr_name : _ Node.attr -> string option = function
  | Node.String (k, _) | Node.Bool (k, _) ->
      Some k
  | Node.Ref _ ->
      Some "data-silica-ref"
  | _ ->
      None

let is_field tag = tag = "input" || tag = "textarea" || tag = "select"

let sync_value tag el v =
  if is_field tag && Dom.value el <> v then Dom.set_value el v

let set_attr tag el : _ Node.attr -> unit = function
  | Node.String (k, v) ->
      if read_attribute el k <> Some v then write_attribute el k v;
      if k = "value" then sync_value tag el v
  | Node.Bool (k, v) ->
      if v then
        write_attribute el k ""
      else
        erase_attribute el k;
      if k = "checked" && tag = "input" then Dom.set_checked el v
  | Node.Ref r ->
      Dom.set_attribute el "data-silica-ref" (Ref.name r)
  | _ ->
      ()

let drop_attr tag el name =
  erase_attribute el name;
  if name = "value" && is_field tag then
    Dom.set_value el ""
  else if name = "checked" && tag = "input" then
    Dom.set_checked el false

let set_last tag el name attrs =
  let last =
    List.fold_left
      (fun found a ->
        if same_option (attr_name a) (Some name) then
          Some a
        else
          found
      )
      None attrs
  in
  Option.iter (set_attr tag el) last

let patch_attrs_from_dom tag el olds news =
  let names = List.filter_map attr_name news in
  List.iter
    (fun a ->
      match attr_name a with
      | Some n when not (List.mem n names) ->
          drop_attr tag el n
      | _ ->
          ()
    )
    olds;
  List.iter (set_attr tag el) news

let patch_attrs tag el olds news =
  let rec same_shape (olds : _ Node.attr list) (news : _ Node.attr list) =
    match (olds, news) with
    | [], [] ->
        true
    | o :: os, n :: ns ->
        same_option (attr_name o) (attr_name n) && same_shape os ns
    | _ ->
        false
  in
  let pair (o : _ Node.attr) (n : _ Node.attr) =
    match (o, n) with
    | Node.String (k, a), Node.String (_, b) ->
        if a <> b then
          set_last tag el k news
        else if k = "value" then
          sync_value tag el b
    | Node.Bool (k, a), Node.Bool (_, b) ->
        if a <> b then
          set_last tag el k news
        else if k = "checked" && tag = "input" then
          Dom.set_checked el b
    | Node.Ref a, Node.Ref b ->
        if Ref.name a <> Ref.name b then set_last tag el "data-silica-ref" news
    | _, n ->
        Option.iter (fun k -> set_last tag el k news) (attr_name n)
  in
  if same_shape olds news then
    List.iter2 pair olds news
  else
    patch_attrs_from_dom tag el olds news

type hooks = {
  start : Dom.node -> string -> unit;
  stop : Dom.node -> unit;
  viewport : Dom.node -> unit;
}

let no_hooks = { start = (fun _ _ -> ()); stop = ignore; viewport = ignore }

let viewport_link tag attrs =
  tag = "a"
  && List.exists
       (function
         | Node.String ("data-silica-preload", "viewport") -> true | _ -> false
         )
       attrs

let stop_islands_in h dom =
  if Dom.node_type dom = 1 then (
    Array.iter h.stop (Dom.array_from (Dom.query_all dom "[data-silica-live]"));
    if Dom.opt (Dom.get_attribute dom "data-silica-live") <> None then
      h.stop dom
  )

let remove_node h parent dom =
  stop_islands_in h dom;
  Dom.remove_child parent dom

let rec stop_islands h v =
  match v.kind with
  | Elt { channel = Some _; _ } ->
      stop_islands_in h (dom_of v)
  | Elt { islands = true; children; _ } ->
      Array.iter (stop_islands h) children
  | Elt _ | Txt _ ->
      ()

let remove h parent v =
  stop_islands h v;
  Dom.remove_child parent (dom_of v)

let rec create h namespace v =
  let dom =
    match v.kind with
    | Txt s ->
        Dom.create_text Dom.document s
    | Elt { tag; attrs; children; channel; _ } ->
        let el =
          match own namespace tag with
          | Svg ->
              Dom.create_element_ns Dom.document svg_namespace tag
          | Html ->
              Dom.create_element Dom.document tag
        in
        let inner = inside namespace tag in
        if Option.is_none channel then
          Array.iter (fun c -> Dom.append_child el (create h inner c)) children;
        List.iter (set_attr tag el) attrs;
        Option.iter (h.start el) channel;
        if viewport_link tag attrs then h.viewport el;
        el
  in
  v.dom <- Some dom;
  dom

let namespace_below parent =
  if Dom.node_type parent <> 1 then
    Html
  else if Dom.tag_name parent = "foreignObject" then
    Html
  else if Dom.opt (Dom.namespace_uri parent) = Some svg_namespace then
    Svg
  else
    Html

let children parent =
  let rec go acc = function
    | None ->
        List.rev acc
    | Some n ->
        go (n :: acc) (Dom.opt (Dom.next_sibling n))
  in
  go [] (Dom.opt (Dom.first_child parent))

let same a b =
  same_option a.key b.key
  &&
  match (a.kind, b.kind) with
  | Txt _, Txt _ ->
      true
  | Elt a, Elt b ->
      a.tag = b.tag
  | _ ->
      false

let increasing_run sources =
  let n = Array.length sources in
  let previous = Array.make n (-1) in
  let tails = Array.make (n + 1) 0 in
  let length = ref 0 in
  for i = 0 to n - 1 do
    let s = sources.(i) in
    if s >= 0 then (
      let low = ref 1 and high = ref !length in
      while !low <= !high do
        let middle = (!low + !high) / 2 in
        if sources.(tails.(middle)) < s then
          low := middle + 1
        else
          high := middle - 1
      done;
      if !low > 1 then previous.(i) <- tails.(!low - 1);
      tails.(!low) <- i;
      if !low > !length then length := !low
    )
  done;
  let kept = Array.make n false in
  let i =
    ref
      ( if !length > 0 then
          tails.(!length)
        else
          -1
      )
  in
  while !i >= 0 do
    kept.(!i) <- true;
    i := previous.(!i)
  done;
  kept

let remove_range h parent olds first last =
  let whole =
    first = 0
    && last = Array.length olds - 1
    && ( match Dom.opt (Dom.first_child parent) with
      | Some d ->
          d == dom_of olds.(0)
      | None ->
          false
      )
    &&
    match Dom.opt (Dom.last_child parent) with
    | Some d ->
        d == dom_of olds.(last)
    | None ->
        false
  in
  if whole then (
    Array.iter (stop_islands h) olds;
    Dom.set_text_content parent ""
  ) else
    for i = first to last do
      remove h parent olds.(i)
    done

let rec patch_node h namespace old nw =
  let dom = dom_of old in
  nw.dom <- Some dom;
  match (old.kind, nw.kind) with
  | Txt a, Txt b ->
      if a <> b then Dom.set_node_value dom b
  | Elt o, Elt n -> (
      let tag = n.tag in
      let changed = not (same_option o.channel n.channel) in
      if changed then Option.iter (fun _ -> h.stop dom) o.channel;
      patch_attrs tag dom o.attrs n.attrs;
      if changed then Option.iter (h.start dom) n.channel;
      if viewport_link tag n.attrs && not (viewport_link tag o.attrs) then
        h.viewport dom;
      let inner = inside namespace tag in
      match (o.channel, n.channel) with
      | None, None ->
          patch_children h inner dom o.children n.children
      | Some _, None ->
          List.iter (remove_node h dom) (children dom);
          Array.iter
            (fun c -> Dom.append_child dom (create h inner c))
            n.children
      | _, Some _ ->
          ()
    )
  | _ ->
      ()

and patch_children h namespace parent olds news =
  let old_count = Array.length olds and new_count = Array.length news in
  if new_count = 0 then (
    if old_count > 0 then remove_range h parent olds 0 (old_count - 1)
  ) else if old_count = 0 then
    Array.iter (fun v -> Dom.append_child parent (create h namespace v)) news
  else
    let start = ref 0 in
    while
      !start < old_count && !start < new_count
      && same olds.(!start) news.(!start)
    do
      patch_node h namespace olds.(!start) news.(!start);
      incr start
    done;
    let old_end = ref (old_count - 1) and new_end = ref (new_count - 1) in
    while
      !old_end >= !start && !new_end >= !start
      && Option.is_some olds.(!old_end).key
      && same olds.(!old_end) news.(!new_end)
    do
      patch_node h namespace olds.(!old_end) news.(!new_end);
      decr old_end;
      decr new_end
    done;
    let start = !start and old_end = !old_end and new_end = !new_end in
    let after () =
      if new_end + 1 < new_count then
        news.(new_end + 1).dom
      else
        Dom.opt (Dom.next_sibling (dom_of olds.(old_count - 1)))
    in
    if start > old_end then
      let anchor = Js.Nullable.fromOption (after ()) in
      for i = start to new_end do
        Dom.insert_before parent (create h namespace news.(i)) anchor
      done
    else if start > new_end then
      remove_range h parent olds start old_end
    else
      patch_middle h namespace parent olds news start old_end new_end (after ())

and patch_middle h namespace parent olds news start old_end new_end tail =
  let count = new_end - start + 1 in
  let by_key = Hashtbl.create (old_end - start + 1) in
  for j = old_end downto start do
    Option.iter (fun k -> Hashtbl.add by_key k j) olds.(j).key
  done;
  let sources = Array.make count (-1) in
  let used = Array.make (old_end - start + 1) false in
  let moved = ref false and last = ref (-1) in
  for i = start to new_end do
    let nw = news.(i) in
    let j =
      match nw.key with
      | Some k -> (
          match Hashtbl.find_opt by_key k with
          | Some j ->
              Hashtbl.remove by_key k;
              if same olds.(j) nw then
                j
              else
                -1
          | None ->
              -1
        )
      | None ->
          if i <= old_end && same olds.(i) nw then
            i
          else
            -1
    in
    if j >= 0 then (
      used.(j - start) <- true;
      sources.(i - start) <- j;
      if j < !last then
        moved := true
      else
        last := j;
      patch_node h namespace olds.(j) nw
    ) else
      ignore (create h namespace nw)
  done;
  if Array.for_all not used then
    remove_range h parent olds start old_end
  else
    for j = start to old_end do
      if not used.(j - start) then remove h parent olds.(j)
    done;
  let kept =
    if !moved then
      increasing_run sources
    else
      Array.map (fun s -> s >= 0) sources
  in
  let anchor = ref tail in
  for k = count - 1 downto 0 do
    let dom = dom_of news.(start + k) in
    if not kept.(k) then
      Dom.insert_before parent dom (Js.Nullable.fromOption !anchor);
    anchor := Some dom
  done

exception Mismatch of string

let is_marker dom = Dom.node_type dom = 8 && Dom.node_value dom = " "

let lowercase = String.lowercase_ascii

let describe dom =
  match Dom.node_type dom with
  | 1 ->
      "<" ^ lowercase (Dom.tag_name dom) ^ ">"
  | 3 ->
      "text"
  | 8 ->
      "comment"
  | _ ->
      "node"

let rec check_adoption parent news =
  let doms = List.filter (fun dom -> not (is_marker dom)) (children parent) in
  if List.length doms <> Array.length news then
    raise
      (Mismatch
         (describe parent ^ " has "
         ^ string_of_int (List.length doms)
         ^ " children in the page and "
         ^ string_of_int (Array.length news)
         ^ " in the view"
         )
      );
  List.iteri
    (fun i dom ->
      let v = news.(i) in
      match v.kind with
      | Txt _ ->
          if Dom.node_type dom <> 3 then
            raise
              (Mismatch ("text in the view, " ^ describe dom ^ " in the page"))
      | Elt { tag; channel; children; _ } ->
          if
            Dom.node_type dom <> 1
            || lowercase (Dom.tag_name dom) <> lowercase tag
          then
            raise
              (Mismatch
                 ("<" ^ tag ^ "> in the view, " ^ describe dom ^ " in the page")
              );
          if channel = None then check_adoption dom children
    )
    doms

let quote text = "\"" ^ String.escaped text ^ "\""

let warn ~dev message =
  if dev then Dom.console_warn ("silica: hydration: " ^ message)

let typed_before_paint tag dom =
  (tag = "input" || tag = "textarea") && Dom.value dom <> Dom.default_value dom

let rec adopt h ~dev ~typed parent news =
  List.iter
    (fun dom -> if is_marker dom then Dom.remove_child parent dom)
    (children parent);
  List.iteri
    (fun i dom ->
      let v = news.(i) in
      v.dom <- Some dom;
      match v.kind with
      | Txt s ->
          if Dom.node_value dom <> s then (
            warn ~dev
              ("text differs: page "
              ^ quote (Dom.node_value dom)
              ^ ", view " ^ quote s
              );
            Dom.set_node_value dom s
          )
      | Elt { tag; attrs; children = cs; channel; _ } ->
          let user_typed = typed_before_paint tag dom in
          if user_typed then typed := dom :: !typed;
          let names = List.filter_map attr_name attrs in
          Array.iter
            (fun name ->
              if not (List.mem name names) then (
                warn ~dev
                  ("attribute " ^ name ^ " of <" ^ tag ^ "> is not in the view");
                if user_typed && name = "value" then
                  erase_attribute dom name
                else
                  drop_attr tag dom name
              )
            )
            (Dom.attribute_names dom);
          List.iter
            (fun attr ->
              match attr with
              | Node.String ("value", v) when user_typed ->
                  write_attribute dom "value" v
              | Node.String (k, v) when read_attribute dom k <> Some v ->
                  warn ~dev
                    ("attribute " ^ k ^ " of <" ^ tag ^ "> differs: page "
                    ^ quote (Option.value (read_attribute dom k) ~default:"")
                    ^ ", view " ^ quote v
                    );
                  set_attr tag dom attr
              | _ ->
                  set_attr tag dom attr
            )
            attrs;
          if channel = None then adopt h ~dev ~typed dom cs;
          Option.iter (h.start dom) channel;
          if viewport_link tag attrs then h.viewport dom
    )
    (children parent)

type 'e t = {
  root : Dom.node;
  hooks : hooks;
  dev : bool;
  on_portal : ('e t -> unit) option;
  namespace : namespace;
  mutable adopting : bool;
  mutable typed : Dom.node list;
  mutable vnodes : 'e vnode array;
  mutable portals : (string option * segment list * 'e t) list;
}

let make ?(hooks = no_hooks) ?(adopt = false) ?(dev = false) ?on_portal root =
  if not adopt then List.iter (remove_node hooks root) (children root);
  {
    root;
    hooks;
    dev;
    on_portal;
    namespace = namespace_below root;
    adopting = adopt;
    typed = [];
    vnodes = [||];
    portals = [];
  }

let clear t = List.iter (remove_node t.hooks t.root) (children t.root)

let first_paint t nw =
  t.adopting <- false;
  if Dom.opt (Dom.first_child t.root) = None then (
    patch_children t.hooks t.namespace t.root [||] nw;
    t.vnodes <- nw
  ) else
    match check_adoption t.root nw with
    | () ->
        let typed = ref [] in
        adopt t.hooks ~dev:t.dev ~typed t.root nw;
        t.typed <- List.rev !typed;
        t.vnodes <- nw
    | exception Mismatch message ->
        if t.dev then
          failwith ("silica: hydration mismatch: " ^ message)
        else (
          Dom.console_warn
            ("silica: hydration mismatch, the page is rendered again: "
           ^ message
            );
          clear t;
          patch_children t.hooks t.namespace t.root [||] nw;
          t.vnodes <- nw
        )

let portal_parent target =
  match target with
  | None ->
      Some (Dom.body Dom.document)
  | Some id ->
      Dom.opt (Dom.get_by_id Dom.document id)

let paint_nodes t nw =
  if t.adopting then
    first_paint t nw
  else (
    patch_children t.hooks t.namespace t.root t.vnodes nw;
    t.vnodes <- nw
  )

let paint_portals t found =
  let kept =
    List.filter_map
      (fun { target; theme; path; nodes } ->
        let sub =
          match
            List.find_opt (fun (g, p, _) -> g = target && p = path) t.portals
          with
          | Some (_, _, sub) ->
              Some sub
          | None -> (
              match portal_parent target with
              | None ->
                  Dom.console_warn "silica: portal target not found";
                  None
              | Some parent ->
                  let container = Dom.create_portal_root parent in
                  let sub =
                    make ~hooks:t.hooks ~dev:t.dev ?on_portal:t.on_portal
                      container
                  in
                  Option.iter (fun f -> f sub) t.on_portal;
                  Some sub
            )
        in
        Option.map
          (fun sub ->
            if Dom.opt (Dom.get_attribute sub.root "data-theme") <> theme then
              Option.fold theme
                ~none:(Dom.remove_attribute sub.root "data-theme")
                ~some:(Dom.set_attribute sub.root "data-theme");
            paint_nodes sub nodes;
            (target, path, sub)
          )
          sub
      )
      (List.rev found)
  in
  List.iter
    (fun (_, _, sub) ->
      if not (List.exists (fun (_, _, kept_sub) -> kept_sub == sub) kept) then (
        clear sub;
        Option.iter
          (fun parent -> Dom.remove_child parent sub.root)
          (Dom.opt (Dom.parent_node sub.root))
      )
    )
    t.portals;
  t.portals <- kept

let render t node =
  let portals = ref [] in
  let nw =
    Array.of_list
      (List.rev (norm ~portals ~theme:None ~parent:[] ~index:0 [] node))
  in
  paint_nodes t nw;
  paint_portals t !portals

let take_typed t =
  let typed = t.typed in
  t.typed <- [];
  typed

let elements t target =
  let is_island n =
    n != t.root
    && Dom.node_type n = 1
    && Dom.opt (Dom.get_attribute n "data-silica-live") <> None
  in
  let rec path n acc =
    if n == t.root then
      Some acc
    else if is_island n then
      None
    else
      match Dom.opt (Dom.parent_node n) with
      | None ->
          None
      | Some p ->
          path p (n :: acc)
  in
  let rec descend vnodes acc = function
    | [] ->
        acc
    | dom :: rest -> (
        match
          Array.find_opt
            (fun v -> match v.dom with Some d -> d == dom | None -> false)
            vnodes
        with
        | Some { kind = Elt { attrs; children; _ }; _ } ->
            descend children ((dom, attrs) :: acc) rest
        | Some { kind = Txt _; _ } | None ->
            acc
      )
  in
  match path target [] with None -> [] | Some p -> descend t.vnodes [] p

let handler t target f =
  List.find_map (fun (_, attrs) -> List.find_map f attrs) (elements t target)
