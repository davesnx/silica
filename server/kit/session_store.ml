type session = {
  id : string;
  created : float;
  last_seen : float;
  data : (string * string) list;
}

type t = {
  table : (string, session) Hashtbl.t;
  lock : Mutex.t;
  max_sessions : int;
  idle : float;
  absolute : float;
}

type ops = {
  get : now:float -> string -> session option;
  set : session -> unit;
  update : now:float -> string -> (session -> session) -> session option;
  remove : string -> unit;
  expire : now:float -> int;
  save : unit -> string;
  load : now:float -> string -> (int, string) result;
}

let create ?(max_sessions = 10_000) ?(idle = 8. *. 3600.)
    ?(absolute = 7. *. 86_400.) () =
  {
    table = Hashtbl.create 64;
    lock = Mutex.create ();
    max_sessions;
    idle;
    absolute;
  }

let expired t ~now s =
  now -. s.last_seen > t.idle || now -. s.created > t.absolute

let find t ~now id =
  match Hashtbl.find_opt t.table id with
  | Some s when expired t ~now s ->
      Hashtbl.remove t.table id;
      None
  | found ->
      found

let get t ~now id = Mutex.protect t.lock (fun () -> find t ~now id)

let update t ~now id f =
  Mutex.protect t.lock (fun () ->
      Option.map
        (fun s ->
          let changed = { (f s) with id } in
          Hashtbl.replace t.table id changed;
          changed
        )
        (find t ~now id)
  )

let oldest t =
  Hashtbl.fold
    (fun id s acc ->
      match acc with
      | Some (_, seen) when seen <= s.last_seen ->
          acc
      | _ ->
          Some (id, s.last_seen)
    )
    t.table None

let set t s =
  Mutex.protect t.lock (fun () ->
      if
        (not (Hashtbl.mem t.table s.id))
        && Hashtbl.length t.table >= t.max_sessions
      then
        Option.iter (fun (id, _) -> Hashtbl.remove t.table id) (oldest t);
      Hashtbl.replace t.table s.id s
  )

let remove t id = Mutex.protect t.lock (fun () -> Hashtbl.remove t.table id)

let expire t ~now =
  Mutex.protect t.lock (fun () ->
      let dead =
        Hashtbl.fold
          (fun id s acc ->
            if expired t ~now s then
              id :: acc
            else
              acc
          )
          t.table []
      in
      List.iter (Hashtbl.remove t.table) dead;
      List.length dead
  )

let count t = Mutex.protect t.lock (fun () -> Hashtbl.length t.table)

let encode s =
  `Assoc
    [
      ("id", `String s.id);
      ("created", `Float s.created);
      ("last_seen", `Float s.last_seen);
      ( "data",
        `List (List.map (fun (k, v) -> `List [ `String k; `String v ]) s.data)
      );
    ]

let save t =
  let sessions =
    Mutex.protect t.lock (fun () ->
        Hashtbl.fold (fun _ s acc -> s :: acc) t.table []
    )
  in
  Jsonkit.to_string
    (Jsonkit.declassify
       (`List (List.map (fun s -> Jsonkit.declassify (encode s)) sessions))
    )

let valid_id id =
  String.length id > 0
  && String.length id <= 128
  && String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) id

let number json =
  match Jsonkit.classify json with
  | `Float f ->
      Some f
  | `Int i ->
      Some (float_of_int i)
  | _ ->
      None

let pair json =
  match Jsonkit.classify json with
  | `List [ key; value ] -> (
      match (Jsonkit.classify key, Jsonkit.classify value) with
      | `String key, `String value ->
          Some (key, value)
      | _ ->
          None
    )
  | _ ->
      None

let decode t ~now json =
  match Jsonkit.classify json with
  | `Assoc fields -> (
      let field name = List.assoc_opt name fields in
      match
        ( Option.map Jsonkit.classify (field "id"),
          Option.bind (field "created") number,
          Option.bind (field "last_seen") number,
          Option.map Jsonkit.classify (field "data")
        )
      with
      | Some (`String id), Some created, Some last_seen, Some (`List data)
        when valid_id id && created <= last_seen && last_seen <= now ->
          let pairs = List.filter_map pair data in
          if List.length pairs <> List.length data then
            None
          else
            let s = { id; created; last_seen; data = pairs } in
            if expired t ~now s then
              None
            else
              Some s
      | _ ->
          None
    )
  | _ ->
      None

let load t ~now text =
  match Jsonkit.of_string text with
  | exception Jsonkit.Of_string_error message ->
      Error ("not JSON: " ^ message)
  | json -> (
      match Jsonkit.classify json with
      | `List items ->
          Ok
            (List.fold_left
               (fun added item ->
                 match decode t ~now item with
                 | Some s when get t ~now s.id = None ->
                     set t s;
                     added + 1
                 | Some _ | None ->
                     added
               )
               0 items
            )
      | _ ->
          Error "not a JSON array of sessions"
    )

let ops t =
  {
    get = (fun ~now id -> get t ~now id);
    set = set t;
    update = (fun ~now id f -> update t ~now id f);
    remove = remove t;
    expire = (fun ~now -> expire t ~now);
    save = (fun () -> save t);
    load = (fun ~now text -> load t ~now text);
  }
