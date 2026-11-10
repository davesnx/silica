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
  remove : string -> unit;
  expire : now:float -> int;
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

let get t ~now id =
  Mutex.protect t.lock (fun () ->
      match Hashtbl.find_opt t.table id with
      | Some s when expired t ~now s ->
          Hashtbl.remove t.table id;
          None
      | found ->
          found
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

let ops t =
  {
    get = (fun ~now id -> get t ~now id);
    set = set t;
    remove = remove t;
    expire = (fun ~now -> expire t ~now);
  }
