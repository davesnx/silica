type entry = {
  machine : string;
  fingerprint : (string, string) result option;
  snapshot : Obj.t;
}

type t = {
  instances : (string * entry) list;
  islands : (string * string) list;
  url : Url.t;
}

let of_running ~islands ~url running =
  {
    instances =
      List.map
        (fun (Instance.Running { path; machine; snapshot }) ->
          ( path,
            {
              machine = Machine.id machine;
              fingerprint =
                Option.map Fingerprint.digest (Machine.fingerprint machine);
              snapshot = Obj.repr snapshot;
            }
          )
        )
        running;
    islands;
    url;
  }

type reason =
  | Other_machine of string
  | No_fingerprint
  | Not_handed_over of string
  | Changed
  | Holds_function
  | Render_failed of string

let reason_text = function
  | Other_machine id ->
      "another machine (" ^ id ^ ") ran here before"
  | No_fingerprint ->
      "its shape has no fingerprint"
  | Not_handed_over reason ->
      reason
  | Changed ->
      "its state type changed"
  | Holds_function ->
      "its state holds a function"
  | Render_failed exn ->
      "its view raised after the reload (" ^ exn ^ ")"

let check (type p s e) ~holds_function entry (machine : (p, s, e) Machine.t) :
    (s, reason) result =
  if entry.machine <> Machine.id machine then
    Error (Other_machine entry.machine)
  else
    match
      ( entry.fingerprint,
        Option.map Fingerprint.digest (Machine.fingerprint machine)
      )
    with
    | _, None ->
        Error No_fingerprint
    | _, Some (Error reason) ->
        Error (Not_handed_over reason)
    | Some (Ok before), Some (Ok now) when String.equal before now ->
        if holds_function entry.snapshot then
          Error Holds_function
        else
          Ok (Obj.obj entry.snapshot : s)
    | (None | Some (Ok _ | Error _)), Some (Ok _) ->
        Error Changed

let take ~holds_function handoff ~path machine =
  Option.map
    (fun entry -> check ~holds_function entry machine)
    (List.assoc_opt path handoff.instances)

let same_as_fresh (type p s e) entry (machine : (p, s, e) Machine.t) props :
    s option =
  let fresh, _ = Machine.start machine props in
  match Obj.repr fresh = entry.snapshot with
  | true ->
      Some fresh
  | false ->
      None
  | exception Invalid_argument _ ->
      None

type restarted = { path : string; machine : string; reason : reason }
type outcome = { kept : string list; restarted : restarted list }

let hook ~holds_function handoff =
  let taken = Hashtbl.create 16 in
  let kept = ref [] and restarted = ref [] in
  let unique_untaken id =
    match
      List.filter
        (fun (_, (entry : entry)) -> String.equal entry.machine id)
        handoff.instances
    with
    | [ (path, entry) ] when not (Hashtbl.mem taken path) ->
        Some (path, entry)
    | _ ->
        None
  in
  let take :
      'p 's 'e. ('p, 's, 'e) Instance.asked -> ('s * 'e Action.host list) option
      =
   fun asked ->
    let component = Instance.asked_component asked
    and props = Instance.asked_props asked in
    let machine = component.machine in
    let path = Instance.asked_path asked in
    let found =
      match List.assoc_opt path handoff.instances with
      | Some entry when not (Hashtbl.mem taken path) ->
          Some (path, entry)
      | Some _ ->
          None
      | None ->
          unique_untaken (Machine.id machine)
    in
    let restart reason =
      restarted := { path; machine = Machine.id machine; reason } :: !restarted;
      None
    in
    let resume snapshot =
      match component.view props snapshot with
      | _ ->
          kept := path :: !kept;
          Some (snapshot, Machine.resume machine props snapshot)
      | exception exn ->
          restart (Render_failed (Printexc.to_string exn))
    in
    match found with
    | None ->
        None
    | Some (old_path, entry) -> (
        Hashtbl.replace taken old_path ();
        match check ~holds_function entry machine with
        | Ok snapshot ->
            resume snapshot
        | Error ((No_fingerprint | Not_handed_over _) as reason) -> (
            match same_as_fresh entry machine props with
            | Some fresh ->
                resume fresh
            | None ->
                restart reason
          )
        | Error
            ( (Other_machine _ | Changed | Holds_function | Render_failed _) as
              reason
            ) ->
            restart reason
      )
  in
  ( { Instance.take },
    fun () -> { kept = List.rev !kept; restarted = List.rev !restarted }
  )
