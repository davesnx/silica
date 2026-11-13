type snapshot = Encoded of string | At_start | Not_encoded of string
type entry = { machine : string; snapshot : snapshot }

type t = {
  instances : (string * entry) list;
  islands : (string * string) list;
  url : Url.t;
}

type codecs = {
  state : 'p 's 'e. ('p, 's, 'e) Machine.t -> ('s Codec.t, string) result;
}

let registered = { state = Machine.state_codec }

let at_start machine props snapshot =
  match fst (Machine.start_with_props machine props) = snapshot with
  | equal ->
      equal
  | exception Invalid_argument _ ->
      false

let encode codecs (Instance.Running { machine; props; snapshot; _ }) =
  match codecs.state machine with
  | Ok codec ->
      Encoded (Codec.to_string codec snapshot)
  | Error _ when at_start machine props snapshot ->
      At_start
  | Error reason ->
      Not_encoded reason

let of_running ?(codecs = registered) ~islands ~url running =
  {
    instances =
      List.map
        (fun (Instance.Running { path; machine; _ } as instance) ->
          ( path,
            { machine = Machine.id machine; snapshot = encode codecs instance }
          )
        )
        running;
    islands;
    url;
  }

type reason =
  | Other_machine of string
  | No_codec of string
  | Not_decoded of string
  | Render_failed of string

let reason_text = function
  | Other_machine id ->
      "another machine (" ^ id ^ ") ran here before"
  | No_codec reason ->
      reason
  | Not_decoded error ->
      "its snapshot does not decode: " ^ error
  | Render_failed exn ->
      "its view raised after the reload (" ^ exn ^ ")"

let decode (type p s e) codecs entry (machine : (p, s, e) Machine.t) props :
    (s, reason) result =
  if entry.machine <> Machine.id machine then
    Error (Other_machine entry.machine)
  else
    match entry.snapshot with
    | Not_encoded reason ->
        Error (No_codec reason)
    | At_start ->
        Ok (fst (Machine.start_with_props machine props))
    | Encoded text -> (
        match codecs.state machine with
        | Error reason ->
            Error (No_codec reason)
        | Ok codec ->
            Result.map_error
              (fun error -> Not_decoded error)
              (Codec.of_string codec text)
      )

type restarted = { path : string; machine : string; reason : reason }
type outcome = { kept : string list; restarted : restarted list }

let hook ?(codecs = registered) handoff =
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
    match found with
    | None ->
        None
    | Some (old_path, entry) -> (
        Hashtbl.replace taken old_path ();
        match decode codecs entry machine props with
        | Error reason ->
            restart reason
        | Ok snapshot -> (
            match component.view props snapshot with
            | _ ->
                kept := path :: !kept;
                Some (snapshot, Machine.resume_with_props machine props snapshot)
            | exception exn ->
                restart (Render_failed (Printexc.to_string exn))
          )
      )
  in
  ( { Instance.take },
    fun () -> { kept = List.rev !kept; restarted = List.rev !restarted }
  )
