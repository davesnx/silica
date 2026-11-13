module Json = Silica_json.Json

let version = 1

type kind = Browser | Server
type resource = { resource : Machine.resource; since : float option }

type instance = {
  path : string;
  machine : string;
  state : string;
  data : Json.t option;
  props : Json.t option;
  owns : resource list;
}

type restarted = { path : string; machine : string; reason : string }
type direction = In | Out

type event =
  | Tree of instance list
  | Mount of instance
  | Unmount of string
  | Step of { transition : Transition.t; owns : resource list option }
  | Live of { direction : direction; channel : string; message : string }
  | Resumed of { kept : string list; restarted : restarted list }
  | Paused of int

type filter = {
  machines : string list;
  path : string option;
  events : string list;
  changes : Transition.change list;
}

let every = { machines = []; path = None; events = []; changes = [] }

type target = { target : string; kind : kind; url : string option }

type request =
  | Targets
  | Get_tree of string option
  | Export of string option
  | Travel of { target : string; seq : int }
  | Return_live of string
  | Seen of string

type message =
  | Hello_target of target
  | Hello_client of string
  | Event of { id : int option; target : string; event : event }
  | Subscribe of { id : int option; target : string option; filter : filter }
  | Request of { id : int option; request : request }
  | Subscribed of { id : int option }
  | Target_list of { id : int option; targets : target list }
  | Trace_file of { id : int option; target : string; trace : string }
  | Seen_steps of {
      id : int option;
      machine : string;
      steps : Transition.t list;
    }
  | Gone of string
  | Failure of { id : int option; message : string }

let string value = Json.declassify (`String value)
let int value = Json.declassify (`Int value)
let strings values = Json.declassify (`List (List.map string values))
let object_of fields = Json.declassify (`Assoc fields)
let optional name encode = function
  | None ->
      []
  | Some value ->
      [ (name, encode value) ]

let kind_name = function Browser -> "browser" | Server -> "server"

let encode_resource { resource; since } =
  let fields =
    match resource with
    | Machine.Timer { id; ms } ->
        [ ("kind", string "timer"); ("id", string id); ("ms", int ms) ]
    | Call { id; path } ->
        [ ("kind", string "call"); ("id", string id); ("path", string path) ]
    | Subscription { id; topic } ->
        [
          ("kind", string "subscription");
          ("id", string id);
          ("topic", string topic);
        ]
    | Held { id; name } ->
        [ ("kind", string "held"); ("id", string id); ("name", string name) ]
  in
  object_of
    (fields @ optional "since" (fun time -> Json.declassify (`Float time)) since)

let resources owns = Json.declassify (`List (List.map encode_resource owns))

let instance_fields (instance : instance) =
  [
    ("path", string instance.path);
    ("machine", string instance.machine);
    ("state", string instance.state);
  ]
  @ optional "data" Fun.id instance.data
  @ optional "props" Fun.id instance.props
  @ [ ("owns", resources instance.owns) ]

let encode_filter filter =
  object_of
    (( match filter.machines with
       | [] ->
           []
       | machines ->
           [ ("machine", strings machines) ]
       )
    @ optional "path" string filter.path
    @ ( match filter.events with
      | [] ->
          []
      | events ->
          [ ("event", strings events) ]
      )
    @
    match filter.changes with
    | [] ->
        []
    | changes ->
        [ ("change", strings (List.map Transition.change_to_string changes)) ]
    )

let encode_target { target; kind; url } =
  [ ("target", string target); ("kind", string (kind_name kind)) ]
  @ optional "url" string url

let typed name ?id fields =
  object_of ((("t", string name) :: optional "id" int id) @ fields)

let encode_event ?id ~target event =
  let typed name fields =
    typed name ?id (("target", string target) :: fields)
  in
  match event with
  | Tree instances ->
      typed "tree"
        [
          ( "instances",
            Json.declassify
              (`List
                 (List.map
                    (fun instance -> object_of (instance_fields instance))
                    instances
                 )
              )
          );
        ]
  | Mount instance ->
      typed "mount" (instance_fields instance)
  | Unmount path ->
      typed "unmount" [ ("path", string path) ]
  | Step { transition; owns } -> (
      match Json.classify (Transition.codec.encode transition) with
      | `Assoc fields ->
          typed "step" (fields @ optional "owns" resources owns)
      | _ ->
          typed "step" []
    )
  | Live { direction; channel; message } ->
      typed "live"
        [
          ("dir", string (match direction with In -> "in" | Out -> "out"));
          ("channel", string channel);
          ("message", string message);
        ]
  | Resumed { kept; restarted } ->
      typed "resumed"
        [
          ("kept", strings kept);
          ( "restarted",
            Json.declassify
              (`List
                 (List.map
                    (fun (restarted : restarted) ->
                      object_of
                        [
                          ("path", string restarted.path);
                          ("machine", string restarted.machine);
                          ("reason", string restarted.reason);
                        ]
                    )
                    restarted
                 )
              )
          );
        ]
  | Paused at ->
      typed "paused" [ ("at", int at) ]

let encode = function
  | Hello_target target ->
      typed "hello"
        ([ ("role", string "target"); ("version", int version) ]
        @ encode_target target
        )
  | Hello_client name ->
      typed "hello"
        [
          ("role", string "client");
          ("version", int version);
          ("name", string name);
        ]
  | Event { id; target; event } ->
      encode_event ?id ~target event
  | Subscribe { id; target; filter } ->
      typed "subscribe" ?id
        (optional "target" string target @ [ ("filter", encode_filter filter) ])
  | Request { id; request = Targets } ->
      typed "targets" ?id []
  | Request { id; request = Get_tree target } ->
      typed "tree" ?id (optional "target" string target)
  | Request { id; request = Export target } ->
      typed "export" ?id (optional "target" string target)
  | Request { id; request = Travel { target; seq } } ->
      typed "travel" ?id [ ("target", string target); ("seq", int seq) ]
  | Request { id; request = Return_live target } ->
      typed "live" ?id [ ("target", string target) ]
  | Request { id; request = Seen machine } ->
      typed "seen" ?id [ ("machine", string machine) ]
  | Subscribed { id } ->
      typed "subscribed" ?id []
  | Target_list { id; targets } ->
      typed "targets" ?id
        [
          ( "targets",
            Json.declassify
              (`List
                 (List.map
                    (fun target -> object_of (encode_target target))
                    targets
                 )
              )
          );
        ]
  | Trace_file { id; target; trace } ->
      typed "trace" ?id [ ("target", string target); ("trace", string trace) ]
  | Seen_steps { id; machine; steps } ->
      typed "seen" ?id
        [
          ("machine", string machine);
          ( "steps",
            Json.declassify (`List (List.map Transition.codec.encode steps))
          );
        ]
  | Gone target ->
      typed "gone" [ ("target", string target) ]
  | Failure { id; message } ->
      typed "error" ?id [ ("message", string message) ]

let ( let* ) = Result.bind

let fields_of json =
  match Json.classify json with
  | `Assoc fields ->
      Ok fields
  | _ ->
      Error "expected a JSON object"

let field fields name decode =
  match List.assoc_opt name fields with
  | Some value ->
      Result.map_error
        (fun message -> "field \"" ^ name ^ "\": " ^ message)
        (decode value)
  | None ->
      Error ("missing field \"" ^ name ^ "\"")

let maybe fields name decode =
  match List.assoc_opt name fields with
  | None ->
      Ok None
  | Some value when Json.classify value = `Null ->
      Ok None
  | Some _ ->
      Result.map Option.some (field fields name decode)

let as_string json =
  match Json.classify json with
  | `String value ->
      Ok value
  | _ ->
      Error "expected a string"

let as_int json =
  match Json.classify json with
  | `Int value ->
      Ok value
  | _ ->
      Error "expected an integer"

let as_float json =
  match Json.classify json with
  | `Int value ->
      Ok (float_of_int value)
  | `Float value ->
      Ok value
  | _ ->
      Error "expected a number"

let as_list decode json =
  match Json.classify json with
  | `List items ->
      List.fold_right
        (fun item rest ->
          let* rest = rest in
          let* item = decode item in
          Ok (item :: rest)
        )
        items (Ok [])
  | _ ->
      Error "expected an array"

let as_json json = Ok json
let list_or_empty fields name decode =
  Result.map (Option.value ~default:[]) (maybe fields name (as_list decode))

let as_resource json =
  let* fields = fields_of json in
  let text name = field fields name as_string in
  let* kind = text "kind" in
  let* id = text "id" in
  let* since = maybe fields "since" as_float in
  let* resource =
    match kind with
    | "timer" ->
        let* ms = field fields "ms" as_int in
        Ok (Machine.Timer { id; ms })
    | "call" ->
        let* path = text "path" in
        Ok (Machine.Call { id; path })
    | "subscription" ->
        let* topic = text "topic" in
        Ok (Machine.Subscription { id; topic })
    | "held" ->
        let* name = text "name" in
        Ok (Machine.Held { id; name })
    | other ->
        Error ("unknown resource kind \"" ^ other ^ "\"")
  in
  Ok { resource; since }

let instance_of fields =
  let text name = field fields name as_string in
  let* path = text "path" in
  let* machine = text "machine" in
  let* state = text "state" in
  let* data = maybe fields "data" as_json in
  let* props = maybe fields "props" as_json in
  let* owns = list_or_empty fields "owns" as_resource in
  Ok { path; machine; state; data; props; owns }

let as_instance json =
  let* fields = fields_of json in
  instance_of fields

let as_restarted json =
  let* fields = fields_of json in
  let text name = field fields name as_string in
  let* path = text "path" in
  let* machine = text "machine" in
  let* reason = text "reason" in
  Ok ({ path; machine; reason } : restarted)

let as_kind json =
  let* name = as_string json in
  match name with
  | "browser" ->
      Ok Browser
  | "server" ->
      Ok Server
  | other ->
      Error ("unknown kind \"" ^ other ^ "\"")

let target_of fields =
  let* target = field fields "target" as_string in
  let* kind = field fields "kind" as_kind in
  let* url = maybe fields "url" as_string in
  Ok { target; kind; url }

let as_target json =
  let* fields = fields_of json in
  target_of fields

let as_change json =
  let* name = as_string json in
  Transition.change_of_string name

let as_filter json =
  let* fields = fields_of json in
  let* machines = list_or_empty fields "machine" as_string in
  let* path = maybe fields "path" as_string in
  let* events = list_or_empty fields "event" as_string in
  let* changes = list_or_empty fields "change" as_change in
  Ok { machines; path; events; changes }

let decode json =
  let* fields = fields_of json in
  let has name = List.mem_assoc name fields in
  let* t = field fields "t" as_string in
  let* id = maybe fields "id" as_int in
  let target_field () = field fields "target" as_string in
  let event event =
    let* target = target_field () in
    Ok (Event { id; target; event })
  in
  let request request = Ok (Request { id; request }) in
  match t with
  | "hello" -> (
      let* given = field fields "version" as_int in
      let* () =
        if given = version then
          Ok ()
        else
          Error
            (Printf.sprintf "protocol version %d; this side speaks version %d"
               given version
            )
      in
      let* role = field fields "role" as_string in
      match role with
      | "target" ->
          let* target = target_of fields in
          Ok (Hello_target target)
      | "client" ->
          let* name = field fields "name" as_string in
          Ok (Hello_client name)
      | other ->
          Error ("unknown role \"" ^ other ^ "\"")
    )
  | "tree" when has "instances" ->
      let* instances = field fields "instances" (as_list as_instance) in
      event (Tree instances)
  | "tree" ->
      let* target = maybe fields "target" as_string in
      request (Get_tree target)
  | "mount" ->
      let* instance = instance_of fields in
      event (Mount instance)
  | "unmount" ->
      let* path = field fields "path" as_string in
      event (Unmount path)
  | "step" ->
      let* transition = Transition.codec.decode json in
      let* owns = maybe fields "owns" (as_list as_resource) in
      event (Step { transition; owns })
  | "live" when has "dir" ->
      let* direction =
        let* name = field fields "dir" as_string in
        match name with
        | "in" ->
            Ok In
        | "out" ->
            Ok Out
        | other ->
            Error ("unknown dir \"" ^ other ^ "\"")
      in
      let* channel = field fields "channel" as_string in
      let* message = field fields "message" as_string in
      event (Live { direction; channel; message })
  | "live" ->
      let* target = target_field () in
      request (Return_live target)
  | "resumed" ->
      let* kept = list_or_empty fields "kept" as_string in
      let* restarted = list_or_empty fields "restarted" as_restarted in
      event (Resumed { kept; restarted })
  | "paused" ->
      let* at = field fields "at" as_int in
      event (Paused at)
  | "subscribe" ->
      let* target = maybe fields "target" as_string in
      let* filter = maybe fields "filter" as_filter in
      Ok (Subscribe { id; target; filter = Option.value filter ~default:every })
  | "targets" when has "targets" ->
      let* targets = field fields "targets" (as_list as_target) in
      Ok (Target_list { id; targets })
  | "targets" ->
      request Targets
  | "export" ->
      let* target = maybe fields "target" as_string in
      request (Export target)
  | "travel" ->
      let* target = target_field () in
      let* seq = field fields "seq" as_int in
      request (Travel { target; seq })
  | "seen" when has "steps" ->
      let* machine = field fields "machine" as_string in
      let* steps = field fields "steps" (as_list Transition.codec.decode) in
      Ok (Seen_steps { id; machine; steps })
  | "seen" ->
      let* machine = field fields "machine" as_string in
      request (Seen machine)
  | "subscribed" ->
      Ok (Subscribed { id })
  | "trace" ->
      let* target = target_field () in
      let* trace = field fields "trace" as_string in
      Ok (Trace_file { id; target; trace })
  | "gone" ->
      let* target = target_field () in
      Ok (Gone target)
  | "error" ->
      let* message = field fields "message" as_string in
      Ok (Failure { id; message })
  | other ->
      Error ("unknown message \"" ^ other ^ "\"")

let to_string message = Json.to_string (encode message)
let of_string text = Result.bind (Json.of_string text) decode

let under prefix path =
  path = prefix
  || String.length path > String.length prefix
     && String.sub path 0 (String.length prefix) = prefix
     && path.[String.length prefix] = '/'

let path_passes filter path =
  match filter.path with None -> true | Some prefix -> under prefix path

let machine_passes filter machine =
  filter.machines = [] || List.mem machine filter.machines

let passes filter = function
  | Step { transition; _ } ->
      machine_passes filter transition.machine
      && path_passes filter transition.instance
      && (filter.events = []
         || List.mem (Transition.label transition) filter.events
         )
      && (filter.changes = [] || List.mem transition.change filter.changes)
  | Mount instance ->
      machine_passes filter instance.machine && path_passes filter instance.path
  | Unmount path ->
      path_passes filter path
  | Tree _ | Live _ | Resumed _ | Paused _ ->
      true

let to_text = function
  | Event { target; event = Step { transition; _ }; _ } ->
      Some ("[" ^ target ^ "] " ^ Trace.line transition)
  | Event { target; event = Resumed { kept; restarted }; _ } ->
      Some
        ("[" ^ target ^ "] hot reload: kept "
        ^ string_of_int (List.length kept)
        ^ String.concat ""
            (List.map
               (fun (restarted : restarted) ->
                 "; " ^ restarted.path ^ " started again: " ^ restarted.reason
               )
               restarted
            )
        )
  | Event { target; event = Paused at; _ } ->
      Some ("[" ^ target ^ "] paused at " ^ string_of_int at)
  | Hello_target { target; kind; url } ->
      Some
        ("[" ^ target ^ "] connected (" ^ kind_name kind
        ^ (match url with Some url -> " " ^ url | None -> "")
        ^ ")"
        )
  | Gone target ->
      Some ("[" ^ target ^ "] disconnected")
  | Failure { message; _ } ->
      Some ("error: " ^ message)
  | Event { event = Tree _ | Mount _ | Unmount _ | Live _; _ }
  | Hello_client _
  | Subscribe _
  | Request _
  | Subscribed _
  | Target_list _
  | Trace_file _
  | Seen_steps _ ->
      None

module Target = struct
  type entry = {
    order : int;
    mutable instance : instance;
    since : (string, float) Hashtbl.t;
  }

  type t = {
    name : string;
    clock : unit -> float;
    send : event -> unit;
    instances : (string, entry) Hashtbl.t;
    mutable next : int;
    secrets : Secret.t;
  }

  let create ~target ~clock ~send () =
    {
      name = target;
      clock;
      send;
      instances = Hashtbl.create 16;
      next = 0;
      secrets = Secret.create ();
    }

  let name target = target.name

  let resource_id = function
    | Machine.Timer { id; _ }
    | Call { id; _ }
    | Subscription { id; _ }
    | Held { id; _ } ->
        id

  let encoded codec value =
    match codec with
    | Ok (codec : _ Codec.t) ->
        Some (codec.encode value)
    | Error _ ->
        None

  let owned since active =
    List.map
      (fun resource ->
        { resource; since = Hashtbl.find_opt since (resource_id resource) }
      )
      active

  let qualify ~number ~entry = function
    | Machine.Timer timer ->
        Machine.Timer
          { timer with id = Printf.sprintf "%d.%d/%s" number entry timer.id }
    | Call call ->
        Call { call with id = Printf.sprintf "%d.%d/%s" number entry call.id }
    | Subscription subscription ->
        Subscription
          {
            subscription with
            id = Printf.sprintf "%d.%d/%s" number entry subscription.id;
          }
    | Held held ->
        Held { held with id = Printf.sprintf "%d.%d/%s" number entry held.id }

  let instance_of target since ~path ~in_tree (Instance.Inspected step) =
    let active = Machine.active step.machine step.props step.snapshot in
    let active =
      if in_tree then
        List.map (qualify ~number:step.number ~entry:step.entry) active
      else
        active
    in
    let ids = List.map resource_id active in
    Hashtbl.filter_map_inplace
      (fun id time ->
        if List.mem id ids then
          Some time
        else
          None
      )
      since;
    List.iter
      (fun id ->
        if not (Hashtbl.mem since id) then
          Hashtbl.replace since id (target.clock ())
      )
      ids;
    let encoded codec value =
      Option.map (Secret.scrub target.secrets) (encoded codec value)
    in
    {
      path;
      machine = Machine.id step.machine;
      state = Machine.name step.machine step.snapshot;
      data = encoded (Machine.state_codec step.machine) step.snapshot;
      props = encoded (Machine.props_codec step.machine) step.props;
      owns = owned since active;
    }

  let combine (inspector : Instance.observer) (app : Instance.observer option) =
    match app with
    | None ->
        inspector
    | Some app ->
        {
          Instance.observe =
            (fun observed ->
              app.observe observed;
              inspector.observe observed
            );
          handler = app.handler;
        }

  let observer ?(path = Fun.id) target =
    let owns = ref None in
    let record pending =
      let transition = Transition.resolve pending in
      let transition =
        { transition with instance = path transition.instance }
      in
      target.send (Step { transition; owns = !owns });
      match transition.change with
      | Ended ->
          Hashtbl.remove target.instances transition.instance;
          target.send (Unmount transition.instance)
      | Started | Resumed | Unhandled | Kept | Entered ->
          ()
    in
    let transitions =
      Transition.observer ~clock:target.clock ~secrets:target.secrets record
    in
    {
      transitions with
      observe =
        (fun observed ->
          let (Instance.Inspected step as inspected) =
            Instance.inspect observed
          in
          let in_tree =
            match observed with Observed _ -> true | Detached _ -> false
          in
          let at = path step.instance in
          Transition.learn target.secrets observed;
          ( match step.change with
          | Started | Resumed ->
              let since = Hashtbl.create 4 in
              let instance =
                instance_of target since ~path:at ~in_tree inspected
              in
              target.next <- target.next + 1;
              Hashtbl.replace target.instances at
                { order = target.next; instance; since };
              owns := Some instance.owns;
              target.send (Mount instance)
          | Unhandled ->
              Option.iter
                (fun entry ->
                  entry.instance <-
                    instance_of target entry.since ~path:at ~in_tree inspected
                )
                (Hashtbl.find_opt target.instances at);
              owns := None
          | Ended ->
              owns := Some []
          | Kept | Entered -> (
              match Hashtbl.find_opt target.instances at with
              | Some entry ->
                  entry.instance <-
                    instance_of target entry.since ~path:at ~in_tree inspected;
                  owns := Some entry.instance.owns
              | None ->
                  owns := None
            )
          );
          transitions.observe observed
        );
    }

  let tree target =
    Hashtbl.fold (fun _ entry all -> entry :: all) target.instances []
    |> List.sort (fun left right -> compare left.order right.order)
    |> List.map (fun entry -> entry.instance)
end
