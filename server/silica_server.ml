open Silica
module Session = Silica_server_kit.Session_store
module Assets = Silica_server_kit.Assets
module Kit = Silica_server_kit
module Json = Silica_json.Json

type request = {
  meth : string;
  path : string;
  query : (string * string) list;
  headers : (string * string) list;
  body : string;
}

type response = {
  status : int;
  headers : (string * string) list;
  body : string;
}

module type IO = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val sleep : float -> unit t
  val async : (unit -> unit t) -> unit
  val now : unit -> float

  type 'a promise

  val promise : unit -> 'a promise
  val resolve : 'a promise -> 'a -> unit
  val await : 'a promise -> timeout:float -> 'a option t
end

type origins = Origins of string list | Localhost

type observer = {
  log : source:string -> string -> unit;
  on_request :
    meth:string -> path:string -> status:int -> seconds:float -> unit;
  on_socket : [ `Opened | `Closed ] -> unit;
}

let stderr_observer =
  {
    log = (fun ~source line -> Printf.eprintf "%s: %s\n%!" source line);
    on_request = (fun ~meth:_ ~path:_ ~status:_ ~seconds:_ -> ());
    on_socket = ignore;
  }

type dev = { state_file : string }

type config = {
  dev : dev option;
  origins : origins;
  secret : string;
  secure_cookie : bool;
  sessions : Session.ops;
  now : unit -> float;
  max_body : int;
  max_message : int;
  heartbeat : float;
  live_keep : float;
  warn_outgoing : int;
  assets_prefix : string;
  observer : observer;
}

let default_config ~origins ~secret =
  {
    dev = None;
    origins;
    secret;
    secure_cookie = false;
    sessions = Session.ops (Session.create ());
    now = Unix.gettimeofday;
    max_body = 1_048_576;
    max_message = Protocol.max_message;
    heartbeat = 30.;
    live_keep = 1800.;
    warn_outgoing = 1_048_576;
    assets_prefix = "/static/";
    observer = stderr_observer;
  }

let dev_of_env () =
  match Sys.getenv_opt "SILICA_DEV_STATE" with
  | Some state_file when state_file <> "" ->
      Some { state_file }
  | Some _ | None ->
      None

let secret_bytes = 16

let secret_of_env ~origins ~dev =
  match (Sys.getenv_opt "SILICA_SECRET", origins, dev) with
  | Some secret, _, _ when String.length secret >= secret_bytes ->
      Ok secret
  | Some secret, _, _ ->
      Error
        (Printf.sprintf "SILICA_SECRET has %d bytes; it needs at least %d"
           (String.length secret) secret_bytes
        )
  | None, Origins _, _ ->
      Error
        "SILICA_SECRET is not set. A server with public origins needs a fixed \
         secret, or every restart and deploy ends the sessions. Make one with \
         `openssl rand -hex 32` and give it as a secret of the host, such as \
         `fly secrets set SILICA_SECRET=...`."
  | None, Localhost, Some _ ->
      Error
        "SILICA_SECRET is not set. Dev mode (SILICA_DEV_STATE) keeps the \
         sessions across restarts, so it needs a fixed secret; `silica dev` \
         gives one."
  | None, Localhost, None ->
      prerr_endline
        "silica: SILICA_SECRET is not set: a random secret is used, and the \
         sessions end when the server stops";
      Ok (Silica_server_kit.Random_source.hex_id ~bytes:32)

let cookie_name = "silica_sid"
let csrf_header = "x-silica-csrf"
let live_path = "/_silica/live"
let health_path = "/_silica/health"
let dev_client_path = "/_silica/dev/client.js"

let header name (headers : (string * string) list) =
  List.find_map
    (fun (key, value) ->
      if String.lowercase_ascii key = name then
        Some value
      else
        None
    )
    headers

let cookie_in_headers name headers =
  match header "cookie" headers with
  | None ->
      None
  | Some line ->
      List.find_map
        (fun part ->
          match String.index_opt part '=' with
          | Some i when String.trim (String.sub part 0 i) = name ->
              Some
                (String.trim
                   (String.sub part (i + 1) (String.length part - i - 1))
                )
          | _ ->
              None
        )
        (String.split_on_char ';' line)

let cookie (request : request) name = cookie_in_headers name request.headers
let host_name host =
  if String.length host > 0 && host.[0] = '[' then
    match String.index_opt host ']' with
    | Some i ->
        String.sub host 0 (i + 1)
    | None ->
        host
  else
    match String.index_opt host ':' with
    | Some i ->
        String.sub host 0 i
    | None ->
        host

let origin_allowed origins headers origin =
  match origins with
  | Origins allowed ->
      List.mem origin allowed
  | Localhost -> (
      match header "host" headers with
      | None ->
          false
      | Some host ->
          List.mem (host_name host) [ "localhost"; "127.0.0.1"; "[::1]" ]
          && (origin = "http://" ^ host || origin = "https://" ^ host)
    )

let fetch_site_ok headers =
  match header "sec-fetch-site" headers with
  | None | Some ("same-origin" | "none") ->
      true
  | Some _ ->
      false

let accepts_encoding headers coding =
  let weights =
    match header "accept-encoding" headers with
    | None ->
        []
    | Some line ->
        List.map
          (fun item ->
            match String.split_on_char ';' item with
            | [] ->
                ("", 0.)
            | name :: params ->
                let q =
                  List.fold_left
                    (fun q param ->
                      match String.split_on_char '=' (String.trim param) with
                      | [ key; value ] when String.lowercase_ascii key = "q" ->
                          Option.value
                            (float_of_string_opt (String.trim value))
                            ~default:0.
                      | _ ->
                          q
                    )
                    1. params
                in
                (String.lowercase_ascii (String.trim name), q)
          )
          (String.split_on_char ',' line)
  in
  match List.assoc_opt coding weights with
  | Some q ->
      q > 0.
  | None -> (
      match List.assoc_opt "*" weights with Some q -> q > 0. | None -> false
    )

let escape s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' ->
          Buffer.add_string b "&amp;"
      | '<' ->
          Buffer.add_string b "&lt;"
      | '>' ->
          Buffer.add_string b "&gt;"
      | '"' ->
          Buffer.add_string b "&quot;"
      | c ->
          Buffer.add_char b c
      )
    s;
  Buffer.contents b

let csrf_meta = function
  | None ->
      ""
  | Some token ->
      Printf.sprintf "<meta name=\"silica-csrf\" content=\"%s\">" (escape token)

let document ?(answers = "") ?(attributes = []) ?csrf ?(preload = []) ~status
    ~title ~scripts ~head body =
  let b = Buffer.create 16384 in
  let add = Buffer.add_string b in
  add "<!doctype html><html";
  List.iter
    (fun (name, value) ->
      Buffer.add_char b ' ';
      add name;
      add "=\"";
      add (escape value);
      Buffer.add_char b '"'
    )
    attributes;
  add "><head><meta charset=\"utf-8\">";
  add "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">";
  add (csrf_meta csrf);
  add "<title>";
  add (escape title);
  add "</title>";
  List.iter
    (fun s ->
      add "<link rel=\"modulepreload\" href=\"";
      add (escape s);
      add "\">"
    )
    (scripts @ List.filter (fun url -> not (List.mem url scripts)) preload);
  add head;
  add "</head><body><div id=\"silica-root\">";
  body b;
  add "</div>";
  add answers;
  List.iter
    (fun s ->
      add "<script type=\"module\" src=\"";
      add (escape s);
      add "\"></script>"
    )
    scripts;
  add "</body></html>";
  {
    status;
    headers = [ ("content-type", "text/html; charset=utf-8") ];
    body = Buffer.contents b;
  }

let default_error_page ~dev (error : Node.error) =
  let detail =
    match (dev, error.detail) with
    | true, Some detail ->
        "<pre>" ^ escape detail ^ "</pre>"
    | _ ->
        ""
  in
  {
    status = 500;
    headers = [ ("content-type", "text/html; charset=utf-8") ];
    body =
      "<!doctype html><html><head><meta charset=\"utf-8\"><title>Something \
       went wrong</title></head><body><h1>Something went wrong</h1><p>Error \
       id: " ^ escape error.id ^ "</p>" ^ detail ^ "</body></html>";
  }

let default_settle_timeout_ms = 1000

exception Render_failed of Node.error

let update tree message =
  try Instance.update tree message
  with exn -> raise (Render_failed (Instance.report tree exn))

let text status body =
  { status; headers = [ ("content-type", "text/plain; charset=utf-8") ]; body }

let with_header name value (res : response) =
  { res with headers = (name, value) :: List.remove_assoc name res.headers }

let last_index_of ~part text =
  let rec search index =
    if index < 0 then
      None
    else if String.sub text index (String.length part) = part then
      Some index
    else
      search (index - 1)
  in
  search (String.length text - String.length part)

let with_dev_client (res : response) =
  match header "content-type" res.headers with
  | Some content_type when String.starts_with ~prefix:"text/html" content_type
    ->
      let script =
        Printf.sprintf "<script type=\"module\" src=\"%s\"></script>"
          dev_client_path
      in
      let body =
        match last_index_of ~part:"</body>" res.body with
        | Some index ->
            String.sub res.body 0 index
            ^ script
            ^ String.sub res.body index (String.length res.body - index)
        | None ->
            res.body ^ script
      in
      { res with body }
  | _ ->
      res

module Make (Io : IO) = struct
  let ( let* ) = Io.bind

  type context = { request : request; session : Session.session }
  type island = Island : ('s, 'e) Live.t -> island

  let island_html islands channel =
    List.find_map
      (fun (Island l) ->
        if l.Live.channel = channel then
          Some
            (Render.to_string
               (l.component.view () (Machine.init l.component.machine))
            )
        else
          None
      )
      islands

  type outcome = {
    title : string;
    status : int option;
    redirect : Url.t option;
    attributes : (string * string option) list;
  }

  let outcome ~title =
    { title; status = None; redirect = None; attributes = [] }

  let apply outcome actions =
    List.fold_left
      (fun o -> function
        | Action.Run (Action.Set_title title) ->
            { o with title }
        | Action.Run (Action.Set_status status) ->
            { o with status = Some status }
        | Action.Run (Action.Set_document_attribute { name; value } as action)
          ->
            Action.check action;
            {
              o with
              attributes = (name, value) :: List.remove_assoc name o.attributes;
            }
        | Action.Run (Action.Navigate url | Action.Replace url)
          when o.redirect = None ->
            { o with redirect = Some url }
        | _ ->
            o
        )
      outcome actions

  let respond ?answers ?csrf ?preload ~status ~scripts ~head outcome body =
    match outcome.redirect with
    | Some url ->
        {
          status = 303;
          headers = [ ("location", Url.to_string url) ];
          body = "";
        }
    | None ->
        let attributes =
          List.rev
            (List.filter_map
               (fun (name, value) -> Option.map (fun v -> (name, v)) value)
               outcome.attributes
            )
        in
        document ?answers ~attributes ?csrf ?preload
          ~status:(Option.value outcome.status ~default:status)
          ~title:outcome.title ~scripts ~head body

  let rendered ?dev ?on_error ?(islands = []) (c : (_, _, _) Component.t) ~title
      =
    let tree = Instance.mount ?dev ?on_error c in
    let node, actions = update tree None in
    let outcome = apply (outcome ~title) actions in
    ( Instance.snapshot tree,
      outcome,
      fun b -> Render.to_buffer ~island:(island_html islands) b node
    )

  let guard ?(dev = false) ?on_error ?error_page f =
    let page = Option.value error_page ~default:(default_error_page ~dev) in
    try f () with
    | Render_failed error ->
        page error
    | exn ->
        page (Instance.failure ~dev ?on_error exn)

  let render_page app ~status ~scripts ?preload ?(head = "") ?dev ?on_error
      ?error_page ?csrf ?islands url =
    guard ?dev ?on_error ?error_page (fun () ->
        let state, outcome, body =
          rendered ?dev ?on_error ?islands (app url) ~title:""
        in
        respond ?csrf ?preload ~status:(status state) ~scripts ~head outcome
          body
    )

  let render_component (c : (_, _, _) Component.t) ~title ~scripts ?preload
      ?(head = "") ?dev ?on_error ?error_page ?csrf ?islands () =
    guard ?dev ?on_error ?error_page (fun () ->
        let _, outcome, body = rendered ?dev ?on_error ?islands c ~title in
        respond ?csrf ?preload ~status:200 ~scripts ~head outcome body
    )

  type failure = Bad_input of string | Rejected of Json.t | Failed of string

  type handler = {
    path : string;
    call : context -> string -> (Json.t, failure) result Io.t;
  }

  let implement endpoint f =
    let call context body =
      match Codec.of_string (Remote.input endpoint) body with
      | Error e ->
          Io.return (Error (Bad_input e))
      | Ok i -> (
          match f context i with
          | exception exn ->
              Io.return (Error (Failed (Printexc.to_string exn)))
          | pending ->
              Io.bind pending (function
                | Ok o ->
                    Io.return (Ok ((Remote.output endpoint).encode o))
                | Error x ->
                    Io.return
                      (Error (Rejected ((Remote.rejected endpoint).encode x)))
                )
        )
    in
    { path = Remote.path endpoint; call }

  type slot = {
    id : string;
    result : (Json.t, failure) result option Atomic.t;
    finish :
      (Json.t, failure) result ->
      (Instance.message * (Answers.t -> Answers.t)) option;
  }

  let settle ~handlers ~timeout_ms ~context ?dev ?on_error ?islands ~title
      component =
    let tree = Instance.mount ?dev ?on_error component in
    let answers = ref Answers.empty in
    let result = ref (outcome ~title) in
    let pending = ref [] in
    let bell = ref (Io.promise ()) in
    let deadline = Io.now () +. (float_of_int timeout_ms /. 1000.) in
    let start : Instance.message Action.host -> unit = function
      | Action.Start_call { id; endpoint; input; on_result; _ } -> (
          let path = Remote.path endpoint in
          match List.find_opt (fun h -> h.path = path) handlers with
          | None ->
              ()
          | Some h ->
              let encoded = Codec.to_string (Remote.input endpoint) input in
              let result = Atomic.make None in
              let finish = function
                | Error _ ->
                    None
                | Ok json -> (
                    match (Remote.output endpoint).decode json with
                    | Ok value ->
                        Some
                          ( on_result (Remote.Ok value),
                            fun table ->
                              Answers.add table ~path ~input:encoded
                                ~output:(Json.to_string json)
                          )
                    | Error _ ->
                        None
                  )
              in
              pending := !pending @ [ { id; result; finish } ];
              Io.async (fun () ->
                  let* outcome = h.call context encoded in
                  Atomic.set result (Some outcome);
                  Io.resolve !bell ();
                  Io.return ()
              )
        )
      | Action.Stop id ->
          pending := List.filter (fun slot -> slot.id <> id) !pending
      | Action.Start_timer _
      | Action.Start_source _
      | Action.Start_live _
      | Action.Run _ ->
          ()
    in
    let take_ready () =
      match
        List.find_opt
          (fun slot -> Option.is_some (Atomic.get slot.result))
          !pending
      with
      | None ->
          None
      | Some slot ->
          pending := List.filter (fun other -> other != slot) !pending;
          Some (slot, Option.get (Atomic.get slot.result))
    in
    let rec go message =
      let node, actions = update tree message in
      result := apply !result actions;
      if !result.redirect <> None then
        Io.return node
      else (
        List.iter start actions;
        wait node
      )
    and wait node =
      bell := Io.promise ();
      match take_ready () with
      | Some (slot, outcome) -> (
          match slot.finish outcome with
          | Some (event, record) ->
              answers := record !answers;
              go (Some event)
          | None ->
              wait node
        )
      | None -> (
          let remaining = deadline -. Io.now () in
          if !pending = [] || remaining <= 0. then
            Io.return node
          else
            let* arrived = Io.await !bell ~timeout:remaining in
            match arrived with None -> Io.return node | Some () -> wait node
        )
    in
    let* node = go None in
    Io.return
      ( Instance.snapshot tree,
        !result,
        (fun b ->
          Render.to_buffer
            ~island:(island_html (Option.value islands ~default:[]))
            b node
        ),
        !answers
      )

  let settle_page ~handlers ~islands ~csrf ~nonce ~context app ~status ~scripts
      ?preload ?(head = "") ?dev ?on_error ?error_page
      ?(settle_timeout_ms = default_settle_timeout_ms) url =
    let islands = Some islands and csrf = Some csrf and nonce = Some nonce in
    let page =
      Option.value error_page
        ~default:(default_error_page ~dev:(Option.value dev ~default:false))
    in
    match
      settle ~handlers ~timeout_ms:settle_timeout_ms ~context ?dev ?on_error
        ?islands ~title:"" (app url)
    with
    | exception Render_failed error ->
        Io.return (page error)
    | exception exn ->
        Io.return (page (Instance.failure ?dev ?on_error exn))
    | settled ->
        let* state, outcome, body, answers = settled in
        Io.return
          ( try
              respond
                ~answers:
                  ( if Answers.is_empty answers then
                      ""
                    else
                      Answers.to_script ?nonce answers
                  )
                ?csrf ?preload ~status:(status state) ~scripts ~head outcome
                body
            with
          | Render_failed error ->
              page error
          | exn ->
              page (Instance.failure ?dev ?on_error exn)
          )

  type runner = {
    receive : string -> unit Io.t;
    event : int -> Json.t -> unit Io.t;
    resend : unit -> unit Io.t;
    set_send : (string -> unit Io.t) option -> unit;
    close : unit -> unit;
    save : unit -> ((string * Json.t) list, string) result;
  }

  let call_endpoint : type i o x.
      handler list ->
      context ->
      (i, o, x) Remote.endpoint ->
      i ->
      (o, x) Remote.answer Io.t =
   fun handlers context endpoint input ->
    match List.find_opt (fun h -> h.path = Remote.path endpoint) handlers with
    | None ->
        Io.return (Remote.Failed (Remote.Http 404))
    | Some h ->
        let* r =
          h.call context (Codec.to_string (Remote.input endpoint) input)
        in
        Io.return
          ( match r with
          | Ok json -> (
              match (Remote.output endpoint).decode json with
              | Ok o ->
                  Remote.Ok o
              | Error e ->
                  Remote.Failed (Remote.Decode e)
            )
          | Error (Rejected json) -> (
              match (Remote.rejected endpoint).decode json with
              | Ok x ->
                  Remote.Rejected x
              | Error e ->
                  Remote.Failed (Remote.Decode e)
            )
          | Error (Bad_input _) ->
              Remote.Failed (Remote.Http 400)
          | Error (Failed _) ->
              Remote.Failed (Remote.Http 500)
          )

  let start_runner ~log ?(on_close = ignore) ?observe ?resume
      (live : (_, _) Live.t) ~handlers ~context ~send =
    let machine = live.component.machine in
    let state = ref (Machine.init machine) in
    let entry = ref 0 in
    let observed ~from ~event change actions cause =
      match observe with
      | None ->
          ()
      | Some (observer : Instance.observer) ->
          observer.observe
            (Instance.Detached
               {
                 instance = Machine.id machine;
                 machine;
                 props = ();
                 entry = !entry;
                 from;
                 snapshot = !state;
                 change;
                 event;
                 actions;
                 cause;
               }
            )
    in
    let lock = Mutex.create () in
    let queue = Queue.create () in
    let running = ref false in
    let closed = ref false in
    let channel = live.channel in
    let owned : (string, int) Hashtbl.t = Hashtbl.create 8 in
    let next_token = ref 0 in
    let sender = ref (Some send) in
    let seq = ref 0 in
    let last_id = ref 0 in
    let fail e =
      log (Printf.sprintf "live %s: %s" channel (Printexc.to_string e));
      Mutex.lock lock;
      Queue.clear queue;
      running := false;
      let first = not !closed in
      closed := true;
      Mutex.unlock lock;
      if first then on_close ()
    in
    let rec drain () =
      Mutex.lock lock;
      match Queue.take_opt queue with
      | None ->
          running := false;
          Mutex.unlock lock;
          Io.return ()
      | Some job ->
          Mutex.unlock lock;
          let* () =
            if !closed then
              Io.return ()
            else
              try job ()
              with e ->
                fail e;
                Io.return ()
          in
          drain ()
    in
    let enqueue job =
      Mutex.lock lock;
      Queue.add job queue;
      if !running then (
        Mutex.unlock lock;
        Io.return ()
      ) else (
        running := true;
        Mutex.unlock lock;
        drain ()
      )
    in
    let send_state actions =
      match !sender with
      | None ->
          Io.return ()
      | Some send ->
          incr seq;
          send
            (Protocol.encode_state live.state ~seq:!seq ~ack:!last_id !state
               (List.filter_map Protocol.host_of_host_action actions)
            )
    in
    let rec publish actions =
      let* () = send_state actions in
      List.iter run actions;
      Io.return ()
    and step ?finished ~cause event () =
      let from = !state in
      let s, actions, how = Machine.step ?finished machine () !state event in
      state := s;
      let change =
        match how with
        | Machine.Unhandled ->
            Instance.Unhandled
        | Machine.Kept ->
            Instance.Kept
        | Machine.Entered ->
            incr entry;
            Instance.Entered
      in
      observed ~from:(Some from) ~event:(Some event) change actions cause;
      publish actions
    and run = function
      | Action.Start_timer { id; ms; event } ->
          let token = start id in
          Io.async (fun () ->
              let* () = Io.sleep (float_of_int ms /. 1000.) in
              enqueue
                (deliver id token
                   (step ~finished:id ~cause:(Instance.Timer (id, ms)) event)
                )
          )
      | Action.Start_call { id; endpoint; input; timeout_ms; on_result } ->
          let token = start id in
          Io.async (fun () ->
              let* result = call_endpoint handlers context endpoint input in
              enqueue
                (deliver id token
                   (step ~finished:id
                      ~cause:(Instance.Answer (id, endpoint, result))
                      (on_result result)
                   )
                )
          );
          Option.iter
            (fun ms ->
              Io.async (fun () ->
                  let* () = Io.sleep (float_of_int ms /. 1000.) in
                  enqueue
                    (let answer = Remote.Failed (Remote.Network "timeout") in
                     deliver id token
                       (step ~finished:id
                          ~cause:(Instance.Answer (id, endpoint, answer))
                          (on_result answer)
                       )
                    )
              )
            )
            timeout_ms
      | Action.Stop id ->
          Hashtbl.remove owned id
      | Action.Start_source _ | Action.Start_live _ | Action.Run _ ->
          ()
    and start id =
      incr next_token;
      Hashtbl.replace owned id !next_token;
      !next_token
    and deliver id token job () =
      if Hashtbl.find_opt owned id = Some token then (
        Hashtbl.remove owned id;
        job ()
      ) else
        Io.return ()
    in
    let* () =
      enqueue (fun () ->
          match resume with
          | Some (snapshot, ack, resumed_entry) ->
              state := snapshot;
              entry := resumed_entry;
              last_id := ack;
              let actions = Machine.resume machine () snapshot in
              observed ~from:None ~event:None Instance.Resumed actions
                Instance.Start;
              publish actions
          | None ->
              let s, actions = Machine.start machine () in
              state := s;
              observed ~from:None ~event:None Instance.Started actions
                Instance.Start;
              publish actions
      )
    in
    let event id json =
      if id <= !last_id then
        Io.return ()
      else
        match Protocol.decode_event_payload live.event json with
        | Error e ->
            log (Printf.sprintf "live %s: dropped event: %s" channel e);
            Io.return ()
        | Ok e ->
            last_id := id;
            enqueue (step ~cause:Instance.Message e)
    in
    Io.return
      {
        receive =
          (fun msg ->
            match Protocol.decode_client msg with
            | Ok (Protocol.Event { id; event = json }) ->
                event id json
            | Ok _ ->
                log
                  (Printf.sprintf "live %s: dropped message: not an event"
                     channel
                  );
                Io.return ()
            | Error e ->
                log (Printf.sprintf "live %s: dropped message: %s" channel e);
                Io.return ()
          );
        event;
        resend = (fun () -> enqueue (fun () -> publish []));
        set_send = (fun next -> sender := next);
        close =
          (fun () ->
            if not !closed then (
              closed := true;
              observed ~from:(Some !state) ~event:None Instance.Ended []
                Instance.End
            )
          );
        save =
          (fun () ->
            Result.map
              (fun (codec : _ Codec.t) ->
                [
                  ("machine", Json.declassify (`String (Machine.id machine)));
                  ("snapshot", codec.encode !state);
                  ("ack", Json.declassify (`Int !last_id));
                  ("entry", Json.declassify (`Int !entry));
                ]
              )
              (Machine.state_codec machine)
          );
      }

  let stderr_log line = Printf.eprintf "silica: %s\n%!" line

  let live ?on_close ?observe live ~handlers ~context ~send =
    start_runner ~log:stderr_log ?on_close ?observe live ~handlers ~context
      ~send

  let receive r msg = r.receive msg
  let resend r = r.resend ()
  let set_send r send = r.set_send send
  let close r = r.close ()

  type topic_def =
    | Topic_def : {
        topic : 's Topic.t;
        init : context -> key:string -> ('s, string) result Io.t;
      }
        -> topic_def

  type page = {
    context : context;
    url : Url.t;
    csrf : string;
    nonce : string;
    asset : string -> string;
    imports : string -> string list;
    handlers : handler list;
    islands : island list;
    dev : bool;
  }

  let render_page_settled app ~status ~scripts ?preload ?head ?dev ?on_error
      ?error_page ?settle_timeout_ms (page : page) =
    settle_page ~handlers:page.handlers ~islands:page.islands ~csrf:page.csrf
      ~nonce:page.nonce ~context:page.context app ~status ~scripts ?preload
      ?head
      ~dev:(Option.value dev ~default:page.dev)
      ?on_error ?error_page ?settle_timeout_ms page.url

  type socket = { send : string -> unit Io.t; close : int -> unit }
  type connection = { receive : string -> unit Io.t; closed : unit -> unit }
  type upgrade = Refused of response | Accepted of (socket -> connection)

  type instance = {
    runner : runner;
    mutable attached : int option;
    mutable socket : socket option;
    mutable detached_at : float option;
  }

  type topic_state = {
    mutable seq : int;
    mutable value : Json.t option;
    mutable subscribers : (int * (string -> unit Io.t)) list;
    mutable idle_since : float option;
  }

  type open_socket = { socket : socket; mutable last_traffic : float }

  type restored = {
    saved : Json.t;
    since : float;
    start :
      on_close:(unit -> unit) ->
      observe:Instance.observer option ->
      context ->
      send:(string -> unit Io.t) ->
      runner Io.t;
  }

  type t = {
    config : config;
    assets : Assets.t option;
    read : string -> string option Io.t;
    handlers : handler list;
    islands : island list;
    topics : topic_def list;
    page : page -> response Io.t;
    observe_live : (context -> channel:string -> Instance.observer) option;
    lock : Mutex.t;
    mutable next_connection : int;
    open_sockets : (int, open_socket) Hashtbl.t;
    instances : (string * string * string, instance) Hashtbl.t;
    topic_states : (string * string, topic_state) Hashtbl.t;
    restored : (string * string * string, restored) Hashtbl.t;
    kept : (string, unit -> string) Hashtbl.t;
    unclaimed : (string, string) Hashtbl.t;
    mutable stopped : bool;
  }

  let empty config ?assets ?observe_live ~read ~handlers ~islands ~topics ~page
      () =
    if String.length config.secret < 16 then
      invalid_arg "Silica_server.create: secret must have at least 16 bytes";
    {
      config;
      assets;
      read;
      handlers;
      islands;
      topics;
      page;
      observe_live;
      lock = Mutex.create ();
      next_connection = 0;
      open_sockets = Hashtbl.create 16;
      instances = Hashtbl.create 16;
      topic_states = Hashtbl.create 16;
      restored = Hashtbl.create 16;
      kept = Hashtbl.create 4;
      unclaimed = Hashtbl.create 4;
      stopped = false;
    }

  let locked t f = Mutex.protect t.lock f
  let log t ~source line = t.config.observer.log ~source line
  let dev_log t line = log t ~source:"silica.dev" line
  let ( let+ ) = Result.bind

  let string_field fields name =
    match Option.map Json.classify (List.assoc_opt name fields) with
    | Some (`String value) ->
        Ok value
    | _ ->
        Error ("no string field " ^ name)

  let count_field fields name =
    match Option.map Json.classify (List.assoc_opt name fields) with
    | Some (`Int value) when value >= 0 ->
        Ok value
    | _ ->
        Error ("no field " ^ name ^ " with a count")

  let restore_island t ~now json =
    let+ fields =
      match Json.classify json with
      | `Assoc fields ->
          Ok fields
      | _ ->
          Error "not an object"
    in
    let+ session = string_field fields "session" in
    let+ channel = string_field fields "channel" in
    let+ instance = string_field fields "instance" in
    let named reason =
      Printf.sprintf "island %S (instance %S): %s" channel instance reason
    in
    let+ (Island live) =
      Option.to_result
        ~none:(named "no island has this channel")
        (List.find_opt (fun (Island l) -> l.Live.channel = channel) t.islands)
    in
    let machine = live.component.machine in
    let+ () =
      match t.config.sessions.get ~now session with
      | Some _ ->
          Ok ()
      | None ->
          Error (named "its session is gone")
    in
    let+ () =
      match string_field fields "machine" with
      | Ok id when id = Machine.id machine ->
          Ok ()
      | Ok id ->
          Error
            (named
               (Printf.sprintf "it ran the machine %S, the channel now has %S"
                  id (Machine.id machine)
               )
            )
      | Error message ->
          Error (named message)
    in
    let+ (codec : _ Codec.t) =
      Result.map_error named (Machine.state_codec machine)
    in
    let+ snapshot =
      Result.map_error
        (fun message -> named ("its snapshot does not decode: " ^ message))
        (Codec.field "snapshot" codec json)
    in
    let+ ack = Result.map_error named (count_field fields "ack") in
    let+ entry = Result.map_error named (count_field fields "entry") in
    Ok
      ( (session, channel, instance),
        {
          saved = json;
          since = now;
          start =
            (fun ~on_close ~observe context ~send ->
              start_runner
                ~log:(log t ~source:"silica.live")
                ~on_close ?observe ~resume:(snapshot, ack, entry) live
                ~handlers:t.handlers ~context ~send
            );
        }
      )

  let read_private_file t path =
    match Unix.lstat path with
    | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
        None
    | exception Unix.Unix_error (error, _, _) ->
        dev_log t (path ^ ": not read: " ^ Unix.error_message error);
        None
    | stats when stats.st_kind <> Unix.S_REG ->
        dev_log t (path ^ ": ignored: not a regular file");
        None
    | stats when stats.st_uid <> Unix.getuid () || stats.st_perm land 0o077 <> 0
      ->
        dev_log t
          (Printf.sprintf
             "%s: ignored: it must belong to this user with mode 600, and has \
              mode %o"
             path stats.st_perm
          );
        None
    | _ -> (
        let text =
          try Some (In_channel.with_open_bin path In_channel.input_all)
          with Sys_error message ->
            dev_log t (path ^ ": not read: " ^ message);
            None
        in
        try
          Sys.remove path;
          text
        with Sys_error message ->
          dev_log t (path ^ ": not removed: " ^ message);
          text
      )

  let load_state t (dev : dev) =
    match read_private_file t dev.state_file with
    | None ->
        ()
    | Some text -> (
        let now = t.config.now () in
        match Json.of_string text with
        | Ok json
          when match Json.classify json with `Assoc _ -> true | _ -> false -> (
            let fields =
              match Json.classify json with `Assoc fields -> fields | _ -> []
            in
            let section name =
              Option.map Json.classify (List.assoc_opt name fields)
            in
            ( match List.assoc_opt "sessions" fields with
            | Some sessions -> (
                match t.config.sessions.load ~now (Json.to_string sessions) with
                | Ok _ ->
                    ()
                | Error message ->
                    dev_log t
                      (dev.state_file ^ ": sessions not restored: " ^ message)
              )
            | None ->
                dev_log t (dev.state_file ^ ": no sessions")
            );
            ( match section "islands" with
            | Some (`List islands) ->
                List.iter
                  (fun island ->
                    match restore_island t ~now island with
                    | Ok (key, restored) ->
                        Hashtbl.replace t.restored key restored
                    | Error message ->
                        dev_log t (message ^ "; it starts again")
                    | exception exn ->
                        dev_log t
                          ("an island is not restored: "
                         ^ Printexc.to_string exn ^ "; it starts again"
                          )
                  )
                  islands
            | _ ->
                dev_log t (dev.state_file ^ ": no islands")
            );
            match section "kept" with
            | Some (`Assoc kept) ->
                List.iter
                  (fun (name, value) ->
                    match Json.classify value with
                    | `String text ->
                        Hashtbl.replace t.unclaimed name text
                    | _ ->
                        dev_log t ("kept value " ^ name ^ " is not a string")
                  )
                  kept
            | _ ->
                dev_log t (dev.state_file ^ ": no kept values")
          )
        | Ok _ ->
            dev_log t (dev.state_file ^ ": ignored: not a JSON object")
        | Error message ->
            dev_log t (dev.state_file ^ ": ignored: not JSON: " ^ message)
      )

  let write_private_file path text =
    let directory = Filename.dirname path in
    if not (Sys.file_exists directory) then Unix.mkdir directory 0o700;
    let temporary = path ^ ".tmp" in
    (try Unix.unlink temporary with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
    let descriptor =
      Unix.openfile temporary
        [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC ]
        0o600
    in
    let channel = Unix.out_channel_of_descr descriptor in
    match Out_channel.output_string channel text with
    | () ->
        Out_channel.close channel;
        Unix.rename temporary path
    | exception exn ->
        Out_channel.close_noerr channel;
        raise exn

  let save_state t (dev : dev) =
    let islands, restored, kept, unclaimed =
      locked t (fun () ->
          ( Hashtbl.fold (fun key inst acc -> (key, inst) :: acc) t.instances [],
            Hashtbl.fold (fun _ r acc -> r.saved :: acc) t.restored [],
            Hashtbl.fold (fun name save acc -> (name, save) :: acc) t.kept [],
            Hashtbl.fold
              (fun name text acc -> (name, text) :: acc)
              t.unclaimed []
          )
      )
    in
    let string value = Json.declassify (`String value) in
    let saved_islands =
      List.filter_map
        (fun ((session, channel, instance), (inst : instance)) ->
          let not_saved reason =
            dev_log t
              (Printf.sprintf
                 "island %S (instance %S) not saved, it starts again: %s"
                 channel instance reason
              );
            None
          in
          match inst.runner.save () with
          | Ok fields ->
              Some
                (Json.declassify
                   (`Assoc
                      (("session", string session)
                      :: ("channel", string channel)
                      :: ("instance", string instance)
                      :: fields
                      )
                   )
                )
          | Error reason ->
              not_saved reason
          | exception exn ->
              not_saved (Printexc.to_string exn)
        )
        islands
    in
    let kept =
      List.filter_map
        (fun (name, save) ->
          match save () with
          | text ->
              Some (name, string text)
          | exception exn ->
              dev_log t
                ("kept value " ^ name ^ " not saved: " ^ Printexc.to_string exn);
              None
        )
        kept
      @ List.map (fun (name, text) -> (name, string text)) unclaimed
    in
    let sessions =
      match Json.of_string (t.config.sessions.save ()) with
      | Ok sessions ->
          sessions
      | Error message ->
          dev_log t ("sessions not saved: " ^ message);
          Json.declassify (`List [])
    in
    let document =
      Json.declassify
        (`Assoc
           [
             ("sessions", sessions);
             ("islands", Json.declassify (`List (saved_islands @ restored)));
             ("kept", Json.declassify (`Assoc kept));
           ]
        )
    in
    match write_private_file dev.state_file (Json.to_string document) with
    | () ->
        ()
    | exception Unix.Unix_error (error, call, _) ->
        dev_log t
          (Printf.sprintf "%s: not written: %s: %s" dev.state_file call
             (Unix.error_message error)
          )
    | exception Sys_error message ->
        dev_log t (dev.state_file ^ ": not written: " ^ message)

  let keep t ~name ~save ~load =
    match t.config.dev with
    | None ->
        ()
    | Some _ -> (
        let text =
          locked t (fun () ->
              Hashtbl.replace t.kept name save;
              let text = Hashtbl.find_opt t.unclaimed name in
              Hashtbl.remove t.unclaimed name;
              text
          )
        in
        let not_loaded reason =
          dev_log t ("kept value " ^ name ^ " not loaded: " ^ reason)
        in
        match Option.map load text with
        | None | Some (Ok ()) ->
            ()
        | Some (Error message) ->
            not_loaded message
        | exception exn ->
            not_loaded (Printexc.to_string exn)
      )

  let shutdown t =
    let first =
      locked t (fun () ->
          let first = not t.stopped in
          t.stopped <- true;
          first
      )
    in
    if first then (
      Option.iter (save_state t) t.config.dev;
      let ended =
        locked t (fun () ->
            let ended =
              Hashtbl.fold
                (fun _ (inst : instance) acc -> inst :: acc)
                t.instances []
            in
            Hashtbl.reset t.instances;
            Hashtbl.reset t.restored;
            ended
        )
      in
      List.iter (fun (inst : instance) -> inst.runner.close ()) ended
    )

  let create config ?assets ?observe_live ~read ~handlers ~islands ~topics ~page
      () =
    let t =
      empty config ?assets ?observe_live ~read ~handlers ~islands ~topics ~page
        ()
    in
    Option.iter (load_state t) config.dev;
    t

  let warning_send t (send : string -> unit Io.t) message =
    if String.length message > t.config.warn_outgoing then
      log t ~source:"silica.live"
        (Printf.sprintf "outgoing message of %d bytes is above %d"
           (String.length message) t.config.warn_outgoing
        );
    send message

  let find_session t (headers : (string * string) list) =
    match cookie_in_headers cookie_name headers with
    | None ->
        None
    | Some id ->
        let now = t.config.now () in
        t.config.sessions.update ~now id (fun current ->
            { current with last_seen = now }
        )

  let new_session t =
    let now = t.config.now () in
    let session =
      {
        Session.id = Kit.Random_source.hex_id ~bytes:16;
        created = now;
        last_seen = now;
        data = [];
      }
    in
    t.config.sessions.set session;
    session

  let set_cookie t id =
    Printf.sprintf "%s=%s; Path=/; HttpOnly; SameSite=Lax%s" cookie_name id
      ( if t.config.secure_cookie then
          "; Secure"
        else
          ""
      )

  let safe_host host =
    host <> ""
    && String.for_all
         (function
           | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | ':' | '[' | ']' | '-'
             ->
               true
           | _ ->
               false
           )
         host

  let security_headers (req : request) (res : response) =
    let res =
      res
      |> with_header "x-content-type-options" "nosniff"
      |> with_header "referrer-policy" "same-origin"
    in
    match
      (header "content-type" res.headers, header "x-silica-nonce" res.headers)
    with
    | Some ct, Some nonce
      when String.length ct >= 9 && String.sub ct 0 9 = "text/html" ->
        let sockets =
          match header "host" req.headers with
          | Some host when safe_host host ->
              Printf.sprintf " ws://%s wss://%s" host host
          | _ ->
              ""
        in
        let res =
          { res with headers = List.remove_assoc "x-silica-nonce" res.headers }
        in
        with_header "content-security-policy"
          (Printf.sprintf
             "default-src 'self'; script-src 'self' 'nonce-%s'; style-src \
              'self' 'unsafe-inline'; img-src 'self' data:; connect-src \
              'self'%s; object-src 'none'; base-uri 'none'; frame-ancestors \
              'none'"
             nonce sockets
          )
          res
    | _ ->
        { res with headers = List.remove_assoc "x-silica-nonce" res.headers }

  let starts_with ~prefix s =
    String.length s >= String.length prefix
    && String.sub s 0 (String.length prefix) = prefix

  let safe_asset_name name =
    name <> ""
    && name.[0] <> '/'
    && (not (String.contains name '\000'))
    && String.for_all (fun c -> Char.code c >= 32 && c <> '\\') name
    && not (List.mem ".." (String.split_on_char '/' name))

  let serve_asset t (req : request) =
    let prefix = t.config.assets_prefix in
    let name =
      String.sub req.path (String.length prefix)
        (String.length req.path - String.length prefix)
    in
    if not (safe_asset_name name) then
      Io.return (text 404 "not found")
    else
      let hashed = Assets.is_hashed name in
      let rec variant = function
        | [] ->
            let* content = t.read name in
            Io.return (Option.map (fun body -> (None, body)) content)
        | (coding, extension) :: rest -> (
            let* content = t.read (name ^ extension) in
            match content with
            | Some body ->
                Io.return (Some (Some coding, body))
            | None ->
                variant rest
          )
      in
      let* found =
        variant
          ( if hashed then
              List.filter
                (fun (coding, _) -> accepts_encoding req.headers coding)
                [ ("br", ".br"); ("gzip", ".gz") ]
            else
              []
          )
      in
      match found with
      | None ->
          Io.return (text 404 "not found")
      | Some (coding, body) ->
          let ctype = Assets.content_type name in
          if hashed then
            let encoding =
              Option.map (fun coding -> ("content-encoding", coding)) coding
            in
            Io.return
              {
                status = 200;
                headers =
                  [
                    ("content-type", ctype);
                    ("cache-control", "public, max-age=31536000, immutable");
                    ("vary", "accept-encoding");
                  ]
                  @ Option.to_list encoding;
                body;
              }
          else
            let etag = Assets.etag body in
            if header "if-none-match" req.headers = Some etag then
              Io.return
                {
                  status = 304;
                  headers = [ ("etag", etag); ("cache-control", "no-cache") ];
                  body = "";
                }
            else
              Io.return
                {
                  status = 200;
                  headers =
                    [
                      ("content-type", ctype);
                      ("cache-control", "no-cache");
                      ("etag", etag);
                    ];
                  body;
                }

  let asset_url t name =
    match Option.bind t.assets (fun assets -> Assets.resolve assets name) with
    | Some url ->
        url
    | None ->
        failwith ("Silica_server: no asset named " ^ name)

  let serve_page t (req : request) =
    let session, fresh =
      match find_session t req.headers with
      | Some session ->
          (session, false)
      | None ->
          (new_session t, true)
    in
    let nonce = Kit.Base64.encode_url (Kit.Random_source.bytes 16) in
    let csrf = Kit.Csrf.token ~secret:t.config.secret ~session:session.id in
    let url = { (Url.of_string req.path) with query = req.query } in
    let* res =
      t.page
        {
          context = { request = req; session };
          url;
          csrf;
          nonce;
          asset = asset_url t;
          handlers = t.handlers;
          islands = t.islands;
          dev = Option.is_some t.config.dev;
          imports =
            (fun public ->
              match t.assets with
              | Some assets ->
                  Assets.imports assets public
              | None ->
                  []
            );
        }
    in
    let res =
      (match t.config.dev with Some _ -> with_dev_client res | None -> res)
      |> with_header "cache-control" "private, no-store"
      |> with_header "x-silica-nonce" nonce
    in
    Io.return
      ( if fresh then
          {
            res with
            headers = ("set-cookie", set_cookie t session.id) :: res.headers;
          }
        else
          res
      )

  let run_handler t (h : handler) context body =
    let* result = h.call context body in
    Io.return
      ( match result with
      | Ok json ->
          {
            status = 200;
            headers = [ ("content-type", "application/json") ];
            body = Json.to_string json;
          }
      | Error (Bad_input e) ->
          log t ~source:"silica.remote"
            (Printf.sprintf "%s: bad input: %s" h.path e);
          text 400 "bad request"
      | Error (Rejected json) ->
          {
            status = 422;
            headers = [ ("content-type", "application/json") ];
            body = Json.to_string json;
          }
      | Error (Failed e) ->
          log t ~source:"silica.remote" (Printf.sprintf "%s: %s" h.path e);
          text 500 "internal error"
      )

  let is_json content_type =
    String.length content_type >= 16
    && String.lowercase_ascii (String.sub content_type 0 16)
       = "application/json"

  let serve_call t (req : request) =
    let forbidden = Io.return (text 403 "forbidden") in
    if String.length req.body > t.config.max_body then
      Io.return (text 413 "payload too large")
    else if not (fetch_site_ok req.headers) then
      forbidden
    else
      match header "origin" req.headers with
      | None ->
          forbidden
      | Some origin when not (origin_allowed t.config.origins req.headers origin)
        ->
          forbidden
      | Some _ -> (
          match find_session t req.headers with
          | None ->
              forbidden
          | Some session -> (
              match header "content-type" req.headers with
              | Some ct when is_json ct -> (
                  match header csrf_header req.headers with
                  | Some token
                    when Kit.Csrf.valid ~secret:t.config.secret
                           ~session:session.id token -> (
                      match
                        List.find_opt (fun h -> h.path = req.path) t.handlers
                      with
                      | None ->
                          Io.return (text 404 "not found")
                      | Some h ->
                          run_handler t h { request = req; session } req.body
                    )
                  | _ ->
                      forbidden
                )
              | _ ->
                  Io.return (text 415 "unsupported media type")
            )
        )

  let route t (req : request) =
    if req.meth = "GET" && req.path = health_path then
      Io.return (with_header "cache-control" "no-store" (text 200 "ok"))
    else if req.meth = "GET" then
      if
        t.config.assets_prefix <> ""
        && starts_with ~prefix:t.config.assets_prefix req.path
      then
        serve_asset t req
      else if List.exists (fun h -> h.path = req.path) t.handlers then
        Io.return (text 405 "method not allowed")
      else
        serve_page t req
    else if List.exists (fun h -> h.path = req.path) t.handlers then
      if req.meth = "POST" then
        serve_call t req
      else
        Io.return (text 405 "method not allowed")
    else
      Io.return (text 404 "not found")

  let handle t (req : request) =
    let started = t.config.now () in
    let* res = route t req in
    let res = security_headers req res in
    t.config.observer.on_request ~meth:req.meth ~path:req.path
      ~status:res.status
      ~seconds:(t.config.now () -. started);
    Io.return res

  let topic_entry t name key =
    match Hashtbl.find_opt t.topic_states (name, key) with
    | Some entry ->
        entry
    | None ->
        let entry =
          { seq = 0; value = None; subscribers = []; idle_since = None }
        in
        Hashtbl.replace t.topic_states (name, key) entry;
        entry

  let state_message ~seq json =
    Protocol.encode_server
      (Protocol.State { seq; ack = 0; state = json; actions = [] })

  let publish_json t ~name ~key json =
    let seq, subscribers =
      locked t (fun () ->
          let entry = topic_entry t name key in
          entry.seq <- entry.seq + 1;
          entry.value <- Some json;
          (entry.seq, entry.subscribers)
      )
    in
    let message = state_message ~seq json in
    List.fold_left
      (fun previous (_, send) ->
        let* () = previous in
        send message
      )
      (Io.return ()) subscribers

  let publish t topic ~key value =
    publish_json t ~name:(Topic.name topic) ~key
      ((Topic.state topic).Codec.encode value)

  let upgrade t (req : request) =
    let refuse status message = Io.return (Refused (text status message)) in
    if req.path <> live_path then
      refuse 404 "not found"
    else if req.meth <> "GET" then
      refuse 405 "method not allowed"
    else if not (fetch_site_ok req.headers) then
      refuse 403 "forbidden"
    else
      match header "origin" req.headers with
      | None ->
          refuse 403 "forbidden"
      | Some origin when not (origin_allowed t.config.origins req.headers origin)
        ->
          refuse 403 "forbidden"
      | Some _ -> (
          match find_session t req.headers with
          | None ->
              Io.return
                (Accepted
                   (fun socket ->
                     socket.close Protocol.close_session;
                     { receive = (fun _ -> Io.return ()); closed = ignore }
                   )
                )
          | Some session ->
              let connect socket =
                let context = { request = req; session } in
                let id =
                  locked t (fun () ->
                      t.next_connection <- t.next_connection + 1;
                      Hashtbl.replace t.open_sockets t.next_connection
                        { socket; last_traffic = t.config.now () };
                      t.next_connection
                  )
                in
                t.config.observer.on_socket `Opened;
                let attached = ref `Waiting in
                let close_with code = socket.close code in
                let send_server message =
                  socket.send (Protocol.encode_server message)
                in
                let fail_hello code =
                  close_with code;
                  Io.return ()
                in
                let hello_island channel instance_id =
                  match
                    List.find_opt
                      (fun (Island l) -> l.Live.channel = channel)
                      t.islands
                  with
                  | None ->
                      let* () =
                        send_server (Protocol.Error { code = "unknown_island" })
                      in
                      fail_hello Protocol.close_unknown
                  | Some (Island l) -> (
                      let key = (session.id, channel, instance_id) in
                      let replaced = ref None in
                      let restored = ref None in
                      let existing =
                        locked t (fun () ->
                            match Hashtbl.find_opt t.instances key with
                            | Some inst ->
                                replaced := inst.socket;
                                inst.attached <- Some id;
                                inst.socket <- Some socket;
                                inst.detached_at <- None;
                                inst.runner.set_send
                                  (Some (warning_send t socket.send));
                                Some inst
                            | None ->
                                restored := Hashtbl.find_opt t.restored key;
                                Hashtbl.remove t.restored key;
                                None
                        )
                      in
                      Option.iter
                        (fun (old : socket) -> old.close 1000)
                        !replaced;
                      match existing with
                      | Some inst ->
                          attached := `Island inst;
                          let* () =
                            send_server
                              (Protocol.Hello_ok { v = Protocol.version })
                          in
                          inst.runner.resend ()
                      | None ->
                          let* () =
                            send_server
                              (Protocol.Hello_ok { v = Protocol.version })
                          in
                          let drop_instance () =
                            let sock =
                              locked t (fun () ->
                                  let inst = Hashtbl.find_opt t.instances key in
                                  Hashtbl.remove t.instances key;
                                  Option.bind inst (fun (i : instance) ->
                                      i.socket
                                  )
                              )
                            in
                            Option.iter
                              (fun (s : socket) ->
                                s.close Protocol.close_server_error
                              )
                              sock
                          in
                          let observe =
                            Option.map
                              (fun observe -> observe context ~channel)
                              t.observe_live
                          in
                          let send = warning_send t socket.send in
                          let* runner =
                            match !restored with
                            | Some restored ->
                                restored.start ~on_close:drop_instance ~observe
                                  context ~send
                            | None ->
                                start_runner
                                  ~log:(log t ~source:"silica.live")
                                  ~on_close:drop_instance ?observe l
                                  ~handlers:t.handlers ~context ~send
                          in
                          let inst =
                            {
                              runner;
                              attached = Some id;
                              socket = Some socket;
                              detached_at = None;
                            }
                          in
                          locked t (fun () ->
                              Hashtbl.replace t.instances key inst
                          );
                          attached := `Island inst;
                          Io.return ()
                    )
                in
                let hello_topic name key =
                  match
                    List.find_opt
                      (fun (Topic_def d) -> Topic.name d.topic = name)
                      t.topics
                  with
                  | None ->
                      let* () =
                        send_server (Protocol.Error { code = "unknown_topic" })
                      in
                      fail_hello Protocol.close_unknown
                  | Some (Topic_def d) -> (
                      let needs_init =
                        locked t (fun () ->
                            let entry = topic_entry t name key in
                            entry.value = None
                        )
                      in
                      let* initial =
                        if needs_init then
                          let* r = d.init context ~key in
                          Io.return (Some r)
                        else
                          Io.return None
                      in
                      match initial with
                      | Some (Error message) ->
                          log t ~source:"silica.live"
                            (Printf.sprintf "topic %s: init failed: %s" name
                               message
                            );
                          let* () =
                            send_server (Protocol.Error { code = "unavailable" })
                          in
                          fail_hello Protocol.close_server_error
                      | _ ->
                          let () =
                            match initial with
                            | Some (Ok value) ->
                                locked t (fun () ->
                                    let entry = topic_entry t name key in
                                    if entry.value = None then (
                                      entry.seq <- entry.seq + 1;
                                      entry.value <-
                                        Some
                                          ((Topic.state d.topic).Codec.encode
                                             value
                                          )
                                    )
                                )
                            | _ ->
                                ()
                          in
                          let* () =
                            send_server
                              (Protocol.Hello_ok { v = Protocol.version })
                          in
                          let seq, value =
                            locked t (fun () ->
                                let entry = topic_entry t name key in
                                entry.subscribers <-
                                  (id, warning_send t socket.send)
                                  :: entry.subscribers;
                                entry.idle_since <- None;
                                (entry.seq, Option.get entry.value)
                            )
                          in
                          attached := `Topic (name, key);
                          warning_send t socket.send (state_message ~seq value)
                    )
                in
                let hello (v, csrf, sub) =
                  if v <> Protocol.version then
                    fail_hello Protocol.close_version
                  else if
                    not
                      (Kit.Csrf.valid ~secret:t.config.secret
                         ~session:session.id csrf
                      )
                  then
                    fail_hello Protocol.close_csrf
                  else
                    match sub with
                    | Protocol.Island { channel; instance } ->
                        hello_island channel instance
                    | Protocol.Topic { name; key } ->
                        hello_topic name key
                in
                let receive text =
                  locked t (fun () ->
                      Option.iter
                        (fun (s : open_socket) ->
                          s.last_traffic <- t.config.now ()
                        )
                        (Hashtbl.find_opt t.open_sockets id)
                  );
                  if String.length text > Protocol.hard_limit then (
                    close_with Protocol.close_too_big;
                    Io.return ()
                  ) else if String.length text > t.config.max_message then
                    send_server (Protocol.Error { code = "too_big" })
                  else
                    match (Protocol.decode_client text, !attached) with
                    | Error message, `Waiting ->
                        log t ~source:"silica.live" ("bad hello: " ^ message);
                        fail_hello Protocol.close_hello
                    | Error message, _ ->
                        log t ~source:"silica.live"
                          ("dropped message: " ^ message);
                        Io.return ()
                    | Ok Protocol.Ping, _ ->
                        send_server Protocol.Pong
                    | Ok (Protocol.Hello { v; csrf; sub }), `Waiting ->
                        hello (v, csrf, sub)
                    | Ok (Protocol.Hello _), _ ->
                        log t ~source:"silica.live" "dropped a second hello";
                        Io.return ()
                    | Ok (Protocol.Event _), `Waiting ->
                        fail_hello Protocol.close_hello
                    | Ok (Protocol.Event { id = event_id; event }), `Island inst
                      ->
                        inst.runner.event event_id event
                    | Ok (Protocol.Event _), `Topic _ ->
                        log t ~source:"silica.live"
                          "dropped an event on a topic socket";
                        Io.return ()
                in
                let closed () =
                  let now = t.config.now () in
                  locked t (fun () ->
                      Hashtbl.remove t.open_sockets id;
                      match !attached with
                      | `Island (inst : instance) ->
                          if inst.attached = Some id then (
                            inst.attached <- None;
                            inst.socket <- None;
                            inst.detached_at <- Some now;
                            inst.runner.set_send None
                          )
                      | `Topic (name, key) -> (
                          match Hashtbl.find_opt t.topic_states (name, key) with
                          | Some entry ->
                              entry.subscribers <-
                                List.filter
                                  (fun (other, _) -> other <> id)
                                  entry.subscribers;
                              if entry.subscribers = [] then
                                entry.idle_since <- Some now
                          | None ->
                              ()
                        )
                      | `Waiting ->
                          ()
                  );
                  t.config.observer.on_socket `Closed
                in
                { receive; closed }
              in
              Io.return (Accepted connect)
        )

  let sweep t =
    let now = t.config.now () in
    let idle, ended_islands =
      locked t (fun () ->
          let idle =
            Hashtbl.fold
              (fun _ (s : open_socket) acc ->
                if now -. s.last_traffic > 2. *. t.config.heartbeat then
                  s.socket :: acc
                else
                  acc
              )
              t.open_sockets []
          in
          let ended =
            Hashtbl.fold
              (fun key (inst : instance) acc ->
                match inst.detached_at with
                | Some at when now -. at > t.config.live_keep ->
                    (key, inst) :: acc
                | _ ->
                    acc
              )
              t.instances []
          in
          List.iter (fun (key, _) -> Hashtbl.remove t.instances key) ended;
          Hashtbl.filter_map_inplace
            (fun _ restored ->
              if now -. restored.since > t.config.live_keep then
                None
              else
                Some restored
            )
            t.restored;
          let stale_topics =
            Hashtbl.fold
              (fun key (entry : topic_state) acc ->
                match entry.idle_since with
                | Some at
                  when entry.subscribers = [] && now -. at > t.config.live_keep
                  ->
                    key :: acc
                | _ ->
                    acc
              )
              t.topic_states []
          in
          List.iter (Hashtbl.remove t.topic_states) stale_topics;
          (idle, ended)
      )
    in
    List.iter (fun (_, (inst : instance)) -> inst.runner.close ()) ended_islands;
    List.iter (fun (s : socket) -> s.close Protocol.close_idle) idle;
    ignore (t.config.sessions.expire ~now)

  let sockets t = locked t (fun () -> Hashtbl.length t.open_sockets)
  let islands t =
    locked t (fun () -> Hashtbl.length t.instances + Hashtbl.length t.restored)
end
