open Silica
module Json = Silica_json.Json
module Memory = Silica_memory

type routes =
  | Routes : {
      routes : 'r Router.routes;
      published : 'r -> bool;
      patterns : string;
    }
      -> routes

let routes routes ~published ~patterns = Routes { routes; published; patterns }
let protocol_version = "2026-07-28"
let ttl_ms = 300_000
let max_rounds = 50
let text value = Json.declassify (`String value)
let assoc fields = Json.declassify (`Assoc fields)
let list values = Json.declassify (`List values)
let int value = Json.declassify (`Int value)
let boolean value = Json.declassify (`Bool value)
let ( let* ) = Result.bind

let member name json =
  match Json.classify json with
  | `Assoc fields ->
      List.assoc_opt name fields
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ ->
      None

let string_member name json =
  Option.bind (member name json) (fun value ->
      match Json.classify value with
      | `String text ->
          Some text
      | `Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _ ->
          None
  )

type instance =
  | Machine_instance : {
      machine : ('p, 's, 'e) Machine.t;
      path : string;
      props : 'p;
      snapshot : unit -> 's;
      send : 'e -> (bool, string) result;
    }
      -> instance

let instance machine ~path ~props ~snapshot ~send =
  Machine_instance { machine; path; props; snapshot; send }

type island = Island_view : ('s, 'e) Live.t * (unit -> 's) -> island

let island live snapshot = Island_view (live, snapshot)

type host = {
  settle : unit -> (unit, string) result;
  url : unit -> string;
  open_url : string -> (unit, string) result;
  instances : unit -> instance list;
  read : unit -> (string * Json.t) list;
}

let settle ~answer app =
  let rec loop round =
    Memory.advance app 0;
    match Memory.pending app with
    | [] ->
        Ok ()
    | requests when round < max_rounds ->
        List.iter
          (fun request ->
            let status, body = answer request in
            Memory.reply ~status request body
          )
          requests;
        loop (round + 1)
    | requests ->
        Error
          (Printf.sprintf "The app still makes calls after %d rounds: %s."
             max_rounds
             (String.concat ", " (List.map Memory.path requests))
          )
  in
  loop 0

let memory ~answer app =
  let settle () = settle ~answer app in
  {
    settle;
    url = (fun () -> Memory.url app);
    open_url =
      (fun url ->
        Memory.open_url app url;
        settle ()
      );
    instances =
      (fun () ->
        List.map
          (fun (Instance.Running running) ->
            Machine_instance
              {
                machine = running.machine;
                path = running.path;
                props = running.props;
                snapshot = (fun () -> running.snapshot);
                send =
                  (fun event ->
                    Memory.send app (running.message event);
                    Result.map (fun () -> true) (settle ())
                  );
              }
          )
          (Memory.running app)
      );
    read =
      (fun () ->
        [
          ("timers", list (List.map int (Memory.timers app)));
          ("page", text (Memory.view_text app));
        ]
      );
  }

let server ?(url = "/") ~app ~islands instances =
  let current = Atomic.make url in
  let island channel =
    List.find_map
      (fun (Island_view (live, snapshot)) ->
        if live.Live.channel = channel then
          Some (Node.map ignore (live.component.view () (snapshot ())))
        else
          None
      )
      islands
  in
  {
    settle = (fun () -> Ok ());
    url = (fun () -> Atomic.get current);
    open_url =
      (fun url ->
        Atomic.set current url;
        Ok ()
      );
    instances = (fun () -> instances);
    read =
      (fun () ->
        let tree = Instance.mount (app (Url.of_string (Atomic.get current))) in
        let view, _ = Instance.update tree None in
        ignore (Instance.unmount tree);
        [ ("page", text (View_text.to_string ~island view)) ]
      );
  }

type tool =
  | Read : tool
  | Navigate : routes * string Mcp.t -> tool
  | Event : ('p, 's, 'e) Machine.t * 'e Mcp.call Mcp.t -> tool

type t = {
  name : string;
  version : string;
  instructions : string option;
  tools : (string * Json.t * tool) list;
  published : int list;
  host : host;
}

let read_tool =
  Mcp.make ~name:"read"
    ~description:
      "The current URL, the running instances of the published machines with \
       their states, and the text of the page; an app that runs in memory also \
       gives the timers that wait (delays in ms)."
    ~annotations:(Mcp.annotations ~read_only:true ~open_world:false ())
    Mcp.Input.none

let url_input =
  Mcp.Input.(field "url" ~description:"A path with its query, such as /" string)

let navigate_tool (Routes { patterns; _ }) =
  Mcp.make ~name:"navigate"
    ~description:
      ("Open a URL of the app, as in the address bar. Published routes:\n"
     ^ patterns
      )
    ~annotations:(Mcp.annotations ~open_world:false ())
    url_input

let machine_uid machine = Type.Id.uid (Machine.type_id machine)

let make ~name ~version ?instructions ?routes publications host =
  let entry tool kind = (Mcp.name tool, Mcp.definition tool, kind) in
  let tools =
    ( match routes with
      | Some routes ->
          let tool = navigate_tool routes in
          [ entry tool (Navigate (routes, tool)) ]
      | None ->
          []
      )
    @ [ entry read_tool Read ]
    @ List.concat_map
        (fun (Mcp.Publication { machine; tools }) ->
          List.map (fun tool -> entry tool (Event (machine, tool))) tools
        )
        publications
  in
  let rec check seen = function
    | [] ->
        ()
    | (tool, _, _) :: rest ->
        if List.mem tool seen then
          invalid_arg ("Silica_mcp.make: two tools are named " ^ tool);
        check (tool :: seen) rest
  in
  check [] tools;
  {
    name;
    version;
    instructions;
    tools;
    published =
      List.map
        (fun (Mcp.Publication { machine; _ }) -> machine_uid machine)
        publications;
    host;
  }

type ('p, 's, 'e) found = {
  path : string;
  props : 'p;
  snapshot : 's;
  send : 'e -> (bool, string) result;
}

let instances : type p s e.
    (p, s, e) Machine.t -> instance list -> (p, s, e) found list =
 fun machine listed ->
  List.filter_map
    (fun (Machine_instance other) : (p, s, e) found option ->
      match
        Type.Id.provably_equal (Machine.type_id machine)
          (Machine.type_id other.machine)
      with
      | Some Type.Equal ->
          Some
            {
              path = other.path;
              props = other.props;
              snapshot = other.snapshot ();
              send = other.send;
            }
      | None ->
          None
    )
    listed

let summary ~published host =
  let state (Machine_instance { path; machine; snapshot; _ }) =
    if List.mem (machine_uid machine) published then
      Some
        (assoc
           [
             ("machine", text (Machine.id machine));
             ("instance", text path);
             ("state", text (Machine.name machine (snapshot ())));
           ]
        )
    else
      None
  in
  [
    ("url", text (host.url ()));
    ("instances", list (List.filter_map state (host.instances ())));
  ]

let decode tool arguments =
  Result.map_error
    (fun reason ->
      Printf.sprintf "The arguments of %s do not decode: %s." (Mcp.name tool)
        reason
    )
    (Mcp.Input.decode (Mcp.input tool) arguments)

let read host arguments =
  let* () = decode read_tool arguments in
  Ok (host.read ())

let navigate host (Routes { routes; published; _ }) tool arguments =
  let* url = decode tool arguments in
  let* route =
    Result.map_error
      (fun reason -> Printf.sprintf "The URL %s: %s" url reason)
      (Router.parse routes (Url.of_string url))
  in
  if published route then
    let* () = host.open_url url in
    Ok []
  else
    Error
      (Printf.sprintf "The URL %s is the route %s, which is not published." url
         (Router.name routes route)
      )

let choose machine ~url instance found =
  let id = Machine.id machine in
  let paths = String.concat ", " (List.map (fun found -> found.path) found) in
  match (instance, found) with
  | None, [ one ] ->
      Ok one
  | (None | Some _), [] ->
      Error (Printf.sprintf "No instance of %s runs at the URL %s." id url)
  | None, _ :: _ :: _ ->
      Error
        (Printf.sprintf
           "Several instances of %s run: %s. Give the argument instance, one \
            of them."
           id paths
        )
  | Some path, (_ :: _ as some) -> (
      match List.find_opt (fun found -> found.path = path) some with
      | Some found ->
          Ok found
      | None ->
          Error
            (Printf.sprintf
               "No instance of %s runs at %s. The running ones: %s." id path
               paths
            )
    )

let send_event : type p s e.
    host ->
    (p, s, e) Machine.t ->
    e Mcp.call Mcp.t ->
    Json.t ->
    ((string * Json.t) list, string) result =
 fun host machine tool arguments ->
  let* { Mcp.event; instance } = decode tool arguments in
  let* found =
    choose machine ~url:(host.url ()) instance
      (instances machine (host.instances ()))
  in
  let refused snapshot =
    Error
      (Printf.sprintf "%s at %s is in the state %s, which ignores %s."
         (Machine.id machine) found.path
         (Machine.name machine snapshot)
         (Mcp.name tool)
      )
  in
  if Machine.can_with_props machine found.props found.snapshot event then
    let* handled = found.send event in
    let after =
      List.find_opt
        (fun after -> after.path = found.path)
        (instances machine (host.instances ()))
    in
    match (handled, after) with
    | false, Some after ->
        refused after.snapshot
    | false, None | true, _ ->
        Ok
          [
            ("instance", text found.path);
            ( "state",
              match after with
              | Some after ->
                  text (Machine.name machine after.snapshot)
              | None ->
                  Json.declassify `Null
            );
          ]
  else
    refused found.snapshot

let call ~published host tool arguments =
  let outcome =
    try
      let* () = host.settle () in
      match tool with
      | Read ->
          read host arguments
      | Navigate (routes, tool) ->
          navigate host routes tool arguments
      | Event (machine, tool) ->
          send_event host machine tool arguments
    with exn -> Error ("The app failed: " ^ Printexc.to_string exn)
  in
  let block value =
    list [ assoc [ ("type", text "text"); ("text", text value) ] ]
  in
  match outcome with
  | Ok fields ->
      let structured = assoc (fields @ summary ~published host) in
      [
        ("content", block (Json.to_string structured));
        ("structuredContent", structured);
        ("isError", boolean false);
      ]
  | Error sentence ->
      [ ("content", block sentence); ("isError", boolean true) ]

let error ?data id code message =
  assoc
    [
      ("jsonrpc", text "2.0");
      ("id", id);
      ( "error",
        assoc
          ([ ("code", int code); ("message", text message) ]
          @ match data with Some data -> [ ("data", data) ] | None -> []
          )
      );
    ]

let check_meta id params =
  let meta = Option.value (member "_meta" params) ~default:(assoc []) in
  match string_member "io.modelcontextprotocol/protocolVersion" meta with
  | None ->
      Error
        (error id (-32602)
           "The request has no _meta io.modelcontextprotocol/protocolVersion."
        )
  | Some requested when requested <> protocol_version ->
      Error
        (error id (-32022) "Unsupported protocol version"
           ~data:
             (assoc
                [
                  ("supported", list [ text protocol_version ]);
                  ("requested", text requested);
                ]
             )
        )
  | Some _ -> (
      match member "io.modelcontextprotocol/clientCapabilities" meta with
      | Some _ ->
          Ok ()
      | None ->
          Error
            (error id (-32602)
               "The request has no _meta \
                io.modelcontextprotocol/clientCapabilities."
            )
    )

let handle { name; version; instructions; tools; published; host } message =
  let complete id fields =
    assoc
      [
        ("jsonrpc", text "2.0");
        ("id", id);
        ( "result",
          assoc
            ((("resultType", text "complete") :: fields)
            @ [
                ( "_meta",
                  assoc
                    [
                      ( "io.modelcontextprotocol/serverInfo",
                        assoc [ ("name", text name); ("version", text version) ]
                      );
                    ]
                );
              ]
            )
        );
      ]
  in
  let cache = [ ("ttlMs", int ttl_ms); ("cacheScope", text "public") ] in
  let params = Option.value (member "params" message) ~default:(assoc []) in
  let answer_request id = function
    | "server/discover" ->
        complete id
          ([
             ("supportedVersions", list [ text protocol_version ]);
             ("capabilities", assoc [ ("tools", assoc []) ]);
           ]
          @ Option.to_list
              (Option.map
                 (fun instructions -> ("instructions", text instructions))
                 instructions
              )
          @ cache
          )
    | "tools/list" -> (
        match member "cursor" params with
        | Some _ ->
            error id (-32602)
              "The tool list has one page, so there is no cursor to follow."
        | None ->
            complete id
              (("tools", list (List.map (fun (_, json, _) -> json) tools))
              :: cache
              )
      )
    | "tools/call" -> (
        let arguments =
          Option.value (member "arguments" params) ~default:(assoc [])
        in
        match (string_member "name" params, Json.classify arguments) with
        | None, _ ->
            error id (-32602) "tools/call needs params.name, a string."
        | Some _, (`Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _) ->
            error id (-32602) "The arguments of tools/call must be an object."
        | Some tool, `Assoc _ -> (
            match List.find_opt (fun (other, _, _) -> other = tool) tools with
            | Some (_, _, tool) ->
                complete id (call ~published host tool arguments)
            | None ->
                error id (-32602) ("Unknown tool: " ^ tool)
          )
      )
    | other ->
        error id (-32601)
          (Printf.sprintf
             "Method not found: %s. This server speaks MCP %s only; send \
              server/discover."
             other protocol_version
          )
  in
  Option.map
    (fun id ->
      match string_member "method" message with
      | None ->
          error id (-32600) "The request has no method."
      | Some (("server/discover" | "tools/list" | "tools/call") as meth) -> (
          match check_meta id params with
          | Ok () ->
              answer_request id meth
          | Error error ->
              error
        )
      | Some other ->
          answer_request id other
    )
    (member "id" message)

let serve ?(input = stdin) ?(output = stdout) server =
  let write json =
    output_string output (Json.to_string json);
    output_char output '\n';
    flush output
  in
  let rec loop () =
    match input_line input with
    | exception End_of_file ->
        ()
    | "" ->
        loop ()
    | line ->
        ( match Json.of_string line with
        | Error reason ->
            write
              (error (Json.declassify `Null) (-32700) ("Parse error: " ^ reason))
        | Ok message ->
            Option.iter write (handle server message)
        );
        loop ()
  in
  loop ()

let header_mismatch id message =
  Error (error id (-32020) ("Header mismatch: " ^ message))

let same_name ~header name =
  header = name
  || header = "=?base64?" ^ Silica_server_kit.Base64.encode name ^ "?="

let check_headers header message id meth =
  let params = Option.value (member "params" message) ~default:(assoc []) in
  let body_version =
    Option.bind (member "_meta" params)
      (string_member "io.modelcontextprotocol/protocolVersion")
  in
  match (header "mcp-protocol-version", header "mcp-method") with
  | None, _ ->
      header_mismatch id "the request has no MCP-Protocol-Version header."
  | Some version, _
    when Option.fold ~none:false
           ~some:(fun body -> body <> version)
           body_version ->
      header_mismatch id
        (Printf.sprintf
           "MCP-Protocol-Version header value '%s' does not match body value \
            '%s'."
           version
           (Option.value body_version ~default:"")
        )
  | Some _, None ->
      header_mismatch id "the request has no Mcp-Method header."
  | Some _, Some method_header when method_header <> meth ->
      header_mismatch id
        (Printf.sprintf
           "Mcp-Method header value '%s' does not match body value '%s'."
           method_header meth
        )
  | Some _, Some _ -> (
      match (meth, string_member "name" params) with
      | "tools/call", Some name -> (
          match header "mcp-name" with
          | None ->
              header_mismatch id "the request has no Mcp-Name header."
          | Some value when not (same_name ~header:value name) ->
              header_mismatch id
                (Printf.sprintf
                   "Mcp-Name header value '%s' does not match body value '%s'."
                   value name
                )
          | Some _ ->
              Ok ()
        )
      | _ ->
          Ok ()
    )

let status_of answer =
  match
    Option.bind (member "error" answer) (fun error ->
        Option.bind (member "code" error) (fun code ->
            match Json.classify code with
            | `Int code ->
                Some code
            | `Null | `Bool _ | `Float _ | `String _ | `List _ | `Assoc _ ->
                None
        )
    )
  with
  | Some -32601 ->
      404
  | Some (-32700 | -32600 | -32020 | -32022) ->
      400
  | Some _ | None ->
      200

let http server (request : Silica_server.request) : Silica_server.response =
  let header name =
    List.find_map
      (fun (key, value) ->
        if String.lowercase_ascii key = name then
          Some value
        else
          None
      )
      request.headers
  in
  let json status body : Silica_server.response =
    {
      status;
      headers = [ ("content-type", "application/json") ];
      body = Json.to_string body;
    }
  in
  let null = Json.declassify `Null in
  let is_json content_type =
    String.length content_type >= 16
    && String.lowercase_ascii (String.sub content_type 0 16)
       = "application/json"
  in
  if request.meth <> "POST" then
    {
      status = 405;
      headers =
        [ ("allow", "POST"); ("content-type", "text/plain; charset=utf-8") ];
      body = "The MCP endpoint takes one JSON-RPC message by POST.";
    }
  else
    match header "content-type" with
    | Some content_type when is_json content_type -> (
        match Json.of_string request.body with
        | Error reason ->
            json 400 (error null (-32700) ("Parse error: " ^ reason))
        | Ok message -> (
            let id = Option.value (member "id" message) ~default:null in
            match string_member "method" message with
            | None ->
                json 400
                  (error id (-32600)
                     "The body must be one JSON-RPC request or notification, \
                      an object with a method."
                  )
            | Some meth -> (
                let checked =
                  match member "id" message with
                  | Some id ->
                      check_headers header message id meth
                  | None ->
                      Ok ()
                in
                match checked with
                | Error answer ->
                    json 400 answer
                | Ok () -> (
                    match handle server message with
                    | None ->
                        { status = 202; headers = []; body = "" }
                    | Some answer ->
                        json (status_of answer) answer
                  )
              )
          )
      )
    | Some _ | None ->
        {
          status = 415;
          headers = [ ("content-type", "text/plain; charset=utf-8") ];
          body = "The MCP endpoint takes application/json.";
        }
