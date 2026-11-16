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

type tool =
  | Read : tool
  | Navigate : routes * string Mcp.t -> tool
  | Event : ('p, 's, 'e) Machine.t * 'e Mcp.call Mcp.t -> tool

type t =
  | Server : {
      name : string;
      version : string;
      instructions : string option;
      answer : Memory.request -> int * string;
      tools : (string * Json.t * tool) list;
      published : int list;
      app : 'p Memory.t;
    }
      -> t

let read_tool =
  Mcp.make ~name:"read"
    ~description:
      "The current URL, the running instances of the published machines with \
       their states, the timers that wait (delays in ms), and the text of the \
       page."
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

let make ~name ~version ?instructions ~answer ?routes publications app =
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
  Server
    {
      name;
      version;
      instructions;
      answer;
      tools;
      published =
        List.map
          (fun (Mcp.Publication { machine; _ }) -> machine_uid machine)
          publications;
      app;
    }

type ('p, 's, 'e) found = {
  path : string;
  props : 'p;
  snapshot : 's;
  message : 'e -> Instance.message;
}

let instances : type p s e.
    (p, s, e) Machine.t -> Instance.running list -> (p, s, e) found list =
 fun machine running ->
  List.filter_map
    (fun (Instance.Running other) : (p, s, e) found option ->
      match
        Type.Id.provably_equal (Machine.type_id machine)
          (Machine.type_id other.machine)
      with
      | Some Type.Equal ->
          Some
            {
              path = other.path;
              props = other.props;
              snapshot = other.snapshot;
              message = other.message;
            }
      | None ->
          None
    )
    running

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

let summary ~published app =
  let state (Instance.Running { path; machine; snapshot; _ }) =
    if List.mem (machine_uid machine) published then
      Some
        (assoc
           [
             ("machine", text (Machine.id machine));
             ("instance", text path);
             ("state", text (Machine.name machine snapshot));
           ]
        )
    else
      None
  in
  [
    ("url", text (Memory.url app));
    ("instances", list (List.filter_map state (Memory.running app)));
  ]

let decode tool arguments =
  Result.map_error
    (fun reason ->
      Printf.sprintf "The arguments of %s do not decode: %s." (Mcp.name tool)
        reason
    )
    (Mcp.Input.decode (Mcp.input tool) arguments)

let read app arguments =
  let* () = decode read_tool arguments in
  Ok
    [
      ("timers", list (List.map int (Memory.timers app)));
      ("page", text (Memory.view_text app));
    ]

let navigate ~answer app (Routes { routes; published; _ }) tool arguments =
  let* url = decode tool arguments in
  let* route =
    Result.map_error
      (fun reason -> Printf.sprintf "The URL %s: %s" url reason)
      (Router.parse routes (Url.of_string url))
  in
  if published route then (
    Memory.open_url app url;
    let* () = settle ~answer app in
    Ok []
  ) else
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
    answer:(Memory.request -> int * string) ->
    _ Memory.t ->
    (p, s, e) Machine.t ->
    e Mcp.call Mcp.t ->
    Json.t ->
    ((string * Json.t) list, string) result =
 fun ~answer app machine tool arguments ->
  let* { Mcp.event; instance } = decode tool arguments in
  let* found =
    choose machine ~url:(Memory.url app) instance
      (instances machine (Memory.running app))
  in
  if Machine.can_with_props machine found.props found.snapshot event then (
    Memory.send app (found.message event);
    let* () = settle ~answer app in
    let state =
      List.find_map
        (fun after ->
          if after.path = found.path then
            Some (text (Machine.name machine after.snapshot))
          else
            None
        )
        (instances machine (Memory.running app))
    in
    Ok
      [
        ("instance", text found.path);
        ("state", Option.value state ~default:(Json.declassify `Null));
      ]
  ) else
    Error
      (Printf.sprintf "%s at %s is in the state %s, which ignores %s."
         (Machine.id machine) found.path
         (Machine.name machine found.snapshot)
         (Mcp.name tool)
      )

let call ~answer ~published app tool arguments =
  let outcome =
    try
      let* () = settle ~answer app in
      match tool with
      | Read ->
          read app arguments
      | Navigate (routes, tool) ->
          navigate ~answer app routes tool arguments
      | Event (machine, tool) ->
          send_event ~answer app machine tool arguments
    with exn -> Error ("The app failed: " ^ Printexc.to_string exn)
  in
  let block value =
    list [ assoc [ ("type", text "text"); ("text", text value) ] ]
  in
  match outcome with
  | Ok fields ->
      let structured = assoc (fields @ summary ~published app) in
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

let handle
    (Server { name; version; instructions; answer; tools; published; app })
    message =
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
                complete id (call ~answer ~published app tool arguments)
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
