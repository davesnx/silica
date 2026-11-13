open Silica
module Json = Silica_json.Json
module Memory = Silica_memory

type 'e event = {
  name : string;
  description : string;
  schema : Json.t;
  decode : Json.t -> ('e, string) result;
}

let event ~name ~description ~schema decode =
  { name; description; schema; decode }

type publication =
  | Machine : {
      machine : ('p, 's, 'e) Machine.t;
      states : string list;
      events : 'e event list;
    }
      -> publication
  | Routes : {
      routes : 'r Router.routes;
      published : 'r -> bool;
      patterns : string;
    }
      -> publication

let machine machine ~states events = Machine { machine; states; events }

let routes routes ~published ~patterns = Routes { routes; published; patterns }

let protocol_version = "2025-11-25"

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

let offered machine states running =
  List.filter
    (fun found -> List.mem (Machine.name machine found.snapshot) states)
    (instances machine running)

let string value = Json.declassify (`String value)
let assoc fields = Json.declassify (`Assoc fields)

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

let with_instance paths schema =
  let property =
    assoc
      [
        ("type", string "string");
        ("enum", Json.declassify (`List (List.map string paths)));
        ("description", string "The instance path of the instance to send to.");
      ]
  in
  match Json.classify schema with
  | `Assoc fields ->
      let properties =
        match Option.map Json.classify (List.assoc_opt "properties" fields) with
        | Some (`Assoc properties) ->
            properties
        | Some (`Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _)
        | None ->
            []
      in
      let required =
        match Option.map Json.classify (List.assoc_opt "required" fields) with
        | Some (`List required) ->
            required
        | Some (`Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _)
        | None ->
            []
      in
      assoc
        (List.filter
           (fun (key, _) -> key <> "properties" && key <> "required")
           fields
        @ [
            ("properties", assoc (properties @ [ ("instance", property) ]));
            ( "required",
              Json.declassify (`List (required @ [ string "instance" ]))
            );
          ]
        )
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ ->
      schema

let tool ~name ~description ~schema =
  assoc
    [
      ("name", string name);
      ("description", string description);
      ("inputSchema", schema);
    ]

let no_input =
  assoc
    [
      ("type", string "object");
      ("additionalProperties", Json.declassify (`Bool false));
    ]

let read_tool =
  tool ~name:"read"
    ~description:
      "The current URL, the published instances with their state names, and \
       the text of the page."
    ~schema:no_input

let navigate_tool publications =
  let patterns =
    List.filter_map
      (function Routes { patterns; _ } -> Some patterns | Machine _ -> None)
      publications
  in
  tool ~name:"navigate"
    ~description:
      ("Open a URL of the app, as in the address bar. Published routes:\n"
      ^ String.concat "\n" patterns
      )
    ~schema:
      (assoc
         [
           ("type", string "object");
           ( "properties",
             assoc
               [
                 ( "url",
                   assoc
                     [
                       ("type", string "string");
                       ("description", string "A path with its query, such as /");
                     ]
                 );
               ]
           );
           ("required", Json.declassify (`List [ string "url" ]));
         ]
      )

let tools publications app =
  let running = Memory.running app in
  navigate_tool publications :: read_tool
  :: List.concat_map
       (function
         | Routes _ ->
             []
         | Machine { machine; states; events } -> (
             match offered machine states running with
             | [] ->
                 []
             | [ _ ] ->
                 List.map
                   (fun event ->
                     tool ~name:event.name ~description:event.description
                       ~schema:event.schema
                   )
                   events
             | several ->
                 let paths = List.map (fun found -> found.path) several in
                 List.map
                   (fun event ->
                     tool ~name:event.name ~description:event.description
                       ~schema:(with_instance paths event.schema)
                   )
                   events
           )
         )
       publications

let tool_names publications app =
  List.filter_map (string_member "name") (tools publications app)

let states publications app =
  let running = Memory.running app in
  List.concat_map
    (function
      | Routes _ ->
          []
      | Machine { machine; _ } ->
          List.map
            (fun found ->
              Printf.sprintf "%s at %s: %s" (Machine.id machine) found.path
                (Machine.name machine found.snapshot)
            )
            (instances machine running)
      )
    publications

let max_rounds = 50

let settle ~answer app =
  let rec loop round =
    Memory.advance app 0;
    match Memory.pending app with
    | [] ->
        ()
    | requests when round < max_rounds ->
        List.iter
          (fun request ->
            let status, body = answer request in
            Memory.reply ~status request body
          )
          requests;
        loop (round + 1)
    | requests ->
        failwith
          (Printf.sprintf "the app still makes calls after %d rounds: %s"
             max_rounds
             (String.concat ", " (List.map Memory.path requests))
          )
  in
  loop 0

let page app =
  let timers =
    match Memory.timers app with
    | [] ->
        ""
    | delays ->
        Printf.sprintf "\nTimers waiting (ms): %s"
          (String.concat ", " (List.map string_of_int delays))
  in
  Printf.sprintf "URL: %s%s\n\nPage:\n%s" (Memory.url app) timers
    (Memory.view_text app)

let lines values = String.concat "\n" (List.map (fun line -> "  " ^ line) values)

let read publications app =
  Ok
    (Printf.sprintf "Instances:\n%s\n%s"
       (lines (states publications app))
       (page app)
    )

let after_step ~answer publications app run =
  let before = states publications app in
  run ();
  settle ~answer app;
  Ok
    (Printf.sprintf "Before:\n%s\nAfter:\n%s\n%s" (lines before)
       (lines (states publications app))
       (page app)
    )

let navigate ~answer publications app arguments =
  match string_member "url" arguments with
  | None ->
      Error "navigate needs the argument url, a string."
  | Some url ->
      let parsed = Url.of_string url in
      let verdicts =
        List.filter_map
          (function
            | Machine _ ->
                None
            | Routes { routes; published; _ } -> (
                match Router.parse routes parsed with
                | route when published route ->
                    Some (Ok ())
                | route ->
                    Some
                      (Error
                         (Printf.sprintf
                            "The URL %s is the route %s, which is not \
                             published."
                            url (Router.name routes route)
                         )
                      )
                | exception Failure reason ->
                    Some (Error (Printf.sprintf "The URL %s: %s" url reason))
              )
            )
          publications
      in
      if List.mem (Ok ()) verdicts then
        after_step ~answer publications app (fun () -> Memory.open_url app url)
      else
        Error
          ( match verdicts with
          | Error reason :: _ ->
              reason
          | Ok () :: _ | [] ->
              "No routes are published."
          )

let send_event : type p s e.
    answer:_ ->
    publication list ->
    _ Memory.t ->
    (p, s, e) Machine.t ->
    string list ->
    e event ->
    Json.t ->
    (string, string) result =
 fun ~answer publications app machine states event arguments ->
  let running = Memory.running app in
  let chosen =
    match offered machine states running with
    | [] ->
        let now =
          match instances machine running with
          | [] ->
              Printf.sprintf "no instance of %s runs" (Machine.id machine)
          | found ->
              String.concat ", "
                (List.map
                   (fun found ->
                     Printf.sprintf "%s at %s is in the state %s"
                       (Machine.id machine) found.path
                       (Machine.name machine found.snapshot)
                   )
                   found
                )
        in
        Error
          (Printf.sprintf
             "The tool %s is not available now: %s, and the tool is published \
              only in the states %s."
             event.name now
             (String.concat ", " states)
          )
    | [ found ] ->
        Ok found
    | several -> (
        match string_member "instance" arguments with
        | None ->
            Error
              (Printf.sprintf
                 "Several instances of %s can take %s: give the argument \
                  instance, one of %s."
                 (Machine.id machine) event.name
                 (String.concat ", "
                    (List.map (fun found -> found.path) several)
                 )
              )
        | Some path -> (
            match List.find_opt (fun found -> found.path = path) several with
            | Some found ->
                Ok found
            | None ->
                Error
                  (Printf.sprintf "No instance at %s can take %s." path
                     event.name
                  )
          )
      )
  in
  Result.bind chosen (fun found ->
      match event.decode arguments with
      | Error reason ->
          Error
            (Printf.sprintf "The arguments of %s do not decode: %s" event.name
               reason
            )
      | Ok value when not (Machine.can machine found.props found.snapshot value)
        ->
          Error
            (Printf.sprintf "%s at %s in the state %s ignores this %s event."
               (Machine.id machine) found.path
               (Machine.name machine found.snapshot)
               event.name
            )
      | Ok value ->
          after_step ~answer publications app (fun () ->
              Memory.send app (found.message value)
          )
  )

type call = Unknown | Done of (string, string) result

let call ~answer publications app name arguments =
  match name with
  | "read" ->
      Done (read publications app)
  | "navigate" ->
      Done (navigate ~answer publications app arguments)
  | _ ->
      List.fold_left
        (fun result publication ->
          match (result, publication) with
          | Done _, _ | Unknown, Routes _ ->
              result
          | Unknown, Machine { machine; states; events } -> (
              match List.find_opt (fun event -> event.name = name) events with
              | Some event ->
                  Done
                    (send_event ~answer publications app machine states event
                       arguments
                    )
              | None ->
                  Unknown
            )
        )
        Unknown publications

let reply id result =
  assoc [ ("jsonrpc", string "2.0"); ("id", id); ("result", result) ]

let error id code message =
  assoc
    [
      ("jsonrpc", string "2.0");
      ("id", id);
      ( "error",
        assoc
          [ ("code", Json.declassify (`Int code)); ("message", string message) ]
      );
    ]

let tool_result text is_error =
  assoc
    [
      ( "content",
        Json.declassify
          (`List [ assoc [ ("type", string "text"); ("text", string text) ] ])
      );
      ("isError", Json.declassify (`Bool is_error));
    ]

let initialize_result ~name ~version =
  assoc
    [
      ("protocolVersion", string protocol_version);
      ( "capabilities",
        assoc
          [ ("tools", assoc [ ("listChanged", Json.declassify (`Bool true)) ]) ]
      );
      ( "serverInfo",
        assoc [ ("name", string name); ("version", string version) ]
      );
    ]

let handle ~name ~version ~answer publications app message =
  let id = member "id" message in
  let params = Option.value (member "params" message) ~default:(assoc []) in
  match (id, string_member "method" message) with
  | None, _ ->
      []
  | Some id, None ->
      [ error id (-32600) "The request has no method." ]
  | Some id, Some "initialize" ->
      [ reply id (initialize_result ~name ~version) ]
  | Some id, Some "ping" ->
      [ reply id (assoc []) ]
  | Some id, Some "tools/list" ->
      [
        reply id
          (assoc [ ("tools", Json.declassify (`List (tools publications app))) ]);
      ]
  | Some id, Some "tools/call" -> (
      match string_member "name" params with
      | None ->
          [ error id (-32602) "tools/call needs params.name." ]
      | Some tool ->
          let arguments =
            Option.value (member "arguments" params) ~default:(assoc [])
          in
          let before = tool_names publications app in
          let outcome =
            try call ~answer publications app tool arguments
            with exn ->
              Done (Error ("The app failed: " ^ Printexc.to_string exn))
          in
          let changed =
            if tool_names publications app <> before then
              [
                assoc
                  [
                    ("jsonrpc", string "2.0");
                    ("method", string "notifications/tools/list_changed");
                  ];
              ]
            else
              []
          in
          ( match outcome with
          | Unknown ->
              error id (-32602) ("Unknown tool: " ^ tool)
          | Done (Ok text) ->
              reply id (tool_result text false)
          | Done (Error text) ->
              reply id (tool_result text true)
          )
          :: changed
    )
  | Some id, Some other ->
      [ error id (-32601) ("Method not found: " ^ other) ]

let serve ?(input = stdin) ?(output = stdout) ~name ~version ~answer
    publications app =
  settle ~answer app;
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
            List.iter write
              (handle ~name ~version ~answer publications app message)
        );
        loop ()
  in
  loop ()
