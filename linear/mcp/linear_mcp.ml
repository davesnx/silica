open Silica
open Linear_model
module Json = Silica_json.Json
module Memory = Silica_memory
module Server = Silica_adapter.Server

let origin = "http://mcp.local"

let metafile =
  {|{"outputs":{"dist/client_main.js":{"entryPoint":"out/client_main.mjs"},"dist/app.css":{"entryPoint":"app.css"}}}|}

let string value = Json.declassify (`String value)
let assoc fields = Json.declassify (`Assoc fields)

let object_schema properties =
  assoc
    [
      ("type", string "object");
      ("properties", assoc properties);
      ( "required",
        Json.declassify
          (`List (List.map (fun (name, _) -> string name) properties))
      );
      ("additionalProperties", Json.declassify (`Bool false));
    ]

let text_property description =
  assoc [ ("type", string "string"); ("description", string description) ]

let choice_property description labels =
  assoc
    [
      ("type", string "string");
      ("enum", Json.declassify (`List (List.map string labels)));
      ("description", string description);
    ]

let field name arguments =
  match Json.classify arguments with
  | `Assoc fields -> (
      match Option.map Json.classify (List.assoc_opt name fields) with
      | Some (`String text) ->
          Ok text
      | Some (`Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _) | None
        ->
          Error (Printf.sprintf "%s must be a string" name)
    )
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ ->
      Error "the arguments must be an object"

let labelled name of_label make arguments =
  Result.bind (field name arguments) (fun label ->
      match of_label label with
      | Some value ->
          Ok (make value)
      | None ->
          Error (Printf.sprintf "%S is not a %s" label name)
  )

let create_events : Linear_create.event Silica_mcp.event list =
  [
    Silica_mcp.event ~name:"new_issue.title"
      ~description:"Set the title of the new issue."
      ~schema:(object_schema [ ("title", text_property "The whole title.") ])
      (fun arguments ->
        Result.map
          (fun title -> Linear_create.Title title)
          (field "title" arguments)
      );
    Silica_mcp.event ~name:"new_issue.description"
      ~description:"Set the description of the new issue."
      ~schema:
        (object_schema
           [ ("description", text_property "The whole description.") ]
        )
      (fun arguments ->
        Result.map
          (fun description -> Linear_create.Description description)
          (field "description" arguments)
      );
    Silica_mcp.event ~name:"new_issue.status_picked"
      ~description:"Pick the status of the new issue."
      ~schema:
        (object_schema
           [
             ( "status",
               choice_property "The status."
                 (List.map Issue_status.label Issue_status.all)
             );
           ]
        )
      (labelled "status" Issue_status.of_label (fun status ->
           Linear_create.Status_picked status
       )
      );
    Silica_mcp.event ~name:"new_issue.priority_picked"
      ~description:"Pick the priority of the new issue."
      ~schema:
        (object_schema
           [
             ( "priority",
               choice_property "The priority."
                 (List.map Priority.label Priority.all)
             );
           ]
        )
      (labelled "priority" Priority.of_label (fun priority ->
           Linear_create.Priority_picked priority
       )
      );
    Silica_mcp.event ~name:"new_issue.submit"
      ~description:"Create the issue from the draft." ~schema:(object_schema [])
      (fun _ -> Ok Linear_create.Submit
    );
    Silica_mcp.event ~name:"new_issue.cancel"
      ~description:"Close the new issue dialog and drop the draft."
      ~schema:(object_schema []) (fun _ -> Ok Linear_create.Cancel
    );
  ]

let publications =
  let routes = Linear_routes.routes in
  [
    Silica_mcp.routes routes
      ~published:(function
        | Linear_routes.Issues _ | Linear_routes.Issue _ ->
            true
        | Linear_routes.Not_found _ ->
            false
        )
      ~patterns:
        (String.concat "\n"
           [
             Router.name routes (Linear_routes.Issues Linear_routes.list)
             ^ " - the list of issues; ?new=true opens the new issue dialog";
             Router.name routes (Linear_routes.Issue 1)
             ^ " - one issue, by number, such as /issue/1";
           ]
        );
    Silica_mcp.machine Linear_create.machine ~states:[ "editing"; "failed" ]
      create_events;
  ]

let after marker body =
  let length = String.length marker in
  let rec find index =
    if index + length > String.length body then
      failwith ("linear_mcp: no " ^ marker ^ " in the page")
    else if String.sub body index length = marker then
      index + length
    else
      find (index + 1)
  in
  let start = find 0 in
  String.sub body start (String.index_from body start '"' - start)

let () =
  let started = Unix.gettimeofday () in
  let config =
    {
      (Silica_server.default_config ~origins:(Silica_server.Origins [ origin ])
         ~secret:"0123456789abcdef0123456789abcdef"
      )
      with
      observer =
        {
          Silica_server.stderr_observer with
          log = (fun ~source line -> prerr_endline (source ^ ": " ^ line));
        };
    }
  in
  let assets =
    match Silica_server.Assets.of_metafile ~prefix:"/static/" metafile with
    | Ok assets ->
        assets
    | Error message ->
        failwith message
  in
  let server = Linear_server.create config ~assets ~read:(fun _ -> None) () in
  let page =
    Server.handle server
      {
        Silica_server.meth = "GET";
        path = "/";
        query = [];
        headers = [];
        body = "";
      }
  in
  let cookie =
    match List.assoc_opt "set-cookie" page.headers with
    | Some set ->
        String.sub set 0 (String.index set ';')
    | None ->
        failwith "linear_mcp: the server gave no session cookie"
  in
  let csrf = after "name=\"silica-csrf\" content=\"" page.body in
  let answer request =
    let response =
      Server.handle server
        {
          Silica_server.meth = "POST";
          path = Memory.path request;
          query = [];
          headers =
            [
              ("cookie", cookie);
              ("origin", origin);
              ("content-type", "application/json");
              (Silica_server.csrf_header, Memory.request_csrf request);
            ];
          body = Memory.body request;
        }
    in
    (response.status, response.body)
  in
  let ready = Unix.gettimeofday () in
  let app = Memory.mount_app ~csrf ~url:"/" Linear_app.app in
  let mounted = Unix.gettimeofday () in
  Printf.eprintf "linear_mcp: server and session %.1f ms, mount %.1f ms\n%!"
    ((ready -. started) *. 1000.)
    ((mounted -. ready) *. 1000.);
  Silica_mcp.serve ~name:"mini-linear" ~version:"0.0.0" ~answer publications app
