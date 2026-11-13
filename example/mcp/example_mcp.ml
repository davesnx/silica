open Silica
module Json = Silica_json.Json
module Memory = Silica_memory
module Server = Silica_adapter.Server

let origin = "http://mcp.local"

let metafile =
  {|{"outputs":{"dist/client_main.js":{"entryPoint":"out/client_main.mjs"}}}|}

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

let no_arguments name description event =
  Silica_mcp.event ~name ~description ~schema:(object_schema []) (fun _ ->
      Ok event
  )

let edit_events : Example_edit.event Silica_mcp.event list =
  [
    Silica_mcp.event ~name:"edit.draft" ~description:"Set the whole draft."
      ~schema:(object_schema [ ("text", text_property "The whole draft.") ])
      (fun arguments ->
        Result.map (fun text -> Example_edit.Draft text) (field "text" arguments)
      );
    no_arguments "edit.save" "Save the draft." Example_edit.Save;
    no_arguments "edit.leave" "Leave the page and drop the draft."
      Example_edit.Leave;
    no_arguments "edit.stay" "Stay on the page and keep the draft."
      Example_edit.Stay;
  ]

let vote_events =
  [ no_arguments "vote.cast" "Cast one vote." (Remote_machine.Load ()) ]

let publications =
  let routes = Example_routes.routes in
  [
    Silica_mcp.routes routes
      ~published:(function
        | Example_routes.Home
        | Example_routes.Item _
        | Example_routes.About
        | Example_routes.Edit ->
            true
        | Example_routes.Locked | Example_routes.Not_found _ ->
            false
        )
      ~patterns:
        (String.concat "\n"
           [
             Router.name routes Example_routes.Home
             ^ " - the home page, with the vote button";
             Router.name routes (Example_routes.Item 1)
             ^ " - one item, by number, such as /item/1";
             Router.name routes Example_routes.About ^ " - the about page";
             Router.name routes Example_routes.Edit
             ^ " - a draft that asks before it is dropped";
           ]
        );
    Silica_mcp.machine Example_vote_button.machine ~states:[ "idle" ]
      vote_events;
    Silica_mcp.machine Example_edit.machine
      ~states:[ "clean"; "dirty"; "asking" ]
      edit_events;
    Silica_mcp.machine Example_item.machine
      ~states:[ "loading"; "loaded"; "missing"; "failed" ]
      [];
  ]

let after marker body =
  let length = String.length marker in
  let rec find index =
    if index + length > String.length body then
      failwith ("example_mcp: no " ^ marker ^ " in the page")
    else if String.sub body index length = marker then
      index + length
    else
      find (index + 1)
  in
  let start = find 0 in
  String.sub body start (String.index_from body start '"' - start)

let () =
  Example_codecs.register ();
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
  let server = Example_server.create config ~assets ~read:(fun _ -> None) in
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
        failwith "example_mcp: the server gave no session cookie"
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
  let app = Memory.mount_app ~csrf ~url:"/" Example_app.app in
  let mounted = Unix.gettimeofday () in
  Printf.eprintf "example_mcp: server and session %.1f ms, mount %.1f ms\n%!"
    ((ready -. started) *. 1000.)
    ((mounted -. ready) *. 1000.);
  Silica_mcp.serve ~name:"silica-example" ~version:"0.0.0" ~answer publications
    app
