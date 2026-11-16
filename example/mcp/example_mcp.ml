open Silica
module Memory = Silica_memory
module Server = Silica_adapter.Server

let origin = "http://mcp.local"

let metafile =
  {|{"outputs":{"dist/client_main.js":{"entryPoint":"out/client_main.mjs"}}}|}

let routes =
  let routes = Example_routes.routes in
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
      )

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
  let script =
    match Silica_server.Assets.resolve assets "client_main.js" with
    | Some script ->
        script
    | None ->
        failwith "example_mcp: no client_main.js in the metafile"
  in
  let server =
    Example_server.create config ~assets ~read:(fun _ -> None) ~script
  in
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
  Silica_mcp.serve
    (Silica_mcp.make ~name:"silica-example" ~version:"0.0.0"
       ~instructions:
         "An example app. navigate opens a page and read shows it. Each tool \
          of a machine sends one event to a running instance; a state that \
          ignores the event refuses the call."
       ~answer ~routes Example_tools.publications app
    )
