module Adapter = Silica_adapter
module Server = Adapter.Server

let metafile =
  {|{"outputs":{"dist/client_main-AAAAAAAA.js":{"entryPoint":"out/client_main.mjs"},"dist/app-AAAAAAAA.css":{"entryPoint":"app.css"}}}|}

let assets =
  match Silica_server.Assets.of_metafile ~prefix:"/static/" metafile with
  | Ok assets ->
      assets
  | Error message ->
      failwith message

let config () =
  let config =
    Silica_server.default_config ~origins:Silica_server.Localhost
      ~secret:"bench-secret-0123456789abcdef"
  in
  {
    config with
    observer = { config.observer with log = (fun ~source:_ _ -> ()) };
  }

let longest_doc () =
  let size (p : Site_doc.page) =
    String.length
      (Silica.Codec.to_string
         (Silica.Remote.output Site_doc.endpoint)
         (Site_doc.Page p)
      )
  in
  List.fold_left
    (fun (best : Site_doc.page) p ->
      if size p > size best then
        p
      else
        best
    )
    (List.hd Site_docs_pages.pages)
    Site_docs_pages.pages

let request ?cookie path =
  {
    Silica_server.meth = "GET";
    path;
    query = [];
    headers =
      ("host", "localhost:8080")
      :: (match cookie with Some c -> [ ("cookie", c) ] | None -> []);
    body = "";
  }

let session_cookie server path =
  let res = Server.handle server (request path) in
  match List.assoc_opt "set-cookie" res.headers with
  | Some value ->
      List.hd (String.split_on_char ';' value)
  | None ->
      failwith "no session cookie"

let percentile sorted p =
  let n = Array.length sorted in
  sorted.(min (n - 1) (int_of_float (Float.of_int n *. p)))

let measure ~runs name server path =
  let cookie = session_cookie server path in
  let once () =
    let res = Server.handle server (request ~cookie path) in
    if res.status <> 200 then
      failwith (Printf.sprintf "%s: status %d" name res.status);
    String.length res.body
  in
  for _ = 1 to max 5 (runs / 10) do
    ignore (once ())
  done;
  Gc.full_major ();
  let times = Array.make runs 0. in
  let before = Gc.quick_stat () in
  let bytes = ref 0 in
  for i = 0 to runs - 1 do
    let started = Unix.gettimeofday () in
    bytes := once ();
    times.(i) <- Unix.gettimeofday () -. started
  done;
  let after = Gc.quick_stat () in
  Array.sort compare times;
  let per field = (field after -. field before) /. Float.of_int runs in
  Printf.printf "| %s | %d | %.3f | %.3f | %.0f | %.0f |\n%!" name !bytes
    (percentile times 0.5 *. 1000.)
    (percentile times 0.9 *. 1000.)
    (per (fun (s : Gc.stat) -> s.minor_words))
    (per (fun (s : Gc.stat) -> s.major_words))

let () =
  let runs = ref 200 in
  let only = ref "" in
  Arg.parse
    [
      ("-n", Arg.Set_int runs, "renders per page (default 200)");
      ("--only", Arg.Set_string only, "landing or doc");
    ]
    ignore "In-process server render benchmark";
  let site =
    match Site_server.create (config ()) ~assets ~read:(fun _ -> None) () with
    | Ok site ->
        site
    | Error message ->
        failwith message
  in
  let doc = longest_doc () in
  let pages = [ ("landing", site, "/"); ("doc", site, "/docs/" ^ doc.slug) ] in
  Printf.printf "runs per page: %d; longest doc: %s\n" !runs doc.slug;
  print_endline
    "| page | HTML bytes | median ms | p90 ms | minor words | major words |";
  print_endline "|---|---|---|---|---|---|";
  List.iter
    (fun (name, server, path) ->
      if !only = "" || !only = name then measure ~runs:!runs name server path
    )
    pages
