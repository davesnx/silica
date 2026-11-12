module Server = Silica_adapter.Server

let metafile =
  {|{"outputs":{"dist/client_main-AAAAAAAA.js":{"entryPoint":"out/client_main.mjs"},"dist/app-BBBBBBBB.css":{"entryPoint":"app.css"}}}|}

let server =
  let config =
    Silica_server.default_config ~origins:Silica_server.Localhost
      ~secret:"0123456789abcdef0123456789abcdef"
  in
  match Silica_server.Assets.of_metafile ~prefix:"/static/" metafile with
  | Ok assets ->
      Site_server.create config ~assets ~read:(fun _ -> None) ()
  | Error message ->
      failwith message

let get path =
  Server.handle server
    { Silica_server.meth = "GET"; path; query = []; headers = []; body = "" }

let starts_with prefix (response : Silica_server.response) =
  String.starts_with ~prefix response.body

let served path prefix () =
  let response = get path in
  Alcotest.(check int) (path ^ " status") 200 response.status;
  Alcotest.(check (option string))
    (path ^ " type") (Some "text/plain; charset=utf-8")
    (List.assoc_opt "content-type" response.headers);
  Alcotest.(check bool)
    (path ^ " starts with " ^ prefix)
    true
    (starts_with prefix response)

let every_page_listed () =
  let index = (get "/llms.txt").body in
  List.iter
    (fun file ->
      if Filename.check_suffix file ".mld" && file <> "index.mld" then
        let url =
          "https://silica-site.fly.dev/docs/"
          ^ Filename.chop_suffix file ".mld"
          ^ ".md)"
        in
        let found =
          let length = String.length url in
          let rec scan at =
            at + length <= String.length index
            && (String.sub index at length = url || scan (at + 1))
          in
          scan 0
        in
        Alcotest.(check bool) (file ^ " is in llms.txt") true found
    )
    (Array.to_list (Sys.readdir "docs"))

let () =
  Alcotest.run "agent text files"
    [
      ( "served",
        [
          Alcotest.test_case "llms.txt" `Quick (served "/llms.txt" "# Silica\n");
          Alcotest.test_case "llms-full.txt" `Quick
            (served "/llms-full.txt" "# Silica\n");
          Alcotest.test_case "a page" `Quick
            (served "/docs/verification.md" "# ");
          Alcotest.test_case "every page is listed" `Quick every_page_listed;
          Alcotest.test_case "code in the glossary" `Quick (fun () ->
              let body = (get "/docs/glossary.md").body in
              let needle = "`Machine.register_codecs`" in
              let length = String.length needle in
              let rec scan at =
                at + length <= String.length body
                && (String.sub body at length = needle || scan (at + 1))
              in
              Alcotest.(check bool) needle true (scan 0)
          );
          Alcotest.test_case "an unknown page" `Quick (fun () ->
              Alcotest.(check bool)
                "not the text" false
                (starts_with "# " (get "/docs/nowhere.md"))
          );
        ]
      );
    ]
