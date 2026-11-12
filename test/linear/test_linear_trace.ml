open Silica
module Server = Silica_adapter.Server

let origin = "http://app.test"

let contains text needle =
  let size = String.length needle and length = String.length text in
  let rec look index =
    index + size <= length
    && (String.sub text index size = needle || look (index + 1))
  in
  look 0

let metafile =
  {|{"outputs":{"dist/client_main-AAAAAAAA.js":{"entryPoint":"out/client_main.mjs"},"dist/app-BBBBBBBB.css":{"entryPoint":"app.css"}}}|}

let scratch name =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "linear-trace-%s-%d" name (Unix.getpid ()))
  in
  if Sys.file_exists dir then
    Array.iter
      (fun file -> Sys.remove (Filename.concat dir file))
      (Sys.readdir dir)
  else
    Sys.mkdir dir 0o755;
  dir

let make ~dev ~traces =
  let config =
    Silica_server.default_config ~origins:(Silica_server.Origins [ origin ])
      ~secret:"0123456789abcdef0123456789abcdef"
  in
  let config =
    if dev then
      {
        config with
        dev =
          Some
            { state_file = Filename.concat (scratch "state") "never-written" };
      }
    else
      config
  in
  let assets =
    match Silica_server.Assets.of_metafile ~prefix:"/static/" metafile with
    | Ok assets ->
        assets
    | Error message ->
        Alcotest.fail message
  in
  Linear_server.create ~traces config ~assets ~read:(fun _ -> None) ()

let request ?(meth = "GET") ?(headers = []) ?(body = "") path =
  { Silica_server.meth; path; query = []; headers; body }

let after marker body =
  let size = String.length marker in
  let rec find index =
    if index + size > String.length body then
      Alcotest.failf "no %S in %s" marker body
    else if String.sub body index size = marker then
      index + size
    else
      find (index + 1)
  in
  let start = find 0 in
  String.sub body start (String.index_from body start '"' - start)

let page server =
  let response = Server.handle server (request "/") in
  let set = List.assoc "set-cookie" response.headers in
  ( String.sub set 0 (String.index set ';'),
    after "name=\"silica-csrf\" content=\"" response.body,
    response.body
  )

let post server text =
  let cookie, csrf, _ = page server in
  Server.handle server
    (request ~meth:"POST" "/api/trace"
       ~body:(Codec.to_string Codec.string text)
       ~headers:
         [
           ("cookie", cookie);
           ("origin", origin);
           ("content-type", "application/json");
           ("x-silica-csrf", csrf);
         ]
    )

let record seq : Transition.t =
  {
    seq;
    time_ms = None;
    instance = "router";
    number = 1;
    entry = 0;
    machine = "router";
    from = "";
    target = "/";
    change = Started;
    event = "";
    data = None;
    props = None;
    snapshot = None;
    actions = [];
    cause = Start;
  }

let trace count =
  Trace.to_jsonl
    {
      header =
        {
          root = "router";
          url = Some "/";
          build = "dev";
          origin = Runtime { platform = "browser" };
          dropped = 0;
          loop_from = None;
        };
      transitions = List.init count (fun index -> record (index + 1));
    }

let files dir = List.sort compare (Array.to_list (Sys.readdir dir))

let production_has_no_endpoint () =
  let traces = scratch "production" in
  let server = make ~dev:false ~traces in
  let _, _, html = page server in
  Alcotest.(check bool)
    "no meta for the client" false
    (contains html "linear-trace");
  Alcotest.(check int) "no endpoint" 404 (post server (trace 1)).status;
  Alcotest.(check (list string)) "no file" [] (files traces)

let development_saves_a_trace () =
  let traces = scratch "dev" in
  let server = make ~dev:true ~traces in
  let _, _, html = page server in
  Alcotest.(check bool)
    "the meta tells the client to record" true
    (contains html "<meta name=\"linear-trace\" content=\"/api/trace\">");
  let response = post server (trace 2) in
  Alcotest.(check int) "saved" 200 response.status;
  match files traces with
  | [ name ] ->
      Alcotest.(check string)
        "the answer is the name it made"
        (Codec.to_string Codec.string name)
        response.body;
      Alcotest.(check bool)
        "a name of the server" true
        (String.starts_with ~prefix:"trace-" name
        && Filename.check_suffix name ".jsonl"
        );
      Alcotest.(check string)
        "the trace, written again" (trace 2)
        (In_channel.with_open_bin
           (Filename.concat traces name)
           In_channel.input_all
        )
  | names ->
      Alcotest.failf "files: %s" (String.concat ", " names)

let refused server text =
  let response = post server text in
  Alcotest.(check int) "refused" 422 response.status;
  response.body

let development_bounds_the_input () =
  let traces = scratch "bounds" in
  let server = make ~dev:true ~traces in
  Alcotest.(check bool)
    "not a trace file" true
    (contains (refused server "../../etc/passwd") "not a trace file");
  Alcotest.(check bool)
    "too many transitions" true
    (contains (refused server (trace 2001)) "more than 2000 transitions");
  Alcotest.(check bool)
    "too many bytes" true
    (contains
       (refused server (trace 1 ^ String.make (512 * 1024) ' '))
       "more than 524288 bytes"
    );
  Alcotest.(check (list string)) "nothing written" [] (files traces);
  List.iter
    (fun index ->
      Out_channel.with_open_bin
        (Filename.concat traces (Printf.sprintf "old-%03d.jsonl" index))
        ignore
    )
    (List.init 200 Fun.id);
  Alcotest.(check bool)
    "a full folder" true
    (contains (refused server (trace 1)) "200 traces are kept");
  Alcotest.(check int) "still 200 files" 200 (List.length (files traces))

let () =
  Alcotest.run "linear trace"
    [
      ( "endpoint",
        [
          Alcotest.test_case "production has no trace endpoint" `Quick
            production_has_no_endpoint;
          Alcotest.test_case "development saves a trace under its own name"
            `Quick development_saves_a_trace;
          Alcotest.test_case "development bounds the input" `Quick
            development_bounds_the_input;
        ]
      );
    ]
