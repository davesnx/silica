open Silica
open Fixture
module Kit = Silica_server_kit
module Json = Silica_json.Json

let pending : (unit -> unit) Queue.t = Queue.create ()
let clock = ref 1000.

module Io = struct
  type 'a t = 'a

  let return x = x
  let bind x f = f x
  let sleep _ = ()
  let async f = Queue.add f pending
  let now () = !clock

  type 'a promise = 'a option ref

  let promise () = ref None
  let resolve p v = if !p = None then p := Some v

  let await p ~timeout:_ =
    while not (Queue.is_empty pending) do
      (Queue.take pending) ()
    done;
    !p
end

module Server = Silica_server.Make (Io)

let flush () =
  while not (Queue.is_empty pending) do
    (Queue.take pending) ()
  done

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

let secret = "0123456789abcdef0123"
let origin = "http://app.test"
let logs : string list ref = ref []
let requests : (string * string * int) list ref = ref []
let sockets_opened = ref 0
let sockets_closed = ref 0

let observer =
  {
    Silica_server.log =
      (fun ~source line -> logs := (source ^ ": " ^ line) :: !logs);
    on_request =
      (fun ~meth ~path ~status ~seconds:_ ->
        requests := (meth, path, status) :: !requests
      );
    on_socket =
      (function
      | `Opened ->
          incr sockets_opened
      | `Closed ->
          incr sockets_closed
      );
  }

let counts_topic = Topic.declare ~name:"counts" ~state:Codec.int
let init_calls = ref 0
let init_fails = ref false

let echo_session =
  Remote.declare ~path:"/api/who" ~input:Codec.unit ~output:Codec.string

let handlers =
  [
    Server.implement double (fun _ n -> Ok (n * 2));
    Server.implement boom (fun _ _ -> Error "secret kaput detail");
    Server.implement ghost (fun _ _ -> failwith "handler raised");
    Server.implement echo_session (fun (ctx : Server.context) () ->
        Ok ctx.session.id
    );
  ]

let metafile =
  {|{"inputs":{},"outputs":{"dist/client_main-4F2A9C1B.js":{"entryPoint":"out/client_main.mjs","bytes":1}}}|}

let files =
  [
    ("client_main-4F2A9C1B.js", "console.log(1)");
    ("client_main-4F2A9C1B.js.br", "brotli bytes");
    ("client_main-4F2A9C1B.js.gz", "gzip bytes");
    ("app-7Q2W3E4R.css", "a{}");
    ("app-7Q2W3E4R.css.gz", "gzip css");
    ("plain.css", "body{}");
    ("plain.css.gz", "gzip plain");
    ("secret.txt", "top secret");
    ("../secret.txt", "top secret");
    ("a/../secret.txt", "top secret");
    ("/secret.txt", "top secret");
    ("a\\..\\secret.txt", "top secret");
    ("x\000y", "top secret");
    ("", "top secret");
  ]

let make ?(modify = Fun.id) ?(topics = true) () =
  Queue.clear pending;
  logs := [];
  requests := [];
  sockets_opened := 0;
  sockets_closed := 0;
  init_calls := 0;
  init_fails := false;
  let config =
    modify
      {
        (Silica_server.default_config
           ~origins:(Silica_server.Origins [ origin ]) ~secret
        )
        with
        now = (fun () -> !clock);
        observer;
        sessions = Silica_server.Session.ops (Silica_server.Session.create ());
      }
  in
  let assets =
    match Silica_server.Assets.of_metafile ~prefix:"/static/" metafile with
    | Ok a ->
        a
    | Error e ->
        Alcotest.fail e
  in
  Server.create config ~assets
    ~read:(fun name -> List.assoc_opt name files)
    ~handlers ~islands:[ Server.Island live ]
    ~topics:
      ( if topics then
          [
            Server.Topic_def
              {
                topic = counts_topic;
                init =
                  (fun _ ~key:_ ->
                    incr init_calls;
                    if !init_fails then
                      Error "no data"
                    else
                      Ok 10
                  );
              };
          ]
        else
          []
      )
    ~page:(fun (page : Server.page) ->
      {
        Silica_server.status = 200;
        headers = [ ("content-type", "text/html; charset=utf-8") ];
        body =
          Printf.sprintf "csrf=%s nonce=%s script=%s" page.csrf page.nonce
            (try page.asset "client_main.js" with Failure _ -> "none");
      }
    )
    ()

let request ?(meth = "GET") ?(headers = []) ?(body = "") path =
  { Silica_server.meth; path; query = []; headers; body }

let header name (r : Silica_server.response) = List.assoc_opt name r.headers

let word_after marker body =
  let n = String.length marker in
  let rec find i =
    if i + n > String.length body then
      Alcotest.failf "no %S in %S" marker body
    else if String.sub body i n = marker then
      i + n
    else
      find (i + 1)
  in
  let start = find 0 in
  let stop =
    match String.index_from_opt body start ' ' with
    | Some i ->
        i
    | None ->
        String.length body
  in
  String.sub body start (stop - start)

type client = { cookie : string; csrf : string; id : string }

let visit t =
  let r = Server.handle t (request "/") in
  let set_cookie =
    match header "set-cookie" r with
    | Some c ->
        c
    | None ->
        Alcotest.fail "no cookie"
  in
  let id =
    let eq = String.index set_cookie '=' in
    let semi = String.index set_cookie ';' in
    String.sub set_cookie (eq + 1) (semi - eq - 1)
  in
  { cookie = "silica_sid=" ^ id; csrf = word_after "csrf=" r.body; id }

let post ?(origin = Some origin) ?(extra = [])
    ?(content_type = "application/json") ?(client : client option) ?csrf t path
    body =
  let client = match client with Some c -> c | None -> visit t in
  let token = Option.value csrf ~default:client.csrf in
  let headers =
    [
      ("cookie", client.cookie);
      ("content-type", content_type);
      ("x-silica-csrf", token);
    ]
    @ (match origin with Some o -> [ ("origin", o) ] | None -> [])
    @ extra
  in
  Server.handle t (request ~meth:"POST" ~headers ~body path)

let status (r : Silica_server.response) = r.status

let page_tests =
  let open Alcotest in
  [
    test_case "the first page sets a session cookie" `Quick (fun () ->
        let t = make () in
        let r = Server.handle t (request "/") in
        let cookie = Option.get (header "set-cookie" r) in
        check bool "name" true (contains cookie "silica_sid=");
        check bool "HttpOnly" true (contains cookie "; HttpOnly");
        check bool "SameSite" true (contains cookie "; SameSite=Lax");
        check bool "path" true (contains cookie "; Path=/");
        check bool "no Max-Age" false
          (contains (String.lowercase_ascii cookie) "max-age");
        check bool "no Expires" false
          (contains (String.lowercase_ascii cookie) "expires");
        check bool "no Secure without TLS" false (contains cookie "Secure");
        let eq = String.index cookie '=' and semi = String.index cookie ';' in
        let id = String.sub cookie (eq + 1) (semi - eq - 1) in
        check int "128 bit id as hex" 32 (String.length id);
        check bool "hex" true
          (String.for_all
             (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
             id
          )
    );
    test_case "Secure is added when configured" `Quick (fun () ->
        let t = make ~modify:(fun c -> { c with secure_cookie = true }) () in
        let r = Server.handle t (request "/") in
        check bool "Secure" true
          (contains (Option.get (header "set-cookie" r)) "; Secure")
    );
    test_case "a visitor with a session gets no new cookie" `Quick (fun () ->
        let t = make () in
        let c = visit t in
        let r =
          Server.handle t (request ~headers:[ ("cookie", c.cookie) ] "/")
        in
        check (option string) "no set-cookie" None (header "set-cookie" r);
        check string "same csrf token" c.csrf (word_after "csrf=" r.body)
    );
    test_case "an unknown cookie gets a new session" `Quick (fun () ->
        let t = make () in
        let r =
          Server.handle t
            (request ~headers:[ ("cookie", "silica_sid=ffff") ] "/")
        in
        check bool "new cookie" true (header "set-cookie" r <> None)
    );
    test_case "two visitors have different sessions and tokens" `Quick
      (fun () ->
        let t = make () in
        let a = visit t and b = visit t in
        check bool "ids" true (a.id <> b.id);
        check bool "tokens" true (a.csrf <> b.csrf)
    );
    test_case "pages are private and not stored, with security headers" `Quick
      (fun () ->
        let t = make () in
        let r = Server.handle t (request "/") in
        check (option string) "cache" (Some "private, no-store")
          (header "cache-control" r);
        check (option string) "nosniff" (Some "nosniff")
          (header "x-content-type-options" r);
        check (option string) "referrer" (Some "same-origin")
          (header "referrer-policy" r);
        let nonce = word_after "nonce=" r.body in
        let csp = Option.get (header "content-security-policy" r) in
        check bool "nonce in CSP" true (contains csp ("'nonce-" ^ nonce ^ "'"));
        check bool "frame-ancestors" true (contains csp "frame-ancestors 'none'");
        check bool "default-src" true (contains csp "default-src 'self'");
        check bool "no unsafe-eval" false (contains csp "unsafe-eval");
        check bool "script-src has no unsafe-inline" false
          (contains
             (List.find
                (fun d -> contains d "script-src")
                (String.split_on_char ';' csp)
             )
             "unsafe-inline"
          );
        check (option string) "the nonce helper header is not sent" None
          (header "x-silica-nonce" r)
    );
    test_case "each page gets a new nonce" `Quick (fun () ->
        let t = make () in
        let a = Server.handle t (request "/")
        and b = Server.handle t (request "/") in
        check bool "different" true
          (word_after "nonce=" a.body <> word_after "nonce=" b.body)
    );
    test_case "the CSP allows the socket of the page host only" `Quick
      (fun () ->
        let t = make () in
        let r =
          Server.handle t (request ~headers:[ ("host", "app.test:8080") ] "/")
        in
        let csp = Option.get (header "content-security-policy" r) in
        check bool "ws" true (contains csp "ws://app.test:8080");
        let r =
          Server.handle t (request ~headers:[ ("host", "bad host;x") ] "/")
        in
        check bool "unsafe host is not copied" false
          (contains (Option.get (header "content-security-policy" r)) "bad host")
    );
    test_case "health" `Quick (fun () ->
        let t = make () in
        let r = Server.handle t (request "/_silica/health") in
        check int "status" 200 r.status;
        check string "body" "ok" r.body;
        check (option string) "no cookie" None (header "set-cookie" r);
        check (option string) "no store" (Some "no-store")
          (header "cache-control" r)
    );
    test_case "a short secret is refused" `Quick (fun () ->
        check bool "invalid_argument" true
          ( match make ~modify:(fun c -> { c with secret = "short" }) () with
          | exception Invalid_argument _ ->
              true
          | _ ->
              false
          )
    );
    test_case "page requests reach the observer" `Quick (fun () ->
        let t = make () in
        ignore (Server.handle t (request "/"));
        check bool "seen" true (List.mem ("GET", "/", 200) !requests)
    );
  ]

let call_tests =
  let open Alcotest in
  [
    test_case "a call with every check passes" `Quick (fun () ->
        let t = make () in
        let r = post t "/api/double" "21" in
        check int "status" 200 r.status;
        check string "body" "42" r.body;
        check (option string) "type" (Some "application/json")
          (header "content-type" r);
        check (option string) "nosniff" (Some "nosniff")
          (header "x-content-type-options" r)
    );
    test_case "the handler gets the session of the cookie" `Quick (fun () ->
        let t = make () in
        let c = visit t in
        let r = post ~client:c t "/api/who" "null" in
        check string "id" ("\"" ^ c.id ^ "\"") r.body
    );
    test_case "no Origin is refused" `Quick (fun () ->
        check int "status" 403
          (status (post ~origin:None (make ()) "/api/double" "1"))
    );
    test_case "another Origin is refused" `Quick (fun () ->
        let t = make () in
        check int "other" 403
          (status (post ~origin:(Some "http://evil.test") t "/api/double" "1"));
        check int "prefix of the allowed one" 403
          (status
             (post ~origin:(Some "http://app.test.evil.test") t "/api/double"
                "1"
             )
          );
        check int "other scheme" 403
          (status (post ~origin:(Some "https://app.test") t "/api/double" "1"));
        check int "other port" 403
          (status (post ~origin:(Some "http://app.test:81") t "/api/double" "1"));
        check int "null" 403
          (status (post ~origin:(Some "null") t "/api/double" "1"))
    );
    test_case "Sec-Fetch-Site decides first" `Quick (fun () ->
        let t = make () in
        check int "cross-site" 403
          (status
             (post
                ~extra:[ ("sec-fetch-site", "cross-site") ]
                t "/api/double" "1"
             )
          );
        check int "same-site" 403
          (status
             (post
                ~extra:[ ("sec-fetch-site", "same-site") ]
                t "/api/double" "1"
             )
          );
        check int "same-origin" 200
          (status
             (post
                ~extra:[ ("sec-fetch-site", "same-origin") ]
                t "/api/double" "1"
             )
          )
    );
    test_case "no session cookie is refused" `Quick (fun () ->
        let t = make () in
        let c = visit t in
        let headers =
          [
            ("origin", origin);
            ("content-type", "application/json");
            ("x-silica-csrf", c.csrf);
          ]
        in
        check int "status" 403
          (status
             (Server.handle t
                (request ~meth:"POST" ~headers ~body:"1" "/api/double")
             )
          )
    );
    test_case "a wrong or missing CSRF token is refused" `Quick (fun () ->
        let t = make () in
        check int "wrong" 403 (status (post ~csrf:"nope" t "/api/double" "1"));
        let other = visit t in
        let c = visit t in
        check int "token of another session" 403
          (status (post ~client:c ~csrf:other.csrf t "/api/double" "1"));
        let headers =
          [
            ("origin", origin);
            ("cookie", c.cookie);
            ("content-type", "application/json");
          ]
        in
        check int "missing" 403
          (status
             (Server.handle t
                (request ~meth:"POST" ~headers ~body:"1" "/api/double")
             )
          )
    );
    test_case "the content type must be JSON" `Quick (fun () ->
        let t = make () in
        check int "text" 415
          (status (post ~content_type:"text/plain" t "/api/double" "1"));
        check int "form" 415
          (status
             (post ~content_type:"application/x-www-form-urlencoded" t
                "/api/double" "1"
             )
          );
        check int "charset parameter is fine" 200
          (status
             (post ~content_type:"application/json; charset=utf-8" t
                "/api/double" "1"
             )
          )
    );
    test_case "the body size is capped" `Quick (fun () ->
        let t = make ~modify:(fun c -> { c with max_body = 8 }) () in
        check int "at the limit" 200 (status (post t "/api/double" "12345678"));
        check int "above" 413 (status (post t "/api/double" "123456789"))
    );
    test_case "GET on an endpoint is 405, unknown POST path is 404" `Quick
      (fun () ->
        let t = make () in
        check int "get" 405 (status (Server.handle t (request "/api/double")));
        check int "put" 405
          (status (Server.handle t (request ~meth:"PUT" "/api/double")));
        check int "unknown" 404 (status (post t "/api/nothing" "1"))
    );
    test_case "bad input is 400" `Quick (fun () ->
        let t = make () in
        check int "garbage" 400 (status (post t "/api/double" "{"));
        check int "wrong type" 400 (status (post t "/api/double" {|"s"|}))
    );
    test_case "a handler error is a fixed 500 and the detail goes to the log"
      `Quick (fun () ->
        let t = make () in
        let r = post t "/api/boom" "1" in
        check int "status" 500 r.status;
        check string "body" "internal error" r.body;
        check bool "logged" true
          (List.exists
             (fun l ->
               contains l "silica.remote" && contains l "secret kaput detail"
             )
             !logs
          );
        let r = post t "/api/ghost" "1" in
        check int "raise status" 500 r.status;
        check string "raise body" "internal error" r.body;
        check bool "raise logged" true
          (List.exists (fun l -> contains l "handler raised") !logs)
    );
    test_case "a request that fails a check never reaches the handler" `Quick
      (fun () ->
        let t = make () in
        let c = visit t in
        ignore (post ~origin:None ~client:c t "/api/who" "null");
        ignore (post ~csrf:"x" ~client:c t "/api/who" "null");
        check bool "no remote log" false
          (List.exists (fun l -> contains l "silica.remote") !logs)
    );
    test_case "an expired session is refused" `Quick (fun () ->
        let t = make () in
        let c = visit t in
        check int "fresh" 200 (status (post ~client:c t "/api/double" "1"));
        clock := !clock +. (8. *. 3600.) +. 1.;
        check int "after 8 hours idle" 403
          (status (post ~client:c t "/api/double" "1"));
        clock := 1000.
    );
    test_case "the session is kept alive by use and ends after 7 days" `Quick
      (fun () ->
        clock := 1000.;
        let t = make () in
        let c = visit t in
        for _ = 1 to 28 do
          clock := !clock +. (6. *. 3600.);
          check int "used" 200 (status (post ~client:c t "/api/double" "1"))
        done;
        clock := !clock +. (6. *. 3600.);
        check int "7 days after creation" 403
          (status (post ~client:c t "/api/double" "1"));
        clock := 1000.
    );
  ]

let localhost_tests =
  let open Alcotest in
  let t () =
    make ~modify:(fun c -> { c with origins = Silica_server.Localhost }) ()
  in
  let ok ~host ~origin =
    let t = t () in
    let c = visit t in
    Server.handle t
      (request ~meth:"POST"
         ~headers:
           [
             ("host", host);
             ("origin", origin);
             ("cookie", c.cookie);
             ("content-type", "application/json");
             ("x-silica-csrf", c.csrf);
           ]
         ~body:"1" "/api/double"
      )
    |> status
  in
  [
    test_case "localhost mode accepts the Host of a local address" `Quick
      (fun () ->
        check int "localhost" 200
          (ok ~host:"localhost:8080" ~origin:"http://localhost:8080");
        check int "127.0.0.1" 200
          (ok ~host:"127.0.0.1:3000" ~origin:"http://127.0.0.1:3000");
        check int "ipv6" 200 (ok ~host:"[::1]:3000" ~origin:"http://[::1]:3000")
    );
    test_case "localhost mode refuses other hosts and other origins" `Quick
      (fun () ->
        check int "public host" 403
          (ok ~host:"app.example.com" ~origin:"http://app.example.com");
        check int "origin differs from Host" 403
          (ok ~host:"localhost:8080" ~origin:"http://localhost:9090");
        check int "other site on the same machine" 403
          (ok ~host:"localhost:8080" ~origin:"http://evil.test")
    );
  ]

let asset_tests =
  let open Alcotest in
  [
    test_case "a hashed file is immutable" `Quick (fun () ->
        let t = make () in
        let r = Server.handle t (request "/static/client_main-4F2A9C1B.js") in
        check int "status" 200 r.status;
        check string "body" "console.log(1)" r.body;
        check (option string) "cache"
          (Some "public, max-age=31536000, immutable") (header "cache-control" r);
        check (option string) "type" (Some "text/javascript; charset=utf-8")
          (header "content-type" r);
        check (option string) "no cookie" None (header "set-cookie" r)
    );
    test_case "a hashed file is sent compressed when the client accepts it"
      `Quick (fun () ->
        let t = make () in
        let get ?accept path =
          Server.handle t
            (request
               ~headers:
                 (Option.to_list
                    (Option.map (fun a -> ("accept-encoding", a)) accept)
                 )
               path
            )
        in
        let js = "/static/client_main-4F2A9C1B.js" in
        let expect name ?accept path ~encoding ~body =
          let r = get ?accept path in
          check int (name ^ ": status") 200 r.status;
          check (option string) (name ^ ": encoding") encoding
            (header "content-encoding" r);
          check string (name ^ ": body") body r.body;
          check (option string) (name ^ ": vary") (Some "accept-encoding")
            (header "vary" r);
          check (option string) (name ^ ": type")
            (Some (Kit.Assets.content_type path))
            (header "content-type" r)
        in
        expect "br first" ~accept:"gzip, deflate, br" js ~encoding:(Some "br")
          ~body:"brotli bytes";
        expect "gzip only" ~accept:"gzip" js ~encoding:(Some "gzip")
          ~body:"gzip bytes";
        expect "br refused" ~accept:"br;q=0, GZIP;q=0.5" js
          ~encoding:(Some "gzip") ~body:"gzip bytes";
        expect "any" ~accept:"*" js ~encoding:(Some "br") ~body:"brotli bytes";
        expect "any but br" ~accept:"br;q=0, *" js ~encoding:(Some "gzip")
          ~body:"gzip bytes";
        expect "identity" ~accept:"identity" js ~encoding:None
          ~body:"console.log(1)";
        expect "no header" js ~encoding:None ~body:"console.log(1)";
        expect "no br file" ~accept:"br, gzip" "/static/app-7Q2W3E4R.css"
          ~encoding:(Some "gzip") ~body:"gzip css";
        let plain = get ~accept:"gzip" "/static/plain.css" in
        check (option string) "an unhashed file is not compressed" None
          (header "content-encoding" plain);
        check string "unhashed body" "body{}" plain.body
    );
    test_case "another file is no-cache with an ETag" `Quick (fun () ->
        let t = make () in
        let r = Server.handle t (request "/static/plain.css") in
        check int "status" 200 r.status;
        check (option string) "cache" (Some "no-cache")
          (header "cache-control" r);
        let etag = Option.get (header "etag" r) in
        check string "etag is the content hash" (Kit.Assets.etag "body{}") etag;
        let again =
          Server.handle t
            (request ~headers:[ ("if-none-match", etag) ] "/static/plain.css")
        in
        check int "304" 304 again.status;
        check string "no body" "" again.body;
        let other =
          Server.handle t
            (request ~headers:[ ("if-none-match", "\"x\"") ] "/static/plain.css")
        in
        check int "stale tag gets the file" 200 other.status
    );
    test_case "missing files and path tricks are 404" `Quick (fun () ->
        let t = make () in
        List.iter
          (fun path ->
            check int path 404
              (status (Server.handle t (request ("/static/" ^ path))))
          )
          [
            "nothing.js";
            "../secret.txt";
            "a/../secret.txt";
            "/secret.txt";
            "";
            "a\\..\\secret.txt";
            "x\000y";
          ]
    );
    test_case "the page gets the hashed URL of a logical name" `Quick (fun () ->
        let t = make () in
        let r = Server.handle t (request "/") in
        check string "script" "/static/client_main-4F2A9C1B.js"
          (word_after "script=" r.body)
    );
    test_case "an unknown logical name fails" `Quick (fun () ->
        let seen = ref false in
        let t = make () in
        ignore t;
        let t =
          Server.create
            (Silica_server.default_config
               ~origins:(Silica_server.Origins [ origin ]) ~secret
            )
            ~read:(fun _ -> None)
            ~handlers:[] ~islands:[] ~topics:[]
            ~page:(fun page ->
              ( try ignore (page.asset "missing.js")
                with Failure _ -> seen := true
              );
              { Silica_server.status = 200; headers = []; body = "" }
            )
            ()
        in
        ignore (Server.handle t (request "/"));
        check bool "Failure" true !seen
    );
    test_case "static files are GET only and need no session" `Quick (fun () ->
        let t = make () in
        check int "post" 404
          (status (Server.handle t (request ~meth:"POST" "/static/plain.css")))
    );
  ]

type fake = {
  sent : string list ref;
  mutable closed_with : int option;
  mutable connection : Server.connection option;
  socket : Server.socket;
}

let fake_socket () =
  let sent = ref [] in
  let rec fake =
    {
      sent;
      closed_with = None;
      connection = None;
      socket =
        {
          Server.send = (fun s -> sent := s :: !sent);
          close =
            (fun code ->
              if fake.closed_with = None then fake.closed_with <- Some code
            );
        };
    }
  in
  fake

let ws_headers ?(origin = Some origin) ?(extra = []) (c : client option) =
  (match origin with Some o -> [ ("origin", o) ] | None -> [])
  @ (match c with Some c -> [ ("cookie", c.cookie) ] | None -> [])
  @ extra

let upgrade t ?(path = "/_silica/live") ?(meth = "GET") headers =
  Server.upgrade t (request ~meth ~headers path)

let connect t (c : client) =
  match upgrade t (ws_headers (Some c)) with
  | Server.Refused r ->
      Alcotest.failf "refused with %d" r.status
  | Server.Accepted attach ->
      let fake = fake_socket () in
      let connection = attach fake.socket in
      fake.connection <- Some connection;
      fake

let send fake text = (Option.get fake.connection).Server.receive text
let close fake = (Option.get fake.connection).Server.closed ()

let messages fake =
  List.rev_map
    (fun s ->
      match Protocol.decode_server s with
      | Ok m ->
          m
      | Error e ->
          Alcotest.failf "bad server message %S: %s" s e
    )
    !(fake.sent)

type shown = Hello_ok | State of int * int * int | Error of string | Pong

let show = function
  | Protocol.Hello_ok _ ->
      Hello_ok
  | Protocol.State { seq; ack; state; _ } -> (
      match Protocol.decode_state Codec.int state with
      | Ok n ->
          State (seq, ack, n)
      | Error e ->
          Alcotest.fail e
    )
  | Protocol.Error { code } ->
      Error code
  | Protocol.Pong ->
      Pong

let shown_t =
  Alcotest.testable
    (fun fmt -> function
      | Hello_ok ->
          Format.pp_print_string fmt "hello"
      | State (a, b, c) ->
          Format.fprintf fmt "state(seq=%d,ack=%d,%d)" a b c
      | Error e ->
          Format.fprintf fmt "error(%s)" e
      | Pong ->
          Format.pp_print_string fmt "pong"
      )
    ( = )

let shown fake = List.map show (messages fake)

let hello ?(v = Protocol.version) ?csrf (c : client) sub =
  Protocol.encode_client
    (Protocol.Hello { v; csrf = Option.value csrf ~default:c.csrf; sub })

let island ?(instance = "i1") () =
  Protocol.Island { channel = "counter"; instance }

let event id e = Protocol.encode_event ev_codec ~id e

let upgrade_tests =
  let open Alcotest in
  let refused ?path ?meth headers t =
    match upgrade t ?path ?meth headers with
    | Server.Refused r ->
        r.status
    | Server.Accepted _ ->
        0
  in
  [
    test_case "an upgrade with every check is accepted" `Quick (fun () ->
        let t = make () in
        let c = visit t in
        check int "accepted" 0 (refused (ws_headers (Some c)) t)
    );
    test_case "another path is 404, another method 405" `Quick (fun () ->
        let t = make () in
        let c = visit t in
        check int "path" 404
          (refused ~path:"/_silica/live/counter" (ws_headers (Some c)) t);
        check int "method" 405 (refused ~meth:"POST" (ws_headers (Some c)) t)
    );
    test_case "no Origin or another Origin is refused" `Quick (fun () ->
        let t = make () in
        let c = visit t in
        check int "none" 403 (refused (ws_headers ~origin:None (Some c)) t);
        check int "other" 403
          (refused (ws_headers ~origin:(Some "http://evil.test") (Some c)) t)
    );
    test_case "cross-site fetch metadata is refused" `Quick (fun () ->
        let t = make () in
        let c = visit t in
        check int "cross-site" 403
          (refused
             (ws_headers ~extra:[ ("sec-fetch-site", "cross-site") ] (Some c))
             t
          );
        check int "same-origin" 0
          (refused
             (ws_headers ~extra:[ ("sec-fetch-site", "same-origin") ] (Some c))
             t
          )
    );
    test_case
      "no session or an unknown one is accepted and closed at once with 4005"
      `Quick (fun () ->
        let t = make () in
        let gone headers =
          match upgrade t headers with
          | Server.Refused r ->
              Alcotest.failf "refused with %d" r.status
          | Server.Accepted attach ->
              let fake = fake_socket () in
              let connection = attach fake.socket in
              fake.connection <- Some connection;
              fake
        in
        let none = gone (ws_headers None) in
        check (option int) "none" (Some Protocol.close_session) none.closed_with;
        let unknown =
          gone (ws_headers ~extra:[ ("cookie", "silica_sid=00") ] None)
        in
        check (option int) "unknown" (Some Protocol.close_session)
          unknown.closed_with;
        check (list string) "nothing sent" [] !(unknown.sent)
    );
    test_case "a session that is gone still needs a good Origin" `Quick
      (fun () ->
        let t = make () in
        check int "no origin" 403 (refused (ws_headers ~origin:None None) t);
        check int "other origin" 403
          (refused (ws_headers ~origin:(Some "http://evil.test") None) t)
    );
  ]

let island_tests =
  let open Alcotest in
  [
    test_case "hello is answered, then the first state" `Quick (fun () ->
        let t = make () in
        let c = visit t in
        let f = connect t c in
        check (list shown_t) "nothing before hello" [] (shown f);
        send f (hello c (island ()));
        check (list shown_t) "hello_ok then state"
          [ Hello_ok; State (1, 0, 0) ]
          (shown f);
        check (option int) "open" None f.closed_with
    );
    test_case "events step the machine; seq grows and ack is the event id"
      `Quick (fun () ->
        let t = make () in
        let c = visit t in
        let f = connect t c in
        send f (hello c (island ()));
        send f (event 1 Inc);
        send f (event 2 Inc);
        check (list shown_t) "states"
          [ Hello_ok; State (1, 0, 0); State (2, 1, 1); State (3, 2, 2) ]
          (shown f)
    );
    test_case "an event id that is not higher is dropped" `Quick (fun () ->
        let t = make () in
        let c = visit t in
        let f = connect t c in
        send f (hello c (island ()));
        send f (event 5 Inc);
        send f (event 5 Inc);
        send f (event 3 Inc);
        send f (event 6 Inc);
        check (list shown_t) "only 5 and 6"
          [ Hello_ok; State (1, 0, 0); State (2, 5, 1); State (3, 6, 2) ]
          (shown f)
    );
    test_case "ping is answered with pong, before or after hello" `Quick
      (fun () ->
        let t = make () in
        let c = visit t in
        let f = connect t c in
        send f (Protocol.encode_client Protocol.Ping);
        send f (hello c (island ()));
        send f (Protocol.encode_client Protocol.Ping);
        check (list shown_t) "pongs"
          [ Pong; Hello_ok; State (1, 0, 0); Pong ]
          (shown f);
        check (option int) "still open" None f.closed_with
    );
    test_case "version, token, island and shape are checked in the hello" `Quick
      (fun () ->
        let t = make () in
        let c = visit t in
        let other = visit t in
        let close_code text =
          let f = connect t c in
          send f text;
          (f.closed_with, shown f)
        in
        check
          (pair (option int) (list shown_t))
          "version"
          (Some Protocol.close_version, [])
          (close_code (hello ~v:1 c (island ())));
        check
          (pair (option int) (list shown_t))
          "token of another session"
          (Some Protocol.close_csrf, [])
          (close_code (hello ~csrf:other.csrf c (island ())));
        check
          (pair (option int) (list shown_t))
          "no token"
          (Some Protocol.close_csrf, [])
          (close_code (hello ~csrf:"" c (island ())));
        check
          (pair (option int) (list shown_t))
          "unknown island"
          (Some Protocol.close_unknown, [ Error "unknown_island" ])
          (close_code
             (hello c (Protocol.Island { channel = "nope"; instance = "i" }))
          );
        check
          (pair (option int) (list shown_t))
          "not a message"
          (Some Protocol.close_hello, [])
          (close_code "garbage");
        check
          (pair (option int) (list shown_t))
          "event first"
          (Some Protocol.close_hello, [])
          (close_code (event 1 Inc))
    );
    test_case "a second hello is dropped" `Quick (fun () ->
        let t = make () in
        let c = visit t in
        let f = connect t c in
        send f (hello c (island ()));
        send f (hello c (island ~instance:"other" ()));
        check (list shown_t) "one hello" [ Hello_ok; State (1, 0, 0) ] (shown f);
        check (option int) "open" None f.closed_with
    );
    test_case "a bad message after hello is dropped, not fatal" `Quick
      (fun () ->
        let t = make () in
        let c = visit t in
        let f = connect t c in
        send f (hello c (island ()));
        send f "garbage";
        send f {|{"t":"event","id":1,"event":"nope"}|};
        send f (event 2 Inc);
        check (list shown_t) "still works"
          [ Hello_ok; State (1, 0, 0); State (2, 2, 1) ]
          (shown f);
        check (option int) "open" None f.closed_with
    );
    test_case
      "a message above the cap gets an error; above the hard limit it closes"
      `Quick (fun () ->
        let t = make () in
        let c = visit t in
        let f = connect t c in
        send f (hello c (island ()));
        send f (String.make 65_537 'a');
        check (list shown_t) "error"
          [ Hello_ok; State (1, 0, 0); Error "too_big" ]
          (shown f);
        check (option int) "open" None f.closed_with;
        send f (event 1 Inc);
        check int "still works" 4 (List.length (shown f));
        send f (String.make (1_048_576 + 1) 'a');
        check (option int) "closed" (Some Protocol.close_too_big) f.closed_with
    );
    test_case "a message at the cap is handled, one byte more is not" `Quick
      (fun () ->
        let t = make ~modify:(fun c -> { c with max_message = 200 }) () in
        let c = visit t in
        let f = connect t c in
        send f (hello c (island ()));
        let pad text n = text ^ String.make (n - String.length text) ' ' in
        send f (pad (event 1 Inc) 200);
        check (list shown_t) "handled"
          [ Hello_ok; State (1, 0, 0); State (2, 1, 1) ]
          (shown f);
        send f (pad (event 2 Inc) 201);
        check (list shown_t) "refused"
          [ Hello_ok; State (1, 0, 0); State (2, 1, 1); Error "too_big" ]
          (shown f)
    );
    test_case "a reconnect finds the island and the event ids go on" `Quick
      (fun () ->
        let t = make () in
        let c = visit t in
        let f = connect t c in
        send f (hello c (island ()));
        send f (event 1 Inc);
        send f (event 2 Inc);
        close f;
        check int "held" 1 (Server.islands t);
        let g = connect t c in
        send g (hello c (island ()));
        check (list shown_t) "fresh full state with a higher seq"
          [ Hello_ok; State (4, 2, 2) ]
          (shown g);
        send g (event 2 Inc);
        send g (event 3 Inc);
        check (list shown_t) "old id dropped, new one handled"
          [ Hello_ok; State (4, 2, 2); State (5, 3, 3) ]
          (shown g);
        check int "one island" 1 (Server.islands t)
    );
    test_case "messages sent while detached are not sent to the new socket"
      `Quick (fun () ->
        let t = make () in
        let c = visit t in
        let f = connect t c in
        send f (hello c (island ()));
        send f (event 1 Later);
        close f;
        flush ();
        let before = List.length (shown f) in
        check int "the old socket gets nothing more" before
          (List.length (shown f));
        let g = connect t c in
        send g (hello c (island ()));
        match shown g with
        | [ Hello_ok; State (_, 1, n) ] ->
            check int "the timer ran while detached" 1 n
        | other ->
            Alcotest.failf "unexpected %d messages" (List.length other)
    );
    test_case "instances of one session are independent" `Quick (fun () ->
        let t = make () in
        let c = visit t in
        let a = connect t c and b = connect t c in
        send a (hello c (island ~instance:"a" ()));
        send b (hello c (island ~instance:"b" ()));
        send a (event 1 Inc);
        check (list shown_t) "a"
          [ Hello_ok; State (1, 0, 0); State (2, 1, 1) ]
          (shown a);
        check (list shown_t) "b" [ Hello_ok; State (1, 0, 0) ] (shown b);
        check int "two" 2 (Server.islands t)
    );
    test_case "another session cannot attach to the island" `Quick (fun () ->
        let t = make () in
        let c = visit t and d = visit t in
        let a = connect t c in
        send a (hello c (island ()));
        send a (event 1 Inc);
        let b = connect t d in
        send b (hello d (island ()));
        check (list shown_t) "own fresh island"
          [ Hello_ok; State (1, 0, 0) ]
          (shown b);
        check int "two" 2 (Server.islands t)
    );
    test_case "a new socket for a held island closes the old one" `Quick
      (fun () ->
        let t = make () in
        let c = visit t in
        let a = connect t c in
        send a (hello c (island ()));
        let b = connect t c in
        send b (hello c (island ()));
        check (option int) "old closed" (Some 1000) a.closed_with;
        close a;
        send b (event 1 Inc);
        check int "new still works" 3 (List.length (shown b))
    );
    test_case "a state is kept for 30 minutes after the last socket" `Quick
      (fun () ->
        clock := 1000.;
        let t = make () in
        let c = visit t in
        let f = connect t c in
        send f (hello c (island ()));
        send f (event 1 Inc);
        close f;
        clock := 1000. +. 1799.;
        Server.sweep t;
        check int "kept" 1 (Server.islands t);
        let g = connect t c in
        send g (hello c (island ()));
        check bool "found" true (List.mem (State (3, 1, 1)) (shown g));
        close g;
        clock := !clock +. 1801.;
        Server.sweep t;
        check int "dropped" 0 (Server.islands t);
        let h = connect t c in
        send h (hello c (island ()));
        check (list shown_t) "fresh" [ Hello_ok; State (1, 0, 0) ] (shown h);
        clock := 1000.
    );
    test_case "an outgoing message above the warning size is sent and logged"
      `Quick (fun () ->
        let t = make ~modify:(fun c -> { c with warn_outgoing = 40 }) () in
        let c = visit t in
        let f = connect t c in
        send f (hello c (island ()));
        check bool "the state message is above 40 bytes and logged" true
          (List.exists
             (fun l -> contains l "silica.live" && contains l "above 40")
             !logs
          );
        check int "sent anyway" 2 (List.length (shown f));
        logs := [];
        let quiet = make () in
        let c = visit quiet in
        let f = connect quiet c in
        send f (hello c (island ()));
        check bool "no warning for a small message" false
          (List.exists (fun l -> contains l "above") !logs)
    );
    test_case "an attached island is never dropped by the keep time" `Quick
      (fun () ->
        clock := 1000.;
        let t = make ~modify:(fun c -> { c with heartbeat = 1e9 }) () in
        let c = visit t in
        let f = connect t c in
        send f (hello c (island ()));
        clock := !clock +. 5000.;
        Server.sweep t;
        check int "kept" 1 (Server.islands t);
        clock := 1000.
    );
    test_case "a socket with no traffic for two heartbeats is closed" `Quick
      (fun () ->
        clock := 1000.;
        let t = make () in
        let c = visit t in
        let f = connect t c in
        send f (hello c (island ()));
        clock := !clock +. 59.;
        Server.sweep t;
        check (option int) "still open" None f.closed_with;
        send f (Protocol.encode_client Protocol.Ping);
        clock := !clock +. 59.;
        Server.sweep t;
        check (option int) "the ping reset the clock" None f.closed_with;
        clock := !clock +. 2.;
        Server.sweep t;
        check (option int) "idle" (Some Protocol.close_idle) f.closed_with;
        clock := 1000.
    );
    test_case "sockets are counted and reported" `Quick (fun () ->
        let t = make () in
        let c = visit t in
        let f = connect t c in
        check int "open" 1 (Server.sockets t);
        check int "observer opened" 1 !sockets_opened;
        close f;
        check int "closed" 0 (Server.sockets t);
        check int "observer closed" 1 !sockets_closed
    );
    test_case "a step that raises closes the socket and ends the island" `Quick
      (fun () ->
        let raising =
          Server.create
            (Silica_server.default_config
               ~origins:(Silica_server.Origins [ origin ]) ~secret
            )
            ~read:(fun _ -> None)
            ~handlers:[]
            ~islands:
              [
                Server.Island
                  (Live.declare ~channel:"counter" ~state:Codec.int
                     ~event:ev_codec
                     {
                       Component.machine =
                         Silica_test_support.Simple.machine ~id:"r" ~init:0
                           (fun _ _ -> failwith "step failed"
                         );
                       view = (fun _ -> Node.text "");
                     }
                  );
              ]
            ~topics:[]
            ~page:(fun page ->
              {
                Silica_server.status = 200;
                headers = [ ("content-type", "text/html") ];
                body = "csrf=" ^ page.csrf ^ " ";
              }
            )
            ()
        in
        let c = visit raising in
        let f = connect raising c in
        send f (hello c (island ()));
        check int "held" 1 (Server.islands raising);
        send f (event 1 Inc);
        check (option int) "closed with a server error"
          (Some Protocol.close_server_error) f.closed_with;
        check int "ended" 0 (Server.islands raising)
    );
  ]

let topic_tests =
  let open Alcotest in
  let sub ?(key = "k") () = Protocol.Topic { name = "counts"; key } in
  [
    test_case "a new key is made by init; the first state has seq 1" `Quick
      (fun () ->
        let t = make () in
        let c = visit t in
        let f = connect t c in
        send f (hello c (sub ()));
        check (list shown_t) "hello, state"
          [ Hello_ok; State (1, 0, 10) ]
          (shown f);
        check int "init once" 1 !init_calls
    );
    test_case
      "a publish while the hello is answered: no state before Hello_ok, and \
       the newest state arrives"
      `Quick (fun () ->
        let t = make () in
        let c = visit t in
        let sent = ref [] in
        let published = ref false in
        let socket =
          {
            Server.send =
              (fun text ->
                if not !published then (
                  published := true;
                  ignore (Server.publish t counts_topic ~key:"k" 42)
                );
                sent := text :: !sent
              );
            close = ignore;
          }
        in
        let connection =
          match upgrade t (ws_headers (Some c)) with
          | Server.Accepted attach ->
              attach socket
          | Server.Refused r ->
              Alcotest.failf "refused with %d" r.status
        in
        connection.Server.receive (hello c (sub ()));
        let shown_list =
          List.rev_map
            (fun text ->
              match Protocol.decode_server text with
              | Ok m ->
                  show m
              | Error e ->
                  Alcotest.fail e
            )
            !sent
        in
        let rec after_hello seen = function
          | Hello_ok :: rest ->
              after_hello true rest
          | State (seq, _, value) :: rest ->
              if not seen then Alcotest.fail "a state came before Hello_ok";
              (seq, value) :: after_hello seen rest
          | _ :: rest ->
              after_hello seen rest
          | [] ->
              []
        in
        let accepted =
          List.fold_left
            (fun (last, value) (seq, v) ->
              if seq > last then
                (seq, Some v)
              else
                (last, value)
            )
            (0, None)
            (after_hello false shown_list)
        in
        check (option int) "the client keeps the newest state" (Some 42)
          (snd accepted)
    );
    test_case "publish goes to every subscriber of the key only" `Quick
      (fun () ->
        let t = make () in
        let c = visit t and d = visit t in
        let a = connect t c and b = connect t d and other = connect t c in
        send a (hello c (sub ()));
        send b (hello d (sub ()));
        send other (hello c (sub ~key:"other" ()));
        ignore (Server.publish t counts_topic ~key:"k" 42);
        check (list shown_t) "a"
          [ Hello_ok; State (1, 0, 10); State (2, 0, 42) ]
          (shown a);
        check (list shown_t) "b"
          [ Hello_ok; State (1, 0, 10); State (2, 0, 42) ]
          (shown b);
        check (list shown_t) "other key"
          [ Hello_ok; State (1, 0, 10) ]
          (shown other);
        check int "init for each key, not for each socket" 2 !init_calls
    );
    test_case "a late subscriber gets the current state with the current seq"
      `Quick (fun () ->
        let t = make () in
        let c = visit t in
        let a = connect t c in
        send a (hello c (sub ()));
        ignore (Server.publish t counts_topic ~key:"k" 1);
        ignore (Server.publish t counts_topic ~key:"k" 2);
        let b = connect t c in
        send b (hello c (sub ()));
        check (list shown_t) "state 3" [ Hello_ok; State (3, 0, 2) ] (shown b);
        check int "no second init" 1 !init_calls
    );
    test_case "a publish before any subscriber is kept" `Quick (fun () ->
        let t = make () in
        let c = visit t in
        ignore (Server.publish t counts_topic ~key:"k" 7);
        let a = connect t c in
        send a (hello c (sub ()));
        check (list shown_t) "published state"
          [ Hello_ok; State (1, 0, 7) ]
          (shown a);
        check int "no init" 0 !init_calls
    );
    test_case "a closed subscriber gets nothing more" `Quick (fun () ->
        let t = make () in
        let c = visit t in
        let a = connect t c in
        send a (hello c (sub ()));
        close a;
        ignore (Server.publish t counts_topic ~key:"k" 5);
        check int "same count" 2 (List.length (shown a))
    );
    test_case "an unknown topic is an error and closes" `Quick (fun () ->
        let t = make () in
        let c = visit t in
        let a = connect t c in
        send a (hello c (Protocol.Topic { name = "nope"; key = "k" }));
        check (list shown_t) "error" [ Error "unknown_topic" ] (shown a);
        check (option int) "closed" (Some Protocol.close_unknown) a.closed_with
    );
    test_case "a failing init is an error and closes the socket" `Quick
      (fun () ->
        let t = make () in
        let c = visit t in
        init_fails := true;
        let a = connect t c in
        send a (hello c (sub ()));
        check (list shown_t) "error" [ Error "unavailable" ] (shown a);
        check (option int) "closed" (Some Protocol.close_server_error)
          a.closed_with
    );
    test_case "events on a topic socket are dropped" `Quick (fun () ->
        let t = make () in
        let c = visit t in
        let a = connect t c in
        send a (hello c (sub ()));
        send a (event 1 Inc);
        check int "nothing new" 2 (List.length (shown a));
        check (option int) "open" None a.closed_with
    );
    test_case "topic state is kept for 30 minutes without sockets" `Quick
      (fun () ->
        clock := 1000.;
        let t = make () in
        let c = visit t in
        let a = connect t c in
        send a (hello c (sub ()));
        ignore (Server.publish t counts_topic ~key:"k" 9);
        close a;
        clock := !clock +. 1700.;
        Server.sweep t;
        let b = connect t c in
        send b (hello c (sub ()));
        check (list shown_t) "kept" [ Hello_ok; State (2, 0, 9) ] (shown b);
        close b;
        clock := !clock +. 1801.;
        Server.sweep t;
        let d = connect t c in
        send d (hello c (sub ()));
        check (list shown_t) "re-initialised"
          [ Hello_ok; State (1, 0, 10) ]
          (shown d);
        clock := 1000.
    );
    test_case "a handler can publish" `Quick (fun () ->
        let t = make () in
        let c = visit t in
        let a = connect t c in
        send a (hello c (sub ()));
        ignore (Server.publish t counts_topic ~key:"k" 3);
        check bool "published" true (List.mem (State (2, 0, 3)) (shown a))
    );
  ]

let () =
  Alcotest.run "silica-policy"
    [
      ("pages", page_tests);
      ("calls", call_tests);
      ("localhost", localhost_tests);
      ("assets", asset_tests);
      ("upgrade", upgrade_tests);
      ("islands", island_tests);
      ("topics", topic_tests);
    ]
