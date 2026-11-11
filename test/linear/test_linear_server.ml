open Silica
open Linear_model
module Server = Silica_adapter.Server

let origin = "http://app.test"

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let has msg html needle =
  if not (contains html needle) then
    Alcotest.failf "%s: %S not found in %s" msg needle html

let metafile =
  {|{"outputs":{"dist/client_main-AAAAAAAA.js":{"entryPoint":"out/client_main.mjs"},"dist/app-BBBBBBBB.css":{"entryPoint":"app.css"}}}|}

let make () =
  let config =
    Silica_server.default_config ~origins:(Silica_server.Origins [ origin ])
      ~secret:"0123456789abcdef0123456789abcdef"
  in
  let assets =
    match Silica_server.Assets.of_metafile ~prefix:"/static/" metafile with
    | Ok assets ->
        assets
    | Error message ->
        Alcotest.fail message
  in
  Linear_server.create config ~assets ~read:(fun _ -> None) ()

let request ?(meth = "GET") ?(headers = []) ?(body = "") path =
  { Silica_server.meth; path; query = []; headers; body }

let after marker body =
  let n = String.length marker in
  let rec find i =
    if i + n > String.length body then
      Alcotest.failf "no %S in %s" marker body
    else if String.sub body i n = marker then
      i + n
    else
      find (i + 1)
  in
  let start = find 0 in
  String.sub body start (String.index_from body start '"' - start)

type visitor = { cookie : string; csrf : string; html : string }

let visit ?cookie t path =
  let headers = match cookie with Some c -> [ ("cookie", c) ] | None -> [] in
  let r = Server.handle t (request ~headers path) in
  let cookie =
    match (List.assoc_opt "set-cookie" r.headers, cookie) with
    | Some set, _ ->
        String.sub set 0 (String.index set ';')
    | None, Some c ->
        c
    | None, None ->
        Alcotest.fail "no cookie"
  in
  {
    cookie;
    csrf = after "name=\"silica-csrf\" content=\"" r.body;
    html = r.body;
  }

let post t (v : visitor) path body =
  Server.handle t
    (request ~meth:"POST" ~body path
       ~headers:
         [
           ("cookie", v.cookie);
           ("origin", origin);
           ("content-type", "application/json");
           ("x-silica-csrf", v.csrf);
         ]
    )

let encode endpoint value =
  (Remote.input endpoint).Codec.encode value |> Silica_json.Json.to_string

let decode endpoint (r : Silica_server.response) =
  match Silica_json.Json.of_string r.body with
  | Error message ->
      Alcotest.fail message
  | Ok json -> (
      match (Remote.output endpoint).Codec.decode json with
      | Ok value ->
          value
      | Error message ->
          Alcotest.fail message
    )

let test_page_is_settled () =
  let t = make () in
  let v = visit t "/" in
  has "issue titles are in the html" v.html "Login page flashes on load";
  has "user is in the html" v.html "you are Ada";
  has "answers are embedded" v.html "silica-answers";
  has "issues answer" v.html "/api/issues";
  has "me answer" v.html "/api/me";
  has "stylesheet" v.html
    "<link rel=\"stylesheet\" href=\"/static/app-BBBBBBBB.css\">";
  has "script" v.html "/static/client_main-AAAAAAAA.js"

let test_first_page_user_is_the_session_user () =
  let t = make () in
  let user html =
    List.find
      (fun name -> contains html ("you are " ^ name))
      (List.map User.name User.all)
  in
  let differ = ref 0 in
  for _ = 1 to 200 do
    let first = visit t "/issue/11" in
    let again = visit ~cookie:first.cookie t "/issue/11" in
    if user first.html <> user again.html then incr differ
  done;
  Alcotest.(check int)
    "the settle calls of one page give its session one user" 0 !differ

let test_detail_page () =
  let t = make () in
  let v = visit t "/issue/3" in
  has "detail" v.html "Login page flashes on load";
  has "id" v.html "ENG-3";
  let missing = Server.handle t (request "/issue/9999") in
  Alcotest.(check int) "unknown issue is 404" 404 missing.status;
  has "message" missing.body "There is no issue ENG-9999.";
  let nowhere = Server.handle t (request "/no/such/page") in
  Alcotest.(check int) "unknown path is 404" 404 nowhere.status

let lacks msg html needle =
  if contains html needle then
    Alcotest.failf "%s: %S found in %s" msg needle html

let nav_link html =
  let marker = "id=\"nav-issues\"" in
  let n = String.length marker in
  let rec find i =
    if i + n > String.length html then
      Alcotest.failf "no nav link in %s" html
    else if String.sub html i n = marker then
      i
    else
      find (i + 1)
  in
  let at = find 0 in
  let open_ = String.rindex_from html at '<' in
  String.sub html open_ (String.index_from html at '>' - open_ + 1)

let test_route_features () =
  let t = make () in
  let list = visit t "/" in
  has "the nav link is the current page on the list" (nav_link list.html)
    "aria-current=\"page\"";
  has "the list heading takes focus after a navigation" list.html
    "<h1 class=\"mb-4 text-xl font-semibold\" \
     data-silica-route-focus=\"\">Issues</h1>";
  has "an issue row preloads on intent" list.html
    "href=\"/issue/3\" data-silica-link data-silica-preload=\"intent\"";
  let detail = visit t "/issue/3" in
  lacks "the nav link is not the current page on a detail"
    (nav_link detail.html) "aria-current";
  has "the detail heading takes focus after a navigation" detail.html
    "data-silica-route-focus=\"\">Login page flashes on load</h1>"

let test_users_in_turn () =
  let t = make () in
  let first = visit t "/" in
  let second = visit t "/" in
  let third = visit t "/" in
  has "first" first.html "you are Ada";
  has "second" second.html "you are Ben";
  has "third" third.html "you are Cleo";
  let again = visit ~cookie:first.cookie t "/" in
  has "the same session keeps its user" again.html "you are Ada";
  let after_all = visit t "/" in
  has "then Ada again for a new session" after_all.html "you are Ada"

let draft title =
  { Draft.empty with title; description = "d"; priority = Priority.High }

let test_create_over_http () =
  let t = make () in
  let v = visit t "/" in
  let created =
    decode create_endpoint
      (post t v "/api/issues/create"
         (encode create_endpoint (draft "From HTTP"))
      )
  in
  Alcotest.(check int) "number" 31 created.number;
  Alcotest.(check string) "title" "From HTTP" created.title;
  let list = decode issues_endpoint (post t v "/api/issues" "null") in
  Alcotest.(check int) "stored" 31 (List.length list);
  let page = visit ~cookie:v.cookie t "/" in
  has "a new page shows it" page.html "From HTTP"

let test_create_rejects_bad_drafts () =
  let t = make () in
  let v = visit t "/" in
  let r =
    post t v "/api/issues/create" (encode create_endpoint (draft "   "))
  in
  Alcotest.(check int) "empty title is refused" 422 r.status;
  Alcotest.(check (result string string))
    "the answer is the message of the store" (Ok "Title is required")
    (Silica.Codec.of_string (Silica.Remote.rejected create_endpoint) r.body);
  let list = decode issues_endpoint (post t v "/api/issues" "null") in
  Alcotest.(check int) "nothing stored" 30 (List.length list);
  let no_csrf =
    Server.handle t
      (request ~meth:"POST"
         ~body:(encode create_endpoint (draft "x"))
         ~headers:
           [
             ("cookie", v.cookie);
             ("origin", origin);
             ("content-type", "application/json");
           ]
         "/api/issues/create"
      )
  in
  Alcotest.(check int) "no token" 403 no_csrf.status

type socket = { sent : string list ref; mutable closed : int option }

let connect ?(name = "issues") ?(key = "all") t (v : visitor) =
  match
    Server.upgrade t
      (request
         ~headers:[ ("cookie", v.cookie); ("origin", origin) ]
         "/_silica/live"
      )
  with
  | Server.Refused r ->
      Alcotest.failf "refused with %d" r.status
  | Server.Accepted attach ->
      let sock = { sent = ref []; closed = None } in
      let connection =
        attach
          {
            Server.send = (fun s -> sock.sent := s :: !(sock.sent));
            close = (fun code -> sock.closed <- Some code);
          }
      in
      connection.receive
        (Protocol.encode_client
           (Protocol.Hello
              {
                v = Protocol.version;
                csrf = v.csrf;
                sub = Protocol.Topic { name; key };
              }
           )
        );
      (sock, connection)

let states sock =
  List.rev !(sock.sent)
  |> List.filter_map (fun text ->
      match Protocol.decode_server text with
      | Ok (Protocol.State { seq; state; _ }) -> (
          match Protocol.decode_state (Topic.state issues_topic) state with
          | Ok issues ->
              Some (seq, List.length issues)
          | Error message ->
              Alcotest.fail message
        )
      | _ ->
          None
  )

let test_topic_between_visitors () =
  let t = make () in
  let ada = visit t "/" in
  let ben = visit t "/" in
  let ada_socket, _ = connect t ada in
  let ben_socket, _ = connect t ben in
  Alcotest.(check (list (pair int int)))
    "first states"
    [ (1, 30) ]
    (states ada_socket);
  ignore
    (post t ben "/api/issues/create" (encode create_endpoint (draft "Live one")));
  Alcotest.(check (list (pair int int)))
    "Ada gets the new list"
    [ (1, 30); (2, 31) ]
    (states ada_socket);
  Alcotest.(check (list (pair int int)))
    "Ben gets it too"
    [ (1, 30); (2, 31) ]
    (states ben_socket);
  ignore
    (post t ada "/api/issues/create" (encode create_endpoint (draft "Another")));
  Alcotest.(check (list (pair int int)))
    "seq grows"
    [ (1, 30); (2, 31); (3, 32) ]
    (states ada_socket);
  let late, _ = connect t (visit t "/") in
  Alcotest.(check (list (pair int int)))
    "a late visitor starts from the current state"
    [ (3, 32) ]
    (states late)

let test_update_over_http () =
  let t = make () in
  let ada = visit t "/" in
  let ben = visit t "/" in
  let ben_socket, _ = connect t ben in
  let update base change =
    decode update_endpoint
      (post t ada "/api/issues/update"
         (encode update_endpoint { Change.number = 2; base; change })
      )
  in
  ( match update 1 (Change.Priority Priority.Urgent) with
  | Change.Saved issue ->
      Alcotest.(check int) "version 2" 2 issue.version
  | Change.Conflict _ ->
      Alcotest.fail "expected Saved"
  );
  Alcotest.(check (list (pair int int)))
    "a saved change is published"
    [ (1, 30); (2, 30) ]
    (states ben_socket);
  ( match update 1 (Change.Title "Stale") with
  | Change.Conflict issue ->
      Alcotest.(check int) "the current version" 2 issue.version
  | Change.Saved _ ->
      Alcotest.fail "expected Conflict"
  );
  Alcotest.(check (list (pair int int)))
    "a conflict is not published"
    [ (1, 30); (2, 30) ]
    (states ben_socket);
  let refused =
    post t ada "/api/issues/update"
      (encode update_endpoint
         { Change.number = 2; base = 2; change = Change.Title " " }
      )
  in
  Alcotest.(check int) "a blank title is refused" 422 refused.status

let comment_bodies sock =
  List.rev !(sock.sent)
  |> List.filter_map (fun text ->
      match Protocol.decode_server text with
      | Ok (Protocol.State { state; _ }) -> (
          match Protocol.decode_state (Topic.state comments_topic) state with
          | Ok comments ->
              Some (List.map (fun (c : Comment.t) -> c.body) comments)
          | Error message ->
              Alcotest.fail message
        )
      | _ ->
          None
  )

let test_comments_over_http () =
  let t = make () in
  let ada = visit t "/" in
  let ben = visit t "/" in
  let ben_socket, _ = connect ~name:"comments" ~key:"3" t ben in
  let thread = decode comments_endpoint (post t ada "/api/comments" "3") in
  Alcotest.(check bool) "the viewer is Ada" true (thread.viewer = User.Ada);
  let added =
    decode add_comment_endpoint
      (post t ada "/api/comments/add"
         (encode add_comment_endpoint { New_comment.number = 3; body = "Hi" })
      )
  in
  Alcotest.(check bool)
    "the author is the session user" true (added.author = User.Ada);
  Alcotest.(check (list (list string)))
    "published to the issue's key" [ []; [ "Hi" ] ]
    (comment_bodies ben_socket);
  let refused =
    post t ben "/api/comments/edit"
      (encode edit_comment_endpoint { Comment_edit.id = added.id; body = "Ben" })
  in
  Alcotest.(check int) "Ben cannot edit it" 422 refused.status;
  let edited =
    decode edit_comment_endpoint
      (post t ada "/api/comments/edit"
         (encode edit_comment_endpoint
            { Comment_edit.id = added.id; body = "Hi, edited" }
         )
      )
  in
  Alcotest.(check bool) "edited" true edited.edited;
  Alcotest.(check (list (list string)))
    "the edit is published"
    [ []; [ "Hi" ]; [ "Hi, edited" ] ]
    (comment_bodies ben_socket)

let test_unknown_topic_key () =
  let t = make () in
  let v = visit t "/" in
  match
    Server.upgrade t
      (request
         ~headers:[ ("cookie", v.cookie); ("origin", origin) ]
         "/_silica/live"
      )
  with
  | Server.Refused _ ->
      Alcotest.fail "refused"
  | Server.Accepted attach ->
      let sock = { sent = ref []; closed = None } in
      let connection =
        attach
          {
            Server.send = (fun s -> sock.sent := s :: !(sock.sent));
            close = (fun code -> sock.closed <- Some code);
          }
      in
      connection.receive
        (Protocol.encode_client
           (Protocol.Hello
              {
                v = Protocol.version;
                csrf = v.csrf;
                sub = Protocol.Topic { name = "issues"; key = "other" };
              }
           )
        );
      Alcotest.(check (list (pair int int))) "no state" [] (states sock);
      Alcotest.(check bool) "closed" true (sock.closed <> None)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "silica linear server"
    [
      ( "pages",
        [
          t "the page is settled with issues and user" test_page_is_settled;
          t "detail, unknown issue and unknown path" test_detail_page;
          t "nav link, route focus and preload in the html" test_route_features;
          t "users in turn, kept by the session" test_users_in_turn;
          t "the first page and the session agree on the user"
            test_first_page_user_is_the_session_user;
        ]
      );
      ( "api",
        [
          t "create over HTTP" test_create_over_http;
          t "bad drafts and missing token" test_create_rejects_bad_drafts;
          t "update, conflict and refusal" test_update_over_http;
          t "comments: add, edit own, live" test_comments_over_http;
        ]
      );
      ( "topic",
        [
          t "a create reaches every subscriber" test_topic_between_visitors;
          t "an unknown key closes the socket" test_unknown_topic_key;
        ]
      );
    ]
