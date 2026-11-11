open Silica

let pending : (unit -> unit) Queue.t = Queue.create ()
let sleeps = ref 0
let awaits : float list ref = ref []
let stall = ref false

module Io = struct
  type 'a t = 'a

  let return x = x
  let bind x f = f x
  let sleep _ = incr sleeps
  let async f = Queue.add f pending
  let now () = 0.

  type 'a promise = 'a option ref

  let promise () = ref None
  let resolve p v = if !p = None then p := Some v

  let await p ~timeout =
    awaits := timeout :: !awaits;
    if not !stall then
      while not (Queue.is_empty pending) do
        (Queue.take pending) ()
      done;
    !p
end

module Server = Silica_server.Make (Io)

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

let check_contains message s sub =
  if not (contains s sub) then Alcotest.failf "%s: %S not in %s" message sub s

let check_absent message s sub =
  if contains s sub then Alcotest.failf "%s: %S in %s" message sub s

let double =
  Remote.declare ~path:"/api/double" ~input:Codec.int ~output:Codec.int
let triple =
  Remote.declare ~path:"/api/triple" ~input:Codec.int ~output:Codec.int

module Lookup = struct
  type event = Answered of (int, Remote.error) result

  module States = struct
    type _ t = Loading : int t | Ready : int t | Failed : string t
  end

  module Lookup_machine = Machine.Make (States)

  let machine ~id ~endpoint ~input =
    let to_ = Lookup_machine.[ States.Ready; States.Failed ] in
    Lookup_machine.make ~id
      ~init:(Lookup_machine.at States.Loading input)
      [
        Lookup_machine.state States.Loading "loading" ~to_
          ~entry:(fun _ -> [ Action.Set_title "loading" ])
          ~invoke:
            [
              Source.call endpoint (fun n -> n) (fun result -> Answered result);
            ]
          (fun _ -> function
            | Answered (Ok n) ->
                Lookup_machine.goto States.Ready n
            | Answered (Error _) ->
                Lookup_machine.goto States.Failed "failed"
            );
        Lookup_machine.state States.Ready "ready"
          ~entry:(fun n -> [ Action.Set_title (Printf.sprintf "ready %d" n) ])
          Lookup_machine.ignore;
        Lookup_machine.state States.Failed "failed" Lookup_machine.ignore;
      ]

  let view id (Lookup_machine.At (state, data) : Lookup_machine.snapshot) :
      event Node.t =
    match state with
    | States.Loading ->
        Node.element "p" []
          [ Node.text (id ^ " loading"); Node.text (string_of_int data) ]
    | States.Ready ->
        Node.element "p" [] [ Node.text (Printf.sprintf "%s ready %d" id data) ]
    | States.Failed ->
        Node.element "p" [] [ Node.text (id ^ " " ^ data) ]

  let component ~id ~endpoint ~input =
    Component.make (machine ~id ~endpoint ~input) (view id)
end

module Page = struct
  type event = Nothing

  module States = struct
    type _ t = Page : unit t
  end

  module Page_machine = Machine.Make (States)

  let machine =
    Page_machine.make ~id:"page"
      ~init:(Page_machine.at States.Page ())
      [ Page_machine.state States.Page "page" Page_machine.ignore ]

  let first = Lookup.component ~id:"first" ~endpoint:double ~input:4
  let second = Lookup.component ~id:"second" ~endpoint:triple ~input:5

  let component =
    Component.make machine (fun _ ->
        Node.element "main" []
          [
            Node.text "top";
            Node.text "bottom";
            Component.node first |> Node.map (fun (_ : Lookup.event) -> Nothing);
            Component.node second |> Node.map (fun (_ : Lookup.event) -> Nothing);
          ]
    )
end

let context =
  {
    Server.request =
      {
        Silica_server.meth = "GET";
        path = "/";
        query = [];
        headers = [];
        body = "";
      };
    session =
      {
        Silica_server.Session.id = "s";
        created = 0.;
        last_seen = 0.;
        data = [];
      };
  }

let handlers ~calls =
  [
    Server.implement double (fun _ n ->
        incr calls;
        Ok (n * 2)
    );
    Server.implement triple (fun _ n ->
        incr calls;
        Ok (n * 3)
    );
  ]

let render ?(timeout = 1000) handlers =
  Server.render_page_settled
    (fun (_ : Url.t) -> Page.component)
    ~status:(fun _ -> 200)
    ~scripts:[ "/app.js" ] ~settle_timeout_ms:timeout ~handlers ~context
    (Url.of_string "/")

let reset () =
  Queue.clear pending;
  sleeps := 0;
  awaits := [];
  stall := false

let answers_of (response : Silica_server.response) =
  let marker = "id=\"silica-answers\">" in
  let n = String.length marker in
  let rec find i =
    if i + n > String.length response.body then
      Alcotest.fail "no answers element"
    else if String.sub response.body i n = marker then
      i + n
    else
      find (i + 1)
  in
  let start = find 0 in
  let stop = String.index_from response.body start '<' in
  match Answers.of_string (String.sub response.body start (stop - start)) with
  | Ok table ->
      table
  | Error message ->
      Alcotest.fail message

let test_settle_answers_and_embeds () =
  reset ();
  let calls = ref 0 in
  let response = render (handlers ~calls) in
  Alcotest.(check int) "status" 200 response.status;
  check_contains "answered state is in the HTML" response.body "first ready 8";
  check_contains "every component settles" response.body "second ready 15";
  check_absent "no loading state" response.body "loading";
  check_contains "last title" response.body "<title>ready 15</title>";
  Alcotest.(check int) "each call ran once" 2 !calls;
  let table = answers_of response in
  Alcotest.(check (option string))
    "double answer" (Some "8")
    (Answers.find table ~path:"/api/double" ~input:"4");
  Alcotest.(check (option string))
    "triple answer" (Some "15")
    (Answers.find table ~path:"/api/triple" ~input:"5");
  check_contains "markers between texts" response.body "top<!-- -->bottom";
  check_contains "answers come before the module scripts" response.body
    "</script><script type=\"module\" src=\"/app.js\">"

let test_settle_nonce () =
  reset ();
  let calls = ref 0 in
  let response =
    Server.render_page_settled
      (fun (_ : Url.t) -> Page.component)
      ~status:(fun _ -> 200)
      ~scripts:[] ~nonce:"abc" ~handlers:(handlers ~calls) ~context
      (Url.of_string "/")
  in
  check_contains "nonce" response.body "id=\"silica-answers\" nonce=\"abc\""

let test_failed_call_stays_loading () =
  reset ();
  let calls = ref 0 in
  let handlers =
    [
      Server.implement double (fun _ _ ->
          incr calls;
          Error "down"
      );
      Server.implement triple (fun _ n ->
          incr calls;
          Ok (n * 3)
      );
    ]
  in
  let response = render handlers in
  check_contains "failed call shows its loading state" response.body
    "first loading";
  check_contains "the other call is settled" response.body "second ready 15";
  let table = answers_of response in
  Alcotest.(check (option string))
    "no answer for the failed call" None
    (Answers.find table ~path:"/api/double" ~input:"4")

let test_missing_handler_stays_loading () =
  reset ();
  let calls = ref 0 in
  let response = render [ List.nth (handlers ~calls) 1 ] in
  check_contains "no handler" response.body "first loading";
  check_contains "handled" response.body "second ready 15"

let test_timeout_sends_the_loading_page () =
  reset ();
  stall := true;
  let calls = ref 0 in
  let response = render ~timeout:50 (handlers ~calls) in
  Alcotest.(check int) "calls never ran" 0 !calls;
  check_contains "loading state" response.body "first loading";
  check_absent "no answers" response.body "silica-answers";
  Alcotest.(check int) "never slept" 0 !sleeps;
  Alcotest.(check (list (float 1e-9)))
    "one wait for the whole step, with the full time" [ 0.05 ] !awaits

let test_no_polling () =
  reset ();
  let calls = ref 0 in
  let response = render (handlers ~calls) in
  check_contains "settled" response.body "first ready 8";
  Alcotest.(check int) "never slept" 0 !sleeps;
  Alcotest.(check bool)
    "each wait is bounded by the settle timeout" true
    (!awaits <> [] && List.for_all (fun t -> t > 0. && t <= 1.) !awaits)

let test_no_calls_no_script () =
  reset ();
  let response =
    Server.render_page_settled
      (fun (_ : Url.t) ->
        Component.make Page.machine (fun _ -> Node.text "static")
      )
      ~status:(fun _ -> 200)
      ~scripts:[] ~handlers:[] ~context (Url.of_string "/")
  in
  check_contains "text" response.body "static";
  check_absent "no answers element" response.body "silica-answers"

module Chain = struct
  type event =
    | Got_a of (int, Remote.error) result
    | Got_b of (int, Remote.error) result

  module States = struct
    type _ t = First : int t | Second : int t | Done : int t
  end

  module Chain_machine = Machine.Make (States)

  let machine =
    let to_ = Chain_machine.[ States.Second; States.Done ] in
    Chain_machine.make ~id:"chain"
      ~init:(Chain_machine.at States.First 1)
      [
        Chain_machine.state States.First "first" ~to_
          ~invoke:[ Source.call double (fun n -> n) (fun r -> Got_a r) ]
          (fun _ -> function
            | Got_a (Ok a) ->
                Chain_machine.goto States.Second a
            | _ ->
                Chain_machine.unhandled
            );
        Chain_machine.state States.Second "second" ~to_
          ~invoke:[ Source.call triple (fun n -> n) (fun r -> Got_b r) ]
          (fun _ -> function
            | Got_b (Ok b) ->
                Chain_machine.goto States.Done b
            | _ ->
                Chain_machine.unhandled
            );
        Chain_machine.state States.Done "done" Chain_machine.ignore;
      ]

  let component =
    Component.make machine (fun (Chain_machine.At (state, data)) ->
        Node.text
          ( match state with
          | States.First ->
              "first"
          | States.Second ->
              "second " ^ string_of_int data
          | States.Done ->
              "done " ^ string_of_int data
          )
    )
end

let test_chained_calls_settle () =
  reset ();
  let calls = ref 0 in
  let response =
    Server.render_page_settled
      (fun (_ : Url.t) -> Chain.component)
      ~status:(fun _ -> 200)
      ~scripts:[] ~handlers:(handlers ~calls) ~context (Url.of_string "/")
  in
  check_contains "second call ran with the first answer" response.body "done 6";
  Alcotest.(check int) "two calls" 2 !calls;
  let table = answers_of response in
  Alcotest.(check (option string))
    "second answer" (Some "6")
    (Answers.find table ~path:"/api/triple" ~input:"2")

module Bad = struct
  type event = Nothing

  module States = struct
    type _ t = Only : unit t
  end

  module Bad_machine = Machine.Make (States)

  let machine =
    Bad_machine.make ~id:"bad"
      ~init:(Bad_machine.at States.Only ())
      [ Bad_machine.state States.Only "only" Bad_machine.ignore ]

  let component =
    Component.make machine (fun _ -> failwith "secret detail of the view")

  let guarded =
    Component.make machine (fun _ ->
        Node.element "main" []
          [
            Node.boundary
              ~fallback:(fun (error : Node.error) ~reset:_ ->
                Node.text ("fallback " ^ error.message)
              )
              (Component.node component |> Node.map (fun (_ : event) -> Nothing));
          ]
    )
end

let test_render_error_gives_500_and_calls_on_error () =
  let seen = ref [] in
  let on_error exn (error : Node.error) =
    seen := (Printexc.to_string exn, error.id) :: !seen
  in
  let response =
    Server.render_page
      (fun (_ : Url.t) -> Bad.component)
      ~status:(fun _ -> 200)
      ~scripts:[] ~on_error (Url.of_string "/")
  in
  Alcotest.(check int) "status 500" 500 response.status;
  ( match !seen with
  | [ (text, id) ] ->
      Alcotest.(check string)
        "hook got the exception" "Failure(\"secret detail of the view\")" text;
      check_contains "the page names the same id" response.body id
  | other ->
      Alcotest.failf "on_error ran %d times" (List.length other)
  );
  check_absent "prod hides the exception" response.body "secret detail";
  check_contains "short message" response.body "Something went wrong";
  let dev =
    Server.render_page
      (fun (_ : Url.t) -> Bad.component)
      ~status:(fun _ -> 200)
      ~scripts:[] ~dev:true ~on_error (Url.of_string "/")
  in
  Alcotest.(check int) "dev status 500" 500 dev.status;
  check_contains "dev shows the exception" dev.body "secret detail";
  check_contains "dev shows the machine" dev.body "machine: bad"

let test_render_error_in_settled_page () =
  reset ();
  let seen = ref 0 in
  let response =
    Server.render_page_settled
      (fun (_ : Url.t) -> Bad.component)
      ~status:(fun _ -> 200)
      ~scripts:[]
      ~on_error:(fun _ _ -> incr seen)
      ~handlers:[] ~context (Url.of_string "/")
  in
  Alcotest.(check int) "status 500" 500 response.status;
  Alcotest.(check int) "hook" 1 !seen

let test_boundary_error_keeps_the_page () =
  let seen = ref 0 in
  let response =
    Server.render_page
      (fun (_ : Url.t) -> Bad.guarded)
      ~status:(fun _ -> 200)
      ~scripts:[]
      ~on_error:(fun _ _ -> incr seen)
      (Url.of_string "/")
  in
  Alcotest.(check int) "the page still answers" 200 response.status;
  check_contains "fallback is in the HTML" response.body
    "fallback Something went wrong.";
  check_absent "no exception text" response.body "secret detail";
  Alcotest.(check int) "the hook ran" 1 !seen

let test_render_component_error () =
  let response =
    Server.render_component Bad.component ~title:"t" ~scripts:[]
      ~on_error:(fun _ _ -> ())
      ()
  in
  Alcotest.(check int) "status 500" 500 response.status

module Gate = struct
  type _ t =
    | Home : unit t
    | Login : unit t
    | Secret : unit t
    | Lost : string t

  let path : type d. d t -> d Route.t = function
    | Home ->
        Route.root
    | Login ->
        Route.s "login"
    | Secret ->
        Route.s "secret"
    | Lost ->
        Route.rest
end

module Gate_router = Router.Make (Gate)

let gate_routes =
  Gate_router.routes ~not_found:Gate.Lost Gate.[ Home; Login; Secret; Lost ]

let gate_app url =
  Gate_router.make gate_routes
    ~guard:(fun ~from ~target ->
      match (from, target) with
      | None, Gate_router.At (Gate.Secret, ()) ->
          Gate_router.Redirect (Gate_router.At (Gate.Login, ()))
      | _ ->
          Gate_router.Allow
    )
    ~render:(fun _ -> Node.text "gate")
    url

let header name (response : Silica_server.response) =
  List.assoc_opt name response.headers

module Only = struct
  type _ t = Only : unit t
end

module Only_machine = Machine.Make (Only)

let starting ~id actions : (Only_machine.snapshot, unit) Component.t =
  Component.make
    (Only_machine.make ~id
       ~init:(Only_machine.at Only.Only ())
       [
         Only_machine.state Only.Only "only"
           ~entry:(fun () -> actions)
           Only_machine.ignore;
       ]
    )
    (fun _ -> Node.text "body")

let test_guard_redirect_at_first_url () =
  let r =
    Server.render_page gate_app
      ~status:(fun _ -> 200)
      ~scripts:[] (Url.of_string "/secret")
  in
  Alcotest.(check int) "303" 303 r.status;
  Alcotest.(check (option string))
    "location" (Some "/login") (header "location" r);
  Alcotest.(check string) "no body" "" r.body;
  let r =
    Server.render_page gate_app
      ~status:(fun _ -> 200)
      ~scripts:[] (Url.of_string "/")
  in
  Alcotest.(check int) "allowed" 200 r.status;
  Alcotest.(check (option string)) "no location" None (header "location" r)

let test_navigate_during_render_redirects () =
  let r =
    Server.render_component
      (starting ~id:"go" [ Action.Navigate (Url.of_string "/login?next=%2Fa") ])
      ~title:"t" ~scripts:[] ()
  in
  Alcotest.(check int) "303" 303 r.status;
  Alcotest.(check (option string))
    "location" (Some "/login?next=%2Fa") (header "location" r)

let test_set_status_last_wins () =
  let r =
    Server.render_page
      (fun _ ->
        starting ~id:"st" [ Action.Set_status 404; Action.Set_status 410 ]
      )
      ~status:(fun _ -> 200)
      ~scripts:[] (Url.of_string "/")
  in
  Alcotest.(check int) "the last Set_status" 410 r.status;
  check_contains "the page renders" r.body "body";
  let r =
    Server.render_page
      (fun _ -> starting ~id:"st" [])
      ~status:(fun _ -> 201)
      ~scripts:[] (Url.of_string "/")
  in
  Alcotest.(check int) "the status function without one" 201 r.status;
  let r =
    Server.render_component
      (starting ~id:"st" [ Action.Set_status 404 ])
      ~title:"t" ~scripts:[] ()
  in
  Alcotest.(check int) "a component page" 404 r.status

let test_document_attributes_on_html () =
  let attr name value = Action.Set_document_attribute { name; value } in
  let r =
    Server.render_page
      (fun _ ->
        starting ~id:"da"
          [
            attr "data-theme" (Some "dark");
            attr "lang" (Some "en");
            attr "data-theme" (Some "contrast\"<x");
            attr "lang" None;
            Action.Set_cookie { name = "c"; value = "v"; max_age = 1 };
          ]
      )
      ~status:(fun _ -> 200)
      ~scripts:[] (Url.of_string "/")
  in
  check_contains "the last value per name, escaped, on <html>" r.body
    "<html data-theme=\"contrast&quot;&lt;x\"><head>";
  Alcotest.(check bool)
    "a removed attribute is not written" false (contains r.body "lang=");
  Alcotest.(check bool)
    "a cookie action sets no header" true
    (List.for_all (fun (name, _) -> name <> "set-cookie") r.headers);
  let r =
    Server.render_component
      (starting ~id:"da" [ attr "data-theme" (Some "light") ])
      ~title:"t" ~scripts:[] ()
  in
  check_contains "a component page" r.body "<html data-theme=\"light\">";
  List.iter
    (fun name ->
      match
        Server.render_page
          (fun _ -> starting ~id:"da" [ attr name (Some "x") ])
          ~status:(fun _ -> 200)
          ~scripts:[] (Url.of_string "/")
      with
      | exception Invalid_argument _ ->
          ()
      | r ->
          Alcotest.(check int)
            ("a bad attribute name is an error: " ^ name)
            500 r.status
    )
    [ "on\"click"; "9a"; "-a"; ""; "Data-x" ]

let test_cookie_reader () =
  let request cookie : Silica_server.request =
    {
      meth = "GET";
      path = "/";
      query = [];
      headers = [ ("cookie", cookie) ];
      body = "";
    }
  in
  Alcotest.(check (option string))
    "the named cookie" (Some "dark")
    (Silica_server.cookie (request "a=1; silica-theme=dark; b=2") "silica-theme");
  Alcotest.(check (option string))
    "no prefix match" None
    (Silica_server.cookie (request "xsilica-theme=dark") "silica-theme")

module After = struct
  type event = Answered of (int, Remote.error) result

  module States = struct
    type _ t = Loading : unit t | Done : unit t
  end

  module After_machine = Machine.Make (States)

  let component on_answer =
    Component.make
      (After_machine.make ~id:"after"
         ~init:(After_machine.at States.Loading ())
         [
           After_machine.state States.Loading "loading"
             ~to_:After_machine.[ States.Done ]
             ~invoke:[ Source.call double (fun () -> 1) (fun r -> Answered r) ]
             (fun () -> function
               | Answered _ ->
                   After_machine.goto ~actions:on_answer States.Done ()
               );
           After_machine.state States.Done "done" After_machine.ignore;
         ]
      )
      (fun _ -> Node.text "after")
end

let test_settled_status_and_redirect () =
  reset ();
  let calls = ref 0 in
  let settled actions =
    Server.render_page_settled
      (fun (_ : Url.t) -> After.component actions)
      ~status:(fun _ -> 200)
      ~scripts:[] ~handlers:(handlers ~calls) ~context (Url.of_string "/")
  in
  let r = settled [ Action.Set_status 404 ] in
  Alcotest.(check int) "Set_status after an answer" 404 r.status;
  let r = settled [ Action.Replace (Url.of_string "/elsewhere") ] in
  Alcotest.(check int) "redirect after an answer" 303 r.status;
  Alcotest.(check (option string))
    "location" (Some "/elsewhere") (header "location" r)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "silica settle"
    [
      ( "settle",
        [
          t "answers every component and embeds them"
            test_settle_answers_and_embeds;
          t "nonce on the answers element" test_settle_nonce;
          t "a failed call stays loading" test_failed_call_stays_loading;
          t "a call with no handler stays loading"
            test_missing_handler_stays_loading;
          t "timeout sends the loading page" test_timeout_sends_the_loading_page;
          t "waits on a promise, never polls" test_no_polling;
          t "no calls, no script" test_no_calls_no_script;
          t "a call that an answer starts is settled too"
            test_chained_calls_settle;
        ]
      );
      ( "redirect and status",
        [
          t "a guard redirect at the first URL is a 303"
            test_guard_redirect_at_first_url;
          t "a Navigate during the render is a 303"
            test_navigate_during_render_redirects;
          t "the last Set_status wins over the status function"
            test_set_status_last_wins;
          t "Set_status and redirect after a settled answer"
            test_settled_status_and_redirect;
          t "Set_document_attribute is written on <html>"
            test_document_attributes_on_html;
          t "cookie reads the request" test_cookie_reader;
        ]
      );
      ( "errors",
        [
          t "render error: 500 and on_error"
            test_render_error_gives_500_and_calls_on_error;
          t "render error in the settled page" test_render_error_in_settled_page;
          t "a boundary keeps the page" test_boundary_error_keeps_the_page;
          t "render_component error" test_render_component_error;
        ]
      );
    ]
