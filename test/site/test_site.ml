module Server = Silica_adapter.Server

let metafile =
  {|{"outputs":{"dist/client_main-AAAAAAAA.js":{"entryPoint":"out/client_main.mjs"},"dist/app-BBBBBBBB.css":{"entryPoint":"app.css"}}}|}

let server =
  lazy
    (let config =
       Silica_server.default_config ~origins:Silica_server.Localhost
         ~secret:"0123456789abcdef0123456789abcdef"
     in
     match Silica_server.Assets.of_metafile ~prefix:"/static/" metafile with
     | Ok assets ->
         Site_server.create config ~assets ~read:(fun _ -> None) ()
     | Error message ->
         failwith message
    )

let get ?(headers = []) path =
  Server.handle (Lazy.force server)
    { Silica_server.meth = "GET"; path; query = []; headers; body = "" }

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let between ~start ~stop text from =
  match Str.search_forward (Str.regexp_string start) text from with
  | exception Not_found ->
      None
  | i ->
      let a = i + String.length start in
      let b = String.index_from text a stop in
      Some (String.sub text a (b - a), b)

let title html =
  match between ~start:"<title>" ~stop:'<' html 0 with
  | Some (t, _) ->
      t
  | None ->
      Alcotest.failf "no title in %s" html

let all ~start ~stop html =
  let rec go from acc =
    match between ~start ~stop html from with
    | Some (v, next) ->
        go next (v :: acc)
    | None ->
        List.rev acc
  in
  go 0 []

let doc_slugs = List.map (fun (slug, _, _) -> slug) Site_docs_index.pages

let doc_path slug =
  if slug = Site_routes.index_slug then
    "/docs"
  else
    "/docs/" ^ slug

let routes =
  [ ("/", "Silica: views that are statecharts", "Views that are statecharts.") ]
  @ List.map
      (fun (slug, page_title, _) ->
        ( doc_path slug,
          ( if slug = Site_routes.index_slug then
              "Documentation"
            else
              page_title
          )
          ^ " · Silica",
          "<h1"
        )
      )
      Site_docs_index.pages
  @ [ ("/components", "Components · Silica", "id=\"component-list\"") ]
  @ List.map
      (fun (e : Site_components.info) ->
        ( "/components/" ^ e.slug,
          e.name ^ " · Silica",
          String.sub e.summary 0 (min 40 (String.length e.summary))
        )
      )
      Site_components.all

let escape text =
  let b = Buffer.create (String.length text) in
  String.iter
    (function
      | '&' ->
          Buffer.add_string b "&amp;"
      | '<' ->
          Buffer.add_string b "&lt;"
      | '>' ->
          Buffer.add_string b "&gt;"
      | '"' ->
          Buffer.add_string b "&quot;"
      | c ->
          Buffer.add_char b c
      )
    text;
  Buffer.contents b

let test_routes () =
  List.iter
    (fun (path, expected_title, key) ->
      let r = get path in
      Alcotest.(check int) (path ^ ": status") 200 r.status;
      Alcotest.(check string)
        (path ^ ": title") (escape expected_title) (title r.body);
      if not (contains r.body (escape key) || contains r.body key) then
        Alcotest.failf "%s: %S not in the page" path key
    )
    routes

let test_doc_content_is_in_the_html () =
  List.iter
    (fun (slug, _, headings) ->
      let r = get (doc_path slug) in
      List.iter
        (fun (_, anchor, _) ->
          if not (contains r.body ("id=\"" ^ anchor ^ "\"")) then
            Alcotest.failf "%s: no heading %s in the server HTML" slug anchor
        )
        headings;
      if not (contains r.body "id=\"silica-answers\"") then
        Alcotest.failf "%s: the answer of /api/doc is not in the page" slug
    )
    Site_docs_index.pages

let test_not_found () =
  List.iter
    (fun (path, key) ->
      let r = get path in
      Alcotest.(check int) (path ^ ": status") 404 r.status;
      if not (contains r.body key) then Alcotest.failf "%s: %S missing" path key
    )
    [
      ("/docs/no-such-page", "There is no documentation page with this name");
      ("/components/no-such-component", "No such component");
      ("/no/such/address", "Nothing at this address");
    ]

let test_theme_cookie () =
  let r = get ~headers:[ ("cookie", "silica-theme=ocean") ] "/" in
  if not (contains r.body "<html data-theme=\"ocean\"") then
    Alcotest.fail "the theme of the cookie is not on <html>";
  let r = get "/" in
  if not (contains r.body "<html data-theme=\"system\"") then
    Alcotest.fail "no cookie: data-theme is not system"

let test_doc_endpoint () =
  Alcotest.(check bool)
    "missing page" true
    (Site_server.doc "nope" = Site_doc.Missing);
  match Site_server.doc "router" with
  | Site_doc.Page page ->
      Alcotest.(check string) "slug" "router" page.slug
  | Site_doc.Missing ->
      Alcotest.fail "router page missing"

let split_link href =
  match String.index_opt href '#' with
  | Some i ->
      ( String.sub href 0 i,
        Some (String.sub href (i + 1) (String.length href - i - 1))
      )
  | None ->
      (href, None)

let strip_query path =
  match String.index_opt path '?' with
  | Some i ->
      String.sub path 0 i
  | None ->
      path

let link_problems () =
  let pages = Hashtbl.create 64 in
  let page path =
    match Hashtbl.find_opt pages path with
    | Some r ->
        r
    | None ->
        let r = get path in
        Hashtbl.add pages path r;
        r
  in
  let problems = ref [] and checked = ref 0 in
  List.iter
    (fun (from, _, _) ->
      let html = (page from).body in
      List.iter
        (fun href ->
          let path, hash = split_link href in
          let internal = path = "" || String.starts_with ~prefix:"/" path in
          if internal && not (String.starts_with ~prefix:"/static/" path) then (
            incr checked;
            let path =
              if path = "" then
                from
              else
                strip_query path
            in
            let target = page path in
            if target.status <> 200 then
              problems :=
                Printf.sprintf "%s -> %s: status %d" from href target.status
                :: !problems
            else
              match hash with
              | Some id when not (contains target.body ("id=\"" ^ id ^ "\"")) ->
                  problems :=
                    Printf.sprintf "%s -> %s: no element with id %s" from href
                      id
                    :: !problems
              | _ ->
                  ()
          )
        )
        (all ~start:"href=\"" ~stop:'"' html)
    )
    routes;
  (!checked, List.rev !problems)

let test_links () =
  let checked, problems = link_problems () in
  Alcotest.(check bool) "links were found" true (checked > 200);
  Alcotest.(check (list string)) "dead internal links" [] problems

let helpers = [ "popup.mli"; "style.mli"; "story.mli" ]

let test_doc_links () =
  List.iter
    (fun (page, href) ->
      if not (contains (get page).body ("href=\"" ^ href ^ "\"")) then
        Alcotest.failf
          "%s: a link to a label of another page keeps its anchor: %s" page href
    )
    [
      ("/docs/data", "/docs/tutorial#part2");
      ("/docs/components", "/docs/checker#children");
    ];
  List.iter
    (fun (path, _, _) ->
      if contains (get path).body "href=\"/docs/index" then
        Alcotest.failf "%s links to /docs/index, not /docs" path
    )
    routes;
  let rec pairs = function
    | a :: (b :: _ as rest) ->
        (a, b) :: pairs rest
    | _ ->
        []
  in
  List.iter
    (fun (a, b) ->
      let html = (get (doc_path a)).body in
      let next =
        List.find_opt
          (fun piece -> contains piece "rel=\"next\"")
          (all ~start:"<a " ~stop:'>' html)
      in
      match next with
      | Some tag when contains tag ("href=\"" ^ doc_path b ^ "\"") ->
          ()
      | _ ->
          Alcotest.failf "%s: the next link is not %s" a b
    )
    (pairs Site_nav.order)

let test_every_component_has_a_page () =
  let dir = "../../ui" in
  let components =
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f ->
        Filename.check_suffix f ".mli" && not (Filename.check_suffix f ".pp.mli")
    )
    |> List.filter (fun f -> not (List.mem f helpers))
    |> List.map (fun f ->
        "Silica_ui." ^ String.capitalize_ascii (Filename.chop_suffix f ".mli")
    )
    |> List.sort compare
  in
  Alcotest.(check bool)
    "components were found" true
    (List.length components = 12);
  let pages =
    List.map (fun (e : Site_components.info) -> e.modules) Site_components.all
  in
  List.iter
    (fun m ->
      if not (List.mem m pages) then Alcotest.failf "%s has no component page" m
    )
    ("Silica_ui.Theme.scope" :: components);
  List.iter
    (fun (e : Site_catalog.entry) ->
      if e.stories = [] then Alcotest.failf "%s has no story" e.info.slug
    )
    Site_catalog.entries

let test_search_targets () =
  let url value =
    match Site_search.target value with
    | Some (Silica.Action.Navigate url) ->
        Silica.Url.to_string url
    | Some _ ->
        Alcotest.failf "%s: not a navigation" value
    | None ->
        Alcotest.failf "%s: no target" value
  in
  Alcotest.(check string) "the docs index" "/docs" (url "doc:index");
  Alcotest.(check string) "a page" "/docs/router" (url "doc:router");
  Alcotest.(check string)
    "a section" "/docs/theming#first-paint"
    (url "doc:theming#first-paint");
  Alcotest.(check string)
    "a component" "/components/switch" (url "component:switch");
  List.iter
    (fun (item : Silica_ui.Combobox.item) ->
      if Site_search.target item.value = None then
        Alcotest.failf "the search item %s goes nowhere" item.value
    )
    Site_search.items

let test_story () =
  let open Silica_ui in
  let story =
    {
      Story.name = "Two words, and more";
      source = "";
      controls =
        [
          Story.toggle "on" ~label:"On" true;
          Story.toggle "off" ~label:"Off" false;
          Story.choice "size" ~label:"Size"
            [ ("s", "Small"); ("l", "Large") ]
            "l";
        ];
      render = (fun ~id:_ _ -> Silica.Node.text "");
    }
  in
  let values = Story.defaults story in
  Alcotest.(check bool) "toggle default true" true (Story.bool values "on");
  Alcotest.(check bool) "toggle default false" false (Story.bool values "off");
  Alcotest.(check string) "choice default" "l" (Story.string values "size");
  let changed =
    Story.set_string (Story.set_bool values "off" true) "size" "s"
  in
  Alcotest.(check bool) "set_bool" true (Story.bool changed "off");
  Alcotest.(check bool) "others kept" true (Story.bool changed "on");
  Alcotest.(check string) "set_string" "s" (Story.string changed "size");
  Alcotest.(check string) "slug" "two-words-and-more" (Story.slug story);
  Alcotest.check_raises "a choice default must be an option"
    (Invalid_argument "Story.choice size: \"m\" is not an option") (fun () ->
      ignore (Story.choice "size" ~label:"Size" [ ("s", "Small") ] "m")
  )

let test_facts () =
  let names (f : Site_facts.t) =
    List.map (fun (s : Site_facts.state) -> s.name) f.states
  in
  Alcotest.(check (list string))
    "select states" [ "closed"; "open" ] (names Site_facts.select);
  Alcotest.(check (list string))
    "tooltip timers of opening" [ "after 600ms" ]
    (List.find
       (fun (s : Site_facts.state) -> s.name = "opening")
       Site_facts.tooltip.states
    )
      .details;
  List.iter
    (fun (e : Site_catalog.entry) ->
      Option.iter
        (fun (f : Site_facts.t) ->
          if f.states = [] || (List.length f.states > 1 && f.edges = []) then
            Alcotest.failf "%s: no facts" e.info.slug;
          List.iter
            (fun (edge : Site_facts.edge) ->
              if
                not
                  (List.mem edge.from (names f)
                  && List.mem edge.target (names f)
                  )
              then
                Alcotest.failf "%s: edge %s -> %s outside the states"
                  e.info.slug edge.from edge.target
            )
            f.edges
        )
        e.facts
    )
    Site_catalog.entries

let test_landing_chart () =
  let spec = Site_landing_chart.spec in
  let names = List.map (fun (s : Silica.Spec.state) -> s.name) spec.states in
  Alcotest.(check (list string))
    "every state of the save machine"
    (List.sort compare (Silica.Machine.shape_names Landing_save.state_shape))
    (List.sort compare names);
  Alcotest.(check string)
    "the initial state"
    ((Silica.Machine.shape_name Landing_save.state_shape)
       (Silica.Machine.init Landing_save.machine)
    )
    spec.init;
  Alcotest.(check bool) "edges were found" true (spec.edges <> []);
  List.iter
    (fun (from, target) ->
      if not (List.mem from names && List.mem target names) then
        Alcotest.failf "edge %s -> %s outside the states" from target
    )
    spec.edges;
  let html = (get "/").body in
  List.iter
    (fun name ->
      if not (contains html ("data-state=\"" ^ name ^ "\"")) then
        Alcotest.failf "the landing chart has no state %s" name
    )
    names

let test_nav_covers_the_docs () =
  Alcotest.(check (list string))
    "every page is in the sidebar once, and every sidebar entry is a page"
    (List.sort compare doc_slugs)
    (List.sort compare Site_nav.order)

let channel hex at =
  let v = float_of_int (int_of_string ("0x" ^ String.sub hex at 2)) /. 255. in
  if v <= 0.03928 then
    v /. 12.92
  else
    ((v +. 0.055) /. 1.055) ** 2.4

let luminance hex =
  if String.length hex <> 7 || hex.[0] <> '#' then
    Alcotest.failf "not a #rrggbb colour: %s" hex;
  (0.2126 *. channel hex 1)
  +. (0.7152 *. channel hex 3)
  +. (0.0722 *. channel hex 5)

let ratio a b =
  let la = luminance a and lb = luminance b in
  (Float.max la lb +. 0.05) /. (Float.min la lb +. 0.05)

let test_site_token_contrast () =
  List.iter
    (fun theme ->
      let extra = Silica_ui.Theme.extra theme in
      let tokens = Silica_ui.Theme.tokens theme in
      let check name fg bg =
        let r = ratio fg bg in
        if r < 4.5 then
          Alcotest.failf "%s: %s is %.2f:1" (Silica_ui.Theme.name theme) name r
      in
      let code = List.assoc "code-surface" extra in
      List.iter
        (fun t -> check (t ^ " on code-surface") (List.assoc t extra) code)
        [ "code-keyword"; "code-string"; "code-name"; "code-comment" ];
      check "text on code-surface" tokens.text code;
      check "link on surface" (List.assoc "link" extra) tokens.surface;
      check "link on raised" (List.assoc "link" extra) tokens.raised
    )
    Site_theme.themes

let outside_js text =
  let n = String.length text in
  let at i sub =
    i + String.length sub <= n && String.sub text i (String.length sub) = sub
  in
  let rec go i inside acc =
    if i >= n then
      List.sort_uniq compare acc
    else if (not inside) && at i "{js|" then
      go (i + 4) true acc
    else if inside && at i "|js}" then
      go (i + 4) false acc
    else if (not inside) && Char.code text.[i] >= 128 then
      let line =
        List.length (String.split_on_char '\n' (String.sub text 0 i))
      in
      go (i + 1) inside (line :: acc)
    else
      go (i + 1) inside acc
  in
  go 0 false []

let shared_dirs =
  [
    "../../site/app/shared";
    "../../site/stories";
    "../../ui";
    "../../linear/app/shared";
    "../../docs/examples/shared";
    "../../site/app/melange";
  ]

let test_text_outside_ascii () =
  Alcotest.(check (list int))
    "the detector" [ 1 ]
    (outside_js "\xc2\xb7 {js|\xc2\xb7|js}");
  let problems =
    List.concat_map
      (fun dir ->
        Sys.readdir dir |> Array.to_list
        |> List.filter (fun f ->
            (Filename.check_suffix f ".ml" || Filename.check_suffix f ".mlx")
            && not (Filename.check_suffix f ".pp.ml")
        )
        |> List.concat_map (fun f ->
            let path = Filename.concat dir f in
            List.map
              (fun line -> Printf.sprintf "%s:%d" path line)
              (outside_js (In_channel.with_open_bin path In_channel.input_all))
        )
      )
      shared_dirs
  in
  Alcotest.(check (list string))
    "Melange reads a plain literal byte by byte: write text that is not ASCII \
     as {js|...|js}"
    [] problems

let test_static_names () =
  let file input = Silica_static_export.answer_file ~path:"/api/doc" ~input in
  Alcotest.(check (list string))
    "FNV-1a of the UTF-8 bytes"
    [
      "api/doc/811c9dc5.json";
      "api/doc/e40c292c.json";
      "api/doc/8bf79db2.json";
      "api/doc/d1ace591.json";
    ]
    [ file ""; file "a"; file {|"router"|}; file {js|"café"|js} ];
  Alcotest.(check (list string))
    "page files"
    [ "index.html"; "docs.html"; "docs/router.html" ]
    (List.map Silica_static_export.page_file [ "/"; "/docs"; "/docs/router" ])

let test_static_export () =
  let out = Filename.temp_dir "silica-static" "" in
  let pages = List.map (fun (path, _, _) -> path) routes in
  let export () =
    Silica_static_export.run
      ~handle:(fun r -> Server.handle (Lazy.force server) r)
      ~pages ~not_found:"/404" ~files:[] ~prefix:"/silica-answers/" ~out
  in
  let first = export () in
  Alcotest.(check (result int string))
    "same count on a second run" first (export ());
  let read name =
    In_channel.with_open_bin (Filename.concat out name) In_channel.input_all
  in
  List.iter
    (fun slug ->
      let html = read (Silica_static_export.page_file (doc_path slug)) in
      if contains html {|<meta name="silica-csrf"|} then
        Alcotest.failf "%s: csrf meta left" slug;
      if
        not
          (contains html
             {|<meta name="silica-static" content="/silica-answers/">|}
          )
      then
        Alcotest.failf "%s: no silica-static meta" slug;
      let answer =
        read
          ("silica-answers/"
          ^ Silica_static_export.answer_file ~path:"/api/doc"
              ~input:(Printf.sprintf "%S" slug)
          )
      in
      if not (contains answer (Printf.sprintf {|"slug":"%s"|} slug)) then
        Alcotest.failf "%s: the answer file is not this page" slug
    )
    doc_slugs;
  if not (contains (read "404.html") "Not found") then
    Alcotest.fail "404.html is not the not-found page";
  ignore (Sys.command ("rm -rf " ^ Filename.quote out))

let round_trip codec value =
  match codec with
  | Error message ->
      Alcotest.fail message
  | Ok codec ->
      Alcotest.(check bool)
        "round trip" true
        (Silica.Codec.of_string codec (Silica.Codec.to_string codec value)
        = Ok value
        )

let test_codecs () =
  Site_codecs.register ();
  let directory = "../../site/app/shared" in
  Alcotest.(check int)
    "machines whose events derive JSON" 3
    (List.length (Silica_test_support.Codec_coverage.required directory));
  Alcotest.(check (list string))
    "each one is in Site_codecs" []
    (Silica_test_support.Codec_coverage.missing directory);
  List.iter
    (round_trip (Silica.Machine.event_codec Site_search.machine))
    Site_search.[ Open_search; Changed true; Ran "install" ];
  round_trip
    (Silica.Machine.state_codec Site_landing.machine)
    (Site_landing.Watching "ready");
  round_trip
    (Silica.Machine.event_codec Site_doc_page.machine)
    (Site_doc_page.Loaded (Failed (Network "down")))

let () =
  Alcotest.run "silica site"
    [
      ( "codecs",
        [
          Alcotest.test_case "the codec module registers every machine" `Quick
            test_codecs;
        ]
      );
      ( "server render",
        [
          Alcotest.test_case "every route: status, title, key text" `Quick
            test_routes;
          Alcotest.test_case "doc text is in the first HTML" `Quick
            test_doc_content_is_in_the_html;
          Alcotest.test_case "unknown pages are 404" `Quick test_not_found;
          Alcotest.test_case "theme cookie on <html>" `Quick test_theme_cookie;
          Alcotest.test_case "doc endpoint" `Quick test_doc_endpoint;
          Alcotest.test_case "static export file names" `Quick test_static_names;
          Alcotest.test_case "static export of every route" `Quick
            test_static_export;
        ]
      );
      ( "site",
        [
          Alcotest.test_case "no dead internal link or anchor" `Quick test_links;
          Alcotest.test_case "anchors, the index URL, previous and next" `Quick
            test_doc_links;
          Alcotest.test_case "every silica-ui component has a page" `Quick
            test_every_component_has_a_page;
          Alcotest.test_case "sidebar and pages agree" `Quick
            test_nav_covers_the_docs;
          Alcotest.test_case "search results open their page" `Quick
            test_search_targets;
          Alcotest.test_case "story values" `Quick test_story;
          Alcotest.test_case "statechart facts" `Quick test_facts;
          Alcotest.test_case "landing chart from a checker run" `Quick
            test_landing_chart;
          Alcotest.test_case "site tokens have contrast 4.5" `Quick
            test_site_token_contrast;
          Alcotest.test_case "text that is not ASCII is a {js|...|js} literal"
            `Quick test_text_outside_ascii;
        ]
      );
    ]
