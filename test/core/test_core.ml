open Silica

let strings = Alcotest.(list string)

let ep =
  Remote.declare ~path:"/api/double" ~input:Codec.int ~output:Codec.int
    ~rejected:Codec.string

let test_action_map () =
  ( match
      Action.map_host
        (fun n -> n + 1)
        (Action.Start_timer { id = "t"; ms = 5; event = 1 })
    with
  | Start_timer { id = "t"; ms = 5; event = 2 } ->
      ()
  | _ ->
      Alcotest.fail "Start_timer not mapped"
  );
  ( match
      Action.map_host string_of_int
        (Action.Start_call
           {
             id = "c";
             endpoint = ep;
             input = 1;
             timeout_ms = Some 7;
             on_result =
               (function
               | Remote.Ok n ->
                   n
               | Remote.Rejected _ ->
                   -2
               | Remote.Failed _ ->
                   -1
               );
           }
        )
    with
  | Start_call { id = "c"; endpoint; on_result; timeout_ms = Some 7; _ } ->
      Alcotest.(check string) "path kept" "/api/double" (Remote.path endpoint);
      Alcotest.(check string)
        "continuation mapped" "-1"
        (on_result (Remote.Failed (Network "x")))
  | _ ->
      Alcotest.fail "Start_call not mapped"
  );
  ( match Action.map_host succ (Action.Stop "t") with
  | Stop "t" ->
      ()
  | _ ->
      Alcotest.fail "Stop not kept"
  );
  ( match
      Action.map succ
        (Action.Measure
           {
             target = Ref.make "m";
             on_rect = (function Some _ -> 1 | None -> 0);
           }
        )
    with
  | Measure { target; on_rect } ->
      Alcotest.(check string) "target kept" "m" (Ref.name target);
      Alcotest.(check int) "result mapped" 1 (on_rect None)
  | _ ->
      Alcotest.fail "Measure not mapped"
  );
  ( match
      Action.map succ
        (Action.Copy
           {
             text = "t";
             on_result =
               Some
                 (fun ok ->
                   if ok then
                     1
                   else
                     0
                 );
           }
        )
    with
  | Copy { on_result = Some k; _ } ->
      Alcotest.(check int) "copy result mapped" 2 (k true)
  | _ ->
      Alcotest.fail "Copy not mapped"
  );
  match Action.map succ (Action.Navigate (Url.of_string "/x")) with
  | Navigate u ->
      Alcotest.check strings "url kept" [ "x" ] u.path
  | _ ->
      Alcotest.fail "Navigate not kept"

let test_node_map () =
  let n =
    Node.element "button"
      [ Attr.on_press 1; Attr.on_input String.length; Attr.string "id" "b" ]
      [ Node.keyed "k" (Node.text "hi") ]
  in
  match Node.view (Node.map (fun n -> n * 10) n) with
  | Element ("button", [ On_press 10; On_input f; String ("id", "b") ], [ kid ])
    -> (
      Alcotest.(check int) "input handler mapped" 30 (f "abc");
      match Node.view kid with
      | Keyed ("k", t) -> (
          match Node.view t with
          | Text "hi" ->
              ()
          | _ ->
              Alcotest.fail "text lost"
        )
      | _ ->
          Alcotest.fail "keyed lost"
    )
  | _ ->
      Alcotest.fail "mapped tree has wrong shape"

let render = Alcotest.(check string)

let test_render_escaping () =
  render "text" "<p>1 &lt; 2 &amp; 3 &gt; 2 &quot;q&quot; &#39;s&#39;</p>"
    (Render.to_string
       (Node.element "p" [] [ Node.text "1 < 2 & 3 > 2 \"q\" 's'" ])
    );
  render "attr" "<a href=\"/x?a=1&amp;b=&quot;2&quot;&#39;\"></a>"
    (Render.to_string
       (Node.element "a" [ Attr.string "href" "/x?a=1&b=\"2\"'" ] [])
    )

let test_render_void_bool () =
  render "void" "<div><br /><input type=\"text\" disabled /></div>"
    (Render.to_string
       (Node.element "div" []
          [
            Node.element "br" [] [];
            Node.element "input"
              [
                Attr.string "type" "text";
                Attr.bool "disabled" true;
                Attr.bool "checked" false;
              ]
              [];
          ]
       )
    )

let test_render_ref_handlers_structure () =
  render "ref and handlers"
    "<button id=\"x\" data-silica-ref=\"go&quot;\">a<b>c</b>d</button>"
    (Render.to_string
       (Node.element "button"
          [
            Attr.string "id" "x";
            Attr.on_press ();
            Attr.on_input (fun _ -> ());
            Attr.ref_ (Ref.make "go\"");
          ]
          [
            Node.list
              [
                Node.text "a";
                Node.keyed "k" (Node.element "b" [] [ Node.text "c" ]);
              ];
            Node.text "d";
          ]
       )
    );
  render "doctype" "<!DOCTYPE html><html><body></body></html>"
    (Render.to_string (Node.element "html" [] [ Node.element "body" [] [] ]))

let url_t =
  Alcotest.testable
    (fun fmt (u : Url.t) -> Format.pp_print_string fmt (Url.to_string u))
    ( = )

let test_url_parse () =
  Alcotest.check url_t "parse"
    {
      path = [ "a"; "b c" ];
      query = [ ("x", "1"); ("y", "&"); ("z", " ") ];
      hash = Some "top";
    }
    (Url.of_string "/a/b%20c?x=1&y=%26&z=+#top");
  Alcotest.check url_t "root"
    { path = []; query = []; hash = None }
    (Url.of_string "/");
  Alcotest.check url_t "flag key"
    { path = [ "p" ]; query = [ ("k", "") ]; hash = None }
    (Url.of_string "/p?k")

let test_url_print () =
  Alcotest.(check string)
    "encoded" "/a%2Fb/%C3%A9?k=v%20w&e=#x%20y"
    (Url.to_string
       {
         path = [ "a/b"; "\xc3\xa9" ];
         query = [ ("k", "v w"); ("e", "") ];
         hash = Some "x y";
       }
    );
  Alcotest.(check string)
    "root" "/"
    (Url.to_string { path = []; query = []; hash = None })

let test_url_round_trip () =
  List.iter
    (fun (u : Url.t) ->
      Alcotest.check url_t (Url.to_string u) u (Url.of_string (Url.to_string u))
    )
    [
      { path = []; query = []; hash = None };
      { path = [ "todos"; "42" ]; query = []; hash = Some "" };
      {
        path = [ "a b"; "%"; "?#&=/" ];
        query = [ ("a&b", "c=d"); ("+", "%20") ];
        hash = Some "#/x";
      };
      {
        path = [ "\xe2\x82\xac" ];
        query = [ ("q", "\xf0\x9f\x98\x80") ];
        hash = None;
      };
    ]

let test_codec () =
  let c = Codec.(list (pair string (option int))) in
  let v = [ ("a", Some 1); ("b\"", None) ] in
  Alcotest.(check string)
    "wire" "[[\"a\",1],[\"b\\\"\",null]]" (Codec.to_string c v);
  Alcotest.(check bool)
    "round trip" true
    (Codec.of_string c (Codec.to_string c v) = Ok v);
  Alcotest.(check bool)
    "mismatch" true
    ( match Codec.of_string Codec.int "\"x\"" with
    | Error _ ->
        true
    | Ok _ ->
        false
    );
  Alcotest.(check bool)
    "bad json" true
    (match Codec.of_string Codec.int "{" with Error _ -> true | Ok _ -> false)

let test_live_slot () =
  let live =
    Live.declare ~channel:"chat" ~state:Codec.int ~event:Codec.unit
      (Component.make
         (Silica_test_support.Simple.machine ~id:"chat" ~init:0 (fun _ () ->
              None
          )
         )
         (fun _ -> Node.text "")
      )
  in
  let slot : unit Node.t = Live.slot live in
  Alcotest.(check string)
    "island" "<div data-silica-live=\"chat\"></div>" (Render.to_string slot);
  match Node.view slot with
  | Element ("div", [ String ("data-silica-live", "chat") ], []) ->
      ()
  | _ ->
      Alcotest.fail "slot must be an empty div with only the island attribute"

let test_class_join () =
  let check name expected input =
    Alcotest.(check string) name expected (Class.join input)
  in
  check "joins with one space" "a b c" [ "a"; "b"; "c" ];
  check "drops empty strings" "a c" [ "a"; ""; "c" ];
  check "all empty" "" [ ""; "" ];
  check "no items" "" [];
  check "keeps the order" "z a" [ "z"; "a" ];
  check "a literal with several classes stays whole" "flex p-4 gap-2"
    [ "flex p-4"; ""; "gap-2" ];
  let join_if active =
    Class.join
      [
        "row";
        ( if active then
            "bg-gray-100"
          else
            ""
        );
      ]
  in
  Alcotest.(check string) "conditional on" "row bg-gray-100" (join_if true);
  Alcotest.(check string) "conditional off" "row" (join_if false)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "silica core"
    [
      ("live", [ t "slot" test_live_slot ]);
      ("action", [ t "map" test_action_map ]);
      ("node", [ t "map" test_node_map ]);
      ( "render",
        [
          t "escaping" test_render_escaping;
          t "void and boolean" test_render_void_bool;
          t "ref, handlers, structure" test_render_ref_handlers_structure;
        ]
      );
      ( "url",
        [
          t "parse" test_url_parse;
          t "print" test_url_print;
          t "round trip" test_url_round_trip;
        ]
      );
      ("codec", [ t "round trip and errors" test_codec ]);
      ("class", [ t "join" test_class_join ]);
    ]
