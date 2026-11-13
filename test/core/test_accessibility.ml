open Silica

let element = Node.element
let text = Node.text
let string_attr = Attr.string
let flag name = Attr.bool name true

let role_testable =
  Alcotest.testable
    (fun formatter role ->
      Format.pp_print_string formatter (Accessibility.role_to_string role)
    )
    ( = )

let only_element view =
  match Accessibility.elements view with
  | [ found ] ->
      found
  | found ->
      Alcotest.failf "%d elements" (List.length found)

let by_id view id =
  match
    List.find_opt
      (fun found -> Accessibility.attribute found "id" = Some id)
      (Accessibility.elements view)
  with
  | Some found ->
      found
  | None ->
      Alcotest.failf "no element with id %s" id

let role_of view = Accessibility.role (only_element view)

let check_role message expected view =
  Alcotest.(check (option role_testable)) message expected (role_of view)

let input input_type = element "input" [ string_attr "type" input_type ] []

let implicit_roles () =
  let open Accessibility in
  check_role "button" (Some Button) (element "button" [] []);
  check_role "a with href" (Some Link)
    (element "a" [ string_attr "href" "/" ] []);
  check_role "a with no href" None (element "a" [] []);
  check_role "h3" (Some Heading) (element "h3" [] []);
  check_role "ul" (Some List) (element "ul" [] []);
  check_role "ol" (Some List) (element "ol" [] []);
  check_role "checkbox" (Some Checkbox) (input "checkbox");
  check_role "radio" (Some Radio) (input "radio");
  check_role "range" (Some Slider) (input "range");
  check_role "number" (Some Spinbutton) (input "number");
  check_role "search" (Some Searchbox) (input "search");
  check_role "email" (Some Textbox) (input "email");
  check_role "no type" (Some Textbox) (element "input" [] []);
  check_role "submit" (Some Button) (input "submit");
  check_role "password" None (input "password");
  check_role "text with list" (Some Combobox)
    (element "input" [ string_attr "list" "words" ] []);
  check_role "textarea" (Some Textbox) (element "textarea" [] []);
  check_role "select" (Some Combobox) (element "select" [] []);
  check_role "select multiple" (Some Listbox)
    (element "select" [ flag "multiple" ] []);
  check_role "select size 4" (Some Listbox)
    (element "select" [ string_attr "size" "4" ] []);
  check_role "img" (Some Img) (element "img" [ string_attr "alt" "Logo" ] []);
  check_role "img with empty alt" None
    (element "img" [ string_attr "alt" "" ] []);
  check_role "nav" (Some Navigation) (element "nav" [] []);
  check_role "main" (Some Main) (element "main" [] []);
  check_role "aside" (Some Complementary) (element "aside" [] []);
  check_role "form" (Some Form) (element "form" [] []);
  check_role "dialog" (Some Dialog) (element "dialog" [] []);
  check_role "fieldset" (Some Group) (element "fieldset" [] []);
  check_role "table" (Some Table) (element "table" [] []);
  check_role "tr" (Some Row) (element "tr" [] []);
  check_role "td" (Some Cell) (element "td" [] []);
  check_role "th" (Some Columnheader) (element "th" [] []);
  check_role "th row" (Some Rowheader)
    (element "th" [ string_attr "scope" "row" ] []);
  check_role "hr" (Some Separator) (element "hr" [] []);
  check_role "progress" (Some Progressbar) (element "progress" [] []);
  check_role "output" (Some Status) (element "output" [] []);
  check_role "div" None (element "div" [] []);
  check_role "span" None (element "span" [] [])

let scoped_landmarks () =
  let view =
    element "div" []
      [
        element "header" [ string_attr "id" "top" ] [];
        element "article" []
          [ element "header" [ string_attr "id" "inner" ] [] ];
        element "footer" [ string_attr "id" "bottom" ] [];
        element "section" [ string_attr "id" "plain" ] [];
        element "section"
          [ string_attr "id" "named"; string_attr "aria-label" "News" ]
          [];
      ]
  in
  let role id = Accessibility.role (by_id view id) in
  let check message expected id =
    Alcotest.(check (option role_testable)) message expected (role id)
  in
  check "a header at the top is a banner" (Some Accessibility.Banner) "top";
  check "a header in an article has no role" None "inner";
  check "a footer at the top is contentinfo" (Some Accessibility.Contentinfo)
    "bottom";
  check "a section with no name has no role" None "plain";
  check "a section with a name is a region" (Some Accessibility.Region) "named"

let explicit_roles () =
  let open Accessibility in
  check_role "role wins over the tag" (Some Tab)
    (element "button" [ string_attr "role" "tab" ] []);
  check_role "the first known token" (Some Switch)
    (element "div" [ string_attr "role" "toggle switch checkbox" ] []);
  check_role "presentation" None
    (element "button" [ string_attr "role" "presentation" ] []);
  check_role "none" None (element "h1" [ string_attr "role" "none" ] []);
  check_role "an unknown role falls back to the tag" (Some Button)
    (element "button" [ string_attr "role" "fancy" ] []);
  check_role "generic" (Some Generic)
    (element "div" [ string_attr "role" "generic" ] []);
  Alcotest.(check (option role_testable))
    "of_string" (Some Menuitemcheckbox)
    (role_of_string "menuitemcheckbox");
  Alcotest.(check (option role_testable))
    "of_string unknown" None (role_of_string "Button")

let names_from_attributes () =
  let view =
    element "div" []
      [
        element "span" [ string_attr "id" "first" ] [ text "Ship" ];
        element "span"
          [ string_attr "id" "second"; flag "hidden" ]
          [ text "  it " ];
        element "button"
          [
            string_attr "id" "labelled";
            string_attr "aria-labelledby" "first second missing";
            string_attr "aria-label" "Ignored";
          ]
          [ text "Content" ];
        element "button"
          [
            string_attr "id" "labelled-by-label";
            string_attr "aria-labelledby" "named";
          ]
          [];
        element "span"
          [
            string_attr "id" "named";
            string_attr "aria-label" "From the label";
            string_attr "aria-labelledby" "first";
          ]
          [ text "Text" ];
        element "button"
          [
            string_attr "id" "aria"; string_attr "aria-label" " Close  dialog ";
          ]
          [ text "x" ];
        element "button"
          [ string_attr "id" "title"; string_attr "title" "Help" ]
          [];
        element "input"
          [ string_attr "id" "placeholder"; string_attr "placeholder" "Search" ]
          [];
        element "input"
          [
            string_attr "id" "titled-field";
            string_attr "title" "Query";
            string_attr "placeholder" "Search";
          ]
          [];
      ]
  in
  let name id = Accessibility.name (by_id view id) in
  Alcotest.(check string)
    "aria-labelledby joins the targets, hidden ones too, and wins" "Ship it"
    (name "labelled");
  Alcotest.(check string)
    "a target gives its aria-label, not its own aria-labelledby"
    "From the label" (name "labelled-by-label");
  Alcotest.(check string)
    "aria-label wins over the content, trimmed and collapsed" "Close dialog"
    (name "aria");
  Alcotest.(check string) "title when nothing else" "Help" (name "title");
  Alcotest.(check string) "placeholder last" "Search" (name "placeholder");
  Alcotest.(check string)
    "title before placeholder" "Query" (name "titled-field")

let names_of_fields () =
  let view =
    element "form" []
      [
        element "label" [ string_attr "for" "email" ] [ text "Email" ];
        element "input"
          [ string_attr "id" "email"; string_attr "type" "email" ]
          [];
        element "label" []
          [
            text "Remember ";
            element "input"
              [ string_attr "id" "remember"; string_attr "type" "checkbox" ]
              [];
            text " me";
          ];
        element "label" [ string_attr "for" "both" ] [ text "First" ];
        element "label" []
          [ text "Second"; element "textarea" [ string_attr "id" "both" ] [] ];
        element "input"
          [ string_attr "id" "submit"; string_attr "type" "submit" ]
          [];
        element "input"
          [
            string_attr "id" "reset";
            string_attr "type" "reset";
            string_attr "value" "Clear";
          ]
          [];
        element "input"
          [
            string_attr "id" "image";
            string_attr "type" "image";
            string_attr "alt" "Go";
          ]
          [];
        element "fieldset"
          [ string_attr "id" "set" ]
          [ element "legend" [] [ text "Shipping" ]; text "Body" ];
        element "table"
          [ string_attr "id" "table" ]
          [ element "caption" [] [ text "Prices" ] ];
      ]
  in
  let name id = Accessibility.name (by_id view id) in
  Alcotest.(check string) "label for" "Email" (name "email");
  Alcotest.(check string)
    "a label around the field, without the field itself" "Remember me"
    (name "remember");
  Alcotest.(check string)
    "label for, then the label around" "First Second" (name "both");
  Alcotest.(check string) "submit with no value" "Submit" (name "submit");
  Alcotest.(check string)
    "an input button gives its value" "Clear" (name "reset");
  Alcotest.(check string) "an image input gives its alt" "Go" (name "image");
  Alcotest.(check string) "fieldset legend" "Shipping" (name "set");
  Alcotest.(check string) "table caption" "Prices" (name "table")

let names_from_content () =
  let view =
    element "div" []
      [
        element "button"
          [ string_attr "id" "spans" ]
          [
            element "span" [] [ text "ENG-" ];
            element "span" [] [ text "2" ];
            element "span" [ string_attr "aria-hidden" "true" ] [ text "icon" ];
          ];
        element "a"
          [ string_attr "id" "image-link"; string_attr "href" "/" ]
          [ element "img" [ string_attr "alt" "Home" ] [] ];
        element "button"
          [ string_attr "id" "inner-label" ]
          [
            element "span" [ string_attr "aria-label" "Delete" ] [ text "x" ];
            text " issue";
          ];
        element "button"
          [ string_attr "id" "field-inside" ]
          [ text "Count "; element "input" [ string_attr "value" "3" ] [] ];
        element "h2"
          [ string_attr "id" "heading" ]
          [ text "\n  Inbox\t  zero " ];
        element "li" [ string_attr "id" "item" ] [ text "Not a name" ];
        element "div" [ string_attr "id" "plain" ] [ text "Not a name" ];
      ]
  in
  let name id = Accessibility.name (by_id view id) in
  Alcotest.(check string)
    "children glue with no space, hidden ones give nothing" "ENG-2"
    (name "spans");
  Alcotest.(check string)
    "an image child gives its alt" "Home" (name "image-link");
  Alcotest.(check string)
    "a child gives its aria-label" "Delete issue" (name "inner-label");
  Alcotest.(check string)
    "a field inside gives nothing" "Count" (name "field-inside");
  Alcotest.(check string) "white space collapses" "Inbox zero" (name "heading");
  Alcotest.(check string)
    "a list item does not take its content" "" (name "item");
  Alcotest.(check string)
    "an element with no role does not either" "" (name "plain")

let hidden () =
  let view =
    element "div" []
      [
        element "div"
          [ flag "hidden" ]
          [ element "button" [ string_attr "id" "under-hidden" ] [] ];
        element "div"
          [ string_attr "aria-hidden" "true" ]
          [ element "button" [ string_attr "id" "under-aria" ] [] ];
        element "div"
          [ flag "inert" ]
          [ element "button" [ string_attr "id" "under-inert" ] [] ];
        element "div"
          [ string_attr "aria-hidden" "false" ]
          [ element "button" [ string_attr "id" "shown" ] [] ];
        element "input"
          [ string_attr "id" "field"; string_attr "type" "hidden" ]
          [];
      ]
  in
  let hidden id = Accessibility.hidden (by_id view id) in
  Alcotest.(check bool) "hidden above" true (hidden "under-hidden");
  Alcotest.(check bool) "aria-hidden above" true (hidden "under-aria");
  Alcotest.(check bool) "inert above" true (hidden "under-inert");
  Alcotest.(check bool) "aria-hidden false" false (hidden "shown");
  Alcotest.(check bool) "hidden input" true (hidden "field")

let paths_and_order () =
  let view =
    element "main" []
      [
        element "form" []
          [
            element "button" [ string_attr "id" "one" ] [];
            Node.portal
              [
                element "div"
                  [ string_attr "id" "outer" ]
                  [
                    Node.portal
                      [ element "button" [ string_attr "id" "inner" ] [] ];
                  ];
              ];
            element "button" [ string_attr "id" "two" ] [];
          ];
      ]
  in
  Alcotest.(check (list string))
    "document order: outside portals, then a portal inside before the one \
     around it"
    [
      "main";
      "main > form";
      "main > form > button[1]";
      "main > form > button[2]";
      "portal > button";
      "portal > div";
    ]
    (List.map Accessibility.path (Accessibility.elements view));
  Alcotest.(check (list (option string)))
    "ids in the same order"
    [ None; None; Some "one"; Some "two"; Some "inner"; Some "outer" ]
    (List.map
       (fun found -> Accessibility.attribute found "id")
       (Accessibility.elements view)
    )

let portal_children_have_nothing_above () =
  let view =
    element "div"
      [ flag "hidden" ]
      [ Node.portal [ element "button" [ string_attr "id" "out" ] [] ] ]
  in
  Alcotest.(check bool)
    "a portal child is not hidden by the element around the portal" false
    (Accessibility.hidden (by_id view "out"))

let states () =
  let view =
    element "div" []
      [
        element "h4" [ string_attr "id" "h4" ] [];
        element "div"
          [
            string_attr "id" "level";
            string_attr "role" "heading";
            string_attr "aria-level" "2";
          ]
          [];
        element "input"
          [
            string_attr "id" "box";
            string_attr "type" "checkbox";
            flag "checked";
          ]
          [];
        element "input"
          [ string_attr "id" "off"; string_attr "type" "checkbox" ]
          [];
        element "div"
          [
            string_attr "id" "switch";
            string_attr "role" "switch";
            string_attr "aria-checked" "false";
          ]
          [];
        element "div"
          [ string_attr "id" "mixed"; string_attr "aria-checked" "mixed" ]
          [];
        element "option" [ string_attr "id" "picked"; flag "selected" ] [];
        element "div"
          [
            string_attr "id" "tab";
            string_attr "aria-selected" "true";
            string_attr "aria-expanded" "false";
          ]
          [];
        element "button" [ string_attr "id" "disabled"; flag "disabled" ] [];
        element "div" [ string_attr "id" "div-disabled"; flag "disabled" ] [];
        element "div"
          [
            string_attr "id" "aria-disabled"; string_attr "aria-disabled" "true";
          ]
          [];
        element "a"
          [
            string_attr "id" "current";
            string_attr "href" "/";
            string_attr "aria-current" "page";
          ]
          [];
        element "a"
          [ string_attr "id" "not-current"; string_attr "aria-current" "false" ]
          [];
        element "input"
          [ string_attr "id" "value"; string_attr "value" "ocaml" ]
          [];
        element "div"
          [
            string_attr "id" "now";
            string_attr "aria-valuenow" "40";
            string_attr "value" "x";
          ]
          [];
        element "div"
          [
            string_attr "id" "valuetext";
            string_attr "aria-valuenow" "40";
            string_attr "aria-valuetext" "40 percent";
          ]
          [];
        element "div"
          [ string_attr "id" "modal"; string_attr "aria-modal" "true" ]
          [];
      ]
  in
  let get id = by_id view id in
  let int_option = Alcotest.(check (option int)) in
  let bool_option = Alcotest.(check (option bool)) in
  let string_option = Alcotest.(check (option string)) in
  int_option "h4" (Some 4) (Accessibility.level (get "h4"));
  int_option "aria-level" (Some 2) (Accessibility.level (get "level"));
  int_option "no level" None (Accessibility.level (get "box"));
  bool_option "checked input" (Some true) (Accessibility.checked (get "box"));
  bool_option "unchecked input" (Some false) (Accessibility.checked (get "off"));
  bool_option "aria-checked" (Some false) (Accessibility.checked (get "switch"));
  bool_option "mixed" None (Accessibility.checked (get "mixed"));
  bool_option "selected option" (Some true)
    (Accessibility.selected (get "picked"));
  bool_option "aria-selected" (Some true) (Accessibility.selected (get "tab"));
  bool_option "aria-expanded" (Some false) (Accessibility.expanded (get "tab"));
  bool_option "no expanded" None (Accessibility.expanded (get "box"));
  Alcotest.(check bool)
    "disabled button" true
    (Accessibility.disabled (get "disabled"));
  Alcotest.(check bool)
    "disabled on a div does nothing" false
    (Accessibility.disabled (get "div-disabled"));
  Alcotest.(check bool)
    "aria-disabled" true
    (Accessibility.disabled (get "aria-disabled"));
  string_option "current" (Some "page") (Accessibility.current (get "current"));
  string_option "current false" None (Accessibility.current (get "not-current"));
  string_option "input value" (Some "ocaml") (Accessibility.value (get "value"));
  string_option "aria-valuenow, and value only on fields" (Some "40")
    (Accessibility.value (get "now"));
  string_option "aria-valuetext" (Some "40 percent")
    (Accessibility.value (get "valuetext"));
  Alcotest.(check bool) "modal" true (Accessibility.modal (get "modal"));
  Alcotest.(check bool) "not modal" false (Accessibility.modal (get "box"))

let child_component =
  Component.make
    (Machine.make ~id:"accessibility-child" ~init:()
       ~name:(fun () -> "only")
       (fun () () -> Machine.Stay [])
    )
    (fun () -> element "button" [] [ text "Inside" ])

let items () =
  let waiting = Chunk.deferred (fun _ -> ()) in
  let ready = Chunk.ready "Ready" in
  let view =
    Node.list
      [
        text "";
        text "Hello";
        Node.component child_component;
        Node.chunk waiting ~loading:(text "Wait") (fun word -> text word);
        Node.chunk ready ~loading:(text "Wait") (fun word -> text word);
        Node.keyed "key" (element "p" [] []);
      ]
  in
  let describe = function
    | Accessibility.Element found ->
        "element " ^ Accessibility.tag found
    | Accessibility.Text text ->
        "text " ^ text
    | Accessibility.Component id ->
        "component " ^ id
    | Accessibility.Loading ->
        "loading"
  in
  Alcotest.(check (list string))
    "items"
    [
      "text Hello";
      "component accessibility-child";
      "loading";
      "text Ready";
      "element p";
    ]
    (List.map describe (Accessibility.items view))

let view_text () =
  let view =
    element "main" []
      [
        element "h1" [] [ text "Issues" ];
        element "p" []
          [ text "You have "; element "b" [] [ text "2" ]; text " open." ];
        element "ul" []
          [
            element "li" []
              [
                element "a"
                  [
                    string_attr "href" "/issue/1";
                    string_attr "aria-current" "page";
                  ]
                  [ text "First" ];
                text "Todo";
              ];
          ];
        element "form"
          [ Attr.on_submit (); Attr.on_key_down (fun _ -> None) ]
          [
            element "input"
              [
                string_attr "aria-label" "Title";
                string_attr "value" "Say \"hi\"";
                Attr.on_input ~rate:(Rate.Debounce 100) (fun _ -> ());
              ]
              [];
            element "input"
              [ string_attr "type" "checkbox"; string_attr "aria-label" "Done" ]
              [];
            element "button"
              [ flag "disabled"; Attr.on_press () ]
              [ text "Save" ];
          ];
        element "div" [ Attr.on_press () ] [ text "Click area" ];
        element "button" [ string_attr "aria-expanded" "false" ] [ text "More" ];
        element "div" [ flag "hidden" ] [ element "button" [] [ text "Gone" ] ];
        Node.portal
          [
            element "div"
              [
                string_attr "role" "dialog";
                string_attr "aria-label" "Menu";
                string_attr "aria-modal" "true";
              ]
              [ text "Inside the portal" ];
          ];
        Node.component child_component;
        Node.chunk
          (Chunk.deferred (fun _ -> ()))
          ~loading:(text "Wait")
          (fun () -> text "");
        element "div" [ string_attr "data-silica-live" "feed" ] [];
      ]
  in
  Alcotest.(check string)
    "text"
    {|main
  heading "Issues" level=1
  text "You have 2 open."
  list
    listitem
      link "First" current=page href="/issue/1"
      text "Todo"
  form [submit, key_down]
    textbox "Title" value="Say \"hi\"" [input]
    checkbox "Done" checked=false
    button "Save" disabled [press]
  div [press]
    text "Click area"
  button "More" expanded=false
  dialog "Menu" modal
    text "Inside the portal"
  component accessibility-child
  loading
  island feed
|}
    (View_text.to_string view)

let block_elements_end_text () =
  let element = Node.element and text = Node.text in
  let view : unit Node.t =
    element "main" []
      [
        element "p" [] [ text "Connecting." ];
        element "label" [] [ text "New todo"; element "input" [] [] ];
        element "div" []
          [
            text "Created";
            element "span" [] [ text "today" ];
            element "br" [] [];
            text "by Ada";
          ];
      ]
  in
  Alcotest.(check string)
    "a block element ends the text line; an inline one does not"
    {|main
  text "Connecting."
  text "New todo"
  textbox "New todo"
  text "Created today"
  text "by Ada"
|}
    (View_text.to_string view)

let () =
  Alcotest.run "silica accessibility"
    [
      ( "roles",
        [
          Alcotest.test_case "implicit roles of tags" `Quick implicit_roles;
          Alcotest.test_case "header, footer and section" `Quick
            scoped_landmarks;
          Alcotest.test_case "explicit roles" `Quick explicit_roles;
        ]
      );
      ( "names",
        [
          Alcotest.test_case "aria attributes, title, placeholder" `Quick
            names_from_attributes;
          Alcotest.test_case "fields, labels and native names" `Quick
            names_of_fields;
          Alcotest.test_case "content" `Quick names_from_content;
        ]
      );
      ( "tree",
        [
          Alcotest.test_case "hidden" `Quick hidden;
          Alcotest.test_case "paths and document order" `Quick paths_and_order;
          Alcotest.test_case "portal children have nothing above" `Quick
            portal_children_have_nothing_above;
          Alcotest.test_case "states" `Quick states;
          Alcotest.test_case "items" `Quick items;
        ]
      );
      ( "view text",
        [
          Alcotest.test_case "one view" `Quick view_text;
          Alcotest.test_case "a block element ends the text" `Quick
            block_elements_end_text;
        ]
      );
    ]
