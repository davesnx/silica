open Silica
module Memory = Silica_memory
module Theme = Silica_ui.Theme

let dark_query = "(prefers-color-scheme: dark)"
let set = Theme.set ~light:Theme.light ~dark:Theme.dark Theme.shipped

let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec at i =
    i + n <= h && (String.sub haystack i n = needle || at (i + 1))
  in
  at 0

let check_contains what haystack needle =
  if not (contains haystack needle) then
    Alcotest.failf "%s: %S not in\n%s" what needle haystack

let opt = Alcotest.(option string)

let raises_invalid what f =
  match f () with
  | exception Invalid_argument message ->
      message
  | _ ->
      Alcotest.failf "%s: no Invalid_argument" what

module Host = struct
  type event = Changed of Theme.current | Set_choice of Theme.choice

  module State = struct
    type _ t = Show : (Theme.choice * string list) t
  end

  module Mach = Machine.Make (State)

  let renders = ref 0

  let machine choice =
    Mach.make ~id:"theme-host"
      ~init:(Mach.at State.Show (choice, []))
      [
        Mach.state State.Show "show" (fun (choice, seen) -> function
          | Changed current ->
              Mach.goto State.Show (choice, seen @ [ Theme.name current.theme ])
          | Set_choice choice ->
              Mach.goto State.Show (choice, seen)
          );
      ]

  let view ~listen (At (State.Show, (choice, seen)) : Mach.snapshot) =
    incr renders;
    let on_change =
      if listen then
        Some (fun current -> Changed current)
      else
        None
    in
    Node.list
      [
        Theme.switcher ?on_change
          (Theme.props ~id:"theme"
             ~label:(Printf.sprintf "Theme %d" (List.length seen))
             set choice
          );
        Node.element "p"
          [ Attr.string "id" "seen" ]
          [ Node.text (String.concat "," seen) ];
        Node.element "button"
          [
            Attr.string "id" "to-ocean";
            Attr.on_press (Set_choice (Theme.Fixed Theme.ocean));
          ]
          [ Node.text "ocean" ];
        Theme.scope Theme.high_contrast [ Node.text "scoped" ];
      ]

  let component ?(listen = true) choice =
    Component.make (machine choice) (view ~listen)
end

let mount ?listen ?(dark = false) choice =
  Memory.mount ~media:[ (dark_query, dark) ] (Host.component ?listen choice)

let seen t = Memory.text (Memory.by_id t "seen")
let theme_attr t = Memory.document_attribute t "data-theme"

let media_sources t =
  List.filter (fun (_, s) -> Cmd.source_name s = "media") (Memory.sources t)

let choose t label =
  Memory.press (Memory.by_id t "theme-trigger");
  Memory.press (Memory.by_text t label)

let find css needle =
  let n = String.length needle in
  let rec go i =
    if i + n > String.length css then
      Alcotest.failf "no %s" needle
    else if String.sub css i n = needle then
      i
    else
      go (i + 1)
  in
  go 0

let from css needle =
  let at = find css needle in
  String.sub css at (String.length css - at)

let block css selector =
  let rec find i =
    if i + String.length selector > String.length css then
      Alcotest.failf "no block %s" selector
    else if String.sub css i (String.length selector) = selector then
      i
    else
      find (i + 1)
  in
  let at = find 0 in
  let close = String.index_from css at '}' in
  String.sub css at (close - at)

let declarations body =
  String.split_on_char '\n' body
  |> List.filter_map (fun line ->
      let line = String.trim line in
      match String.index_opt line ':' with
      | Some i when String.length line > 2 && String.sub line 0 2 = "--" ->
          Some (String.sub line 0 i)
      | _ ->
          None
  )

let stylesheet_has_every_token_in_every_theme () =
  let css = Theme.stylesheet set in
  let names =
    List.map
      (fun theme ->
        let body =
          block css (Printf.sprintf "[data-theme=\"%s\"]" (Theme.name theme))
        in
        let tokens = Theme.tokens theme in
        check_contains "surface value" body
          ("--ui-surface: " ^ tokens.surface ^ ";");
        check_contains "accent text value" body
          ("--ui-accent-text: " ^ tokens.accent_text ^ ";");
        check_contains "spacing value" body
          ("--ui-spacing: " ^ tokens.spacing ^ ";");
        check_contains "color-scheme" body
          ( match Theme.scheme theme with
          | Light ->
              "color-scheme: light;"
          | Dark ->
              "color-scheme: dark;"
          );
        List.sort compare (declarations body)
      )
      (Theme.themes set)
  in
  let first = List.hd names in
  Alcotest.(check int) "15 tokens" 15 (List.length first);
  List.iter
    (fun n -> Alcotest.(check (list string)) "same tokens" first n)
    names;
  let map = block css "@theme inline" in
  List.iter
    (fun property ->
      check_contains "the Tailwind map reads each token" map
        ("var(" ^ property ^ ")")
    )
    first;
  check_contains "colour utility" map
    "--color-ui-accent-text: var(--ui-accent-text);";
  check_contains "radius utility" map "--radius-ui: var(--ui-radius);";
  check_contains "spacing hook" map "--spacing: var(--ui-spacing);"

let stylesheet_system_follows_the_media_query () =
  let css = Theme.stylesheet set in
  let system = block css "[data-theme=\"system\"]" in
  check_contains "light theme by default" system
    ("--ui-surface: " ^ (Theme.tokens Theme.light).surface ^ ";");
  check_contains "light scheme" system "color-scheme: light;";
  let rest = from css "@media (prefers-color-scheme: dark)" in
  let dark_system = block rest "[data-theme=\"system\"]" in
  check_contains "dark theme under the media query" dark_system
    ("--ui-surface: " ^ (Theme.tokens Theme.dark).surface ^ ";");
  check_contains "dark scheme" dark_system "color-scheme: dark;";
  let other =
    Theme.set ~light:Theme.light ~dark:Theme.high_contrast
      [ Theme.light; Theme.high_contrast ]
  in
  let rest =
    from (Theme.stylesheet other) "@media (prefers-color-scheme: dark)"
  in
  check_contains "the dark theme of the set" rest
    ("--ui-surface: " ^ (Theme.tokens Theme.high_contrast).surface ^ ";")

let extra_tokens () =
  let brand name value =
    Theme.make ~name ~scheme:Light
      ~extra:[ ("brand-logo", value) ]
      (Theme.tokens Theme.light)
  in
  let a = brand "a" "#111111" and b = brand "b" "#222222" in
  let d =
    Theme.make ~name:"d" ~scheme:Dark
      ~extra:[ ("brand-logo", "#333333") ]
      (Theme.tokens Theme.dark)
  in
  let css = Theme.stylesheet (Theme.set ~light:a ~dark:d [ a; b; d ]) in
  check_contains "extra token of a"
    (block css "[data-theme=\"a\"]")
    "--brand-logo: #111111;";
  check_contains "extra token of b"
    (block css "[data-theme=\"b\"]")
    "--brand-logo: #222222;";
  let message =
    raises_invalid "a theme without the extra token" (fun () ->
        Theme.set ~light:a ~dark:Theme.dark [ a; b; Theme.dark ]
    )
  in
  check_contains "names the theme" message "dark";
  check_contains "names the token" message "brand-logo"

let bad_sets () =
  ignore
    (raises_invalid "empty" (fun () ->
         Theme.set ~light:Theme.light ~dark:Theme.dark []
     )
    );
  ignore
    (raises_invalid "light not in the list" (fun () ->
         Theme.set ~light:Theme.light ~dark:Theme.dark [ Theme.dark ]
     )
    );
  ignore
    (raises_invalid "dark not in the list" (fun () ->
         Theme.set ~light:Theme.light ~dark:Theme.dark [ Theme.light ]
     )
    );
  ignore
    (raises_invalid "light with a dark scheme" (fun () ->
         Theme.set ~light:Theme.ocean ~dark:Theme.dark
           [ Theme.ocean; Theme.dark ]
     )
    );
  ignore
    (raises_invalid "dark with a light scheme" (fun () ->
         Theme.set ~light:Theme.light ~dark:Theme.light [ Theme.light ]
     )
    );
  let copy = Theme.make ~name:"light" ~scheme:Light (Theme.tokens Theme.dark) in
  let message =
    raises_invalid "two themes with one name" (fun () ->
        Theme.set ~light:Theme.light ~dark:Theme.dark
          [ Theme.light; Theme.dark; copy ]
    )
  in
  check_contains "names the theme" message "light"

let bad_names_and_values () =
  let tokens = Theme.tokens Theme.light in
  List.iter
    (fun name ->
      ignore
        (raises_invalid ("name " ^ name) (fun () ->
             Theme.make ~name ~scheme:Light tokens
         )
        )
    )
    [ ""; "system"; "Dark"; "9a"; "a b"; "a\"b"; "-a" ];
  List.iter
    (fun value ->
      ignore
        (raises_invalid ("value " ^ value) (fun () ->
             Theme.make ~name:"x" ~scheme:Light { tokens with surface = value }
         )
        )
    )
    [
      "red; color: blue";
      "red}";
      "{";
      "</style>";
      "a\\b";
      "\"x\"";
      "a\nb";
      "'x";
      "";
    ];
  ignore
    (raises_invalid "extra name" (fun () ->
         Theme.make ~name:"x" ~scheme:Light ~extra:[ ("Bad", "1px") ] tokens
     )
    );
  ignore
    (raises_invalid "extra value" (fun () ->
         Theme.make ~name:"x" ~scheme:Light ~extra:[ ("ok", "1px;") ] tokens
     )
    );
  let font =
    Theme.make ~name:"x" ~scheme:Light
      { tokens with font = "'Inter', sans-serif" }
  in
  Alcotest.(check string)
    "quoted font kept" "'Inter', sans-serif" (Theme.tokens font).font

let cookie_choice () =
  let name c = Theme.choice_name c in
  Alcotest.(check string)
    "no cookie" "system"
    (name (Theme.choice_of_cookie set None));
  Alcotest.(check string)
    "a theme" "ocean"
    (name (Theme.choice_of_cookie set (Some "ocean")));
  Alcotest.(check string)
    "system" "system"
    (name (Theme.choice_of_cookie set (Some "system")));
  Alcotest.(check string)
    "a removed theme" "system"
    (name (Theme.choice_of_cookie set (Some "sepia")));
  Alcotest.(check string) "cookie name" "silica-theme" Theme.cookie_name

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

let shipped_contrast () =
  List.iter
    (fun theme ->
      let k = Theme.tokens theme in
      List.iter
        (fun (what, fg, bg) ->
          let r = ratio fg bg in
          if r < 4.5 then
            Alcotest.failf "%s: %s %s on %s is %.2f" (Theme.name theme) what fg
              bg r
        )
        [
          ("text on surface", k.text, k.surface);
          ("text on raised", k.text, k.raised);
          ("text on highlight", k.text, k.highlight);
          ("muted on surface", k.muted, k.surface);
          ("muted on raised", k.muted, k.raised);
          ("accent text on accent", k.accent_text, k.accent);
          ("danger on surface", k.danger, k.surface);
        ]
    )
    Theme.shipped;
  Alcotest.(check (float 0.01)) "black on white" 21. (ratio "#000000" "#ffffff")

let system_follows_the_setting () =
  let t = mount ~dark:false Theme.System in
  Alcotest.check opt "data-theme" (Some "system") (theme_attr t);
  Alcotest.(check (list (pair string string)))
    "no cookie at the start" [] (Memory.cookies t);
  Alcotest.(check string) "the light theme at the start" "light" (seen t);
  Memory.set_media t dark_query true;
  Alcotest.(check string) "live follow" "light,dark" (seen t);
  Alcotest.check opt "the attribute stays system" (Some "system") (theme_attr t);
  Memory.set_media t dark_query false;
  Alcotest.(check string) "and back" "light,dark,light" (seen t);
  let t = mount ~dark:true Theme.System in
  Alcotest.(check string) "dark at the start" "dark" (seen t)

let choose_a_theme () =
  let t = mount Theme.System in
  choose t "High contrast";
  Alcotest.check opt "data-theme" (Some "high-contrast") (theme_attr t);
  Alcotest.(check (list (pair string string)))
    "the cookie"
    [ ("silica-theme", "high-contrast") ]
    (Memory.cookies t);
  Alcotest.(check string) "on_change" "light,high-contrast" (seen t);
  Alcotest.(check int)
    "no media source outside System" 0
    (List.length (media_sources t));
  Memory.set_media t dark_query true;
  Alcotest.(check string)
    "the setting does not matter now" "light,high-contrast" (seen t);
  check_contains "the switcher shows it"
    (Memory.text (Memory.by_id t "theme-trigger"))
    "High contrast";
  choose t "System";
  Alcotest.check opt "back to system" (Some "system") (theme_attr t);
  Alcotest.(check (list (pair string string)))
    "the cookie"
    [ ("silica-theme", "system") ]
    (Memory.cookies t);
  Alcotest.(check string) "resolved at once" "light,high-contrast,dark" (seen t);
  Alcotest.(check int) "one media source" 1 (List.length (media_sources t))

let fixed_at_start () =
  let t = mount (Theme.Fixed Theme.ocean) in
  Alcotest.check opt "data-theme" (Some "ocean") (theme_attr t);
  Alcotest.(check (list (pair string string))) "no cookie" [] (Memory.cookies t);
  Alcotest.(check int) "no media source" 0 (List.length (media_sources t));
  check_contains "the switcher shows the choice"
    (Memory.text (Memory.by_id t "theme-trigger"))
    "Ocean"

let props_change_is_a_change () =
  let t = mount Theme.System in
  Memory.press (Memory.by_id t "to-ocean");
  Alcotest.check opt "data-theme" (Some "ocean") (theme_attr t);
  Alcotest.(check (list (pair string string)))
    "the cookie"
    [ ("silica-theme", "ocean") ]
    (Memory.cookies t);
  Memory.press (Memory.by_id t "to-ocean");
  Alcotest.(check (list (pair string string)))
    "same props, no new write"
    [ ("silica-theme", "ocean") ]
    (Memory.cookies t)

let new_props_with_the_same_choice_keep_the_pick () =
  let t = mount Theme.System in
  choose t "Dark";
  Alcotest.(check string)
    "the parent heard it and has new props" "light,dark" (seen t);
  Alcotest.check opt "the pick stays" (Some "dark") (theme_attr t);
  Alcotest.(check (list (pair string string)))
    "one write"
    [ ("silica-theme", "dark") ]
    (Memory.cookies t)

let switch_costs_no_extra_render () =
  let renders f =
    let t =
      Memory.mount ~media:[] (Host.component ~listen:false Theme.System)
    in
    let before = !Host.renders and html = Memory.html t in
    f t;
    (!Host.renders - before, html, t)
  in
  let switch, _, t = renders (fun t -> choose t "Dark") in
  Alcotest.check opt "switched" (Some "dark") (theme_attr t);
  let browse, html, t =
    renders (fun t ->
        Memory.press (Memory.by_id t "theme-trigger");
        Memory.press (Memory.by_id t "theme-trigger")
    )
  in
  Alcotest.(check int)
    "a switch renders as often as open and close" browse switch;
  Alcotest.(check string)
    "open and close leave the page as it was" html (Memory.html t)

let scope_marks_the_subtree () =
  let t = mount Theme.System in
  let scoped = Memory.by_text t "scoped" in
  Alcotest.check opt "data-theme on the scope" (Some "high-contrast")
    (Memory.attribute scoped "data-theme");
  Alcotest.(check string) "a div" "div" (Memory.tag scoped)

let server_html_has_the_theme () =
  let html = Render.to_string (Theme.scope Theme.ocean [ Node.text "x" ]) in
  check_contains "static scope html" html "<div data-theme=\"ocean\">x</div>"

let () =
  Alcotest.run "silica-ui theme"
    [
      ( "css",
        [
          Alcotest.test_case "every token in every theme" `Quick
            stylesheet_has_every_token_in_every_theme;
          Alcotest.test_case "system follows the media query" `Quick
            stylesheet_system_follows_the_media_query;
          Alcotest.test_case "extra tokens" `Quick extra_tokens;
          Alcotest.test_case "bad sets" `Quick bad_sets;
          Alcotest.test_case "bad names and values" `Quick bad_names_and_values;
          Alcotest.test_case "shipped contrast at least 4.5" `Quick
            shipped_contrast;
        ]
      );
      ("choice", [ Alcotest.test_case "cookie" `Quick cookie_choice ]);
      ( "machine",
        [
          Alcotest.test_case "system follows the setting" `Quick
            system_follows_the_setting;
          Alcotest.test_case "choose a theme" `Quick choose_a_theme;
          Alcotest.test_case "fixed at the start" `Quick fixed_at_start;
          Alcotest.test_case "props change" `Quick props_change_is_a_change;
          Alcotest.test_case "new props with the same choice keep the pick"
            `Quick new_props_with_the_same_choice_keep_the_pick;
          Alcotest.test_case "a switch costs no extra render" `Quick
            switch_costs_no_extra_render;
          Alcotest.test_case "scope" `Quick scope_marks_the_subtree;
          Alcotest.test_case "scope html" `Quick server_html_has_the_theme;
        ]
      );
    ]
