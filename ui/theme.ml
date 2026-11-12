open Silica

type scheme = Light | Dark

type tokens = {
  surface : string;
  raised : string;
  text : string;
  muted : string;
  border : string;
  highlight : string;
  accent : string;
  accent_text : string;
  danger : string;
  backdrop : string;
  focus : string;
  radius : string;
  shadow : string;
  font : string;
  spacing : string;
}

type kind = Color | Radius | Shadow | Font | Spacing

let token_list
    {
      surface;
      raised;
      text;
      muted;
      border;
      highlight;
      accent;
      accent_text;
      danger;
      backdrop;
      focus;
      radius;
      shadow;
      font;
      spacing;
    } =
  [
    ("surface", Color, surface);
    ("raised", Color, raised);
    ("text", Color, text);
    ("muted", Color, muted);
    ("border", Color, border);
    ("highlight", Color, highlight);
    ("accent", Color, accent);
    ("accent-text", Color, accent_text);
    ("danger", Color, danger);
    ("backdrop", Color, backdrop);
    ("focus", Color, focus);
    ("radius", Radius, radius);
    ("shadow", Shadow, shadow);
    ("font", Font, font);
    ("spacing", Spacing, spacing);
  ]

type t = {
  name : string;
  label : string;
  scheme : scheme;
  tokens : tokens;
  extra : (string * string) list;
}

let fail fmt = Printf.ksprintf invalid_arg ("Silica_ui.Theme: " ^^ fmt)
let is_lower c = c >= 'a' && c <= 'z'

let good_name name =
  name <> ""
  && is_lower name.[0]
  && String.for_all
       (fun c -> is_lower c || (c >= '0' && c <= '9') || c = '-')
       name

let good_value value =
  let quotes = ref 0 in
  value <> ""
  && String.for_all
       (fun c ->
         if c = '\'' then incr quotes;
         not (String.contains ";{}<>\\\"\n\r" c)
       )
       value
  && !quotes mod 2 = 0

let make ~name ?label ~scheme ?(extra = []) tokens =
  if (not (good_name name)) || name = "system" then
    fail "bad theme name %S" name;
  List.iter
    (fun (token, _, value) ->
      if not (good_value value) then
        fail "theme %S: bad value %S for token %S" name value token
    )
    (token_list tokens);
  List.iteri
    (fun i (token, value) ->
      if (not (good_name token)) || String.starts_with ~prefix:"ui-" token then
        fail "theme %S: bad extra token name %S" name token;
      if not (good_value value) then
        fail "theme %S: bad value %S for token %S" name value token;
      if
        List.exists
          (fun (other, _) -> other = token)
          (List.filteri (fun j _ -> j < i) extra)
      then
        fail "theme %S: token %S given twice" name token
    )
    extra;
  { name; label = Option.value label ~default:name; scheme; tokens; extra }

let name t = t.name
let label t = t.label
let scheme t = t.scheme
let tokens t = t.tokens
let extra t = t.extra
let font = "ui-sans-serif, system-ui, sans-serif"

let light =
  make ~name:"light" ~label:"Light" ~scheme:Light
    {
      surface = "#ffffff";
      raised = "#ffffff";
      text = "#111827";
      muted = "#6b7280";
      border = "#e5e7eb";
      highlight = "#f3f4f6";
      accent = "#4f46e5";
      accent_text = "#ffffff";
      danger = "#dc2626";
      backdrop = "rgb(0 0 0 / 0.4)";
      focus = "#6366f1";
      radius = "0.5rem";
      shadow =
        "0 10px 15px -3px rgb(0 0 0 / 0.1), 0 4px 6px -4px rgb(0 0 0 / 0.1)";
      font;
      spacing = "0.25rem";
    }

let dark =
  make ~name:"dark" ~label:"Dark" ~scheme:Dark
    {
      surface = "#111827";
      raised = "#1f2937";
      text = "#f3f4f6";
      muted = "#9ca3af";
      border = "#374151";
      highlight = "#374151";
      accent = "#4f46e5";
      accent_text = "#ffffff";
      danger = "#f87171";
      backdrop = "rgb(0 0 0 / 0.6)";
      focus = "#818cf8";
      radius = "0.5rem";
      shadow =
        "0 10px 15px -3px rgb(0 0 0 / 0.5), 0 4px 6px -4px rgb(0 0 0 / 0.5)";
      font;
      spacing = "0.25rem";
    }

let high_contrast =
  make ~name:"high-contrast" ~label:"High contrast" ~scheme:Dark
    {
      surface = "#000000";
      raised = "#000000";
      text = "#ffffff";
      muted = "#e5e5e5";
      border = "#ffffff";
      highlight = "#333333";
      accent = "#ffff00";
      accent_text = "#000000";
      danger = "#ff8080";
      backdrop = "rgb(0 0 0 / 0.8)";
      focus = "#ffff00";
      radius = "0.25rem";
      shadow = "0 0 0 1px #ffffff";
      font;
      spacing = "0.25rem";
    }

let ocean =
  make ~name:"ocean" ~label:"Ocean" ~scheme:Dark
    {
      surface = "#0b1e33";
      raised = "#10294a";
      text = "#e6f1ff";
      muted = "#9fb8d6";
      border = "#2a4d75";
      highlight = "#1b3b62";
      accent = "#22d3ee";
      accent_text = "#062330";
      danger = "#fca5a5";
      backdrop = "rgb(2 8 23 / 0.6)";
      focus = "#67e8f9";
      radius = "0.75rem";
      shadow = "0 10px 15px -3px rgb(2 8 23 / 0.6)";
      font;
      spacing = "0.25rem";
    }

let shipped = [ light; dark; high_contrast; ocean ]

type set = { themes : t list; light : t; dark : t }

let extra_names t = List.sort compare (List.map fst t.extra)

let set ~light ~dark themes =
  if themes = [] then fail "a set needs a theme";
  List.iteri
    (fun i t ->
      if
        List.exists
          (fun other -> other.name = t.name)
          (List.filteri (fun j _ -> j < i) themes)
      then
        fail "two themes are named %S" t.name
    )
    themes;
  let member t =
    List.exists (fun other -> other.name = t.name && other = t) themes
  in
  if not (member light) then
    fail "the light theme %S is not in the set" light.name;
  if not (member dark) then fail "the dark theme %S is not in the set" dark.name;
  if light.scheme <> Light then
    fail "the light theme %S has a dark scheme" light.name;
  if dark.scheme <> Dark then
    fail "the dark theme %S has a light scheme" dark.name;
  let all = List.sort_uniq compare (List.concat_map extra_names themes) in
  List.iter
    (fun t ->
      List.iter
        (fun token ->
          if not (List.mem_assoc token t.extra) then
            fail "theme %S has no token %S" t.name token
        )
        all
    )
    themes;
  { themes; light; dark }

let themes s = s.themes
let system_light s = s.light
let system_dark s = s.dark

type choice = System | Fixed of t

let choice_name = function System -> "system" | Fixed t -> t.name

let choice_of_name set name =
  if name = "system" then
    Some System
  else
    Option.map
      (fun t -> Fixed t)
      (List.find_opt (fun t -> t.name = name) set.themes)

let cookie_name = "silica-theme"

let choice_of_cookie set = function
  | None ->
      System
  | Some name ->
      Option.value (choice_of_name set name) ~default:System

let tailwind = function
  | name, Color ->
      "--color-ui-" ^ name
  | _, Radius ->
      "--radius-ui"
  | _, Shadow ->
      "--shadow-ui"
  | _, Font ->
      "--font-ui"
  | _, Spacing ->
      "--spacing"

let block ~indent selector t =
  let line (property, value) =
    Printf.sprintf "%s  %s: %s;\n" indent property value
  in
  String.concat ""
    ([
       Printf.sprintf "%s%s {\n" indent selector;
       line
         ( "color-scheme",
           match t.scheme with Light -> "light" | Dark -> "dark"
         );
     ]
    @ List.map
        (fun (token, _, value) -> line ("--ui-" ^ token, value))
        (token_list t.tokens)
    @ List.map (fun (token, value) -> line ("--" ^ token, value)) t.extra
    @ [ indent ^ "}\n" ]
    )

let selector name = Printf.sprintf "[data-theme=\"%s\"]" name

let stylesheet set =
  let map =
    List.map
      (fun (token, kind, _) ->
        Printf.sprintf "  %s: var(--ui-%s);\n" (tailwind (token, kind)) token
      )
      (token_list set.light.tokens)
  in
  String.concat ""
    ((("@theme inline {\n" :: map) @ [ "}\n" ])
    @ List.map (fun t -> block ~indent:"" (selector t.name) t) set.themes
    @ [
        block ~indent:"" (selector "system") set.light;
        "@media (prefers-color-scheme: dark) {\n";
        block ~indent:"  " (selector "system") set.dark;
        "}\n";
      ]
    )

let scope ?key t nodes =
  let node = Node.element "div" [ Attr.string "data-theme" t.name ] nodes in
  match key with Some key -> Node.keyed key node | None -> node

type props = { id : string; label : string; set : set; choice : choice }

let props ~id ?(label = "Theme") set choice = { id; label; set; choice }

type current = { choice : choice; theme : t }
type event = Choose of string | System_dark of bool

type state = Fixed_theme of t | System_scheme of bool option
[@@deriving fingerprint]

let state_name = function
  | Fixed_theme _ ->
      "fixed"
  | System_scheme _ ->
      "system"
let states = [ "fixed"; "system" ]

let event_name = function
  | Choose name ->
      "choose " ^ name
  | System_dark dark ->
      "system_dark " ^ string_of_bool dark

let one_year = 365 * 24 * 60 * 60

let apply ~store choice : (state, event) Machine.next =
  let actions =
    Action.Set_document_attribute
      { name = "data-theme"; value = Some (choice_name choice) }
    ::
    ( if store then
        [
          Action.Set_cookie
            {
              name = cookie_name;
              value = choice_name choice;
              max_age = one_year;
            };
        ]
      else
        []
    )
  in
  match choice with
  | System ->
      Go_with (System_scheme None, actions)
  | Fixed theme ->
      Go_with (Fixed_theme theme, actions)

let current_name = function
  | Fixed_theme theme ->
      theme.name
  | System_scheme _ ->
      "system"

let machine =
  Machine.make_with_props ~id:"theme" ~init:(Fixed_theme light) ~name:state_name
    ~states ~event_name ~fingerprint:state_fingerprint
    ~during:(fun _ -> function
      | System_scheme _ ->
          [
            Source.media "(prefers-color-scheme: dark)" (fun dark ->
                System_dark dark
            );
          ]
      | Fixed_theme _ ->
          []
      )
    ~on_props:(fun previous (props : props) _ ->
      match previous with
      | None ->
          apply ~store:false props.choice
      | Some (previous : props)
        when choice_name previous.choice <> choice_name props.choice ->
          apply ~store:true props.choice
      | Some _ ->
          Ignore
    )
    (fun (props : props) state event ->
      match (state, event) with
      | System_scheme _, System_dark dark ->
          Go (System_scheme (Some dark))
      | Fixed_theme _, System_dark _ ->
          Ignore
      | (Fixed_theme _ | System_scheme _), Choose name -> (
          match choice_of_name props.set name with
          | None ->
              Ignore
          | Some choice when choice_name choice = current_name state ->
              Stay []
          | Some choice ->
              apply ~store:true choice
        )
    )

let current (props : props) = function
  | Fixed_theme theme ->
      Some { choice = Fixed theme; theme }
  | System_scheme None ->
      None
  | System_scheme (Some dark) ->
      Some
        {
          choice = System;
          theme =
            ( if dark then
                props.set.dark
              else
                props.set.light
            );
        }

let items set =
  { Select.value = "system"; label = "System"; disabled = false }
  :: List.map
       (fun t -> { Select.value = t.name; label = t.label; disabled = false })
       set.themes

let select (props : props) value =
  Select.make
    ~on_value_change:(fun name -> Choose name)
    (Select.props ~id:props.id ~label:props.label ~value ~classes:Style.select
       (items props.set)
    )

let switcher_view props state = select props (current_name state)
let switcher_component = Component.make_with_props machine switcher_view
let provider_component = Component.make machine (fun _ -> Node.list [])

let output on_change props event state =
  match (on_change, event) with
  | Some on_change, (Choose _ | System_dark _) ->
      Option.to_list (Option.map on_change (current props state))
  | None, (Choose _ | System_dark _) ->
      []

let switcher ?key ?on_change props =
  Component.node_with_props ?key switcher_component ~props
    ~output:(output on_change props)

let provider ?key ?on_change props =
  Component.node_with_props ?key provider_component ~props
    ~output:(output on_change props)
