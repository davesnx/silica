module Theme = Silica_ui.Theme

type code = {
  link : string;
  code_surface : string;
  code_keyword : string;
  code_string : string;
  code_name : string;
  code_comment : string;
}

let extra t =
  [
    ("link", t.link);
    ("code-surface", t.code_surface);
    ("code-keyword", t.code_keyword);
    ("code-string", t.code_string);
    ("code-name", t.code_name);
    ("code-comment", t.code_comment);
  ]

let with_code theme code =
  Theme.make ~name:(Theme.name theme) ~label:(Theme.label theme)
    ~scheme:(Theme.scheme theme) ~extra:(extra code) (Theme.tokens theme)

let light =
  with_code Theme.light
    {
      link = "#4338ca";
      code_surface = "#f5f6f8";
      code_keyword = "#6d28d9";
      code_string = "#047857";
      code_name = "#1d4ed8";
      code_comment = "#5b6472";
    }

let dark =
  with_code Theme.dark
    {
      link = "#a5b4fc";
      code_surface = "#0b1220";
      code_keyword = "#c4b5fd";
      code_string = "#6ee7b7";
      code_name = "#93c5fd";
      code_comment = "#9ca3af";
    }

let high_contrast =
  with_code Theme.high_contrast
    {
      link = "#ffff00";
      code_surface = "#000000";
      code_keyword = "#ffff00";
      code_string = "#80ff80";
      code_name = "#80ffff";
      code_comment = "#e5e5e5";
    }

let ocean =
  with_code Theme.ocean
    {
      link = "#67e8f9";
      code_surface = "#081729";
      code_keyword = "#7dd3fc";
      code_string = "#86efac";
      code_name = "#fcd34d";
      code_comment = "#9fb8d6";
    }

let themes = [ light; dark; high_contrast; ocean ]
let set = Theme.set ~light ~dark themes
let choice cookie = Theme.choice_of_cookie set cookie
