open Silica_ui

type entry = {
  info : Site_components.info;
  chart : Silica.Statechart_layout.t option;
  stories : (string * Story.t) list;
      (** The file name of each story in [site/stories/], and the story. *)
  grid : Story.t option;
      (** The story shown in every theme. A story with fixed element ids can
          show only once in a page, so it is never the grid story. *)
}

let sides default =
  Story.choice "side" ~label:"Side"
    [
      ("top", "Top"); ("bottom", "Bottom"); ("left", "Left"); ("right", "Right");
    ]
    default

let open_toggle = Story.toggle "open" ~label:"Open" false

let dialog_basic =
  {
    Story.name = "Basic";
    source = Site_story_sources.dialog_basic;
    controls =
      [
        Story.toggle "dismissible" ~label:"Dismissible" true;
        Story.toggle "alert" ~label:"Alert" false;
      ];
    render = Dialog_basic.render;
  }

let dialog =
  {
    info = Site_components.dialog;
    chart = Some Site_charts.note_dialog;
    stories =
      [
        ("dialog_basic.mlx", dialog_basic);
        ( "dialog_events.mlx",
          {
            Story.name = "Events";
            source = Site_story_sources.dialog_events;
            controls = [];
            render = Dialog_events.render;
          }
        );
      ];
    grid = Some dialog_basic;
  }

let popover_basic =
  {
    Story.name = "Basic";
    source = Site_story_sources.popover_basic;
    controls = [ sides "bottom"; open_toggle ];
    render = Popover_basic.render;
  }

let popover =
  {
    info = Site_components.popover;
    chart = Some Site_charts.filter_popover;
    stories =
      [
        ("popover_basic.mlx", popover_basic);
        ( "popover_events.mlx",
          {
            Story.name = "Events";
            source = Site_story_sources.popover_events;
            controls = [];
            render = Popover_events.render;
          }
        );
      ];
    grid = Some popover_basic;
  }

let tooltip_basic =
  {
    Story.name = "Basic";
    source = Site_story_sources.tooltip_basic;
    controls =
      [
        sides "top";
        Story.choice "delay" ~label:"Delay"
          [
            ("instant", "Instant");
            ("short", "300 ms");
            ("medium", "600 ms");
            ("long", "1000 ms");
          ]
          "medium";
      ];
    render = Tooltip_basic.render;
  }

let tooltip =
  {
    info = Site_components.tooltip;
    chart = Some Site_charts.tooltip;
    stories =
      [
        ("tooltip_basic.mlx", tooltip_basic);
        ( "tooltip_events.mlx",
          {
            Story.name = "Events";
            source = Site_story_sources.tooltip_events;
            controls = [];
            render = Tooltip_events.render;
          }
        );
      ];
    grid = Some tooltip_basic;
  }

let menu_basic =
  {
    Story.name = "Basic";
    source = Site_story_sources.menu_basic;
    controls = [ open_toggle ];
    render = Menu_basic.render;
  }

let menu =
  {
    info = Site_components.menu;
    chart = Some Site_charts.menu;
    stories =
      [
        ("menu_basic.mlx", menu_basic);
        ( "menu_events.mlx",
          {
            Story.name = "Events";
            source = Site_story_sources.menu_events;
            controls = [];
            render = Menu_events.render;
          }
        );
      ];
    grid = Some menu_basic;
  }

let select_basic =
  {
    Story.name = "Basic";
    source = Site_story_sources.select_basic;
    controls =
      [
        Story.choice "value" ~label:"Value"
          [ ("apple", "Apple"); ("banana", "Banana"); ("pear", "Pear") ]
          "apple";
      ];
    render = Select_basic.render;
  }

let select =
  {
    info = Site_components.select;
    chart = Some Site_charts.select;
    stories =
      [
        ("select_basic.mlx", select_basic);
        ( "select_placeholder.mlx",
          {
            Story.name = "Placeholder";
            source = Site_story_sources.select_placeholder;
            controls = [];
            render = Select_placeholder.render;
          }
        );
        ( "select_events.mlx",
          {
            Story.name = "Events";
            source = Site_story_sources.select_events;
            controls = [];
            render = Select_events.render;
          }
        );
      ];
    grid = Some select_basic;
  }

let combobox_basic =
  {
    Story.name = "Basic";
    source = Site_story_sources.combobox_basic;
    controls = [ Story.toggle "inline" ~label:"Inline list" false ];
    render = Combobox_basic.render;
  }

let combobox =
  {
    info = Site_components.combobox;
    chart = Some Site_charts.combobox;
    stories =
      [
        ("combobox_basic.mlx", combobox_basic);
        ( "combobox_events.mlx",
          {
            Story.name = "Events";
            source = Site_story_sources.combobox_events;
            controls = [];
            render = Combobox_events.render;
          }
        );
      ];
    grid = Some combobox_basic;
  }

let command_menu_basic =
  {
    Story.name = "Basic";
    source = Site_story_sources.command_menu_basic;
    controls = [];
    render = Command_menu_basic.render;
  }

let command_menu =
  {
    info = Site_components.command_menu;
    chart = Some Site_charts.command_menu;
    stories =
      [
        ("command_menu_basic.mlx", command_menu_basic);
        ( "command_menu_events.mlx",
          {
            Story.name = "Events";
            source = Site_story_sources.command_menu_events;
            controls = [];
            render = Command_menu_events.render;
          }
        );
      ];
    grid = Some command_menu_basic;
  }

let checkbox_basic =
  {
    Story.name = "Basic";
    source = Site_story_sources.checkbox_basic;
    controls =
      [
        Story.toggle "checked" ~label:"Checked" false;
        Story.toggle "mixed" ~label:"Mixed" false;
        Story.toggle "disabled" ~label:"Disabled" false;
      ];
    render = Checkbox_basic.render;
  }

let checkbox =
  {
    info = Site_components.checkbox;
    chart = Some Site_charts.checkbox;
    stories =
      [
        ("checkbox_basic.mlx", checkbox_basic);
        ( "checkbox_form.mlx",
          {
            Story.name = "In a form";
            source = Site_story_sources.checkbox_form;
            controls = [];
            render = Checkbox_form.render;
          }
        );
        ( "checkbox_events.mlx",
          {
            Story.name = "Events";
            source = Site_story_sources.checkbox_events;
            controls = [];
            render = Checkbox_events.render;
          }
        );
      ];
    grid = Some checkbox_basic;
  }

let switch_basic =
  {
    Story.name = "Basic";
    source = Site_story_sources.switch_basic;
    controls =
      [
        Story.toggle "checked" ~label:"Checked" false;
        Story.toggle "disabled" ~label:"Disabled" false;
      ];
    render = Switch_basic.render;
  }

let switch =
  {
    info = Site_components.switch;
    chart = Some Site_charts.switch;
    stories =
      [
        ("switch_basic.mlx", switch_basic);
        ( "switch_form.mlx",
          {
            Story.name = "In a form";
            source = Site_story_sources.switch_form;
            controls = [];
            render = Switch_form.render;
          }
        );
        ( "switch_events.mlx",
          {
            Story.name = "Events";
            source = Site_story_sources.switch_events;
            controls = [];
            render = Switch_events.render;
          }
        );
      ];
    grid = Some switch_basic;
  }

let tabs_basic =
  {
    Story.name = "Basic";
    source = Site_story_sources.tabs_basic;
    controls =
      [
        Story.choice "orientation" ~label:"Orientation"
          [ ("horizontal", "Horizontal"); ("vertical", "Vertical") ]
          "horizontal";
      ];
    render = Tabs_basic.render;
  }

let tabs =
  {
    info = Site_components.tabs;
    chart = Some Site_charts.tabs;
    stories =
      [
        ("tabs_basic.mlx", tabs_basic);
        ( "tabs_events.mlx",
          {
            Story.name = "Events";
            source = Site_story_sources.tabs_events;
            controls = [];
            render = Tabs_events.render;
          }
        );
      ];
    grid = Some tabs_basic;
  }

let toast_basic =
  {
    Story.name = "Basic";
    source = Site_story_sources.toast_basic;
    controls =
      [
        Story.choice "priority" ~label:"Priority"
          [ ("low", "Low"); ("high", "High") ]
          "low";
      ];
    render = Toast_basic.render;
  }

let toast =
  {
    info = Site_components.toast;
    chart = Some Site_charts.toast;
    stories =
      [
        ("toast_basic.mlx", toast_basic);
        ( "toast_events.mlx",
          {
            Story.name = "Events";
            source = Site_story_sources.toast_events;
            controls = [];
            render = Toast_events.render;
          }
        );
      ];
    grid = Some toast_basic;
  }

let theme_switcher_tokens =
  {
    Story.name = "Tokens";
    source = Site_story_sources.theme_switcher_tokens;
    controls = [];
    render = Theme_switcher_tokens.render;
  }

let theme_switcher =
  {
    info = Site_components.theme_switcher;
    chart = Some Site_charts.theme;
    stories =
      [
        ("theme_switcher_tokens.mlx", theme_switcher_tokens);
        ( "theme_switcher_parts.mlx",
          {
            Story.name = "Parts";
            source = Site_story_sources.theme_switcher_parts;
            controls = [];
            render = Theme_switcher_parts.render;
          }
        );
      ];
    grid = Some theme_switcher_tokens;
  }

let theme_scope =
  {
    info = Site_components.theme_scope;
    chart = None;
    stories =
      [
        ( "theme_scope_basic.mlx",
          {
            Story.name = "Scope";
            source = Site_story_sources.theme_scope_basic;
            controls =
              [
                Story.choice "theme" ~label:"Theme"
                  (List.map
                     (fun t -> (Theme.name t, Theme.label t))
                     Theme.shipped
                  )
                  "dark";
              ];
            render = Theme_scope_basic.render;
          }
        );
        ( "theme_scope_nested.mlx",
          {
            Story.name = "Nested";
            source = Site_story_sources.theme_scope_nested;
            controls = [];
            render = Theme_scope_nested.render;
          }
        );
        ( "theme_scope_samples.mlx",
          {
            Story.name = "Samples";
            source = Site_story_sources.theme_scope_samples;
            controls = [];
            render = Theme_scope_samples.render;
          }
        );
      ];
    grid = None;
  }

let entries =
  [
    dialog;
    popover;
    tooltip;
    menu;
    select;
    combobox;
    command_menu;
    checkbox;
    switch;
    tabs;
    toast;
    theme_switcher;
    theme_scope;
  ]

let find slug = List.find_opt (fun e -> e.info.slug = slug) entries
