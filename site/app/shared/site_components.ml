type info = { slug : string; name : string; modules : string; summary : string }

let dialog =
  {
    slug = "dialog";
    name = "Dialog";
    modules = "Silica_ui.Dialog";
    summary =
      "A modal window over a backdrop. Focus stays inside it and the page does \
       not scroll; Escape and a press outside close it, and focus goes back to \
       the trigger. The content and its events are yours.";
  }

let popover =
  {
    slug = "popover";
    name = "Popover";
    modules = "Silica_ui.Popover";
    summary =
      "A non-modal popup next to a trigger button. Focus moves into it but is \
       not trapped, and the popup flips to the other side when there is no \
       room.";
  }

let tooltip =
  {
    slug = "tooltip";
    name = "Tooltip";
    modules = "Silica_ui.Tooltip";
    summary =
      "A short text that describes a button. It shows after a delay when the \
       pointer rests on the trigger, at once on keyboard focus, and never \
       takes focus.";
  }

let menu =
  {
    slug = "menu";
    name = "Menu";
    modules = "Silica_ui.Menu";
    summary =
      "A button that opens a list of actions. Focus moves to the highlighted \
       item; arrows, Home, End and letters move it, and disabled items are \
       skipped.";
  }

let select =
  {
    slug = "select";
    name = "Select";
    modules = "Silica_ui.Select";
    summary =
      "A button that shows the selected option and opens a list to pick \
       another. Focus stays on the button. With a name, a hidden input carries \
       the value for a form.";
  }

let combobox =
  {
    slug = "combobox";
    name = "Combobox";
    modules = "Silica_ui.Combobox";
    summary =
      "A text input with a list of options that filters as you type, by label \
       and keywords. The list is a popup under the input, or inline and always \
       shown.";
  }

let command_menu =
  {
    slug = "command-menu";
    name = "Command menu";
    modules = "Silica_ui.Command_menu";
    summary =
      "A command palette: a modal dialog with a search input and an inline \
       list. The parent opens it, for example from a global shortcut; Enter \
       runs the first match and closes it.";
  }

let checkbox =
  {
    slug = "checkbox";
    name = "Checkbox";
    modules = "Silica_ui.Checkbox";
    summary =
      "A button with the checkbox role: checked, not checked or mixed. Space \
       and a press toggle it. With a name it adds its value to a form, as a \
       native checkbox does.";
  }

let switch =
  {
    slug = "switch";
    name = "Switch";
    modules = "Silica_ui.Switch";
    summary =
      "A button with the switch role that turns a setting on or off. With a \
       name it adds its value to a form, as a checkbox does.";
  }

let tabs =
  {
    slug = "tabs";
    name = "Tabs";
    modules = "Silica_ui.Tabs";
    summary =
      "A list of tabs with manual activation: arrows move focus, Enter, Space \
       or a press select. You paint the panels from the value you follow.";
  }

let toast =
  {
    slug = "toast";
    name = "Toast";
    modules = "Silica_ui.Toast";
    summary =
      "Short messages in a live region. You own the list; the oldest toast \
       closes by itself after a timeout, which pauses while the pointer or \
       focus is in the region. A high priority toast is an alert dialog.";
  }

let theme_switcher =
  {
    slug = "theme-switcher";
    name = "Theme switcher";
    modules = "Silica_ui.Theme";
    summary =
      "A select of System and each theme of a set. It sets data-theme on the \
       page and stores the choice in a cookie, so the first HTML has the \
       theme. A page has one theme component: this site's is in the header, \
       and the stories here follow it.";
  }

let theme_scope =
  {
    slug = "theme-scope";
    name = "Theme scope";
    modules = "Silica_ui.Theme.scope";
    summary =
      "Puts a part of the page in another theme, whatever the theme of the \
       page. Popups opened from inside the scope use its theme too.";
  }

let all =
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

let find slug = List.find_opt (fun i -> i.slug = slug) all
