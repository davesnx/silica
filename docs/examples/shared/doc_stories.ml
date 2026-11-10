open Silica_ui

let checkbox =
  {
    Story.name = "Checkbox";
    source = Doc_checkbox_story_source.text;
    controls =
      [
        Story.choice "label" ~label:"Label"
          [ ("Accept the terms", "Terms"); ("Send me the news", "News") ]
          "Accept the terms";
        Story.toggle "checked" ~label:"Checked" false;
        Story.toggle "disabled" ~label:"Disabled" false;
      ];
    render = Doc_checkbox_story.render;
  }
