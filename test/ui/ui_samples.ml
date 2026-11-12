open Silica
open Silica_ui

type note = { text : string }
type note_event = Edit of string | Save

let note_view { text } =
  Node.element "form"
    [ Attr.on_submit Save ]
    [
      Node.element "input"
        [
          Attr.string "id" "note-text";
          Attr.string "aria-label" "Note";
          Attr.string "value" text;
          Attr.on_input (fun value -> Edit value);
        ]
        [];
      Node.element "button"
        [ Attr.string "type" "submit"; Attr.string "id" "note-save" ]
        [ Node.text "Save" ];
    ]

let note_dialog =
  Dialog.component ~id:"note-dialog"
    ~closes:(function Save -> true | Edit _ -> false)
    ~event_name:(function Edit _ -> "edit" | Save -> "save")
    note_view

type parent_event =
  | Opened of bool * Popup.reason
  | Edited of note_event
  | Set_open of bool option

let reason = Popup.reason_name

let page_machine =
  Machine.make ~id:"note-page" ~init:(None, [])
    ~name:(fun _ -> "page")
    (fun (open_, log) event ->
      match event with
      | Opened (opened, why) ->
          Go (open_, log @ [ Printf.sprintf "open %b %s" opened (reason why) ])
      | Edited (Edit text) ->
          Go (open_, log @ [ "edit " ^ text ])
      | Edited Save ->
          Go (open_, log @ [ "save" ])
      | Set_open open_ ->
          Go (open_, log)
    )

let page_view ?(alert = false) ?(dismissible = true) (open_, log) =
  Node.element "main" []
    [
      Node.element "p"
        [ Attr.string "id" "log" ]
        [ Node.text (String.concat "; " log) ];
      Dialog.make
        ~on_open_change:(fun opened why -> Opened (opened, why))
        ~on_content:(fun event -> Edited event)
        note_dialog
        (Dialog.props ~id:"note" ~title:"Edit note"
           ~description:"Change the text of the note." ~trigger:"Edit"
           ~close:"Close" ?open_ ~dismissible ~alert { text = "hello" }
        );
    ]

let page = Component.make page_machine (fun s -> page_view s)

let alert_page = Component.make page_machine (fun s -> page_view ~alert:true s)

let locked_page =
  Component.make page_machine (fun s -> page_view ~dismissible:false s)

module Checkbox_samples = Checkbox_samples
module Switch_samples = Switch_samples
module Tabs_samples = Tabs_samples
module Toast_samples = Toast_samples
