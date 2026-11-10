open Silica
open Silica_ui

module Note = struct
  type props = { text : string }
  type event = Edit of string | Save

  let id = "note-dialog"

  let view { text } =
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

  let closes = function Save -> true | Edit _ -> false
end

module Note_dialog = Dialog.Make (Note)

type parent_event =
  | Opened of bool * Popup.reason
  | Edited of Note.event
  | Set_open of bool option

module Parent_state = struct
  type _ t = Page : (bool option * string list) t
end

module Parent = Machine.Make (Parent_state)

let reason = Popup.reason_name

let parent_machine ~trigger =
  ignore trigger;
  Parent.make ~id:"note-page"
    ~init:(Parent.at Page (None, []))
    [
      Parent.state Page "page" (fun (open_, log) -> function
        | Opened (opened, why) ->
            Parent.goto Page
              (open_, log @ [ Printf.sprintf "open %b %s" opened (reason why) ])
        | Edited (Note.Edit text) ->
            Parent.goto Page (open_, log @ [ "edit " ^ text ])
        | Edited Note.Save ->
            Parent.goto Page (open_, log @ [ "save" ])
        | Set_open open_ ->
            Parent.goto Page (open_, log)
        );
    ]

let page_machine = parent_machine ~trigger:true

let page_view ?(alert = false) ?(dismissible = true)
    (Parent.At (Page, (open_, log))) =
  Node.element "main" []
    [
      Node.element "p"
        [ Attr.string "id" "log" ]
        [ Node.text (String.concat "; " log) ];
      Note_dialog.make
        ~on_open_change:(fun opened why -> Opened (opened, why))
        ~on_content:(fun event -> Edited event)
        (Note_dialog.props ~id:"note" ~title:"Edit note"
           ~description:"Change the text of the note." ~trigger:"Edit"
           ~close:"Close" ?open_ ~dismissible ~alert { Note.text = "hello" }
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
