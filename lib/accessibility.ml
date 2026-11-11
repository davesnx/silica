type role =
  | Alert
  | Alertdialog
  | Banner
  | Button
  | Cell
  | Checkbox
  | Columnheader
  | Combobox
  | Complementary
  | Contentinfo
  | Dialog
  | Form
  | Generic
  | Grid
  | Gridcell
  | Group
  | Heading
  | Img
  | Link
  | List
  | Listbox
  | Listitem
  | Main
  | Menu
  | Menubar
  | Menuitem
  | Menuitemcheckbox
  | Menuitemradio
  | Navigation
  | Option
  | Progressbar
  | Radio
  | Radiogroup
  | Region
  | Row
  | Rowheader
  | Search
  | Searchbox
  | Separator
  | Slider
  | Spinbutton
  | Status
  | Switch
  | Tab
  | Table
  | Tablist
  | Tabpanel
  | Textbox
  | Toolbar
  | Tooltip
  | Tree
  | Treeitem

let role_to_string = function
  | Alert ->
      "alert"
  | Alertdialog ->
      "alertdialog"
  | Banner ->
      "banner"
  | Button ->
      "button"
  | Cell ->
      "cell"
  | Checkbox ->
      "checkbox"
  | Columnheader ->
      "columnheader"
  | Combobox ->
      "combobox"
  | Complementary ->
      "complementary"
  | Contentinfo ->
      "contentinfo"
  | Dialog ->
      "dialog"
  | Form ->
      "form"
  | Generic ->
      "generic"
  | Grid ->
      "grid"
  | Gridcell ->
      "gridcell"
  | Group ->
      "group"
  | Heading ->
      "heading"
  | Img ->
      "img"
  | Link ->
      "link"
  | List ->
      "list"
  | Listbox ->
      "listbox"
  | Listitem ->
      "listitem"
  | Main ->
      "main"
  | Menu ->
      "menu"
  | Menubar ->
      "menubar"
  | Menuitem ->
      "menuitem"
  | Menuitemcheckbox ->
      "menuitemcheckbox"
  | Menuitemradio ->
      "menuitemradio"
  | Navigation ->
      "navigation"
  | Option ->
      "option"
  | Progressbar ->
      "progressbar"
  | Radio ->
      "radio"
  | Radiogroup ->
      "radiogroup"
  | Region ->
      "region"
  | Row ->
      "row"
  | Rowheader ->
      "rowheader"
  | Search ->
      "search"
  | Searchbox ->
      "searchbox"
  | Separator ->
      "separator"
  | Slider ->
      "slider"
  | Spinbutton ->
      "spinbutton"
  | Status ->
      "status"
  | Switch ->
      "switch"
  | Tab ->
      "tab"
  | Table ->
      "table"
  | Tablist ->
      "tablist"
  | Tabpanel ->
      "tabpanel"
  | Textbox ->
      "textbox"
  | Toolbar ->
      "toolbar"
  | Tooltip ->
      "tooltip"
  | Tree ->
      "tree"
  | Treeitem ->
      "treeitem"

let all_roles =
  [
    Alert;
    Alertdialog;
    Banner;
    Button;
    Cell;
    Checkbox;
    Columnheader;
    Combobox;
    Complementary;
    Contentinfo;
    Dialog;
    Form;
    Generic;
    Grid;
    Gridcell;
    Group;
    Heading;
    Img;
    Link;
    List;
    Listbox;
    Listitem;
    Main;
    Menu;
    Menubar;
    Menuitem;
    Menuitemcheckbox;
    Menuitemradio;
    Navigation;
    Option;
    Progressbar;
    Radio;
    Radiogroup;
    Region;
    Row;
    Rowheader;
    Search;
    Searchbox;
    Separator;
    Slider;
    Spinbutton;
    Status;
    Switch;
    Tab;
    Table;
    Tablist;
    Tabpanel;
    Textbox;
    Toolbar;
    Tooltip;
    Tree;
    Treeitem;
  ]

let role_of_string name =
  List.find_opt (fun role -> role_to_string role = name) all_roles

let content_roles =
  [
    Button;
    Cell;
    Checkbox;
    Columnheader;
    Gridcell;
    Heading;
    Link;
    Menuitem;
    Menuitemcheckbox;
    Menuitemradio;
    Option;
    Radio;
    Row;
    Rowheader;
    Switch;
    Tab;
    Tooltip;
    Treeitem;
  ]

let name_from_content role = List.mem role content_roles

type 'e element = {
  tag : string;
  attrs : 'e Node.attr list;
  ancestors : 'e element list;
  path : string;
  document : 'e document;
  mutable children : 'e item list;
}

and 'e item =
  | Element of 'e element
  | Text of string
  | Component of string
  | Loading

and 'e document = {
  mutable all : 'e element list;
  ids : (string, 'e element) Hashtbl.t;
}

type 'e raw =
  | Raw_element of string * 'e Node.attr list * 'e raw list
  | Raw_text of string
  | Raw_component of string
  | Raw_loading
  | Raw_portal of 'e raw list

let rec flatten : type e. e Node.t -> e raw list =
 fun node ->
  match Node.view node with
  | Node.Element (tag, attrs, children) ->
      [ Raw_element (tag, attrs, List.concat_map flatten children) ]
  | Node.Text "" ->
      []
  | Node.Text text ->
      [ Raw_text text ]
  | Node.List nodes ->
      List.concat_map flatten nodes
  | Node.Keyed (_, node) ->
      flatten node
  | Node.Portal (_, nodes) ->
      [ Raw_portal (List.concat_map flatten nodes) ]
  | Node.Boundary { body; _ } ->
      flatten body
  | Node.Component { component; _ } ->
      [ Raw_component (Machine.id component.machine) ]
  | Node.Chunk (Node.Chunk_view { chunk; render; loading = _ }) -> (
      match Chunk.get chunk with
      | Some value ->
          flatten (render value)
      | None ->
          [ Raw_loading ]
    )

let attribute_in attrs name =
  List.find_map
    (function[@warning "-4"]
      | Node.String (key, value) when key = name ->
          Some value
      | Node.Bool (key, true) when key = name ->
          Some ""
      | Node.Ref reference when name = "data-silica-ref" ->
          Some (Ref.name reference)
      | _ ->
          None
      )
    attrs

let build node =
  let document = { all = []; ids = Hashtbl.create 16 } in
  let portals = ref [] in
  let rec make ~ancestors ~prefix ~collect raws =
    let counts = Hashtbl.create 8 in
    List.iter
      (function[@warning "-4"]
        | Raw_element (tag, _, _) ->
            Hashtbl.replace counts tag
              (1 + Option.value (Hashtbl.find_opt counts tag) ~default:0)
        | _ ->
            ()
        )
      raws;
    let count tag = Option.value (Hashtbl.find_opt counts tag) ~default:0 in
    let seen = Hashtbl.create 8 in
    List.concat_map
      (function
        | Raw_text text ->
            [ Text text ]
        | Raw_component id ->
            [ Component id ]
        | Raw_loading ->
            [ Loading ]
        | Raw_portal raws ->
            let inner = ref [] in
            let items =
              make ~ancestors:[] ~prefix:"portal > " ~collect:inner raws
            in
            portals := List.rev !inner :: !portals;
            items
        | Raw_element (tag, attrs, raws) ->
            let position =
              1 + Option.value (Hashtbl.find_opt seen tag) ~default:0
            in
            Hashtbl.replace seen tag position;
            let step =
              if count tag > 1 then
                Printf.sprintf "%s[%d]" tag position
              else
                tag
            in
            let element =
              {
                tag;
                attrs;
                ancestors;
                path = prefix ^ step;
                document;
                children = [];
              }
            in
            collect := element :: !collect;
            element.children <-
              make ~ancestors:(element :: ancestors)
                ~prefix:(element.path ^ " > ") ~collect raws;
            [ Element element ]
        )
      raws
  in
  let main = ref [] in
  let items = make ~ancestors:[] ~prefix:"" ~collect:main (flatten node) in
  document.all <- List.rev !main @ List.concat (List.rev !portals);
  List.iter
    (fun element ->
      match attribute_in element.attrs "id" with
      | Some id when not (Hashtbl.mem document.ids id) ->
          Hashtbl.add document.ids id element
      | _ ->
          ()
    )
    document.all;
  (items, document)

let items node = fst (build node)
let elements node = (snd (build node)).all
let children element = element.children
let tag element = element.tag
let attributes element = element.attrs
let attribute element name = attribute_in element.attrs name
let path element = element.path

let words text =
  List.filter
    (fun word -> word <> "")
    (String.split_on_char ' '
       (String.map
          (function '\n' | '\t' | '\r' | '\012' -> ' ' | other -> other)
          text
       )
    )

let normalize text = String.concat " " (words text)

let input_type element =
  String.lowercase_ascii
    (Option.value (attribute element "type") ~default:"text")

let is_true element name = attribute element name = Some "true"

let self_hidden element =
  attribute element "hidden" <> None
  || attribute element "inert" <> None
  || is_true element "aria-hidden"
  || (element.tag = "input" && input_type element = "hidden")

let hidden element = List.exists self_hidden (element :: element.ancestors)

let is_field element =
  match element.tag with
  | "textarea" | "select" ->
      true
  | "input" -> (
      match input_type element with
      | "button" | "submit" | "reset" | "image" | "hidden" ->
          false
      | _ ->
          true
    )
  | _ ->
      false

let explicit_role element =
  match attribute element "role" with
  | None ->
      None
  | Some value -> (
      let tokens = words value in
      match
        List.find_map
          (fun token ->
            if token = "none" || token = "presentation" then
              Some None
            else
              Option.map Option.some (role_of_string token)
          )
          tokens
      with
      | Some found ->
          Some found
      | None ->
          None
    )

let non_empty text =
  if text = "" then
    None
  else
    Some text

let rec first_of = function
  | [] ->
      ""
  | candidate :: rest -> (
      match candidate () with Some text -> text | None -> first_of rest
    )

let by_ids element value =
  List.filter_map
    (fun id -> Hashtbl.find_opt element.document.ids id)
    (words value)

let rec implicit_role element =
  let scoped = [ "article"; "aside"; "main"; "nav"; "section" ] in
  match element.tag with
  | "button" ->
      Some Button
  | "a" | "area" ->
      if attribute element "href" <> None then
        Some Link
      else
        None
  | "h1" | "h2" | "h3" | "h4" | "h5" | "h6" ->
      Some Heading
  | "ul" | "ol" | "menu" ->
      Some List
  | "li" ->
      Some Listitem
  | "input" -> (
      let listed = attribute element "list" <> None in
      match input_type element with
      | "button" | "submit" | "reset" | "image" ->
          Some Button
      | "checkbox" ->
          Some Checkbox
      | "radio" ->
          Some Radio
      | "range" ->
          Some Slider
      | "number" ->
          Some Spinbutton
      | "search" ->
          Some
            ( if listed then
                Combobox
              else
                Searchbox
            )
      | "text" | "email" | "tel" | "url" ->
          Some
            ( if listed then
                Combobox
              else
                Textbox
            )
      | "hidden"
      | "password"
      | "file"
      | "color"
      | "date"
      | "datetime-local"
      | "month"
      | "time"
      | "week" ->
          None
      | _ ->
          Some
            ( if listed then
                Combobox
              else
                Textbox
            )
    )
  | "textarea" ->
      Some Textbox
  | "select" ->
      let size =
        Option.value
          (Option.bind (attribute element "size") int_of_string_opt)
          ~default:0
      in
      if attribute element "multiple" <> None || size > 1 then
        Some Listbox
      else
        Some Combobox
  | "option" ->
      Some Option
  | "optgroup" | "fieldset" | "details" ->
      Some Group
  | "img" ->
      if attribute element "alt" = Some "" then
        None
      else
        Some Img
  | "nav" ->
      Some Navigation
  | "main" ->
      Some Main
  | "aside" ->
      Some Complementary
  | "form" ->
      Some Form
  | "search" ->
      Some Search
  | "dialog" ->
      Some Dialog
  | "header" | "footer" ->
      if List.exists (fun above -> List.mem above.tag scoped) element.ancestors
      then
        None
      else if element.tag = "header" then
        Some Banner
      else
        Some Contentinfo
  | "section" ->
      if own_name element = "" then
        None
      else
        Some Region
  | "table" ->
      Some Table
  | "tr" ->
      Some Row
  | "td" ->
      Some Cell
  | "th" ->
      if attribute element "scope" = Some "row" then
        Some Rowheader
      else
        Some Columnheader
  | "hr" ->
      Some Separator
  | "progress" ->
      Some Progressbar
  | "output" ->
      Some Status
  | _ ->
      None

and role element =
  match explicit_role element with
  | Some found ->
      found
  | None ->
      implicit_role element

and own_name element =
  normalize
    (first_of
       [
         (fun () ->
           Option.bind (attribute element "aria-labelledby") (fun value ->
               non_empty (labelled_by element value)
           )
         );
         (fun () -> Option.bind (attribute element "aria-label") non_empty_trim);
         (fun () -> Option.bind (attribute element "title") non_empty_trim);
       ]
    )

and non_empty_trim text = non_empty (String.trim text)

and labelled_by element value =
  String.concat " "
    (List.map (fun target -> inner_name target) (by_ids element value))

and content element =
  String.concat ""
    (List.map
       (function
         | Text text ->
             text
         | Element child ->
             if self_hidden child then
               ""
             else
               inner_name child
         | Component _ | Loading ->
             ""
         )
       element.children
    )

and inner_name element =
  if is_field element then
    ""
  else
    first_of
      [
        (fun () -> Option.bind (attribute element "aria-label") non_empty_trim);
        (fun () -> native_name element);
        (fun () -> non_empty (content element));
        (fun () -> Option.bind (attribute element "title") non_empty_trim);
      ]

and native_name element =
  let child_text tag =
    List.find_map
      (function[@warning "-4"]
        | Element child when child.tag = tag ->
            non_empty (content child)
        | _ ->
            None
        )
      element.children
  in
  match element.tag with
  | "input" when not (is_field element) -> (
      match (input_type element, attribute element "value") with
      | "image", _ ->
          Option.bind (attribute element "alt") non_empty
      | _, Some value when value <> "" ->
          Some value
      | "submit", _ ->
          Some "Submit"
      | "reset", _ ->
          Some "Reset"
      | _ ->
          None
    )
  | ("input" | "textarea" | "select") when is_field element ->
      let for_labels =
        match attribute element "id" with
        | Some id ->
            List.filter
              (fun other ->
                other.tag = "label" && attribute other "for" = Some id
              )
              element.document.all
        | None ->
            []
      in
      let around =
        List.filter (fun above -> above.tag = "label") element.ancestors
      in
      let labels =
        List.filter_map
          (fun label -> non_empty (content label))
          (for_labels
          @ List.filter (fun label -> not (List.memq label for_labels)) around
          )
      in
      non_empty (String.concat " " labels)
  | "img" | "area" ->
      Option.bind (attribute element "alt") non_empty
  | "fieldset" ->
      child_text "legend"
  | "table" ->
      child_text "caption"
  | _ ->
      None

let name element =
  let from_content () =
    match role element with
    | Some found when name_from_content found ->
        non_empty (normalize (content element))
    | Some _ | None ->
        None
  in
  normalize
    (first_of
       [
         (fun () ->
           Option.bind (attribute element "aria-labelledby") (fun value ->
               non_empty (normalize (labelled_by element value))
           )
         );
         (fun () -> Option.bind (attribute element "aria-label") non_empty_trim);
         (fun () ->
           Option.bind (native_name element) (fun text ->
               non_empty (normalize text)
           )
         );
         from_content;
         (fun () -> Option.bind (attribute element "title") non_empty_trim);
         (fun () ->
           if element.tag = "input" || element.tag = "textarea" then
             Option.bind (attribute element "placeholder") non_empty_trim
           else
             None
         );
       ]
    )

let tristate element name =
  match attribute element name with
  | Some "true" ->
      Some true
  | Some "false" ->
      Some false
  | Some _ | None ->
      None

let level element =
  match Option.bind (attribute element "aria-level") int_of_string_opt with
  | Some level ->
      Some level
  | None -> (
      match element.tag with
      | "h1" ->
          Some 1
      | "h2" ->
          Some 2
      | "h3" ->
          Some 3
      | "h4" ->
          Some 4
      | "h5" ->
          Some 5
      | "h6" ->
          Some 6
      | _ ->
          None
    )

let checked element =
  match attribute element "aria-checked" with
  | Some "true" ->
      Some true
  | Some "false" ->
      Some false
  | Some _ ->
      None
  | None ->
      if
        element.tag = "input"
        && (input_type element = "checkbox" || input_type element = "radio")
      then
        Some (attribute element "checked" <> None)
      else
        None

let selected element =
  match tristate element "aria-selected" with
  | Some state ->
      Some state
  | None ->
      if element.tag = "option" then
        Some (attribute element "selected" <> None)
      else
        None

let expanded element = tristate element "aria-expanded"

let disabled element =
  is_true element "aria-disabled"
  || List.mem element.tag
       [
         "button";
         "input";
         "select";
         "textarea";
         "option";
         "optgroup";
         "fieldset";
       ]
     && attribute element "disabled" <> None

let current element =
  match attribute element "aria-current" with
  | Some "false" | None ->
      None
  | Some value ->
      Some value

let value element =
  match attribute element "aria-valuetext" with
  | Some text ->
      Some text
  | None -> (
      match attribute element "aria-valuenow" with
      | Some number ->
          Some number
      | None ->
          if List.mem element.tag [ "input"; "textarea"; "select" ] then
            attribute element "value"
          else
            None
    )

let modal element = is_true element "aria-modal"
