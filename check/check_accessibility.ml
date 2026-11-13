open Silica

type 'e placed = {
  element : 'e Accessibility.element;
  ancestors : 'e Accessibility.element list;
  hidden : bool;
}

let rec walk ~ancestors items =
  List.concat_map
    (function
      | Accessibility.Element element ->
          { element; ancestors; hidden = Accessibility.hidden element }
          :: walk ~ancestors:(element :: ancestors)
               (Accessibility.children element)
      | Accessibility.Text _ | Accessibility.Component _ | Accessibility.Loading
        ->
          []
      )
    items

let rec has_unseen items =
  List.exists
    (function
      | Accessibility.Element element ->
          has_unseen (Accessibility.children element)
      | Accessibility.Component _ | Accessibility.Loading ->
          true
      | Accessibility.Text _ ->
          false
      )
    items

let starts_with prefix text =
  String.length text >= String.length prefix
  && String.sub text 0 (String.length prefix) = prefix

let in_portal placed =
  starts_with "portal > " (Accessibility.path placed.element)

let document_order placed =
  let main, portals =
    List.partition (fun item -> not (in_portal item)) placed
  in
  main @ portals

let attribute placed name = Accessibility.attribute placed.element name
let path placed = Accessibility.path placed.element
let tag placed = Accessibility.tag placed.element
let role placed = Accessibility.role placed.element
let name placed = Accessibility.name placed.element

let role_name placed =
  match role placed with
  | Some found ->
      Accessibility.role_to_string found
  | None ->
      tag placed ^ " element"

let words text =
  List.filter
    (fun word -> word <> "")
    (String.split_on_char ' '
       (String.map
          (function '\n' | '\t' | '\r' | '\012' -> ' ' | other -> other)
          text
       )
    )

let quoted text = "\"" ^ text ^ "\""

let element_values placed =
  [ ("role", role_name placed); ("name", quoted (name placed)) ]

let finding ?(values = []) rule placed =
  Check_rules.finding rule ~element:(path placed)
    ~values:(element_values placed @ values)

let input_type placed =
  String.lowercase_ascii (Option.value (attribute placed "type") ~default:"text")

let is_field placed =
  match tag placed with
  | "textarea" | "select" ->
      true
  | "input" -> (
      match input_type placed with
      | "button" | "submit" | "reset" | "image" | "hidden" ->
          false
      | _ ->
          true
    )
  | _ ->
      false

let disabled placed = Accessibility.disabled placed.element

let pressable placed =
  (not (disabled placed))
  && List.exists
       (function
         | Node.On_press _ | Node.On_double_press _ ->
             true
         | Node.Rated (_, (Node.On_press _ | Node.On_double_press _)) ->
             true
         | _ ->
             false
         )
       (Accessibility.attributes placed.element)

let field_roles =
  Accessibility.[ Textbox; Searchbox; Combobox; Listbox; Slider; Spinbutton ]

let named_roles =
  Accessibility.
    [
      Button;
      Link;
      Checkbox;
      Radio;
      Switch;
      Tab;
      Menuitem;
      Menuitemcheckbox;
      Menuitemradio;
      Option;
      Treeitem;
    ]

let is_field_like placed =
  is_field placed
  ||
  match role placed with
  | Some found ->
      List.mem found field_roles
  | None ->
      false

let accessible_name placed =
  if
    placed.hidden || is_field_like placed
    || name placed <> ""
    || not
         (pressable placed
         ||
         match role placed with
         | Some found ->
             List.mem found named_roles
         | None ->
             false
         )
  then
    []
  else
    [ finding "accessible-name" placed ]

let field_label placed =
  if placed.hidden || (not (is_field_like placed)) || name placed <> "" then
    []
  else
    match
      List.find_opt
        (fun ancestor -> Accessibility.tag ancestor = "label")
        placed.ancestors
    with
    | Some label ->
        [
          {
            (finding "field-label" placed
               ~values:[ ("label", Accessibility.path label) ]
            )
            with
            custom = Some Check_rules.empty_label_texts;
          };
        ]
    | None ->
        [ finding "field-label" placed ]

let img_alt placed =
  if
    placed.hidden
    || tag placed <> "img"
    || attribute placed "alt" <> None
    || name placed <> ""
    ||
    match attribute placed "role" with
    | Some value ->
        List.exists
          (fun token -> token = "none" || token = "presentation")
          (words value)
    | None ->
        false
  then
    []
  else
    [ finding "img-alt" placed ]

let unique_ids placed =
  let seen = Hashtbl.create 16 in
  List.filter_map
    (fun item ->
      match attribute item "id" with
      | Some id when Hashtbl.mem seen id ->
          Some (finding "unique-ids" item ~values:[ ("id", id) ])
      | Some id ->
          Hashtbl.replace seen id ();
          None
      | None ->
          None
    )
    placed

let reference_attributes =
  [ "aria-labelledby"; "aria-describedby"; "aria-activedescendant" ]

let aria_reference ~ids placed =
  let references =
    List.concat_map
      (fun attribute_name ->
        match attribute placed attribute_name with
        | Some value ->
            List.map (fun id -> (attribute_name, id)) (words value)
        | None ->
            []
      )
      reference_attributes
    @
    match (tag placed, attribute placed "for") with
    | "label", Some id when id <> "" ->
        [ ("for", id) ]
    | _ ->
        []
  in
  List.filter_map
    (fun (attribute_name, id) ->
      if Hashtbl.mem ids id then
        None
      else
        Some
          (finding "aria-reference" placed
             ~values:[ ("attribute", attribute_name); ("id", id) ]
          )
    )
    references

let aria_roles =
  [
    "alert";
    "alertdialog";
    "application";
    "article";
    "banner";
    "blockquote";
    "button";
    "caption";
    "cell";
    "checkbox";
    "code";
    "columnheader";
    "combobox";
    "comment";
    "complementary";
    "contentinfo";
    "definition";
    "deletion";
    "dialog";
    "directory";
    "document";
    "emphasis";
    "feed";
    "figure";
    "form";
    "generic";
    "grid";
    "gridcell";
    "group";
    "heading";
    "image";
    "img";
    "insertion";
    "link";
    "list";
    "listbox";
    "listitem";
    "log";
    "main";
    "mark";
    "marquee";
    "math";
    "menu";
    "menubar";
    "menuitem";
    "menuitemcheckbox";
    "menuitemradio";
    "meter";
    "navigation";
    "none";
    "note";
    "option";
    "paragraph";
    "presentation";
    "progressbar";
    "radio";
    "radiogroup";
    "region";
    "row";
    "rowgroup";
    "rowheader";
    "scrollbar";
    "search";
    "searchbox";
    "separator";
    "slider";
    "spinbutton";
    "status";
    "strong";
    "subscript";
    "suggestion";
    "superscript";
    "switch";
    "tab";
    "table";
    "tablist";
    "tabpanel";
    "term";
    "textbox";
    "time";
    "timer";
    "toolbar";
    "tooltip";
    "tree";
    "treegrid";
    "treeitem";
  ]

let is_aria_role token =
  List.mem token aria_roles || starts_with "doc-" token
  || starts_with "graphics-" token

let native_checked placed =
  tag placed = "input"
  && (input_type placed = "checkbox" || input_type placed = "radio")

let required_attribute placed = function
  | "checkbox" | "radio" | "switch" | "menuitemcheckbox" | "menuitemradio" ->
      if attribute placed "aria-checked" = None && not (native_checked placed)
      then
        Some "aria-checked"
      else
        None
  | "heading" ->
      if
        attribute placed "aria-level" = None
        && not (List.mem (tag placed) [ "h1"; "h2"; "h3"; "h4"; "h5"; "h6" ])
      then
        Some "aria-level"
      else
        None
  | "combobox" ->
      if attribute placed "aria-expanded" = None then
        Some "aria-expanded"
      else
        None
  | "slider" ->
      if attribute placed "aria-valuenow" = None && input_type placed <> "range"
      then
        Some "aria-valuenow"
      else
        None
  | _ ->
      None

let valid_role placed =
  match attribute placed "role" with
  | None ->
      []
  | Some value -> (
      let tokens = words value in
      match List.find_opt is_aria_role tokens with
      | None when tokens <> [] ->
          [
            finding "valid-role" placed
              ~values:
                [ ("value", value); ("problem", "which is not an ARIA role") ];
          ]
      | None ->
          []
      | Some token -> (
          match required_attribute placed token with
          | Some missing ->
              [
                finding "valid-role" placed
                  ~values:
                    [
                      ("value", value);
                      ( "problem",
                        "but it has no " ^ missing ^ ", which the role " ^ token
                        ^ " needs"
                      );
                    ];
              ]
          | None ->
              []
        )
    )

let owned_roles =
  Accessibility.
    [
      (Listbox, [ Option ], "option");
      (Tablist, [ Tab ], "tab");
      (Menu, [ Menuitem; Menuitemcheckbox; Menuitemradio ], "menuitem");
      (Menubar, [ Menuitem; Menuitemcheckbox; Menuitemradio ], "menuitem");
    ]

let rec owned items =
  List.concat_map
    (function
      | Accessibility.Element element -> (
          if Accessibility.hidden element then
            []
          else
            match Accessibility.role element with
            | None | Some Accessibility.Generic | Some Accessibility.Group ->
                owned (Accessibility.children element)
            | Some found ->
                [ `Owned (element, found) ]
        )
      | Accessibility.Text text ->
          if String.trim text = "" then
            []
          else
            [ `Text text ]
      | Accessibility.Component _ | Accessibility.Loading ->
          []
      )
    items

let required_children placed =
  match role placed with
  | None ->
      []
  | Some container -> (
      match
        List.find_opt (fun (found, _, _) -> found = container) owned_roles
      with
      | None ->
          []
      | Some _ when placed.hidden ->
          []
      | Some (_, allowed, allowed_name) ->
          List.filter_map
            (function
              | `Owned (element, found) ->
                  if List.mem found allowed then
                    None
                  else
                    Some
                      ("the "
                      ^ Accessibility.role_to_string found
                      ^ " at " ^ Accessibility.path element
                      )
              | `Text text ->
                  Some ("the text " ^ quoted (String.trim text))
              )
            (owned (Accessibility.children placed.element))
          |> List.map (fun child ->
              finding "required-children" placed
                ~values:[ ("child", child); ("allowed", allowed_name) ]
          )
    )

let focusable_tag placed =
  match tag placed with
  | "button" | "select" | "textarea" | "summary" ->
      true
  | "a" | "area" ->
      attribute placed "href" <> None
  | "input" ->
      input_type placed <> "hidden"
  | _ ->
      false

let interactive placed =
  (not placed.hidden)
  && (focusable_tag placed
     || ( match attribute placed "tabindex" with
       | Some value -> (
           match int_of_string_opt (String.trim value) with
           | Some index ->
               index >= 0
           | None ->
               false
         )
       | None ->
           false
       )
     ||
     match role placed with
     | Some found ->
         List.mem found named_roles || List.mem found field_roles
     | None ->
         false
     )

let atomic_roles =
  Accessibility.
    [
      Button;
      Link;
      Tab;
      Option;
      Checkbox;
      Radio;
      Switch;
      Menuitemcheckbox;
      Menuitemradio;
    ]

let nested_interactive placed =
  if not (interactive placed) then
    []
  else
    match
      List.find_opt
        (fun above ->
          match Accessibility.role above with
          | Some found ->
              List.mem found atomic_roles
          | None ->
              false
        )
        placed.ancestors
    with
    | None ->
        []
    | Some outer ->
        let outer_role =
          match Accessibility.role outer with
          | Some found ->
              Accessibility.role_to_string found
          | None ->
              Accessibility.tag outer
        in
        [
          Check_rules.finding "nested-interactive"
            ~element:(Accessibility.path outer)
            ~values:
              [
                ("role", outer_role);
                ("name", quoted (Accessibility.name outer));
                ("inner_role", role_name placed);
                ("inner_name", quoted (name placed));
                ("inner", path placed);
              ];
        ]

let keyboard_roles = Accessibility.[ Button; Link; Checkbox; Switch ]

let keyboard_access placed =
  if
    placed.hidden
    || (not (pressable placed))
    || focusable_tag placed
    || attribute placed "tabindex" <> None
  then
    []
  else
    match role placed with
    | None | Some Accessibility.Generic ->
        [
          finding "keyboard-access" placed
            ~values:[ ("problem", "it has no role and no tabindex") ];
        ]
    | Some found when List.mem found keyboard_roles ->
        [
          finding "keyboard-access" placed
            ~values:[ ("problem", "it has a role but no tabindex") ];
        ]
    | Some _ ->
        []

let is_dialog placed =
  match role placed with
  | Some (Accessibility.Dialog | Accessibility.Alertdialog) ->
      true
  | _ ->
      false

let dialog_name placed =
  if placed.hidden || (not (is_dialog placed)) || name placed <> "" then
    []
  else
    [ finding "dialog-name" placed ]

let landmark_main placed =
  let mains =
    List.filter
      (fun item -> (not item.hidden) && role item = Some Accessibility.Main)
      placed
  in
  let nested item =
    List.exists
      (fun ancestor -> Accessibility.role ancestor = Some Accessibility.Main)
      item.ancestors
  in
  List.concat
    (List.mapi
       (fun index item ->
         if nested item then
           [
             finding "landmark-main" item
               ~values:[ ("problem", "is inside another main landmark") ];
           ]
         else if index > 0 then
           [
             finding "landmark-main" item
               ~values:
                 [
                   ( "problem",
                     Printf.sprintf "is main landmark %d of %d in the view"
                       (index + 1) (List.length mains)
                   );
                 ];
           ]
         else
           []
       )
       mains
    )

let heading_order placed =
  let check (previous, findings) item =
    match (role item, Accessibility.level item.element) with
    | Some Accessibility.Heading, Some level when not item.hidden ->
        let previous =
          if in_portal item && item.ancestors = [] then
            None
          else
            previous
        in
        let findings =
          match previous with
          | Some before when level > before + 1 ->
              finding "heading-order" item
                ~values:
                  [
                    ("level", string_of_int level);
                    ("previous", string_of_int before);
                    ("next", string_of_int (before + 1));
                  ]
              :: findings
          | _ ->
              findings
        in
        (Some level, findings)
    | _ ->
        let previous =
          if in_portal item && item.ancestors = [] then
            None
          else
            previous
        in
        (previous, findings)
  in
  List.rev (snd (List.fold_left check (None, []) (document_order placed)))

let inline_tags =
  [
    "a";
    "abbr";
    "b";
    "bdi";
    "bdo";
    "cite";
    "code";
    "data";
    "dfn";
    "em";
    "i";
    "img";
    "kbd";
    "label";
    "mark";
    "q";
    "s";
    "samp";
    "small";
    "span";
    "strong";
    "sub";
    "sup";
    "time";
    "u";
    "var";
  ]

let spaced_roles =
  Accessibility.
    [
      Button;
      Link;
      Tab;
      Option;
      Menuitem;
      Menuitemcheckbox;
      Menuitemradio;
      Checkbox;
      Radio;
      Switch;
      Treeitem;
      Heading;
    ]

let non_empty text =
  let trimmed = String.trim text in
  if trimmed = "" then
    None
  else
    Some trimmed

let content_parts element =
  let buffer = Buffer.create 32 in
  let joints = ref [] in
  let joint inline = joints := (Buffer.length buffer, inline) :: !joints in
  let rec add element =
    List.iter
      (function
        | Accessibility.Text text ->
            Buffer.add_string buffer text
        | Accessibility.Element child ->
            let inline = List.mem (Accessibility.tag child) inline_tags in
            let field =
              match Accessibility.tag child with
              | "textarea" | "select" | "input" ->
                  true
              | _ ->
                  false
            in
            if not (Accessibility.hidden child || field) then (
              joint inline;
              ( match
                  Option.bind
                    (Accessibility.attribute child "aria-label")
                    non_empty
                with
              | Some label ->
                  Buffer.add_string buffer label
              | None -> (
                  match
                    ( Accessibility.tag child,
                      Accessibility.attribute child "alt"
                    )
                  with
                  | "img", alt ->
                      Buffer.add_string buffer (Option.value alt ~default:"")
                  | _ ->
                      add child
                )
              );
              joint inline
            )
        | Accessibility.Component _ | Accessibility.Loading ->
            ()
        )
      (Accessibility.children element)
  in
  add element;
  (Buffer.contents buffer, List.rev !joints)

let is_word_character = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' ->
      true
  | _ ->
      false

let word_before text position =
  let rec start index =
    if index > 0 && text.[index - 1] <> ' ' then
      start (index - 1)
    else
      index
  in
  let from = start position in
  String.sub text from (position - from)

let word_after text position =
  let rec stop index =
    if index < String.length text && text.[index] <> ' ' then
      stop (index + 1)
    else
      index
  in
  String.sub text position (stop position - position)

let name_spacing placed =
  let content_role =
    match role placed with
    | Some found ->
        List.mem found spaced_roles
    | None ->
        false
  in
  if
    placed.hidden || (not content_role)
    || attribute placed "aria-label" <> None
    || attribute placed "aria-labelledby" <> None
  then
    []
  else
    let text, joints = content_parts placed.element in
    let length = String.length text in
    let flat =
      String.map
        (function '\n' | '\t' | '\r' | '\012' -> ' ' | other -> other)
        text
    in
    if String.concat " " (words flat) <> name placed then
      []
    else
      match
        List.find_opt
          (fun (joint, inline) ->
            inline && joint > 0 && joint < length
            && (not (List.mem (joint, false) joints))
            && is_word_character flat.[joint - 1]
            && is_word_character flat.[joint]
          )
          joints
      with
      | None ->
          []
      | Some (joint, _) ->
          [
            finding "name-spacing" placed
              ~values:
                [
                  ("left", quoted (word_before flat joint));
                  ("right", quoted (word_after flat joint));
                ];
          ]

let view_rules ?memo view =
  let compute () =
    let items = Accessibility.items view in
    let placed = walk ~ancestors:[] items in
    let ids = Hashtbl.create 16 in
    List.iter
      (fun item ->
        Option.iter (fun id -> Hashtbl.replace ids id ()) (attribute item "id")
      )
      placed;
    let unseen = has_unseen items in
    let each =
      List.concat_map
        (fun item ->
          accessible_name item @ field_label item @ img_alt item
          @ ( if unseen then
                []
              else
                aria_reference ~ids item
            )
          @ valid_role item @ required_children item @ nested_interactive item
          @ keyboard_access item @ dialog_name item @ name_spacing item
        )
        (document_order placed)
    in
    each
    @ unique_ids (document_order placed)
    @ heading_order placed
    @ landmark_main (document_order placed)
  in
  match memo with
  | None ->
      compute ()
  | Some table -> (
      let rec attribute = function
        | Node.String (key, value) ->
            `String (key, value)
        | Node.Bool (key, value) ->
            `Bool (key, value)
        | Node.Ref reference ->
            `Ref (Ref.name reference)
        | Node.On_press _ ->
            `On_press
        | Node.On_double_press _ ->
            `On_double_press
        | Node.On_input _ ->
            `On_input
        | Node.On_submit _ ->
            `On_submit
        | Node.On_focus _ ->
            `On_focus
        | Node.On_blur _ ->
            `On_blur
        | Node.On_key_down { prevent; _ } ->
            `On_key_down prevent
        | Node.On_key_up { prevent; _ } ->
            `On_key_up prevent
        | Node.On_pointer_enter { prevent; _ } ->
            `On_pointer_enter prevent
        | Node.On_pointer_leave { prevent; _ } ->
            `On_pointer_leave prevent
        | Node.On_reset ->
            `On_reset
        | Node.Rated (rate, inner) ->
            `Rated (rate, attribute inner)
      in
      let rec shape : type e. e Node.t -> _ =
       fun node ->
        match Node.view node with
        | Node.Element (tag, attrs, children) ->
            `Element (tag, List.map attribute attrs, List.map shape children)
        | Node.Text text ->
            `Text text
        | Node.List nodes ->
            `List (List.map shape nodes)
        | Node.Keyed (key, child) ->
            `Keyed (key, shape child)
        | Node.Portal (id, nodes) ->
            `Portal (id, List.map shape nodes)
        | Node.Boundary { body; _ } ->
            `Boundary (shape body)
        | Node.Component { component; _ } ->
            `Component (Machine.id component.machine)
        | Node.Chunk (Node.Chunk_view { chunk; render; loading = _ }) -> (
            match Chunk.get chunk with
            | Some value ->
                `Chunk (shape (render value))
            | None ->
                `Loading
          )
      in
      match shape view with
      | exception _ ->
          compute ()
      | key -> (
          let key = (Hashtbl.hash_param 256 1024 key, key) in
          match Hashtbl.find_opt table key with
          | Some findings ->
              findings
          | None ->
              let findings = compute () in
              Hashtbl.replace table key findings;
              findings
        )
    )

let dialog_focus ~held view =
  let traps =
    List.filter_map
      (function
        | Action.Focus_trap { container; _ } ->
            Some (Ref.name container)
        | _ ->
            None
        )
      held
  in
  let trapped element =
    match Accessibility.attribute element "data-silica-ref" with
    | Some reference ->
        List.mem reference traps
    | None ->
        false
  in
  List.filter_map
    (fun item ->
      if
        is_dialog item && (not item.hidden)
        && Accessibility.modal item.element
        && not (List.exists trapped (item.element :: item.ancestors))
      then
        Some (finding "dialog-focus" item)
      else
        None
    )
    (walk ~ancestors:[] (Accessibility.items view))
