type piece = Line of int * string | Words of int * string

let quote text =
  let buffer = Buffer.create (String.length text + 2) in
  Buffer.add_char buffer '"';
  String.iter
    (function
      | ('"' | '\\') as special ->
          Buffer.add_char buffer '\\';
          Buffer.add_char buffer special
      | other ->
          Buffer.add_char buffer other
      )
    text;
  Buffer.add_char buffer '"';
  Buffer.contents buffer

let rec handler_name : type e. e Node.attr -> string option = function
  | Node.String _ | Node.Bool _ | Node.Ref _ ->
      None
  | Node.On_press _ ->
      Some "press"
  | Node.On_double_press _ ->
      Some "double_press"
  | Node.On_input _ ->
      Some "input"
  | Node.On_submit _ ->
      Some "submit"
  | Node.On_focus _ ->
      Some "focus"
  | Node.On_blur _ ->
      Some "blur"
  | Node.On_key_down _ ->
      Some "key_down"
  | Node.On_key_up _ ->
      Some "key_up"
  | Node.On_pointer_enter _ ->
      Some "pointer_enter"
  | Node.On_pointer_leave _ ->
      Some "pointer_leave"
  | Node.On_reset ->
      Some "reset"
  | Node.Rated (_, inner) ->
      handler_name inner

let handlers element =
  List.fold_left
    (fun names attr ->
      match handler_name attr with
      | Some name when not (List.mem name names) ->
          names @ [ name ]
      | Some _ | None ->
          names
    )
    []
    (Accessibility.attributes element)

let flag name = function
  | Some true ->
      [ name ]
  | Some false ->
      [ name ^ "=false" ]
  | None ->
      []

let states element =
  List.concat
    [
      Option.fold ~none:[]
        ~some:(fun level -> [ "level=" ^ string_of_int level ])
        (Accessibility.level element);
      Option.fold ~none:[]
        ~some:(fun value -> [ "value=" ^ quote value ])
        (Accessibility.value element);
      flag "checked" (Accessibility.checked element);
      ( if Accessibility.selected element = Some true then
          [ "selected" ]
        else
          []
      );
      flag "expanded" (Accessibility.expanded element);
      ( if Accessibility.disabled element then
          [ "disabled" ]
        else
          []
      );
      Option.fold ~none:[]
        ~some:(fun current -> [ "current=" ^ current ])
        (Accessibility.current element);
      ( if Accessibility.role element = Some Accessibility.Link then
          Option.fold ~none:[]
            ~some:(fun href -> [ "href=" ^ quote href ])
            (Accessibility.attribute element "href")
        else
          []
      );
      ( if Accessibility.modal element then
          [ "modal" ]
        else
          []
      );
    ]

let describe element role handlers =
  let head =
    match role with
    | Some role ->
        Accessibility.role_to_string role
    | None ->
        Accessibility.tag element
  in
  let name =
    match Accessibility.name element with "" -> [] | name -> [ quote name ]
  in
  let handlers =
    match handlers with
    | [] ->
        []
    | names ->
        [ "[" ^ String.concat ", " names ^ "]" ]
  in
  String.concat " " ((head :: name) @ states element @ handlers)

let rec pieces depth ~skip_text items =
  List.concat_map (piece depth ~skip_text) items

and piece depth ~skip_text = function
  | Accessibility.Text text ->
      if skip_text then
        []
      else
        [ Words (depth, text) ]
  | Accessibility.Component id ->
      [ Line (depth, "component " ^ id) ]
  | Accessibility.Loading ->
      [ Line (depth, "loading") ]
  | Accessibility.Element element -> (
      if Accessibility.hidden element then
        []
      else
        match Accessibility.attribute element "data-silica-live" with
        | Some channel ->
            [ Line (depth, "island " ^ channel) ]
        | None -> (
            let role = Accessibility.role element in
            let handlers = handlers element in
            let children = Accessibility.children element in
            match (role, handlers) with
            | None, [] ->
                (Words (depth, " ") :: pieces depth ~skip_text children)
                @ [ Words (depth, " ") ]
            | _ ->
                let skip_text =
                  match role with
                  | Some role ->
                      Accessibility.name_from_content role
                  | None ->
                      false
                in
                Line (depth, describe element role handlers)
                :: pieces (depth + 1) ~skip_text children
          )
    )

let collapse text =
  String.concat " "
    (List.filter
       (fun word -> word <> "")
       (String.split_on_char ' '
          (String.map
             (function '\n' | '\t' | '\r' | '\012' -> ' ' | other -> other)
             text
          )
       )
    )

let to_string node =
  let buffer = Buffer.create 256 in
  let line depth text =
    Buffer.add_string buffer (String.make (2 * depth) ' ');
    Buffer.add_string buffer text;
    Buffer.add_char buffer '\n'
  in
  let flush = function
    | None ->
        ()
    | Some (depth, words) -> (
        match collapse (String.concat "" (List.rev words)) with
        | "" ->
            ()
        | text ->
            line depth ("text " ^ quote text)
      )
  in
  let rec emit pending = function
    | [] ->
        flush pending
    | Words (depth, text) :: rest -> (
        match pending with
        | Some (same, words) when same = depth ->
            emit (Some (depth, text :: words)) rest
        | Some _ | None ->
            flush pending;
            emit (Some (depth, [ text ])) rest
      )
    | Line (depth, text) :: rest ->
        flush pending;
        line depth text;
        emit None rest
  in
  emit None (pieces 0 ~skip_text:false (Accessibility.items node));
  Buffer.contents buffer

let visible node =
  let buffer = Buffer.create 256 in
  let rec walk items =
    List.iter
      (function
        | Accessibility.Element element ->
            if not (Accessibility.hidden element) then
              walk (Accessibility.children element)
        | Accessibility.Text text ->
            Buffer.add_string buffer text
        | Accessibility.Component _ | Accessibility.Loading ->
            ()
        )
      items
  in
  walk (Accessibility.items node);
  Buffer.contents buffer

let contains text part =
  let length = String.length part in
  let rec go index =
    index + length <= String.length text
    && (String.sub text index length = part || go (index + 1))
  in
  go 0

let shows node text = contains (visible node) text
