open Silica

type kind = Plain | Keyword | String | Name | Comment

let keywords =
  [
    "and"; "as"; "begin"; "do"; "done"; "else"; "end"; "exception";
    "external"; "false"; "for"; "fun"; "function"; "functor"; "if"; "in";
    "include"; "let"; "match"; "module"; "mutable"; "nonrec"; "of"; "open";
    "private"; "rec"; "sig"; "struct"; "then"; "to"; "true"; "try"; "type";
    "val"; "when"; "while"; "with";
  ]
[@@ocamlformat "disable"]

let is_ident c =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' ->
      true
  | _ ->
      false

let tokens ~ml text =
  let n = String.length text in
  let out = ref [] in
  let push kind a b =
    if b > a then out := (kind, String.sub text a (b - a)) :: !out
  in
  let rec comment i depth =
    if i >= n then
      n
    else if i + 1 < n && text.[i] = '*' && text.[i + 1] = ')' then
      if depth = 1 then
        i + 2
      else
        comment (i + 2) (depth - 1)
    else if i + 1 < n && text.[i] = '(' && text.[i + 1] = '*' then
      comment (i + 2) (depth + 1)
    else
      comment (i + 1) depth
  in
  let rec string i =
    if i >= n then
      n
    else if text.[i] = '\\' then
      string (i + 2)
    else if text.[i] = '"' then
      i + 1
    else
      string (i + 1)
  in
  let rec quoted i =
    if i + 1 >= n then
      n
    else if text.[i] = '|' && text.[i + 1] = '}' then
      i + 2
    else
      quoted (i + 1)
  in
  let rec ident i =
    if i < n && is_ident text.[i] then
      ident (i + 1)
    else
      i
  in
  let rec line i =
    if i < n && text.[i] <> '\n' then
      line (i + 1)
    else
      i
  in
  let rec go plain i =
    if i >= n then
      push Plain plain n
    else
      let token kind stop =
        push Plain plain i;
        push kind i stop;
        go stop stop
      in
      match text.[i] with
      | '(' when ml && i + 1 < n && text.[i + 1] = '*' ->
          token Comment (comment (i + 2) 1)
      | ';' when (not ml) && i + 1 < n && text.[i + 1] = ';' ->
          token Comment (line i)
      | '"' ->
          token String (string (i + 1))
      | '{' when ml && i + 1 < n && text.[i + 1] = '|' ->
          token String (quoted (i + 2))
      | ('a' .. 'z' | '_') when i = 0 || not (is_ident text.[i - 1]) ->
          let stop = ident i in
          if ml && List.mem (String.sub text i (stop - i)) keywords then
            token Keyword stop
          else
            go plain stop
      | 'A' .. 'Z' when ml && (i = 0 || not (is_ident text.[i - 1])) ->
          token Name (ident i)
      | _ ->
          go plain (i + 1)
  in
  go 0 0;
  List.rev !out

let class_of = function
  | Plain ->
      None
  | Keyword ->
      Some "text-code-keyword font-semibold"
  | String ->
      Some "text-code-string"
  | Name ->
      Some "text-code-name"
  | Comment ->
      Some "text-code-comment italic"

let highlight ~lang text : 'e Node.t list =
  let ml = List.mem lang [ "ocaml"; "mlx"; "ml" ] in
  List.map
    (fun (kind, part) ->
      match class_of kind with
      | None ->
          Node.text part
      | Some c ->
          Node.element "span" [ Attr.string "class" c ] [ Node.text part ]
    )
    (tokens ~ml text)

let view ?file ~lang source : 'e Node.t =
  Node.element "figure"
    [
      Attr.string "class"
        "my-6 overflow-hidden rounded-ui border border-ui-border \
         bg-code-surface";
    ]
    [
      ( match file with
      | Some file ->
          Node.element "figcaption"
            [
              Attr.string "class"
                "flex items-center justify-between border-b border-ui-border \
                 px-4 py-2 font-mono text-xs text-ui-muted";
            ]
            [
              Node.element "span" [] [ Node.text file ];
              Node.element "span" [] [ Node.text lang ];
            ]
      | None ->
          Node.list []
      );
      Node.element "pre"
        [
          Attr.string "class"
            "overflow-x-auto p-4 font-mono text-[0.8125rem] leading-6 \
             text-ui-text";
        ]
        [ Node.element "code" [] (highlight ~lang source) ];
    ]
