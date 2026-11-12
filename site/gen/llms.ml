module Ast = Odoc_parser.Ast

type page = {
  slug : string;
  file : string;
  title : string;
  anchors : (string * string) list;
  blocks : Ast.block_element Ast.with_location list;
}

type site = { base : string; docs : string; pages : page list }

let fail file fmt = Printf.ksprintf (fun m -> failwith (file ^ ": " ^ m)) fmt
let read path = In_channel.with_open_bin path In_channel.input_all

let slugify text =
  let buffer = Buffer.create (String.length text) in
  let dash = ref false in
  String.iter
    (fun character ->
      match Char.lowercase_ascii character with
      | ('a' .. 'z' | '0' .. '9') as character ->
          if !dash && Buffer.length buffer > 0 then Buffer.add_char buffer '-';
          dash := false;
          Buffer.add_char buffer character
      | _ ->
          dash := true
    )
    text;
  Buffer.contents buffer

let is_word_character = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' ->
      true
  | _ ->
      false

let escape text =
  let buffer = Buffer.create (String.length text) in
  let length = String.length text in
  String.iteri
    (fun index character ->
      match character with
      | '\\' | '`' | '*' | '[' | ']' | '<' | '|' ->
          Buffer.add_char buffer '\\';
          Buffer.add_char buffer character
      | '_'
        when index = 0
             || index = length - 1
             || (not (is_word_character text.[index - 1]))
             || not (is_word_character text.[index + 1]) ->
          Buffer.add_string buffer "\\_"
      | character ->
          Buffer.add_char buffer character
    )
    text;
  Buffer.contents buffer

let longest_run character text =
  let best = ref 0 and current = ref 0 in
  String.iter
    (fun c ->
      if c = character then (
        incr current;
        best := max !best !current
      ) else
        current := 0
    )
    text;
  !best

let code_span text =
  let fence = String.make (longest_run '`' text + 1) '`' in
  let pad =
    if text <> "" && (text.[0] = '`' || text.[String.length text - 1] = '`')
    then
      " "
    else
      ""
  in
  fence ^ pad ^ text ^ pad ^ fence

let rec plain (inline : Ast.inline_element Ast.with_location) =
  match inline.value with
  | `Space _ ->
      " "
  | `Word word | `Code_span word ->
      word
  | `Styled (_, content) | `Link (_, content) ->
      String.concat "" (List.map plain content)
  | `Reference (_, target, []) ->
      target.value
  | `Reference (_, _, content) ->
      String.concat "" (List.map plain content)
  | `Raw_markup _ | `Math_span _ ->
      ""

let anchors_of file blocks =
  let used = Hashtbl.create 16 in
  List.filter_map
    (fun (block : Ast.block_element Ast.with_location) ->
      match block.value with
      | `Heading (level, label, content) when level > 0 ->
          let text = String.concat "" (List.map plain content) in
          let anchor =
            match label with
            | Some label ->
                label
            | None ->
                let base = slugify text in
                let rec free count =
                  let candidate =
                    if count = 1 then
                      base
                    else
                      base ^ "-" ^ string_of_int count
                  in
                  if Hashtbl.mem used candidate then
                    free (count + 1)
                  else
                    candidate
                in
                free 1
          in
          if Hashtbl.mem used anchor then
            fail file "two headings with the anchor %S" anchor;
          Hashtbl.replace used anchor ();
          Some (anchor, text)
      | _ ->
          None
    )
    blocks

let parse path =
  let file = Filename.basename path in
  let parsed =
    Odoc_parser.parse_comment
      ~location:
        { Lexing.pos_fname = file; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
      ~text:(read path)
  in
  List.iter
    (fun warning -> fail file "%s" (Odoc_parser.Warning.to_string warning))
    (Odoc_parser.warnings parsed);
  match Odoc_parser.ast parsed with
  | { value = `Heading (0, _, content); _ } :: blocks ->
      {
        slug = Filename.chop_suffix file ".mld";
        file;
        title = String.concat "" (List.map plain content);
        anchors = anchors_of file blocks;
        blocks;
      }
  | _ ->
      fail file "the page does not start with a {0 title}"

let page_url site slug anchor =
  let path =
    if slug = "index" then
      "/docs"
    else
      "/docs/" ^ slug
  in
  site.base ^ path ^ match anchor with Some a -> "#" ^ a | None -> ""

let markdown_url site slug = site.base ^ "/docs/" ^ slug ^ ".md"

let find_page site file slug =
  match List.find_opt (fun page -> page.slug = slug) site.pages with
  | Some page ->
      page
  | None ->
      fail file "a link to an unknown page %S" slug

let heading_text file page anchor =
  match List.assoc_opt anchor page.anchors with
  | Some text ->
      text
  | None ->
      fail file "a link to an unknown anchor %s#%s" page.slug anchor

let reference_name target =
  String.split_on_char '.' target
  |> List.map (fun part ->
      match String.index_opt part '-' with
      | Some index ->
          String.sub part (index + 1) (String.length part - index - 1)
      | None ->
          part
  )
  |> String.concat "."

let split_page_target rest =
  match String.index_opt rest '.' with
  | Some index ->
      ( String.sub rest 0 index,
        Some (String.sub rest (index + 1) (String.length rest - index - 1))
      )
  | None ->
      (rest, None)

let after prefix text =
  String.sub text (String.length prefix)
    (String.length text - String.length prefix)

let rec inline site (page : page)
    (element : Ast.inline_element Ast.with_location) =
  match element.value with
  | `Space _ ->
      " "
  | `Word word ->
      escape word
  | `Code_span code ->
      code_span code
  | `Styled (`Bold, content) ->
      "**" ^ inlines site page content ^ "**"
  | `Styled ((`Italic | `Emphasis), content) ->
      "*" ^ inlines site page content ^ "*"
  | `Link (href, []) ->
      "<" ^ href ^ ">"
  | `Link (href, content) ->
      "[" ^ inlines site page content ^ "](" ^ href ^ ")"
  | `Reference (_, target, content) -> (
      let target = target.value in
      let link slug anchor =
        let linked = find_page site page.file slug in
        let label =
          match (content, anchor) with
          | [], Some anchor ->
              escape (heading_text page.file linked anchor)
          | [], None ->
              escape linked.title
          | content, _ ->
              inlines site page content
        in
        Option.iter (fun a -> ignore (heading_text page.file linked a)) anchor;
        "[" ^ label ^ "](" ^ page_url site slug anchor ^ ")"
      in
      if String.starts_with ~prefix:"page-" target then
        let slug, anchor = split_page_target (after "page-" target) in
        link slug anchor
      else if String.starts_with ~prefix:"section-" target then
        link page.slug (Some (after "section-" target))
      else
        match content with
        | [] ->
            code_span (reference_name target)
        | content ->
            inlines site page content ^ " ("
            ^ code_span (reference_name target)
            ^ ")"
    )
  | `Styled ((`Superscript | `Subscript), _) | `Raw_markup _ | `Math_span _ ->
      fail page.file "unsupported inline markup at line %d"
        element.location.start.line

and inlines site page content =
  String.concat "" (List.map (inline site page) content)

let paragraph site page content =
  let text = inlines site page content in
  let starts_a_block =
    text <> ""
    &&
    match text.[0] with
    | '#' | '-' | '+' | '=' | '>' ->
        true
    | '0' .. '9' ->
        let rec digits index =
          index < String.length text
          &&
          match text.[index] with
          | '0' .. '9' ->
              digits (index + 1)
          | '.' | ')' ->
              true
          | _ ->
              false
        in
        digits 0
    | _ ->
        false
  in
  if starts_a_block then
    "\\" ^ text
  else
    text

let fence language source =
  let ticks = String.make (max 3 (longest_run '`' source + 1)) '`' in
  ticks ^ language ^ "\n" ^ source ^ "\n" ^ ticks

let indent prefix text =
  String.split_on_char '\n' text
  |> List.mapi (fun index line ->
      if index = 0 then
        prefix ^ line
      else if line = "" then
        ""
      else
        String.make (String.length prefix) ' ' ^ line
  )
  |> String.concat "\n"

let code_source site (block : Ast.code_block) (location : Odoc_parser.Loc.span)
    =
  let language, path =
    match block.meta with
    | None ->
        ("", None)
    | Some { language; tags } ->
        ( language.value,
          List.find_map
            (function
              | `Binding ({ Odoc_parser.Loc.value = "file"; _ }, value) ->
                  Some value.Odoc_parser.Loc.value
              | _ ->
                  None
              )
            tags
        )
  in
  let source =
    match path with
    | Some path ->
        let text = read (Filename.concat site.docs path) in
        if String.ends_with ~suffix:"\n" text then
          String.sub text 0 (String.length text - 1)
        else
          text
    | None ->
        fst (Odoc_parser.codeblock_content location block.content.value)
  in
  (language, source)

let rec nestable site page (block : Ast.nestable_block_element Ast.with_location)
    =
  match block.value with
  | `Paragraph content ->
      paragraph site page content
  | `Code_block code ->
      let language, source = code_source site code block.location in
      fence language source
  | `Verbatim text ->
      fence "" (fst (Odoc_parser.verbatim_content block.location text))
  | `List (kind, _, items) ->
      let tight =
        List.for_all
          (function
            | [ { Odoc_parser.Loc.value = `Paragraph _; _ } ] ->
                true
            | _ ->
                false
            )
          items
      in
      List.mapi
        (fun index item ->
          let marker =
            match kind with
            | `Unordered ->
                "- "
            | `Ordered ->
                string_of_int (index + 1) ^ ". "
          in
          indent marker
            (String.concat "\n\n" (List.map (nestable site page) item))
        )
        items
      |> String.concat
           ( if tight then
               "\n"
             else
               "\n\n"
           )
  | `Table ((grid, _), _) -> (
      let cell (content, _) =
        List.map
          (fun (block : Ast.nestable_block_element Ast.with_location) ->
            match block.value with
            | `Paragraph content ->
                inlines site page content
            | _ ->
                fail page.file "a table cell holds only text, line %d"
                  block.location.start.line
          )
          content
        |> String.concat " "
      in
      let row cells = "| " ^ String.concat " | " (List.map cell cells) ^ " |" in
      match grid with
      | head :: rows ->
          String.concat "\n"
            (row head
            :: ("|" ^ String.concat "|" (List.map (fun _ -> "---") head) ^ "|")
            :: List.map row rows
            )
      | [] ->
          fail page.file "an empty table at line %d" block.location.start.line
    )
  | `Modules _ | `Math_block _ | `Media _ ->
      fail page.file "unsupported block markup at line %d"
        block.location.start.line

let block site page (block : Ast.block_element Ast.with_location) =
  match block.value with
  | `Heading (level, _, content) ->
      String.make (level + 1) '#' ^ " " ^ inlines site page content
  | `Tag _ ->
      fail page.file "tags are not supported, line %d" block.location.start.line
  | #Ast.nestable_block_element as nestable_block ->
      nestable site page { block with value = nestable_block }

let markdown site page =
  String.concat "\n\n"
    (("# " ^ escape page.title) :: List.map (block site page) page.blocks)
  ^ "\n"

type entry = {
  label : string;
  url : string;
  description : string;
  listed : string option;
}

type group = { name : string; entries : entry list }

let pages_section (index : page) =
  let rec from = function
    | { Odoc_parser.Loc.value = `Heading (1, Some "pages", _); _ } :: rest ->
        rest
    | _ :: rest ->
        from rest
    | [] ->
        fail index.file "no {1:pages ...} section"
  in
  let rec until = function
    | { Odoc_parser.Loc.value = `Heading (level, _, _); _ } :: _ when level <= 1
      ->
        []
    | block :: rest ->
        block :: until rest
    | [] ->
        []
  in
  until (from index.blocks)

let strip_colon text =
  if String.starts_with ~prefix:": " text then
    after ": " text
  else
    text

let entry site index (item : Ast.nestable_block_element Ast.with_location list)
    =
  match item with
  | [ { value = `Paragraph (first :: rest); location } ] -> (
      let description = strip_colon (inlines site index rest) in
      match first.value with
      | `Reference (_, target, [])
        when String.starts_with ~prefix:"page-" target.value ->
          let slug = after "page-" target.value in
          let linked = find_page site index.file slug in
          {
            label = escape linked.title;
            url = markdown_url site slug;
            description;
            listed = Some slug;
          }
      | `Link (href, content) ->
          {
            label = inlines site index content;
            url = href;
            description;
            listed = None;
          }
      | _ ->
          fail index.file
            "line %d: an item of the pages section starts with {!page-...} or \
             a link"
            location.start.line
    )
  | _ ->
      fail index.file "an item of the pages section is one paragraph"

let groups site index =
  let rec collect name groups = function
    | { Odoc_parser.Loc.value = `Paragraph content; location } :: rest ->
        let text = inlines site index content in
        if String.ends_with ~suffix:":" text then
          collect
            (Some (String.sub text 0 (String.length text - 1)))
            groups rest
        else
          fail index.file
            "line %d: the pages section holds group names and lists"
            location.start.line
    | { Odoc_parser.Loc.value = `List (_, _, items); location } :: rest -> (
        match name with
        | Some name ->
            collect None
              ({ name; entries = List.map (entry site index) items } :: groups)
              rest
        | None ->
            fail index.file "line %d: a list with no group name before it"
              location.start.line
      )
    | { Odoc_parser.Loc.location; _ } :: _ ->
        fail index.file "line %d: the pages section holds group names and lists"
          location.start.line
    | [] ->
        List.rev groups
  in
  collect None [] (pages_section index)

let load ~base ~docs =
  let files =
    Sys.readdir docs |> Array.to_list
    |> List.filter (fun file -> Filename.check_suffix file ".mld")
    |> List.sort compare
  in
  let pages = List.map (fun file -> parse (Filename.concat docs file)) files in
  let site = { base; docs; pages } in
  let index = find_page site "index.mld" "index" in
  let groups = groups site index in
  let listed =
    List.concat_map
      (fun group -> List.filter_map (fun entry -> entry.listed) group.entries)
      groups
  in
  List.iter
    (fun page ->
      let count = List.length (List.filter (( = ) page.slug) listed) in
      if page.slug <> "index" && count <> 1 then
        fail "index.mld" "the pages section lists %s %d times, not once"
          page.file count
    )
    pages;
  (site, index, groups, listed)

let summary site (index : page) =
  match index.blocks with
  | { value = `Paragraph content; _ } :: _ ->
      inlines site index content
  | _ ->
      fail index.file "the first block after the title is not a paragraph"

let llms_txt (site, index, groups, _) =
  let group group =
    "## " ^ group.name ^ "\n\n"
    ^ String.concat ""
        (List.map
           (fun entry ->
             Printf.sprintf "- [%s](%s): %s\n" entry.label entry.url
               entry.description
           )
           group.entries
        )
  in
  String.concat "\n"
    (Printf.sprintf "# %s\n\n> %s\n" (escape index.title) (summary site index)
    :: Printf.sprintf
         "Each page below is Markdown. All pages in one file: %s/llms-full.txt\n"
         site.base
    :: List.map group groups
    )

let ordered (site, index, _, listed) =
  index :: List.map (fun slug -> find_page site index.file slug) listed

let llms_full ((site, _, _, _) as loaded) =
  String.concat "\n" (List.map (markdown site) (ordered loaded))

let ocaml_module ((site, _, _, _) as loaded) =
  Printf.sprintf
    "[@@@ocamlformat \"disable\"]\n\n\
     let index = %S\n\n\
     let full = %S\n\n\
     let pages = [%s]\n"
    (llms_txt loaded) (llms_full loaded)
    (String.concat "; "
       (List.map
          (fun page -> Printf.sprintf "(%S, %S)" page.slug (markdown site page))
          (ordered loaded)
       )
    )

let main () =
  match Array.to_list Sys.argv with
  | [ _; "index"; base; docs ] ->
      print_string (llms_txt (load ~base ~docs))
  | [ _; "full"; base; docs ] ->
      print_string (llms_full (load ~base ~docs))
  | [ _; "page"; base; docs; slug ] ->
      let site, _, _, _ = load ~base ~docs in
      print_string (markdown site (find_page site "page" slug))
  | [ _; "ml"; base; docs ] ->
      print_string (ocaml_module (load ~base ~docs))
  | _ ->
      prerr_endline
        "usage: llms (index|full|ml) BASE_URL DOCS_DIR\n\
        \       llms page BASE_URL DOCS_DIR SLUG";
      exit 2

let () =
  try main ()
  with Failure message ->
    prerr_endline ("llms: " ^ message);
    exit 1
