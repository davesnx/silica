module Ast = Odoc_parser.Ast
module Loc = Odoc_parser.Loc

type inline =
  | Text of string
  | Code of string
  | Strong of inline list
  | Emph of inline list
  | Link of string * inline list
  | Page_link of string * string option * inline list option

type block =
  | Heading of int * string * inline list
  | Paragraph of inline list
  | Items of bool * block list list
  | Code_block of string * string option * string
  | Table of inline list list * inline list list list

type page = {
  slug : string;
  file : string;
  title : string;
  blocks : block list;
}

let fail file fmt = Printf.ksprintf (fun m -> failwith (file ^ ": " ^ m)) fmt
let read path = In_channel.with_open_bin path In_channel.input_all

let slugify text =
  let b = Buffer.create (String.length text) in
  let dash = ref false in
  String.iter
    (fun c ->
      match Char.lowercase_ascii c with
      | ('a' .. 'z' | '0' .. '9') as c ->
          if !dash && Buffer.length b > 0 then Buffer.add_char b '-';
          dash := false;
          Buffer.add_char b c
      | _ ->
          dash := true
    )
    text;
  Buffer.contents b

let rec inline file (x : Ast.inline_element Ast.with_location) =
  match x.value with
  | `Space _ ->
      [ Text " " ]
  | `Word w ->
      [ Text w ]
  | `Code_span c ->
      [ Code c ]
  | `Styled (`Bold, content) ->
      [ Strong (inlines file content) ]
  | `Styled ((`Italic | `Emphasis), content) ->
      [ Emph (inlines file content) ]
  | `Link (href, content) ->
      [
        Link
          (href, match content with [] -> [ Text href ] | c -> inlines file c);
      ]
  | `Reference (kind, target, content) -> (
      let label =
        match kind with
        | `Simple ->
            None
        | `With_text ->
            Some (inlines file content)
      in
      let target = target.value in
      let split rest =
        match String.index_opt rest '.' with
        | Some i ->
            ( String.sub rest 0 i,
              Some (String.sub rest (i + 1) (String.length rest - i - 1))
            )
        | None ->
            (rest, None)
      in
      match String.split_on_char '-' target with
      | "page" :: _ ->
          let page, anchor =
            split (String.sub target 5 (String.length target - 5))
          in
          [ Page_link (page, anchor, label) ]
      | "section" :: _ ->
          [
            Page_link
              ("", Some (String.sub target 8 (String.length target - 8)), label);
          ]
      | _ -> (
          match label with Some content -> content | None -> [ Code target ]
        )
    )
  | `Styled ((`Superscript | `Subscript), _) | `Raw_markup _ | `Math_span _ ->
      fail file "unsupported inline markup at line %d" x.location.start.line

and inlines file l =
  let merged =
    List.fold_left
      (fun acc i ->
        match (i, acc) with
        | Text a, Text b :: rest ->
            Text (b ^ a) :: rest
        | i, acc ->
            i :: acc
      )
      []
      (List.concat_map (inline file) l)
  in
  List.rev merged

let code_meta = function
  | None ->
      ("text", None)
  | Some { Ast.language; tags } ->
      ( language.value,
        List.find_map
          (function
            | `Binding ({ Loc.value = "file"; _ }, { Loc.value; _ }) ->
                Some value
            | _ ->
                None
            )
          tags
      )

let rec nestable ~docs file (x : Ast.nestable_block_element Ast.with_location) =
  match x.value with
  | `Paragraph content ->
      Paragraph (inlines file content)
  | `Code_block { meta; content; _ } ->
      let lang, path = code_meta meta in
      let source =
        match path with
        | Some path ->
            let text = read (Filename.concat docs path) in
            if String.ends_with ~suffix:"\n" text then
              String.sub text 0 (String.length text - 1)
            else
              text
        | None ->
            fst (Odoc_parser.codeblock_content x.location content.value)
      in
      Code_block (lang, path, source)
  | `Verbatim text ->
      Code_block ("text", None, text)
  | `List (kind, _, items) ->
      Items (kind = `Ordered, List.map (List.map (nestable ~docs file)) items)
  | `Table ((grid, _), _) -> (
      let cell (content, _) =
        List.concat_map
          (fun (b : Ast.nestable_block_element Ast.with_location) ->
            match b.value with
            | `Paragraph p ->
                inlines file p
            | _ ->
                fail file "a table cell holds only text, line %d"
                  b.location.start.line
          )
          content
      in
      match grid with
      | head :: rows ->
          Table (List.map cell head, List.map (List.map cell) rows)
      | [] ->
          fail file "empty table"
    )
  | `Modules _ | `Math_block _ | `Media _ ->
      fail file "unsupported block markup at line %d" x.location.start.line

let rec plain = function
  | Text t | Code t ->
      t
  | Strong c | Emph c | Link (_, c) | Page_link (_, _, Some c) ->
      String.concat "" (List.map plain c)
  | Page_link (p, _, None) ->
      p

let page ~docs path =
  let file = Filename.basename path in
  let slug = Filename.chop_suffix file ".mld" in
  let text = read path in
  let parsed =
    Odoc_parser.parse_comment
      ~location:
        { Lexing.pos_fname = file; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
      ~text
  in
  List.iter
    (fun w -> fail file "%s" (Odoc_parser.Warning.to_string w))
    (Odoc_parser.warnings parsed);
  let title = ref None in
  let used = Hashtbl.create 16 in
  let blocks =
    List.filter_map
      (fun (b : Ast.block_element Ast.with_location) ->
        match b.value with
        | `Heading (0, _, content) ->
            title :=
              Some (String.concat "" (List.map plain (inlines file content)));
            None
        | `Heading (level, label, content) ->
            let text = inlines file content in
            let anchor =
              match label with
              | Some l ->
                  l
              | None ->
                  let base = slugify (String.concat "" (List.map plain text)) in
                  let rec free n =
                    let a =
                      if n = 1 then
                        base
                      else
                        base ^ "-" ^ string_of_int n
                    in
                    if Hashtbl.mem used a then
                      free (n + 1)
                    else
                      a
                  in
                  free 1
            in
            Hashtbl.replace used anchor ();
            Some (Heading (level, anchor, text))
        | `Tag _ ->
            fail file "tags are not supported, line %d" b.location.start.line
        | #Ast.nestable_block_element as n ->
            Some (nestable ~docs file { b with value = n })
      )
      (Odoc_parser.ast parsed)
  in
  match !title with
  | Some title ->
      { slug; file; title; blocks }
  | None ->
      fail file "no {0 title}"

let anchors page =
  List.filter_map
    (function Heading (_, a, _) -> Some a | _ -> None)
    page.blocks

let check pages =
  let find slug = List.find_opt (fun p -> p.slug = slug) pages in
  let rec walk_inline page = function
    | Page_link (target, anchor, label) -> (
        let target_page =
          if target = "" then
            Some page
          else
            find target
        in
        match target_page with
        | None ->
            fail page.file "link to an unknown page %S" target
        | Some t -> (
            Option.iter (List.iter (walk_inline page)) label;
            match anchor with
            | Some a when not (List.mem a (anchors t)) ->
                fail page.file "link to an unknown anchor %s#%s" t.slug a
            | _ ->
                ()
          )
      )
    | Strong c | Emph c | Link (_, c) ->
        List.iter (walk_inline page) c
    | Text _ | Code _ ->
        ()
  and walk_block page = function
    | Heading (_, _, c) | Paragraph c ->
        List.iter (walk_inline page) c
    | Items (_, items) ->
        List.iter (List.iter (walk_block page)) items
    | Table (head, rows) ->
        List.iter (List.iter (List.iter (walk_inline page))) (head :: rows)
    | Code_block _ ->
        ()
  in
  List.iter
    (fun p ->
      let seen = Hashtbl.create 16 in
      List.iter
        (fun a ->
          if Hashtbl.mem seen a then
            fail p.file "two headings with the anchor %S" a;
          Hashtbl.add seen a ()
        )
        (anchors p);
      List.iter (walk_block p) p.blocks
    )
    pages

let pages_with_titles pages =
  let title slug = (List.find (fun p -> p.slug = slug) pages).title in
  let rec fix_inline current = function
    | Page_link (target, anchor, None) ->
        let slug =
          if target = "" then
            current
          else
            target
        in
        Page_link (slug, anchor, Some [ Text (title slug) ])
    | Page_link (target, anchor, Some l) ->
        Page_link
          ( ( if target = "" then
                current
              else
                target
            ),
            anchor,
            Some (List.map (fix_inline current) l)
          )
    | Strong c ->
        Strong (List.map (fix_inline current) c)
    | Emph c ->
        Emph (List.map (fix_inline current) c)
    | Link (h, c) ->
        Link (h, List.map (fix_inline current) c)
    | (Text _ | Code _) as i ->
        i
  and fix_block current = function
    | Heading (l, a, c) ->
        Heading (l, a, List.map (fix_inline current) c)
    | Paragraph c ->
        Paragraph (List.map (fix_inline current) c)
    | Items (o, items) ->
        Items (o, List.map (List.map (fix_block current)) items)
    | Table (head, rows) ->
        Table
          ( List.map (List.map (fix_inline current)) head,
            List.map (List.map (List.map (fix_inline current))) rows
          )
    | Code_block _ as b ->
        b
  in
  List.map
    (fun p -> { p with blocks = List.map (fix_block p.slug) p.blocks })
    pages

let list f l = "[" ^ String.concat "; " (List.map f l) ^ "]"
let option f = function None -> "None" | Some x -> "(Some " ^ f x ^ ")"
let rec lit s =
  if String.for_all (fun c -> Char.code c < 128) s then
    Printf.sprintf "%S" s
  else
    match Str.bounded_split_delim (Str.regexp_string "|js}") s 2 with
    | [ before; after ] ->
        Printf.sprintf "(%s ^ \"|js}\" ^ %s)" (lit before) (lit after)
    | _ ->
        "{js|" ^ s ^ "|js}"

let str = lit

let rec ml_inline = function
  | Text t ->
      "Site_doc.Text " ^ str t
  | Code t ->
      "Site_doc.Code " ^ str t
  | Strong c ->
      "Site_doc.Strong " ^ list ml_inline c
  | Emph c ->
      "Site_doc.Emph " ^ list ml_inline c
  | Link (href, c) ->
      Printf.sprintf "Site_doc.Link { href = %s; content = %s }" (str href)
        (list ml_inline c)
  | Page_link (page, anchor, label) ->
      Printf.sprintf "Site_doc.Page_link { page = %s; anchor = %s; label = %s }"
        (str page) (option str anchor)
        (list ml_inline (Option.value label ~default:[]))

let rec ml_block = function
  | Heading (level, anchor, text) ->
      Printf.sprintf "Site_doc.Heading { level = %d; anchor = %s; text = %s }"
        level (str anchor) (list ml_inline text)
  | Paragraph c ->
      "Site_doc.Paragraph " ^ list ml_inline c
  | Items (ordered, items) ->
      Printf.sprintf "Site_doc.Items { ordered = %b; items = %s }" ordered
        (list (list ml_block) items)
  | Code_block (lang, file, source) ->
      Printf.sprintf "Site_doc.Code_block { lang = %s; file = %s; source = %s }"
        (str lang) (option str file) (str source)
  | Table (head, rows) ->
      Printf.sprintf "Site_doc.Table { head = %s; rows = %s }"
        (list (list ml_inline) head)
        (list (list (list ml_inline)) rows)

let () =
  match Array.to_list Sys.argv with
  | [ _; mode; docs ] -> (
      let files =
        Sys.readdir docs |> Array.to_list
        |> List.filter (fun f -> Filename.check_suffix f ".mld")
        |> List.sort compare
        |> List.map (Filename.concat docs)
      in
      let pages = List.map (page ~docs) files in
      check pages;
      let pages = pages_with_titles pages in
      print_string "[@@@ocamlformat \"disable\"]\n\n";
      match mode with
      | "pages" ->
          Printf.printf "let pages : Site_doc.page list = %s\n"
            (list
               (fun p ->
                 Printf.sprintf
                   "{ Site_doc.slug = %s; title = %s; blocks = %s }"
                   (str p.slug) (str p.title) (list ml_block p.blocks)
               )
               pages
            )
      | "index" ->
          Printf.printf
            "let pages : (string * string * (int * string * string) list) list \
             = %s\n"
            (list
               (fun p ->
                 Printf.sprintf "(%s, %s, %s)" (str p.slug) (str p.title)
                   (list
                      (fun (l, a, t) ->
                        Printf.sprintf "(%d, %s, %s)" l (str a) (str t)
                      )
                      (List.filter_map
                         (function
                           | Heading (l, a, t) when l <= 2 ->
                               Some (l, a, String.concat "" (List.map plain t))
                           | _ ->
                               None
                           )
                         p.blocks
                      )
                   )
               )
               pages
            )
      | m ->
          failwith ("unknown mode " ^ m)
    )
  | _ ->
      prerr_endline "usage: docs_gen (pages|index) DOCS_DIR";
      exit 2
