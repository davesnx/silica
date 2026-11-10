let escape text =
  let buffer = Buffer.create (String.length text) in
  String.iter
    (fun c ->
      match c with
      | '{' | '}' | '[' | ']' | '@' | '\\' ->
          Buffer.add_char buffer '\\';
          Buffer.add_char buffer c
      | c ->
          Buffer.add_char buffer c
    )
    text;
  Buffer.contents buffer

let inline text =
  let buffer = Buffer.create (String.length text) in
  let length = String.length text in
  let rec closing marker from =
    match String.index_from_opt text from marker.[0] with
    | None ->
        None
    | Some i
      when i + String.length marker <= length
           && String.sub text i (String.length marker) = marker ->
        Some i
    | Some i ->
        closing marker (i + 1)
  in
  let rec go i =
    if i < length then
      let span marker before after =
        let size = String.length marker in
        match closing marker (i + size) with
        | Some j ->
            let content = String.sub text (i + size) (j - i - size) in
            Buffer.add_string buffer before;
            Buffer.add_string buffer (escape content);
            Buffer.add_string buffer after;
            go (j + size)
        | None ->
            Buffer.add_string buffer (escape marker);
            go (i + size)
      in
      if i + 1 < length && text.[i] = '*' && text.[i + 1] = '*' then
        span "**" "{b " "}"
      else if text.[i] = '`' then
        span "`" "[" "]"
      else if text.[i] = '_' && (i = 0 || text.[i - 1] = ' ') then
        span "_" "{i " "}"
      else (
        Buffer.add_string buffer (escape (String.make 1 text.[i]));
        go (i + 1)
      )
  in
  go 0;
  Buffer.contents buffer

let starts_with prefix line = String.starts_with ~prefix line

let () =
  let source = Sys.argv.(1) in
  let lines = In_channel.with_open_bin source In_channel.input_lines in
  print_string
    "{0 Glossary}\n\n\
     The terms of Silica. A dune rule makes this page from [CONTEXT.md] at the \
     root of the repository. Change that file, not this page.\n";
  let paragraph = ref [] in
  let flush () =
    if !paragraph <> [] then (
      print_string "\n";
      print_string (String.concat "\n" (List.rev !paragraph));
      print_string "\n";
      paragraph := []
    )
  in
  List.iter
    (fun line ->
      if starts_with "## " line then (
        flush ();
        Printf.printf "\n{1 %s}\n"
          (escape (String.sub line 3 (String.length line - 3)))
      ) else if starts_with "# " line then
        flush ()
      else if String.trim line = "" then
        flush ()
      else if starts_with "_Avoid_" line then (
        flush ();
        paragraph := [ inline line ]
      ) else
        paragraph := inline line :: !paragraph
    )
    lines;
  flush ()
