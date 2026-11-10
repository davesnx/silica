let read path = In_channel.with_open_bin path In_channel.input_lines

let quoted line from =
  let start = String.index_from line from '"' + 1 in
  let stop = String.index_from line start '"' in
  (String.sub line start (stop - start), stop + 1)

let short machine name =
  let prefix = machine ^ "." in
  if String.starts_with ~prefix name then
    String.sub name (String.length prefix)
      (String.length name - String.length prefix)
  else
    failwith ("state outside the machine: " ^ name)

let label line =
  match String.index_opt line '[' with
  | None ->
      None
  | Some i -> (
      let attrs = String.sub line i (String.length line - i) in
      match Str.search_forward (Str.regexp_string "label=\"") attrs 0 with
      | exception Not_found ->
          None
      | j ->
          Some (fst (quoted attrs (j + 5)))
    )

let first_words text =
  List.sort_uniq compare
    (List.map
       (fun e -> List.hd (String.split_on_char ' ' (String.trim e)))
       (String.split_on_char ',' text)
    )

let facts path =
  let lines = read path in
  let machine = fst (quoted (List.hd lines) 0) in
  let init = ref "" and states = ref [] and edges = ref [] in
  List.iter
    (fun line ->
      if String.starts_with ~prefix:"\"" line then
        let a, next = quoted line 0 in
        if String.length line > next + 4 && String.sub line next 4 = " -> " then (
          let b, _ = quoted line (next + 4) in
          if a = machine ^ ".__init" then
            init := short machine b
          else
            let a = short machine a and b = short machine b in
            match label line with
            | Some l ->
                edges := (a, b, first_words l) :: !edges
            | None ->
                if a <> b then edges := (a, b, []) :: !edges
        ) else if a <> machine ^ ".__init" then
          let lines =
            String.split_on_char '\n' (Scanf.unescaped (Option.get (label line)))
          in
          let final =
            try
              ignore
                (Str.search_forward (Str.regexp_string "doublecircle") line 0);
              true
            with Not_found -> false
          in
          states := (short machine a, List.tl lines, final) :: !states
    )
    lines;
  (machine, !init, List.rev !states, List.rev !edges)

let () =
  print_string "[@@@ocamlformat \"disable\"]\n\n";
  print_string
    "type state = { name : string; details : string list; final : bool }\n\
     type edge = { from : string; target : string; events : string list }\n\
     type t = { machine : string; init : string; states : state list; edges : \
     edge list }\n\n";
  let list f l = "[" ^ String.concat "; " (List.map f l) ^ "]" in
  Array.iteri
    (fun i path ->
      if i > 0 then
        let machine, init, states, edges = facts path in
        Printf.printf
          "let %s = { machine = %S; init = %S; states = %s; edges = %s }\n"
          (String.map
             (function '-' -> '_' | c -> c)
             (Filename.remove_extension (Filename.basename path))
          )
          machine init
          (list
             (fun (n, d, f) ->
               Printf.sprintf "{ name = %S; details = %s; final = %b }" n
                 (list (Printf.sprintf "%S") d)
                 f
             )
             states
          )
          (list
             (fun (a, b, e) ->
               Printf.sprintf "{ from = %S; target = %S; events = %s }" a b
                 (list (Printf.sprintf "%S") e)
             )
             edges
          )
    )
    Sys.argv
