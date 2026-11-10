let marker = "From ["

let read path = In_channel.with_open_bin path In_channel.input_all

let contains ~sub text =
  let n = String.length sub and m = String.length text in
  let rec at i = i + n <= m && (String.sub text i n = sub || at (i + 1)) in
  at 0

let check_page ~root page =
  let lines = In_channel.with_open_bin page In_channel.input_lines in
  let rec scan count failures = function
    | line :: opening :: rest
      when String.starts_with ~prefix:marker line
           && String.ends_with ~suffix:"]:" line
           && String.starts_with ~prefix:"{@" opening ->
        let stop = String.index_from line (String.length marker) ']' in
        let file =
          String.sub line (String.length marker) (stop - String.length marker)
        in
        let rec block body = function
          | "]}" :: rest ->
              (String.concat "\n" (List.rev body), rest)
          | line :: rest ->
              block (line :: body) rest
          | [] ->
              failwith (page ^ ": no end of the block after " ^ file)
        in
        let excerpt, rest = block [] rest in
        if contains ~sub:excerpt (read (Filename.concat root file)) then
          scan (count + 1) failures rest
        else
          scan (count + 1)
            (Printf.sprintf "%s: the block after %S is not a part of %s" page
               line file
            :: failures
            )
            rest
    | _ :: rest ->
        scan count failures rest
    | [] ->
        (count, failures)
  in
  scan 0 [] lines

let () =
  let root = Sys.argv.(1) in
  let pages = List.tl (List.tl (Array.to_list Sys.argv)) in
  let failures =
    List.concat_map
      (fun page ->
        match check_page ~root page with
        | 0, _ ->
            [ page ^ ": no excerpt found" ]
        | _, failures ->
            failures
      )
      pages
  in
  List.iter prerr_endline failures;
  if failures <> [] then exit 1
