type t = {
  path : string list;
  query : (string * string) list;
  hash : string option;
}

let is_unreserved = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' ->
      true
  | _ ->
      false

let encode s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if is_unreserved c then
        Buffer.add_char b c
      else
        let hex = "0123456789ABCDEF" and code = Char.code c in
        Buffer.add_char b '%';
        Buffer.add_char b hex.[code lsr 4];
        Buffer.add_char b hex.[code land 15]
    )
    s;
  Buffer.contents b

let hex = function
  | '0' .. '9' as c ->
      Some (Char.code c - 48)
  | 'A' .. 'F' as c ->
      Some (Char.code c - 55)
  | 'a' .. 'f' as c ->
      Some (Char.code c - 87)
  | _ ->
      None

let decode ~plus s =
  let n = String.length s in
  let b = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    ( match s.[!i] with
    | '%' when !i + 2 < n -> (
        match (hex s.[!i + 1], hex s.[!i + 2]) with
        | Some h, Some l ->
            Buffer.add_char b (Char.chr ((h * 16) + l));
            i := !i + 2
        | _ ->
            Buffer.add_char b '%'
      )
    | '+' when plus ->
        Buffer.add_char b ' '
    | c ->
        Buffer.add_char b c
    );
    incr i
  done;
  Buffer.contents b

let split_at c s =
  match String.index_opt s c with
  | None ->
      (s, None)
  | Some i ->
      (String.sub s 0 i, Some (String.sub s (i + 1) (String.length s - i - 1)))

let of_string s =
  let rest, hash = split_at '#' s in
  let path, query = split_at '?' rest in
  let query =
    match query with
    | None ->
        []
    | Some q ->
        String.split_on_char '&' q
        |> List.filter (fun p -> p <> "")
        |> List.map (fun p ->
            let k, v = split_at '=' p in
            (decode ~plus:true k, decode ~plus:true (Option.value v ~default:""))
        )
  in
  {
    path =
      String.split_on_char '/' path
      |> List.filter (fun p -> p <> "")
      |> List.map (decode ~plus:false);
    query;
    hash = Option.map (decode ~plus:false) hash;
  }

let to_string { path; query; hash } =
  let path =
    match path with
    | [] ->
        "/"
    | _ ->
        String.concat "" (List.map (fun s -> "/" ^ encode s) path)
  in
  let query =
    match query with
    | [] ->
        ""
    | _ ->
        "?"
        ^ String.concat "&"
            (List.map (fun (k, v) -> encode k ^ "=" ^ encode v) query)
  in
  let hash = match hash with None -> "" | Some h -> "#" ^ encode h in
  path ^ query ^ hash
