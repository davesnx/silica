type 'a t = {
  take : string list -> (string * string) list -> ('a * string list) option;
  give : 'a -> string list * (string * string) list;
  name : string;
}

type 'a query = {
  read : (string * string) list -> 'a;
  write : 'a -> (string * string) list;
  keys : string list;
}

let s text =
  {
    take =
      (fun segments _ ->
        match segments with
        | first :: rest when first = text ->
            Some ((), rest)
        | _ ->
            None
      );
    give = (fun () -> ([ text ], []));
    name = "/" ^ text;
  }

let root =
  {
    take =
      (fun segments _ -> match segments with [] -> Some ((), []) | _ -> None);
    give = (fun () -> ([], []));
    name = "/";
  }

let one ~label ~read ~write =
  {
    take =
      (fun segments _ ->
        match segments with
        | first :: rest ->
            Option.map (fun value -> (value, rest)) (read first)
        | [] ->
            None
      );
    give = (fun value -> ([ write value ], []));
    name = "/:" ^ label;
  }

let int =
  one ~label:"int"
    ~read:(fun text ->
      match int_of_string_opt text with
      | Some n when string_of_int n = text ->
          Some n
      | _ ->
          None
    )
    ~write:string_of_int

let string = one ~label:"string" ~read:Option.some ~write:Fun.id

let rest =
  {
    take =
      (fun segments _ ->
        match segments with
        | [] ->
            Some ("/", [])
        | _ ->
            Some (String.concat "/" ("" :: segments), [])
      );
    give =
      (fun text ->
        ( List.filter (fun part -> part <> "") (String.split_on_char '/' text),
          []
        )
      );
    name = "/*";
  }

let sequence first second =
  {
    take =
      (fun segments query ->
        match first.take segments query with
        | None ->
            None
        | Some (a, remaining) -> (
            match second.take remaining query with
            | None ->
                None
            | Some (b, remaining) ->
                Some ((a, b), remaining)
          )
      );
    give =
      (fun (a, b) ->
        let path_a, query_a = first.give a in
        let path_b, query_b = second.give b in
        (path_a @ path_b, query_a @ query_b)
      );
    name = first.name ^ second.name;
  }

let ( / ) first second =
  let pair = sequence first second in
  {
    take =
      (fun segments query ->
        Option.map
          (fun ((_, b), remaining) -> (b, remaining))
          (pair.take segments query)
      );
    give = (fun b -> pair.give ((), b));
    name = pair.name;
  }

let ( // ) first second =
  let pair = sequence first second in
  {
    take =
      (fun segments query ->
        Option.map
          (fun ((a, _), remaining) -> (a, remaining))
          (pair.take segments query)
      );
    give = (fun a -> pair.give (a, ()));
    name = pair.name;
  }

let ( /+ ) = sequence

let query ~name ~default ~parse ~print =
  {
    read =
      (fun pairs ->
        match List.assoc_opt name pairs with
        | None ->
            default
        | Some text ->
            Option.value (parse text) ~default
      );
    write =
      (fun value ->
        if value = default then
          []
        else
          [ (name, print value) ]
      );
    keys = [ name ];
  }

let string_query name ~default =
  query ~name ~default ~parse:Option.some ~print:Fun.id

let int_query name ~default =
  query ~name ~default ~parse:int_of_string_opt ~print:string_of_int

let bool_query name ~default =
  query ~name ~default
    ~parse:(function "true" -> Some true | "false" -> Some false | _ -> None)
    ~print:string_of_bool

let choice_query name ~default choices =
  query ~name ~default
    ~parse:(fun text -> List.assoc_opt text choices)
    ~print:(fun value ->
      match List.find_opt (fun (_, v) -> v = value) choices with
      | Some (text, _) ->
          text
      | None ->
          invalid_arg ("Route.choice_query: unlisted value for " ^ name)
    )

let map_query read write query =
  {
    read = (fun pairs -> read (query.read pairs));
    write = (fun value -> query.write (write value));
    keys = query.keys;
  }

let ( &? ) first second =
  {
    read = (fun pairs -> (first.read pairs, second.read pairs));
    write = (fun (a, b) -> first.write a @ second.write b);
    keys = first.keys @ second.keys;
  }

let with_query path query =
  {
    take =
      (fun segments pairs ->
        Option.map
          (fun (a, remaining) -> ((a, query.read pairs), remaining))
          (path.take segments pairs)
      );
    give =
      (fun (a, q) ->
        let segments, pairs = path.give a in
        (segments, pairs @ query.write q)
      );
    name = path.name ^ "?" ^ String.concat "&" query.keys;
  }

let parse route (url : Url.t) =
  match route.take url.path url.query with
  | Some (value, []) ->
      Some value
  | _ ->
      None

let print route value : Url.t =
  let path, query = route.give value in
  { path; query; hash = None }

let describe route = route.name

let ( +? ) path query =
  let with_unit = with_query path query in
  {
    take =
      (fun segments pairs ->
        Option.map
          (fun ((_, q), remaining) -> (q, remaining))
          (with_unit.take segments pairs)
      );
    give = (fun q -> with_unit.give ((), q));
    name = with_unit.name;
  }
