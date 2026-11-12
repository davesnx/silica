let sections =
  [
    ("Start", [ "index"; "install"; "tutorial" ]);
    ("For coding agents", [ "verification" ]);
    ( "Concepts",
      [
        "machines";
        "components";
        "data";
        "optimistic";
        "actions";
        "platform";
        "router";
        "server";
        "hydration";
        "checker";
      ]
    );
    ("Components", [ "ui"; "theming"; "stories" ]);
    ( "Guides",
      [ "testing"; "devtools"; "recipes"; "deploy"; "performance"; "linear" ]
    );
    ("Reference", [ "glossary" ]);
  ]

let order = List.concat_map snd sections

let find slug = List.find_opt (fun (s, _, _) -> s = slug) Site_docs_index.pages

let title slug =
  match find slug with Some (_, title, _) -> Some title | None -> None

let neighbours slug =
  let rec go before = function
    | s :: rest when s = slug ->
        (before, match rest with next :: _ -> Some next | [] -> None)
    | s :: rest ->
        go (Some s) rest
    | [] ->
        (None, None)
  in
  go None order
