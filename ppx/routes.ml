open Ppxlib
open Ast_builder.Default

let route_attribute =
  Attribute.declare "route" Attribute.Context.constructor_declaration
    Ast_pattern.(single_expr_payload __)
    (fun path -> path)

let variables ~loc count =
  List.init count (fun index -> "data" ^ string_of_int index)
  |> List.map (fun name -> (ppat_var ~loc { txt = name; loc }, evar ~loc name))

let tuple_pattern ~loc = function
  | [] ->
      [%pat? ()]
  | [ (pattern, _) ] ->
      pattern
  | many ->
      ppat_tuple ~loc (List.map fst many)

let tuple_expression ~loc = function
  | [] ->
      [%expr ()]
  | [ (_, expression) ] ->
      expression
  | many ->
      pexp_tuple ~loc (List.map snd many)

let route ~loc ~several (constructor : constructor_declaration) =
  let path =
    match Attribute.get route_attribute constructor with
    | Some path ->
        path
    | None ->
        Location.raise_errorf ~loc:constructor.pcd_loc
          "[silica] the route %s needs a path: [@route Route.(...)]"
          constructor.pcd_name.txt
  in
  let count =
    match constructor.pcd_args with
    | Pcstr_tuple arguments ->
        List.length arguments
    | Pcstr_record _ ->
        Location.raise_errorf ~loc:constructor.pcd_loc
          "[silica] the route %s has a record; use a record type in a tuple \
           argument"
          constructor.pcd_name.txt
  in
  let data = variables ~loc count in
  let name = { txt = Lident constructor.pcd_name.txt; loc } in
  let argument =
    match data with
    | [] ->
        None
    | [ (_, expression) ] ->
        Some expression
    | many ->
        Some (pexp_tuple ~loc (List.map snd many))
  in
  let argument_pattern =
    match data with
    | [] ->
        None
    | [ (pattern, _) ] ->
        Some pattern
    | many ->
        Some (ppat_tuple ~loc (List.map fst many))
  in
  let make =
    pexp_fun ~loc Nolabel None (tuple_pattern ~loc data)
      (pexp_construct ~loc name argument)
  in
  let matching =
    case
      ~lhs:(ppat_construct ~loc name argument_pattern)
      ~guard:None
      ~rhs:[%expr Some [%e tuple_expression ~loc data]]
  in
  let others =
    if several then
      [ case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:[%expr None] ]
    else
      []
  in
  let read =
    {
      (pexp_function_cases ~loc (matching :: others)) with
      pexp_attributes =
        [
          attribute ~loc
            ~name:{ txt = "ocaml.warning"; loc }
            ~payload:(PStr [ pstr_eval ~loc (estring ~loc "-4") [] ]);
        ];
    }
  in
  [%expr Silica.Router.route [%e path] [%e make] [%e read]]

let derive ~loc (declaration : type_declaration) =
  match declaration.ptype_kind with
  | Ptype_variant constructors ->
      let several = List.length constructors > 1 in
      let type_name = declaration.ptype_name.txt in
      let value_name =
        match type_name with "t" -> "routes" | name -> name ^ "_routes"
      in
      [
        pstr_value ~loc Nonrecursive
          [
            value_binding ~loc
              ~pat:(ppat_var ~loc { txt = value_name; loc })
              ~expr:
                (pexp_constraint ~loc
                   (elist ~loc (List.map (route ~loc ~several) constructors))
                   [%type:
                     [%t ptyp_constr ~loc { txt = Lident type_name; loc } []]
                     Silica.Router.route
                     list]
                );
          ];
      ]
  | Ptype_abstract | Ptype_record _ | Ptype_open ->
      Location.raise_errorf ~loc:declaration.ptype_loc
        "[silica] [@@@@deriving routes] needs a variant type"

let generate ~ctxt (_rec_flag, declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.concat_map (derive ~loc) declarations

let deriver =
  Deriving.add "routes"
    ~str_type_decl:(Deriving.Generator.V2.make_noarg generate)
