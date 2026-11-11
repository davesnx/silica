open Ppxlib
open Ast_builder.Default

let attribute =
  Attribute.declare "state_name" Attribute.Context.constructor_declaration
    Ast_pattern.(single_expr_payload (estring __))
    (fun name -> name)

let prefix name = function "t" -> name | type_name -> type_name ^ "_" ^ name

let constructor_name (constructor : constructor_declaration) =
  match Attribute.get attribute constructor with
  | Some name ->
      name
  | None ->
      String.lowercase_ascii constructor.pcd_name.txt

let check_unique names =
  let rec go seen = function
    | [] ->
        ()
    | ((name, (constructor : constructor_declaration)) as entry) :: rest -> (
        match List.assoc_opt name seen with
        | Some (first : constructor_declaration) ->
            Location.raise_errorf ~loc:constructor.pcd_loc
              "[silica] the states %s and %s have the same name %S"
              first.pcd_name.txt constructor.pcd_name.txt name
        | None ->
            go (entry :: seen) rest
      )
  in
  go [] names

let case ~loc ((name, constructor) : string * constructor_declaration) =
  let argument =
    match constructor.pcd_args with
    | Pcstr_tuple [] ->
        None
    | Pcstr_tuple _ | Pcstr_record _ ->
        Some (ppat_any ~loc)
  in
  case
    ~lhs:
      (ppat_construct ~loc
         { txt = Lident constructor.pcd_name.txt; loc }
         argument
      )
    ~guard:None ~rhs:(estring ~loc name)

let derive ~loc (declaration : type_declaration) =
  match declaration.ptype_kind with
  | Ptype_variant [] ->
      Location.raise_errorf ~loc:declaration.ptype_loc
        "[silica] [@@@@deriving state_shape] needs at least one state"
  | Ptype_variant constructors ->
      List.iter
        (fun (constructor : constructor_declaration) ->
          if Option.is_some constructor.pcd_res then
            Location.raise_errorf ~loc:constructor.pcd_loc
              "[silica] [@@@@deriving state_shape] needs a plain variant; %s \
               is a GADT constructor"
              constructor.pcd_name.txt
        )
        constructors;
      if declaration.ptype_params <> [] then
        Location.raise_errorf ~loc:declaration.ptype_loc
          "[silica] [@@@@deriving state_shape] needs a state type with no \
           parameter: a shape is one value for one type";
      let names =
        List.map
          (fun constructor -> (constructor_name constructor, constructor))
          constructors
      in
      check_unique names;
      let type_name = declaration.ptype_name.txt in
      let shape_value = prefix "shape" type_name in
      [
        pstr_value ~loc Nonrecursive
          [
            value_binding ~loc
              ~pat:(ppat_var ~loc { txt = shape_value; loc })
              ~expr:
                (pexp_constraint ~loc
                   [%expr
                     Silica.Machine.shape
                       ~name:
                         [%e
                           pexp_function_cases ~loc (List.map (case ~loc) names)]
                       ~names:
                         [%e
                           elist ~loc
                             (List.map
                                (fun (name, _) -> estring ~loc name)
                                names
                             )]
                       ()]
                   [%type:
                     [%t ptyp_constr ~loc { txt = Lident type_name; loc } []]
                     Silica.Machine.shape]
                );
          ];
      ]
  | Ptype_abstract | Ptype_record _ | Ptype_open ->
      Location.raise_errorf ~loc:declaration.ptype_loc
        "[silica] [@@@@deriving state_shape] needs a variant type"

let generate ~ctxt (_rec_flag, declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.concat_map (derive ~loc) declarations

let deriver =
  Deriving.add "state_shape"
    ~str_type_decl:(Deriving.Generator.V2.make_noarg generate)
