open Ppxlib
open Ast_builder.Default

type kind = {
  deriver : string;
  attribute : (constructor_declaration, string) Attribute.t;
  value : string;
  listed : bool;
  of_data : (constructor_declaration, expression) Attribute.t option;
}

let declare name =
  Attribute.declare name Attribute.Context.constructor_declaration
    Ast_pattern.(single_expr_payload (estring __))
    (fun name -> name)

let state =
  {
    deriver = "state_shape";
    attribute = declare "state_name";
    value = "state";
    listed = true;
    of_data = None;
  }
let event =
  {
    deriver = "event_shape";
    attribute = declare "event_name";
    value = "event";
    listed = false;
    of_data =
      Some
        (Attribute.declare "event_name_of"
           Attribute.Context.constructor_declaration
           Ast_pattern.(single_expr_payload __)
           (fun name -> name)
        );
  }

let prefix name = function "t" -> name | type_name -> type_name ^ "_" ^ name

let constructor_name kind (constructor : constructor_declaration) =
  match Attribute.get kind.attribute constructor with
  | Some name ->
      name
  | None ->
      String.lowercase_ascii constructor.pcd_name.txt

let check_unique kind names =
  let rec go seen = function
    | [] ->
        ()
    | ((name, (constructor : constructor_declaration)) as entry) :: rest -> (
        match List.assoc_opt name seen with
        | Some (first : constructor_declaration) ->
            Location.raise_errorf ~loc:constructor.pcd_loc
              "[silica] the %ss %s and %s have the same name %S" kind.value
              first.pcd_name.txt constructor.pcd_name.txt name
        | None ->
            go (entry :: seen) rest
      )
  in
  go [] names

let of_data kind (constructor : constructor_declaration) =
  match kind.of_data with
  | Some attribute ->
      Attribute.get attribute constructor
  | None ->
      None

let case kind ~loc ((name, constructor) : string * constructor_declaration) =
  let lhs argument =
    ppat_construct ~loc { txt = Lident constructor.pcd_name.txt; loc } argument
  in
  match (of_data kind constructor, constructor.pcd_args) with
  | None, Pcstr_tuple [] ->
      case ~lhs:(lhs None) ~guard:None ~rhs:(estring ~loc name)
  | None, (Pcstr_tuple _ | Pcstr_record _) ->
      case ~lhs:(lhs (Some (ppat_any ~loc))) ~guard:None ~rhs:(estring ~loc name)
  | Some _, (Pcstr_tuple [] | Pcstr_record _) ->
      Location.raise_errorf ~loc:constructor.pcd_loc
        "[silica] [@@event_name_of] needs a constructor with tuple data; %s \
         has %s"
        constructor.pcd_name.txt
        ( match constructor.pcd_args with
        | Pcstr_record _ ->
            "a record"
        | Pcstr_tuple _ ->
            "none"
        )
  | Some _, Pcstr_tuple _
    when Option.is_some (Attribute.get kind.attribute constructor) ->
      Location.raise_errorf ~loc:constructor.pcd_loc
        "[silica] %s has both [@@event_name] and [@@event_name_of]; keep one"
        constructor.pcd_name.txt
  | Some name_of, Pcstr_tuple arguments ->
      let variables =
        List.mapi (fun index _ -> "data" ^ string_of_int index) arguments
      in
      let pattern =
        match List.map (fun variable -> pvar ~loc variable) variables with
        | [ single ] ->
            single
        | patterns ->
            ppat_tuple ~loc patterns
      in
      case ~lhs:(lhs (Some pattern)) ~guard:None
        ~rhs:(eapply ~loc name_of (List.map (evar ~loc) variables))

let derive kind ~loc (declaration : type_declaration) =
  match declaration.ptype_kind with
  | Ptype_variant [] ->
      Location.raise_errorf ~loc:declaration.ptype_loc
        "[silica] [@@@@deriving %s] needs at least one %s" kind.deriver
        kind.value
  | Ptype_variant constructors ->
      List.iter
        (fun (constructor : constructor_declaration) ->
          if Option.is_some constructor.pcd_res then
            Location.raise_errorf ~loc:constructor.pcd_loc
              "[silica] [@@@@deriving %s] needs a plain variant; %s is a GADT \
               constructor"
              kind.deriver constructor.pcd_name.txt
        )
        constructors;
      if declaration.ptype_params <> [] then
        Location.raise_errorf ~loc:declaration.ptype_loc
          "[silica] [@@@@deriving %s] needs %s %s type with no parameter: a \
           shape is one value for one type"
          kind.deriver
          ( if kind.value = "event" then
              "an"
            else
              "a"
          )
          kind.value;
      let names =
        List.map
          (fun constructor -> (constructor_name kind constructor, constructor))
          constructors
      in
      check_unique kind names;
      let type_name = declaration.ptype_name.txt in
      let shape_value = prefix "shape" type_name in
      let name_function =
        pexp_function_cases ~loc (List.map (case kind ~loc) names)
      in
      [
        pstr_value ~loc Nonrecursive
          [
            value_binding ~loc
              ~pat:(ppat_var ~loc { txt = shape_value; loc })
              ~expr:
                (pexp_constraint ~loc
                   ( if kind.listed then
                       [%expr
                         Silica.Machine.shape ~name:[%e name_function]
                           ~names:
                             [%e
                               elist ~loc
                                 (List.map
                                    (fun (name, _) -> estring ~loc name)
                                    names
                                 )]
                           ()]
                     else
                       [%expr Silica.Machine.shape ~name:[%e name_function] ()]
                   )
                   [%type:
                     [%t ptyp_constr ~loc { txt = Lident type_name; loc } []]
                     Silica.Machine.shape]
                );
          ];
      ]
  | Ptype_abstract | Ptype_record _ | Ptype_open ->
      Location.raise_errorf ~loc:declaration.ptype_loc
        "[silica] [@@@@deriving %s] needs a variant type" kind.deriver

let generate kind ~ctxt (_rec_flag, declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.concat_map (derive kind ~loc) declarations

let add kind =
  Deriving.add kind.deriver
    ~str_type_decl:(Deriving.Generator.V2.make_noarg (generate kind))

let state_deriver = add state
let event_deriver = add event
