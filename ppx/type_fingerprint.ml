open Ppxlib
open Ast_builder.Default

type problem = Unknown of string | Refused of string

exception Problem of problem

type declared = Type of type_declaration | Class

let declared_names : (string, declared list) Hashtbl.t = Hashtbl.create 64
let openings : (int * int) list ref = ref []
let use_position = ref max_int

let scan structure =
  Hashtbl.reset declared_names;
  openings := [];
  let add name declared =
    let previous =
      Option.value ~default:[] (Hashtbl.find_opt declared_names name)
    in
    Hashtbl.replace declared_names name (declared :: previous)
  in
  let scope_ends = ref [ max_int ] in
  let collector =
    object
      inherit Ast_traverse.iter as super

      method! module_expr expression =
        match expression.pmod_desc with
        | Pmod_structure _ ->
            scope_ends := expression.pmod_loc.loc_end.pos_cnum :: !scope_ends;
            super#module_expr expression;
            scope_ends := List.tl !scope_ends
        | _ ->
            super#module_expr expression

      method! type_declaration declaration =
        add declaration.ptype_name.txt (Type declaration);
        super#type_declaration declaration

      method! structure_item item =
        ( match item.pstr_desc with
        | Pstr_open _ | Pstr_include _ ->
            openings :=
              (item.pstr_loc.loc_start.pos_cnum, List.hd !scope_ends)
              :: !openings
        | _ ->
            ()
        );
        super#structure_item item

      method! class_infos : 'a. ('a -> unit) -> 'a class_infos -> unit =
        fun visit info ->
          add info.pci_name.txt Class;
          super#class_infos visit info
    end
  in
  collector#structure structure;
  structure

let enabled () = Sys.getenv_opt "SILICA_FINGERPRINTS" <> Some "false"

let prefix name = function "t" -> name | type_name -> type_name ^ "_" ^ name

let versions =
  String.concat "\n"
    [
      "silica-fingerprint-1";
      "ocaml " ^ Fingerprint_versions.ocaml;
      "melange " ^ Fingerprint_versions.melange;
      "ppxlib " ^ Fingerprint_versions.ppxlib;
    ]

let hash text = Digest.to_hex (Digest.string (versions ^ "\n" ^ text))

let stripped = [ "ocaml.doc"; "ocaml.text"; "doc"; "deriving" ]

let allowed_predefined =
  [
    ("int", ("Stdlib.Int.t", 0));
    ("char", ("Stdlib.Char.t", 0));
    ("string", ("Stdlib.String.t", 0));
    ("bytes", ("Stdlib.Bytes.t", 0));
    ("float", ("Stdlib.Float.t", 0));
    ("bool", ("Stdlib.Bool.t", 0));
    ("unit", ("Stdlib.Unit.t", 0));
    ("int32", ("Stdlib.Int32.t", 0));
    ("int64", ("Stdlib.Int64.t", 0));
    ("nativeint", ("Stdlib.Nativeint.t", 0));
    ("list", ("Stdlib.List.t", 1));
    ("array", ("Stdlib.Array.t", 1));
    ("option", ("Stdlib.Option.t", 1));
    ("result", ("Stdlib.Result.t", 2));
    ("ref", ("Stdlib.ref", 1));
  ]

let refused_predefined = function
  | "exn" ->
      Some "its state type holds an exception"
  | "lazy_t" ->
      Some "its state type holds a lazy value"
  | ( "format"
    | "format4"
    | "format6"
    | "in_channel"
    | "out_channel"
    | "open_flag"
    | "fpclass"
    | "extension_constructor"
    | "floatarray" ) as name ->
      Some ("its state type mentions " ^ name ^ ", which can not be handed over")
  | _ ->
      None

let refuse reason = raise (Problem (Refused reason))

let normalizer ~loc ~variables =
  object
    inherit Ast_traverse.map as super
    method! location _ = loc
    method! location_stack _ = []

    method! attributes attributes =
      super#attributes
        (List.filter
           (fun attribute -> not (List.mem attribute.attr_name.txt stripped))
           attributes
        )

    method! core_type_desc desc =
      match desc with
      | Ptyp_var name -> (
          match List.assoc_opt name variables with
          | Some position ->
              Ptyp_var position
          | None ->
              desc
        )
      | _ ->
          super#core_type_desc desc
  end

let position index = "p" ^ string_of_int index

let normalize ~loc declaration =
  let variables =
    List.concat
      (List.mapi
         (fun index ((parameter : core_type), _) ->
           match parameter.ptyp_desc with
           | Ptyp_var name ->
               [ (name, position index) ]
           | _ ->
               []
         )
         declaration.ptype_params
      )
  in
  let declaration = (normalizer ~loc ~variables)#type_declaration declaration in
  {
    declaration with
    ptype_params =
      List.mapi
        (fun index ((parameter : core_type), variance) ->
          ({ parameter with ptyp_desc = Ptyp_var (position index) }, variance)
        )
        declaration.ptype_params;
  }

let print rec_flag declarations =
  let loc = Location.none in
  Format.asprintf "%a" Pprintast.structure_item
    (pstr_type ~loc rec_flag
       (List.map (normalize ~loc:Location.none) declarations)
    )

type walk = {
  group : string list;
  recursive : bool;
  mutable followed : (string * type_declaration) list;
  mutable predefined : string list;
  mutable parts : expression list;
  mutable part_names : string list;
}

type resolution = Group | Followed of type_declaration | Predefined

let declared_kind name (declaration : type_declaration) =
  match (declaration.ptype_kind, declaration.ptype_manifest) with
  | Ptype_open, _ ->
      refuse ("its state type mentions " ^ name ^ ", an extensible type")
  | Ptype_abstract, None ->
      refuse ("its state type mentions " ^ name ^ ", an abstract type")
  | _ ->
      ()

let resolve walk ~in_group name =
  if List.mem name walk.group then
    if walk.recursive || not in_group then
      Group
    else
      raise (Problem (Unknown (name ^ " (type nonrec)")))
  else
    match Hashtbl.find_opt declared_names name with
    | Some [ Type declaration ]
      when List.exists
             (fun (position, scope_end) ->
               position > declaration.ptype_loc.loc_start.pos_cnum
               && position < !use_position && !use_position < scope_end
             )
             !openings ->
        raise
          (Problem
             (Unknown
                (name
               ^ " (an open or include after its declaration may hide it)"
                )
             )
          )
    | Some [ Type declaration ] ->
        declared_kind name declaration;
        Followed declaration
    | Some [ Class ] ->
        refuse ("its state type holds an object of the class " ^ name)
    | Some (_ :: _ :: _) ->
        raise
          (Problem (Unknown (name ^ " (declared more than once in this file)")))
    | Some [] | None -> (
        if List.mem_assoc name allowed_predefined then
          Predefined
        else
          match refused_predefined name with
          | Some reason ->
              refuse reason
          | None ->
              raise (Problem (Unknown name))
      )

let rec has_application = function
  | Lident _ ->
      false
  | Ldot (path, _) ->
      has_application path
  | Lapply _ ->
      true

let constant ~loc text =
  [%expr Silica.Fingerprint.declared [%e estring ~loc (hash text)] []]

let anonymous_variables ~parameters =
  object
    inherit Ast_traverse.map as super

    method! core_type_desc desc =
      match desc with
      | Ptyp_var name when not (List.mem_assoc name parameters) ->
          Ptyp_any
      | _ ->
          super#core_type_desc desc
  end

let rec typed_lookup ~loc ~walk ~parameters path type_name arguments =
  let value = prefix "fingerprint" type_name in
  let written = Longident.name (Ldot (path, type_name)) in
  let unknown = [%expr Silica.Fingerprint.unknown [%e estring ~loc written]] in
  let default =
    List.fold_left (fun body _ -> [%expr fun _ -> [%e body]]) unknown arguments
  in
  let lookup =
    [%expr
      let module Silica_default = struct
        let [%p pvar ~loc value] = [%e default]
      end in
      let open! Silica_default in
      [%e
        pexp_open ~loc
          (open_infos ~loc ~override:Override
             ~expr:(pmod_ident ~loc { txt = path; loc })
          )
          (evar ~loc value)]]
  in
  let applied =
    match arguments with
    | [] ->
        lookup
    | _ ->
        eapply ~loc lookup
          (List.map (argument_fingerprint ~loc ~walk ~parameters) arguments)
  in
  let typ =
    (anonymous_variables ~parameters)#core_type
      (ptyp_constr ~loc { txt = Ldot (path, type_name); loc } arguments)
  in
  pexp_constraint ~loc applied [%type: [%t typ] Silica.Fingerprint.t]

and argument_fingerprint ~loc ~walk ~parameters (argument : core_type) =
  match argument.ptyp_desc with
  | Ptyp_var name -> (
      match List.assoc_opt name parameters with
      | Some parameter ->
          evar ~loc parameter
      | None ->
          constant ~loc "variable"
    )
  | Ptyp_constr ({ txt = Lident name; _ }, arguments) -> (
      match resolve walk ~in_group:false name with
      | Predefined ->
          predefined ~loc ~walk ~parameters name arguments
      | Group | Followed _ ->
          constant ~loc ("same-file " ^ name)
    )
  | Ptyp_constr ({ txt = Ldot (Lident "Stdlib", name); _ }, arguments)
    when List.mem_assoc name allowed_predefined ->
      predefined ~loc ~walk ~parameters name arguments
  | Ptyp_constr ({ txt = Ldot (path, type_name); _ }, arguments) ->
      typed_lookup ~loc ~walk ~parameters path type_name arguments
  | _ ->
      constant ~loc "structure"

and predefined ~loc ~walk ~parameters name arguments =
  [%expr
    Silica.Fingerprint.declared
      [%e estring ~loc (hash ("predefined " ^ name))]
      [%e
        elist ~loc
          (List.map
             (fun argument ->
               [%expr
                 Silica.Fingerprint.part
                   [%e argument_fingerprint ~loc ~walk ~parameters argument]]
             )
             arguments
          )]]

let name ~loc ~walk ~parameters ~in_group lid arguments =
  match lid with
  | Lident name -> (
      match resolve walk ~in_group name with
      | Group ->
          ()
      | Followed declaration ->
          if not (List.mem_assoc name walk.followed) then
            walk.followed <- walk.followed @ [ (name, declaration) ]
      | Predefined ->
          if not (List.mem name walk.predefined) then
            walk.predefined <- walk.predefined @ [ name ]
    )
  | Ldot (_, _) when has_application lid ->
      raise (Problem (Unknown (Longident.name lid)))
  | Ldot ((Lident "Lazy" | Ldot (Lident "Stdlib", "Lazy")), "t") ->
      refuse "its state type holds a lazy value"
  | Ldot (Lident "Stdlib", name) when List.mem_assoc name allowed_predefined ->
      ()
  | Ldot (Lident "Stdlib", name) when Option.is_some (refused_predefined name)
    ->
      refuse (Option.get (refused_predefined name))
  | Ldot (path, type_name) ->
      walk.parts <-
        [%expr
          Silica.Fingerprint.part
            [%e typed_lookup ~loc ~walk ~parameters path type_name arguments]]
        :: walk.parts;
      walk.part_names <- Longident.name lid :: walk.part_names
  | Lapply _ ->
      raise (Problem (Unknown (Longident.name lid)))

let rec core_type ~loc ~walk ~parameters ~in_group (typ : core_type) =
  let recurse = core_type ~loc ~walk ~parameters ~in_group in
  match typ.ptyp_desc with
  | Ptyp_any ->
      refuse "its state type holds _"
  | Ptyp_var _ ->
      ()
  | Ptyp_arrow _ ->
      refuse "its state type holds a function"
  | Ptyp_tuple types ->
      List.iter recurse types
  | Ptyp_constr ({ txt; _ }, arguments) ->
      name ~loc ~walk ~parameters ~in_group txt arguments;
      List.iter recurse arguments
  | Ptyp_object _ ->
      refuse "its state type holds an object"
  | Ptyp_class _ ->
      refuse "its state type holds a #class type"
  | Ptyp_alias (inner, _) ->
      recurse inner
  | Ptyp_variant (rows, Closed, None) ->
      List.iter
        (fun row ->
          match row.prf_desc with
          | Rtag (_, _, types) ->
              List.iter recurse types
          | Rinherit inner ->
              recurse inner
        )
        rows
  | Ptyp_variant _ ->
      refuse "its state type holds an open polymorphic variant"
  | Ptyp_poly ([], inner) ->
      recurse inner
  | Ptyp_poly _ ->
      refuse "its state type holds a polymorphic type"
  | Ptyp_package _ ->
      refuse "its state type holds a first-class module"
  | Ptyp_open _ ->
      refuse "its state type holds a local open"
  | Ptyp_extension _ ->
      refuse "its state type holds an extension node"

let declaration ~loc ~walk ~parameters ~in_group (declaration : type_declaration)
    =
  let recurse = core_type ~loc ~walk ~parameters ~in_group in
  let labels =
    List.iter (fun (label : label_declaration) -> recurse label.pld_type)
  in
  declared_kind declaration.ptype_name.txt declaration;
  ( match declaration.ptype_kind with
  | Ptype_variant constructors ->
      List.iter
        (fun (constructor : constructor_declaration) ->
          if Option.is_some constructor.pcd_res || constructor.pcd_vars <> []
          then
            refuse
              ("its state type holds the GADT constructor "
             ^ constructor.pcd_name.txt
              );
          match constructor.pcd_args with
          | Pcstr_tuple types ->
              List.iter recurse types
          | Pcstr_record fields ->
              labels fields
        )
        constructors
  | Ptype_record fields ->
      labels fields
  | Ptype_abstract | Ptype_open ->
      ()
  );
  Option.iter recurse declaration.ptype_manifest;
  List.iter
    (fun (left, right, _) ->
      recurse left;
      recurse right
    )
    declaration.ptype_cstrs

let check_name name = "silica_check_" ^ name

let type_arguments ~loc (declaration : type_declaration) =
  List.map (fun (parameter, _) -> parameter) declaration.ptype_params
  |> List.map (fun (parameter : core_type) -> { parameter with ptyp_loc = loc })

let follow_check ~loc (name, original) =
  let declaration = normalize ~loc original in
  let arguments = type_arguments ~loc declaration in
  let scope_type = ptyp_constr ~loc { txt = Lident name; loc } arguments in
  match (declaration.ptype_kind, declaration.ptype_manifest) with
  | (Ptype_variant _ | Ptype_record _), _ ->
      [
        pstr_type ~loc Recursive
          [
            {
              declaration with
              ptype_name = { txt = check_name name; loc };
              ptype_manifest = Some scope_type;
            };
          ];
      ]
  | _, Some manifest -> (
      let into =
        [%stri let _ = fun (value : [%t scope_type]) : [%t manifest] -> value]
      in
      match declaration.ptype_private with
      | Private ->
          [
            [%stri
              let _ = fun (value : [%t scope_type]) -> (value :> [%t manifest])];
          ]
      | Public ->
          [
            into;
            [%stri
              let _ = fun (value : [%t manifest]) : [%t scope_type] -> value];
          ]
    )
  | _, None ->
      []

let predefined_check ~loc name =
  let stdlib, arity = List.assoc name allowed_predefined in
  let arguments =
    List.init arity (fun index -> ptyp_var ~loc (position index))
  in
  let written = ptyp_constr ~loc { txt = Lident name; loc } arguments in
  let canonical =
    ptyp_constr ~loc { txt = Longident.parse stdlib; loc } arguments
  in
  [%stri let _ = fun (value : [%t written]) : [%t canonical] -> value]

let silenced ~loc expression =
  {
    expression with
    pexp_attributes =
      [
        attribute ~loc
          ~name:{ txt = "ocaml.warning"; loc }
          ~payload:
            (PStr [ pstr_eval ~loc (estring ~loc "-32-33-34-37-38-60-69") [] ]);
      ];
  }

let parameter_names (declaration : type_declaration) =
  List.mapi
    (fun index ((parameter : core_type), _) ->
      let name = "parameter_" ^ string_of_int index in
      match parameter.ptyp_desc with
      | Ptyp_var variable ->
          (Some variable, name)
      | _ ->
          (None, name)
    )
    declaration.ptype_params

let body ~loc ~rec_flag ~group (target : type_declaration) =
  let named = parameter_names target in
  let parameters =
    List.filter_map
      (fun (variable, name) ->
        Option.map (fun variable -> (variable, name)) variable
      )
      named
  in
  let walk =
    {
      group =
        List.map (fun (item : type_declaration) -> item.ptype_name.txt) group;
      recursive = rec_flag = Recursive;
      followed = [];
      predefined = [];
      parts = [];
      part_names = [];
    }
  in
  try
    List.iter
      (fun (item : type_declaration) ->
        let parameters =
          if item == target then
            parameters
          else
            []
        in
        declaration ~loc ~walk ~parameters ~in_group:true item
      )
      group;
    let rec follow index =
      match List.nth_opt walk.followed index with
      | None ->
          ()
      | Some (_, item) ->
          declaration ~loc ~walk ~parameters:[] ~in_group:false item;
          follow (index + 1)
    in
    follow 0;
    let followed =
      List.sort
        (fun (left, _) (right, _) -> String.compare left right)
        walk.followed
    in
    let text =
      String.concat "\n"
        ([ "fingerprint of " ^ target.ptype_name.txt; print rec_flag group ]
        @ List.map (fun (_, item) -> print Recursive [ item ]) followed
        @ List.rev_map (fun name -> "part " ^ name) walk.part_names
        )
    in
    let parts =
      List.map
        (fun (_, name) -> [%expr Silica.Fingerprint.part [%e evar ~loc name]])
        named
      @ List.rev walk.parts
    in
    let value =
      [%expr
        Silica.Fingerprint.declared
          [%e estring ~loc (hash text)]
          [%e elist ~loc parts]]
    in
    let checks =
      List.concat_map (follow_check ~loc) followed
      @ List.map (predefined_check ~loc)
          (List.sort String.compare walk.predefined)
    in
    ( true,
      silenced ~loc
        ( match checks with
        | [] ->
            value
        | _ ->
            pexp_letmodule ~loc
              { txt = Some "Silica_check"; loc }
              (pmod_structure ~loc checks)
              value
        )
    )
  with
  | Problem (Unknown written) ->
      (false, [%expr Silica.Fingerprint.unknown [%e estring ~loc written]])
  | Problem (Refused reason) ->
      (false, [%expr Silica.Fingerprint.refused [%e estring ~loc reason]])

let value_type ~loc (declaration : type_declaration) =
  let variables =
    List.mapi
      (fun index ((parameter : core_type), _) ->
        match parameter.ptyp_desc with
        | Ptyp_var _ ->
            { parameter with ptyp_loc = loc; ptyp_attributes = [] }
        | _ ->
            ptyp_var ~loc ("silica_parameter_" ^ string_of_int index)
      )
      declaration.ptype_params
  in
  let fingerprint typ = [%type: [%t typ] Silica.Fingerprint.t] in
  List.fold_right
    (fun variable result ->
      ptyp_arrow ~loc Nolabel (fingerprint variable) result
    )
    variables
    (fingerprint
       (ptyp_constr ~loc
          { txt = Lident declaration.ptype_name.txt; loc }
          variables
       )
    )

let value_name (declaration : type_declaration) =
  prefix "fingerprint" declaration.ptype_name.txt

let binding ~loc ~rec_flag ~group (target : type_declaration) =
  let uses_parameters, body = body ~loc ~rec_flag ~group target in
  let expression =
    List.fold_right
      (fun (_, name) body ->
        let pattern =
          if uses_parameters then
            pvar ~loc name
          else
            ppat_any ~loc
        in
        [%expr fun [%p pattern] -> [%e body]]
      )
      (parameter_names target) body
  in
  pstr_value ~loc Nonrecursive
    [
      value_binding ~loc
        ~pat:(pvar ~loc (value_name target))
        ~expr:(pexp_constraint ~loc expression (value_type ~loc target));
    ]

let release_binding ~loc (target : type_declaration) =
  let expression =
    List.fold_right
      (fun _ body -> [%expr fun _ -> [%e body]])
      (parameter_names target) [%expr Silica.Fingerprint.off]
  in
  pstr_value ~loc Nonrecursive
    [
      value_binding ~loc
        ~pat:(pvar ~loc (value_name target))
        ~expr:(pexp_constraint ~loc expression (value_type ~loc target));
    ]

let generate ~ctxt (rec_flag, declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  use_position := loc.loc_start.pos_cnum;
  if enabled () then
    List.map (binding ~loc ~rec_flag ~group:declarations) declarations
  else
    List.map (release_binding ~loc) declarations

let generate_signature ~ctxt (_rec_flag, declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map
    (fun declaration ->
      psig_value ~loc
        (value_description ~loc
           ~name:{ txt = value_name declaration; loc }
           ~type_:(value_type ~loc declaration)
           ~prim:[]
        )
    )
    declarations

let deriver =
  Deriving.add "fingerprint"
    ~str_type_decl:(Deriving.Generator.V2.make_noarg generate)
    ~sig_type_decl:(Deriving.Generator.V2.make_noarg generate_signature)

let extension =
  Extension.V3.declare "fingerprint" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~ctxt expression ->
      if enabled () then
        expression
      else
        let loc = Expansion_context.Extension.extension_point_loc ctxt in
        [%expr Silica.Fingerprint.off]
    )

let () =
  Driver.register_transformation "silica.fingerprint"
    ~rules:[ Context_free.Rule.extension extension ]
