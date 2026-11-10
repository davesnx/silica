open Silica

type control =
  | Toggle of { name : string; label : string; default : bool }
  | Choice of {
      name : string;
      label : string;
      options : (string * string) list;
      default : string;
    }

let toggle name ~label default = Toggle { name; label; default }

let choice name ~label options default =
  if not (List.mem_assoc default options) then
    invalid_arg
      (Printf.sprintf "Story.choice %s: %S is not an option" name default);
  Choice { name; label; options; default }

type value = Bool of bool | String of string
type values = (string * value) list

type t = {
  name : string;
  source : string;
  controls : control list;
  render : 'e. id:string -> values -> 'e Node.t;
}

let slug t =
  let b = Buffer.create (String.length t.name) in
  let dash = ref false in
  String.iter
    (fun c ->
      match Char.lowercase_ascii c with
      | ('a' .. 'z' | '0' .. '9') as c ->
          if !dash && Buffer.length b > 0 then Buffer.add_char b '-';
          dash := false;
          Buffer.add_char b c
      | _ ->
          dash := true
    )
    t.name;
  Buffer.contents b

let defaults t =
  List.map
    (function
      | Toggle { name; default; _ } ->
          (name, Bool default)
      | Choice { name; default; _ } ->
          (name, String default)
      )
    t.controls

let bool values name =
  match List.assoc name values with Bool b -> b | String _ -> raise Not_found

let string values name =
  match List.assoc name values with String s -> s | Bool _ -> raise Not_found

let set values name value =
  List.map
    (fun (n, v) ->
      if n = name then
        (n, value)
      else
        (n, v)
    )
    values

let set_bool values name b = set values name (Bool b)
let set_string values name s = set values name (String s)

let controls ~id t values on_change =
  match t.controls with
  | [] ->
      Node.list []
  | controls ->
      Node.element "fieldset"
        [ Attr.string "class" "flex flex-col gap-3" ]
        (Node.element "legend"
           [ Attr.string "class" "mb-2 text-xs font-semibold text-ui-muted" ]
           [ Node.text "Controls" ]
        :: List.map
             (function
               | Toggle { name; label; _ } ->
                   Switch.make
                     ~on_checked_change:(fun b ->
                       on_change (set_bool values name b)
                     )
                     (Switch.props
                        ~id:(id ^ "-" ^ name)
                        ~label ~checked:(bool values name) ~classes:Style.switch
                        ()
                     )
               | Choice { name; label; options; _ } ->
                   Node.element "div"
                     [ Attr.string "class" "flex flex-col gap-1 text-sm" ]
                     [
                       Node.element "span"
                         [ Attr.string "class" "text-ui-muted" ]
                         [ Node.text label ];
                       Select.make
                         ~on_value_change:(fun v ->
                           on_change (set_string values name v)
                         )
                         (Select.props
                            ~id:(id ^ "-" ^ name)
                            ~label ~value:(string values name)
                            ~classes:Style.select
                            (List.map
                               (fun (value, label) ->
                                 { Select.value; label; disabled = false }
                               )
                               options
                            )
                         );
                     ]
               )
             controls
        )

let render ~id t values = t.render ~id values
