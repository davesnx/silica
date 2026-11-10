open Silica

type item = { value : string; label : string; disabled : bool }
type classes = { trigger : string; popup : string; item : string }

let no_classes = { trigger = ""; popup = ""; item = "" }

type props = {
  id : string;
  label : string;
  items : item list;
  open_ : bool option;
  classes : classes;
}

type nav = Next | Previous | First | Last | Letter of char
type edge = Start | End

type event =
  | Props of props
  | Trigger_press
  | Open_key of Popup.reason * edge
  | Move of nav
  | Highlight of int
  | Choose of int
  | Close of Popup.reason

type opened = { props : props; highlighted : int option }

module State = struct
  type _ t = Idle : unit t | Closed : props t | Open : opened t
end

module M = Machine.Make (State)

type snapshot = M.snapshot = At : 'data State.t * 'data -> snapshot

let item_id id i = id ^ "-item-" ^ string_of_int i
let parts (props : props) = Popup.parts props.id

let sources =
  List.map
    (Popup.scoped (fun (o : opened) -> o.props.id))
    [
      Popup.dismiss ~escape:(Close Popup.Escape_key)
        ~outside:(Close Popup.Outside_press) Popup.template;
      Source.anchor ~anchor:Popup.template.trigger Popup.template.popup;
    ]

let enabled (props : props) i =
  match List.nth_opt props.items i with
  | Some item ->
      not item.disabled
  | None ->
      false

let starts_with letter (item : item) =
  item.label <> "" && Char.lowercase_ascii item.label.[0] = letter

let find (props : props) pred order =
  List.find_opt (fun i -> enabled props i && pred i) order

let range from until = List.init (max 0 (until - from)) (fun i -> from + i)

let navigate (props : props) highlighted nav =
  let n = List.length props.items in
  let at = Option.value highlighted ~default:(-1) in
  let any _ = true in
  let found =
    match nav with
    | First ->
        find props any (range 0 n)
    | Last ->
        find props any (List.rev (range 0 n))
    | Next ->
        find props any (range (at + 1) n @ range 0 (at + 1))
    | Previous ->
        let at = Option.value highlighted ~default:n in
        find props any (List.rev (range 0 at) @ List.rev (range at n))
    | Letter letter ->
        find props
          (fun i -> starts_with letter (List.nth props.items i))
          (range (at + 1) n @ range 0 (at + 1))
  in
  match found with Some _ -> found | None -> highlighted

let focus (o : opened) =
  match o.highlighted with
  | Some i ->
      Cmd.Focus (Ref.make (item_id o.props.id i))
  | None ->
      Cmd.Focus (parts o.props).popup

let show (o : opened) = M.goto Open o ~cmds:[ focus o ]

let close ?(focus = true) (props : props) =
  M.goto Closed props
    ~cmds:
      ( if focus then
          [ Cmd.Focus (parts props).trigger ]
        else
          []
      )

let opened_by (previous : props option) (props : props) =
  props.open_ = Some true
  && Option.map (fun (p : props) -> p.open_) previous <> Some (Some true)

let closed_by (previous : props) (props : props) =
  props.open_ = Some false && previous.open_ <> Some false

let machine =
  M.make ~id:"menu" ~init:(M.at Idle ())
    [
      M.state Idle "idle" ~to_:[ Closed; Open ] (fun () -> function
        | Props props when opened_by None props ->
            show { props; highlighted = None }
        | Props props ->
            M.goto Closed props
        | _ ->
            M.unhandled
        );
      M.state Closed "closed" ~to_:[ Open ] (fun previous -> function
        | Props props when opened_by (Some previous) props ->
            show { props; highlighted = None }
        | Props props ->
            M.goto Closed props
        | Trigger_press ->
            show { props = previous; highlighted = None }
        | Open_key (_, edge) ->
            let nav = match edge with Start -> First | End -> Last in
            show { props = previous; highlighted = navigate previous None nav }
        | Move _ | Highlight _ | Choose _ | Close _ ->
            M.unhandled
        );
      M.state Open "open" ~to_:[ Closed ] ~invoke:sources (fun o -> function
        | Props props when closed_by o.props props ->
            close props
        | Props props ->
            let highlighted =
              Option.bind o.highlighted (fun i ->
                  if enabled props i then
                    Some i
                  else
                    None
              )
            in
            M.goto Open { props; highlighted }
        | Trigger_press ->
            close o.props
        | Move nav -> (
            match navigate o.props o.highlighted nav with
            | highlighted when highlighted = o.highlighted ->
                M.stay []
            | highlighted ->
                show { o with highlighted }
          )
        | Highlight i when enabled o.props i ->
            if o.highlighted = Some i then
              M.stay []
            else
              show { o with highlighted = Some i }
        | Choose i when enabled o.props i ->
            close o.props
        | Close Popup.Outside_press ->
            close ~focus:false o.props
        | Close _ ->
            close o.props
        | Highlight _ | Choose _ | Open_key _ ->
            M.unhandled
        );
    ]

let el = Node.element
let text = Node.text
let plain (key : Key.t) = not (key.ctrl || key.meta || key.alt)

let trigger_key (key : Key.t) =
  if not (plain key) then
    None
  else
    match key.key with
    | "Enter" | " " ->
        Some (Open_key (Popup.Trigger_press, Start))
    | "ArrowDown" ->
        Some (Open_key (Popup.List_navigation, Start))
    | "ArrowUp" ->
        Some (Open_key (Popup.List_navigation, End))
    | _ ->
        None

let menu_key highlighted (key : Key.t) =
  if not (plain key) then
    None
  else
    match key.key with
    | "ArrowDown" ->
        Some (Move Next)
    | "ArrowUp" ->
        Some (Move Previous)
    | "Home" ->
        Some (Move First)
    | "End" ->
        Some (Move Last)
    | "Enter" | " " ->
        Option.map (fun i -> Choose i) highlighted
    | k when String.length k = 1 && k.[0] > ' ' && k.[0] <= '~' ->
        Some (Move (Letter (Char.lowercase_ascii k.[0])))
    | _ ->
        None

let tab_key (key : Key.t) =
  if key.key = "Tab" then
    Some (Close Popup.Focus_out)
  else
    None

let trigger (props : props) ~open_ =
  el "button"
    (Attr.string "type" "button"
     :: Attr.on_press Trigger_press
     :: Attr.on_key_down ~prevent:true (fun key ->
         if open_ then
           None
         else
           trigger_key key
     )
     :: Popup.trigger_attrs (parts props) ~haspopup:"menu" ~open_
    @ Popup.open_attrs open_
    @ Popup.class_attr props.classes.trigger
    )
    [ text props.label ]

let item (o : opened) i (item : item) =
  let id = item_id o.props.id i in
  let highlighted = o.highlighted = Some i in
  el "div"
    ([
       Attr.string "id" id;
       Attr.ref_ (Ref.make id);
       Attr.string "role" "menuitem";
       Attr.string "tabindex"
         ( if highlighted then
             "0"
           else
             "-1"
         );
     ]
    @ ( if highlighted then
          [ Attr.string "data-highlighted" "" ]
        else
          []
      )
    @ ( if item.disabled then
          [ Attr.string "aria-disabled" "true"; Attr.string "data-disabled" "" ]
        else
          [
            Attr.on_press (Choose i);
            Attr.on_pointer_enter (fun _ -> Some (Highlight i));
          ]
      )
    @ Popup.class_attr o.props.classes.item
    )
    [ text item.label ]

let popup (o : opened) =
  let parts = parts o.props in
  Node.portal
    [
      el "div"
        ([
           Attr.string "id" parts.popup_id;
           Attr.ref_ parts.popup;
           Attr.string "role" "menu";
           Attr.string "aria-labelledby" parts.trigger_id;
           Attr.string "tabindex" "-1";
           Attr.on_key_down ~prevent:true (menu_key o.highlighted);
           Attr.on_key_down tab_key;
         ]
        @ Popup.open_attrs true
        @ Popup.class_attr o.props.classes.popup
        )
        (List.mapi (item o) o.props.items);
    ]

let view : snapshot -> event Node.t = function
  | At (Idle, ()) ->
      Node.list []
  | At (Closed, props) ->
      trigger props ~open_:false
  | At (Open, o) ->
      Node.list [ trigger o.props ~open_:true; popup o ]

let component = Component.make machine view

let props ~id ~label ?open_ ?(classes = no_classes) items =
  { id; label; items; open_; classes }

let make ?key ?on_open_change ?on_select props =
  let opened value reason =
    Option.to_list (Option.map (fun f -> f value reason) on_open_change)
  in
  let output event (snapshot : snapshot) =
    match (event, snapshot) with
    | Trigger_press, At (Open, _) ->
        opened true Popup.Trigger_press
    | Trigger_press, At (Closed, _) ->
        opened false Popup.Trigger_press
    | Open_key (reason, _), At (Open, _) ->
        opened true reason
    | Choose i, At (Closed, props) ->
        Option.to_list
          (Option.map (fun f -> f (List.nth props.items i).value) on_select)
        @ opened false Popup.Item_press
    | Close reason, At (Closed, _) ->
        opened false reason
    | _ ->
        []
  in
  Component.node ?key ~props:(Props props) ~output component
