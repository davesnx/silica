open Silica

type classes = {
  trigger : string;
  popup : string;
  title : string;
  close : string;
}

let no_classes = { trigger = ""; popup = ""; title = ""; close = "" }

type ('content, 'ce) props = {
  id : string;
  trigger : string;
  title : string option;
  close : string option;
  open_ : bool option;
  place : Place.t;
  classes : classes;
  content : 'content;
}

type 'ce event =
  | Open_request of Popup.reason
  | Close_request of Popup.reason
  | Content of 'ce

type state = Closed | Open

let name = function Closed -> "closed" | Open -> "open"
let states = [ "closed"; "open" ]

let parts (props : (_, _) props) = Popup.parts props.id

let open_ (props : (_, _) props) : (state, _ event) Machine.next =
  Go_with (Open, [ Action.Focus (parts props).popup ])

let close ?focus (props : (_, _) props) : (state, _ event) Machine.next =
  Popup.close ?focus (parts props) Closed

let event_name content_name = function
  | Open_request reason ->
      "open_request " ^ Popup.reason_name reason
  | Close_request reason ->
      "close_request " ^ Popup.reason_name reason
  | Content event -> (
      match content_name with Some name -> name event | None -> "content"
    )

let machine ~id ~closes ?event_name:content_name () =
  Machine.make_with_props ~id ~init:Closed ~name ~states
    ~event_name:(event_name content_name)
    ~during:(fun props -> function
      | Open ->
          Popup.layer ~escape:(Close_request Popup.Escape_key)
            ~outside:(Close_request Popup.Outside_press) ~place:props.place
            (parts props)
      | Closed ->
          []
      )
    ~on_props:(fun previous props state ->
      match
        ( Popup.open_change
            (fun (props : (_, _) props) -> props.open_)
            previous props,
          state,
          previous
        )
      with
      | Some true, Closed, _ ->
          open_ props
      | Some false, Open, _ ->
          close props
      | (Some _ | None), Open, Some previous when previous.place <> props.place
        ->
          Reenter Open
      | (Some _ | None), (Open | Closed), (Some _ | None) ->
          Ignore
    )
    (fun props state event ->
      match (state, event) with
      | Closed, Open_request _ ->
          open_ props
      | Open, Close_request reason ->
          close ~focus:(reason <> Popup.Outside_press) props
      | Open, Content event when closes event ->
          close props
      | Open, Content _ ->
          Stay []
      | Closed, (Close_request _ | Content _) | Open, Open_request _ ->
          Ignore
    )

let el = Node.element
let text = Node.text

let trigger (props : (_, _) props) ~open_ =
  el "button"
    (Attr.string "type" "button"
     :: Attr.on_press
          ( if open_ then
              Close_request Popup.Trigger_press
            else
              Open_request Popup.Trigger_press
          )
     :: Popup.trigger_attrs (parts props) ~haspopup:"dialog" ~open_
    @ Popup.open_attrs open_
    @ Popup.class_attr props.classes.trigger
    )
    [ text props.trigger ]

let popup ~content (props : (_, _) props) =
  let parts = parts props and title_id = props.id ^ "-title" in
  Node.portal
    [
      el "div"
        ([
           Attr.string "id" parts.popup_id;
           Attr.ref_ parts.popup;
           Attr.string "role" "dialog";
           Attr.string "tabindex" "-1";
         ]
        @ ( match props.title with
          | Some _ ->
              [ Attr.string "aria-labelledby" title_id ]
          | None ->
              []
          )
        @ Popup.open_attrs true
        @ Popup.class_attr props.classes.popup
        )
        [
          ( match props.title with
          | Some title ->
              el "h2"
                (Attr.string "id" title_id
                :: Popup.class_attr props.classes.title
                )
                [ text title ]
          | None ->
              Node.list []
          );
          Node.map (fun event -> Content event) (content props.content);
          ( match props.close with
          | Some label ->
              el "button"
                ([
                   Attr.string "type" "button";
                   Attr.string "id" (props.id ^ "-close");
                   Attr.on_press (Close_request Popup.Close_press);
                 ]
                @ Popup.class_attr props.classes.close
                )
                [ text label ]
          | None ->
              Node.list []
          );
        ];
    ]

let view ~content props = function
  | Closed ->
      trigger props ~open_:false
  | Open ->
      Node.list [ trigger props ~open_:true; popup ~content props ]

type ('content, 'ce) t = {
  component : (('content, 'ce) props, state, 'ce event) Component.t;
  closes : 'ce -> bool;
}

let component ~id ~closes ?event_name content =
  {
    component =
      Component.make_with_props
        (machine ~id ~closes ?event_name ())
        (view ~content);
    closes;
  }

let props ~id ~trigger ?title ?close ?open_ ?(place = Place.default)
    ?(classes = no_classes) content =
  { id; trigger; title; close; open_; place; classes; content }

let make ?key ?on_open_change ?on_content popover props =
  let opened = Popup.report on_open_change in
  let output event state =
    match (event, state) with
    | Open_request reason, Open ->
        opened true reason
    | Close_request reason, Closed ->
        opened false reason
    | Content event, _ ->
        Option.to_list (Option.map (fun f -> f event) on_content)
        @
        if popover.closes event then
          opened false Popup.Close_press
        else
          []
    | Open_request _, Closed | Close_request _, Open ->
        []
  in
  Component.node_with_props ?key ~props ~output popover.component
