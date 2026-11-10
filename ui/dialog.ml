open Silica

type classes = {
  trigger : string;
  backdrop : string;
  popup : string;
  title : string;
  description : string;
  close : string;
}

let no_classes =
  {
    trigger = "";
    backdrop = "";
    popup = "";
    title = "";
    description = "";
    close = "";
  }

module type CONTENT = sig
  type props
  type event

  val id : string
  val view : props -> event Node.t
  val closes : event -> bool
end

module Make (Content : CONTENT) = struct
  type props = {
    id : string;
    title : string;
    description : string option;
    trigger : string option;
    close : string option;
    open_ : bool option;
    dismissible : bool;
    alert : bool;
    classes : classes;
    content : Content.props;
  }

  type event =
    | Props of props
    | Open_request of Popup.reason
    | Close_request of Popup.reason
    | Content of Content.event

  module State = struct
    type _ t = Idle : unit t | Closed : props t | Open : props t
  end

  module M = Machine.Make (State)

  type snapshot = M.snapshot = At : 'data State.t * 'data -> snapshot

  let parts (props : props) = Popup.parts props.id

  let sources =
    List.map
      (Popup.scoped (fun (props : props) -> props.id))
      [
        Source.focus_trap Popup.template.popup;
        Source.scroll_lock;
        Popup.dismiss ~escape:(Close_request Popup.Escape_key)
          ~outside:(Close_request Popup.Outside_press) Popup.template;
      ]

  let dismisses (props : props) = function
    | Popup.Escape_key ->
        props.dismissible
    | Popup.Outside_press ->
        props.dismissible && not props.alert
    | _ ->
        true

  let opened_by (previous : props option) (props : props) =
    props.open_ = Some true
    && Option.map (fun (p : props) -> p.open_) previous <> Some (Some true)

  let closed_by (previous : props) (props : props) =
    props.open_ = Some false && previous.open_ <> Some false

  let close (props : props) =
    let cmds =
      match props.trigger with
      | Some _ ->
          [ Cmd.Focus (parts props).trigger ]
      | None ->
          []
    in
    M.goto Closed props ~cmds

  let machine =
    M.make ~id:Content.id ~init:(M.at Idle ())
      [
        M.state Idle "idle" ~to_:[ Closed; Open ] (fun () -> function
          | Props props when opened_by None props ->
              M.goto Open props
          | Props props ->
              M.goto Closed props
          | _ ->
              M.unhandled
          );
        M.state Closed "closed" ~to_:[ Open ] (fun previous -> function
          | Props props when opened_by (Some previous) props ->
              M.goto Open props
          | Props props ->
              M.goto Closed props
          | Open_request _ ->
              M.goto Open previous
          | Close_request _ | Content _ ->
              M.unhandled
          );
        M.state Open "open" ~to_:[ Closed ] ~invoke:sources
          (fun previous -> function
          | Props props when closed_by previous props ->
              close props
          | Props props ->
              M.goto Open props
          | Close_request reason when dismisses previous reason ->
              close previous
          | Close_request _ ->
              M.unhandled
          | Content event when Content.closes event ->
              close previous
          | Content _ ->
              M.stay []
          | Open_request _ ->
              M.unhandled
          );
      ]

  let el = Node.element
  let text = Node.text

  let trigger (props : props) ~open_ =
    match props.trigger with
    | None ->
        Node.list []
    | Some label ->
        el "button"
          (Attr.string "type" "button"
           :: Attr.on_press (Open_request Popup.Trigger_press)
           :: Popup.trigger_attrs (parts props) ~haspopup:"dialog" ~open_
          @ Popup.open_attrs open_
          @ Popup.class_attr props.classes.trigger
          )
          [ text label ]

  let popup (props : props) =
    let parts = parts props in
    let title_id = props.id ^ "-title"
    and description_id = props.id ^ "-description" in
    Node.portal
      [
        el "div"
          ([ Attr.string "data-ui" "dialog-backdrop" ]
          @ Popup.open_attrs true
          @ Popup.class_attr props.classes.backdrop
          )
          [];
        el "div"
          ([
             Attr.string "id" parts.popup_id;
             Attr.ref_ parts.popup;
             Attr.string "role"
               ( if props.alert then
                   "alertdialog"
                 else
                   "dialog"
               );
             Attr.string "aria-modal" "true";
             Attr.string "aria-labelledby" title_id;
             Attr.string "tabindex" "-1";
           ]
          @ ( match props.description with
            | Some _ ->
                [ Attr.string "aria-describedby" description_id ]
            | None ->
                []
            )
          @ Popup.open_attrs true
          @ Popup.class_attr props.classes.popup
          )
          [
            el "h2"
              (Attr.string "id" title_id :: Popup.class_attr props.classes.title)
              [ text props.title ];
            ( match props.description with
            | Some description ->
                el "p"
                  (Attr.string "id" description_id
                  :: Popup.class_attr props.classes.description
                  )
                  [ text description ]
            | None ->
                Node.list []
            );
            Node.map (fun event -> Content event) (Content.view props.content);
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

  let view : snapshot -> event Node.t = function
    | At (Idle, ()) ->
        Node.list []
    | At (Closed, props) ->
        trigger props ~open_:false
    | At (Open, props) ->
        Node.list [ trigger props ~open_:true; popup props ]

  let component = Component.make machine view

  let props ~id ~title ?description ?trigger ?close ?open_ ?(dismissible = true)
      ?(alert = false) ?(classes = no_classes) content =
    {
      id;
      title;
      description;
      trigger;
      close;
      open_;
      dismissible;
      alert;
      classes;
      content;
    }

  let make ?key ?on_open_change ?on_content props =
    let opened value reason =
      Option.to_list (Option.map (fun f -> f value reason) on_open_change)
    in
    let output event (snapshot : snapshot) =
      match (event, snapshot) with
      | Open_request reason, At (Open, _) ->
          opened true reason
      | Close_request reason, At (Closed, _) ->
          opened false reason
      | Content event, _ ->
          Option.to_list (Option.map (fun f -> f event) on_content)
          @
          if Content.closes event then
            opened false Popup.Close_press
          else
            []
      | _ ->
          []
    in
    Component.node ?key ~props:(Props props) ~output component
end
