type 'e t =
  | Focus of Ref.t
  | Blur of Ref.t
  | Scroll_into_view of Ref.t
  | Scroll_to of { target : Ref.t option; left : int; top : int }
  | Measure of { target : Ref.t; on_rect : Rect.t option -> 'e }
  | Copy of { text : string; on_result : (bool -> 'e) option }
  | Load_script of { src : string; on_result : bool -> 'e }
  | Navigate of Url.t
  | Replace of Url.t
  | Back
  | Forward
  | Reload
  | Resume of Nav.t
  | Revert_navigation
  | Preload of Endpoint.request
  | Load_chunk of { chunk : Chunk.any; on_ready : 'e option }
  | Set_title of string
  | Set_status of int
  | Set_document_attribute of { name : string; value : string option }
  | Set_cookie of { name : string; value : string; max_age : int }
  | Remount

type 'e source =
  | Focus_trap of { container : Ref.t; return_focus : bool }
  | Scroll_lock
  | Outside_press of { container : Ref.t; event : 'e }
  | Global_keys of {
      bindings : (Key.t * 'e) list;
      prevent : bool;
      rate : Rate.t option;
    }
  | Resize of { target : Ref.t; rate : Rate.t option; on_resize : Rect.t -> 'e }
  | Intersection of {
      target : Ref.t;
      rate : Rate.t option;
      on_change : bool -> 'e;
    }
  | Scroll of {
      target : Ref.t option;
      rate : Rate.t option;
      on_scroll : Scroll.t -> 'e;
    }
  | Visibility of (bool -> 'e)
  | Online of (bool -> 'e)
  | Media of { query : string; on_change : bool -> 'e }
  | Dismiss of {
      containers : Ref.t list;
      on_escape : 'e option;
      on_outside : 'e option;
      on_focus_out : 'e option;
    }
  | Block of { on_attempt : Nav.t -> 'e }
  | Anchor of { anchor : Ref.t; popup : Ref.t; place : Place.t }

type 'e host =
  | Run of 'e t
  | Start_source of { id : string; source : 'e source }
  | Start_timer of { id : string; ms : int; event : 'e }
  | Start_call : {
      id : string;
      endpoint : ('i, 'o, 'x) Endpoint.t;
      input : 'i;
      timeout_ms : int option;
      on_result : ('o, 'x) Endpoint.answer -> 'e;
    }
      -> 'e host
  | Start_live : {
      id : string;
      topic : 's Live_topic.t;
      key : string;
      on_update : 's Live_topic.update -> 'e;
    }
      -> 'e host
  | Stop of string

let map (type a b) (f : a -> b) : a t -> b t = function
  | Focus r ->
      Focus r
  | Blur r ->
      Blur r
  | Scroll_into_view r ->
      Scroll_into_view r
  | Scroll_to { target; left; top } ->
      Scroll_to { target; left; top }
  | Measure { target; on_rect } ->
      Measure { target; on_rect = (fun rect -> f (on_rect rect)) }
  | Copy { text; on_result } ->
      Copy { text; on_result = Option.map (fun k ok -> f (k ok)) on_result }
  | Load_script { src; on_result } ->
      Load_script { src; on_result = (fun ok -> f (on_result ok)) }
  | Navigate u ->
      Navigate u
  | Replace u ->
      Replace u
  | Back ->
      Back
  | Forward ->
      Forward
  | Reload ->
      Reload
  | Resume nav ->
      Resume nav
  | Revert_navigation ->
      Revert_navigation
  | Preload request ->
      Preload request
  | Load_chunk { chunk; on_ready } ->
      Load_chunk { chunk; on_ready = Option.map f on_ready }
  | Set_title title ->
      Set_title title
  | Set_status status ->
      Set_status status
  | Set_document_attribute a ->
      Set_document_attribute a
  | Set_cookie c ->
      Set_cookie c
  | Remount ->
      Remount

let is_lower c = c >= 'a' && c <= 'z'
let is_digit c = c >= '0' && c <= '9'

let is_cookie_char c =
  is_lower c
  || (c >= 'A' && c <= 'Z')
  || is_digit c || c = '-' || c = '_' || c = '.'

let reject what text =
  invalid_arg ("Silica.Action: bad " ^ what ^ " \"" ^ String.escaped text ^ "\"")

let check = function[@warning "-4"]
  | Set_document_attribute { name; _ } ->
      if
        name = ""
        || (not (is_lower name.[0]))
        || not
             (String.for_all (fun c -> is_lower c || is_digit c || c = '-') name)
      then
        reject "document attribute name" name
  | Set_cookie { name; value; _ } ->
      if name = "" || not (String.for_all is_cookie_char name) then
        reject "cookie name" name;
      if not (String.for_all is_cookie_char value) then
        reject "cookie value" value
  | _ ->
      ()

let source_name = function
  | Focus_trap _ ->
      "focus_trap"
  | Scroll_lock ->
      "scroll_lock"
  | Outside_press _ ->
      "outside_press"
  | Global_keys _ ->
      "keys"
  | Resize _ ->
      "resize"
  | Intersection _ ->
      "intersection"
  | Dismiss _ ->
      "dismiss"
  | Anchor _ ->
      "anchor"
  | Scroll _ ->
      "scroll"
  | Visibility _ ->
      "visibility"
  | Online _ ->
      "online"
  | Media _ ->
      "media"
  | Block _ ->
      "block"

let map_source (type a b) (f : a -> b) : a source -> b source = function
  | Focus_trap r ->
      Focus_trap r
  | Scroll_lock ->
      Scroll_lock
  | Outside_press { container; event } ->
      Outside_press { container; event = f event }
  | Global_keys { bindings; prevent; rate } ->
      Global_keys
        {
          bindings = List.map (fun (key, event) -> (key, f event)) bindings;
          prevent;
          rate;
        }
  | Resize { target; rate; on_resize } ->
      Resize { target; rate; on_resize = (fun rect -> f (on_resize rect)) }
  | Intersection { target; rate; on_change } ->
      Intersection
        { target; rate; on_change = (fun visible -> f (on_change visible)) }
  | Dismiss { containers; on_escape; on_outside; on_focus_out } ->
      Dismiss
        {
          containers;
          on_escape = Option.map f on_escape;
          on_outside = Option.map f on_outside;
          on_focus_out = Option.map f on_focus_out;
        }
  | Anchor { anchor; popup; place } ->
      Anchor { anchor; popup; place }
  | Scroll { target; rate; on_scroll } ->
      Scroll { target; rate; on_scroll = (fun scroll -> f (on_scroll scroll)) }
  | Visibility on_change ->
      Visibility (fun visible -> f (on_change visible))
  | Online on_change ->
      Online (fun online -> f (on_change online))
  | Media { query; on_change } ->
      Media { query; on_change = (fun matches -> f (on_change matches)) }
  | Block { on_attempt } ->
      Block { on_attempt = (fun nav -> f (on_attempt nav)) }

let map_refs f = function
  | Focus_trap { container; return_focus } ->
      Focus_trap { container = f container; return_focus }
  | Scroll_lock ->
      Scroll_lock
  | Outside_press { container; event } ->
      Outside_press { container = f container; event }
  | Global_keys _ as keys ->
      keys
  | Resize { target; rate; on_resize } ->
      Resize { target = f target; rate; on_resize }
  | Intersection { target; rate; on_change } ->
      Intersection { target = f target; rate; on_change }
  | Dismiss d ->
      Dismiss { d with containers = List.map f d.containers }
  | Anchor { anchor; popup; place } ->
      Anchor { anchor = f anchor; popup = f popup; place }
  | Scroll s ->
      Scroll { s with target = Option.map f s.target }
  | (Visibility _ | Online _ | Media _ | Block _) as source ->
      source

let map_host (type a b) (f : a -> b) : a host -> b host = function
  | Run action ->
      Run (map f action)
  | Start_source { id; source } ->
      Start_source { id; source = map_source f source }
  | Start_timer { id; ms; event } ->
      Start_timer { id; ms; event = f event }
  | Start_call { id; endpoint; input; timeout_ms; on_result } ->
      Start_call
        {
          id;
          endpoint;
          input;
          timeout_ms;
          on_result = (fun r -> f (on_result r));
        }
  | Start_live { id; topic; key; on_update } ->
      Start_live { id; topic; key; on_update = (fun u -> f (on_update u)) }
  | Stop id ->
      Stop id
