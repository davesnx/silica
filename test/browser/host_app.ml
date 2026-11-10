open Silica

let el = Node.element
let txt = Node.text
let id v = Attr.string "id" v
let simple = App.simple

module Dialog = struct
  type event =
    | Open
    | Close
    | Outside
    | Escape
    | Toggle_k
    | Resized of float
    | Seen of bool

  type data = {
    outsides : int;
    leaks : int;
    escapes : int;
    width : float;
    seen : string;
  }

  module States = struct
    type _ t = Closed : data t | Opened : data t
  end

  module M = Machine.Make (States)

  let opener = Ref.make "opener"
  let dialog = Ref.make "dialog"
  let box = Ref.make "box"

  let initial = { outsides = 0; leaks = 0; escapes = 0; width = 0.; seen = "-" }

  let machine ~id ~sources =
    let to_ = M.[ States.Closed; States.Opened ] in
    M.make ~id
      ~init:(M.at States.Closed initial)
      [
        M.state States.Closed "closed" ~to_ (fun data -> function
          | Open | Toggle_k ->
              M.goto States.Opened data
          | Outside | Escape ->
              M.goto States.Closed { data with leaks = data.leaks + 1 }
          | Close | Resized _ | Seen _ ->
              M.unhandled
          );
        M.state States.Opened "opened" ~to_ ~invoke:sources
          (fun data -> function
          | Close ->
              M.goto States.Closed data
          | Toggle_k ->
              M.goto States.Closed data
          | Outside ->
              M.goto States.Closed { data with outsides = data.outsides + 1 }
          | Escape ->
              M.goto States.Closed { data with escapes = data.escapes + 1 }
          | Resized width ->
              M.goto States.Opened { data with width }
          | Seen visible ->
              M.goto States.Opened
                {
                  data with
                  seen =
                    ( if visible then
                        "visible"
                      else
                        "hidden"
                    );
                }
          | Open ->
              M.unhandled
          );
      ]

  let data (M.At (state, data) : M.snapshot) : data =
    match state with States.Closed -> data | States.Opened -> data

  let info snapshot =
    let d = data snapshot in
    el "p"
      [ id "info" ]
      [
        txt
          (Printf.sprintf "outsides=%d leaks=%d escapes=%d width=%.0f seen=%s"
             d.outsides d.leaks d.escapes d.width d.seen
          );
      ]

  let view (M.At (state, _) as snapshot : M.snapshot) : event Node.t =
    match state with
    | States.Closed ->
        el "main" []
          [
            el "button"
              [ id "opener"; Attr.ref_ opener; Attr.on_press Open ]
              [ txt "open" ];
            el "button" [ id "outside" ] [ txt "elsewhere" ];
            info snapshot;
          ]
    | States.Opened ->
        el "main" []
          [
            el "button" [ id "outside" ] [ txt "elsewhere" ];
            el "div"
              [ id "dialog"; Attr.ref_ dialog ]
              [
                el "button" [ id "first" ] [ txt "first" ];
                el "button" [ id "second" ] [ txt "second" ];
                el "button" [ id "close"; Attr.on_press Close ] [ txt "close" ];
                el "span" [ id "box"; Attr.ref_ box ] [ txt "box" ];
              ];
            info snapshot;
          ]

  let sources_of = function
    | "trap" ->
        [ Source.focus_trap dialog ]
    | "trap-no-return" ->
        [ Source.focus_trap ~return_focus:false dialog ]
    | "lock" ->
        [ Source.scroll_lock ]
    | "double-lock" ->
        [ Source.scroll_lock; Source.scroll_lock ]
    | "outside" ->
        [ Source.outside_press dialog Outside ]
    | "keys" ->
        [
          Source.keys ~prevent:true
            [
              (Key.escape, Escape);
              (Key.make ~ctrl:true "k", Toggle_k);
              (Key.make "j", Toggle_k);
            ];
        ]
    | "observers" ->
        [
          Source.resize box (fun rect -> Resized rect.Rect.width);
          Source.intersection box (fun visible -> Seen visible);
        ]
    | _ ->
        []

  let component kind =
    {
      Component.machine =
        machine ~id:("dialog-" ^ kind) ~sources:(sources_of kind);
      view;
    }
end

let mount_dialog kind root = Silica_browser.mount ~root (Dialog.component kind)

module Events = struct
  type event = Log of string

  let view log =
    el "main" []
      [
        el "input"
          [
            id "prevent";
            Attr.string "aria-label" "prevent";
            Attr.on_key_down ~prevent:true (fun key ->
                if key.Key.key = "ArrowDown" then
                  Some (Log ("prevent:" ^ Key.to_string key))
                else
                  None
            );
          ]
          [];
        el "input"
          [
            id "plain";
            Attr.string "aria-label" "plain";
            Attr.on_key_down (fun key -> Some (Log ("plain:" ^ key.Key.key)));
          ]
          [];
        el "input"
          [
            id "debounced";
            Attr.string "aria-label" "debounced";
            Attr.on_input ~rate:(Rate.Debounce 30) (fun value ->
                Log ("debounced:" ^ value)
            );
          ]
          [];
        el "input"
          [
            id "throttled";
            Attr.string "aria-label" "throttled";
            Attr.on_key_down ~rate:(Rate.Throttle 50) (fun key ->
                Some (Log ("throttled:" ^ key.Key.key))
            );
          ]
          [];
        el "input"
          [
            id "keyup";
            Attr.string "aria-label" "keyup";
            Attr.on_key_up (fun key -> Some (Log ("up:" ^ key.Key.key)));
          ]
          [];
        el "div"
          [
            id "hover";
            Attr.on_pointer_enter (fun _ -> Some (Log "enter"));
            Attr.on_pointer_leave (fun _ -> Some (Log "leave"));
          ]
          [ el "span" [ id "inner" ] [ txt "hover me" ] ];
        el "p" [ id "log" ] [ txt (String.concat ";" (List.rev log)) ];
      ]

  let component =
    {
      Component.machine =
        simple ~id:"events" ~init:[] (fun log (Log entry) ->
            Some (entry :: log, [])
        );
      view;
    }
end

let mount_events root = Silica_browser.mount ~root Events.component

module Hydrate = struct
  type event = Inc | Type of string

  type state = { count : int; typed : string }

  let svg_view =
    el "svg"
      [ id "icon"; Attr.string "viewBox" "0 0 10 10" ]
      [
        el "use" [ id "use"; Attr.string "xlink:href" "#sym" ] [];
        el "path" [ id "path"; Attr.string "d" "M0 0L10 10" ] [];
      ]

  let view { count; typed } =
    el "div"
      [ id "app"; Attr.string "class" "a" ]
      [
        el "h1" [ id "title" ] [ txt "Hello" ];
        el "p" [ id "pair" ] [ txt "a"; txt "b" ];
        el "input"
          [
            id "field";
            Attr.string "aria-label" "field";
            Attr.string "value" typed;
            Attr.on_input (fun v -> Type v);
          ]
          [];
        el "button"
          [ id "inc"; Attr.on_press Inc ]
          [ txt "count "; txt (string_of_int count) ];
        svg_view;
        el "p" [ id "typed" ] [ txt typed ];
      ]

  let component =
    {
      Component.machine =
        simple ~id:"hydrate" ~init:{ count = 0; typed = "" } (fun s -> function
          | Inc ->
              Some ({ s with count = s.count + 1 }, [])
          | Type typed ->
              Some ({ s with typed }, [])
          );
      view;
    }
end

let server_html component =
  let tree = Instance.mount component in
  Render.to_string (fst (Instance.update tree None))

let hydrate_html () = server_html Hydrate.component

let mount_hydrate ~dev root = Silica_browser.mount ~dev ~root Hydrate.component

let lookup_html () = fst (App.lookup_server ())
let lookup_answers () = Answers.to_script (snd (App.lookup_server ()))
let mount_lookup root = Silica_browser.mount ~root App.Lookup.component

module Svg = struct
  type event = Toggle | Grow

  type state = { alt : bool; circles : int }

  let view { alt; circles } =
    el "div" []
      [
        el "svg"
          [ id "svg"; Attr.string "viewBox" "0 0 20 20" ]
          ([
             el "path" [ id "path"; Attr.string "d" "M0 0" ] [];
             el "use"
               [
                 id "use";
                 Attr.string "xlink:href"
                   ( if alt then
                       "#b"
                     else
                       "#a"
                   );
                 Attr.string "xml:space" "preserve";
               ]
               [];
             el "a" [ id "link" ] [ el "text" [] [ txt "t" ] ];
             el "foreignObject"
               [ id "fo" ]
               [ el "div" [ id "in-fo" ] [ txt "html" ] ];
           ]
          @ List.init circles (fun i ->
              el "circle" [ Attr.string "data-i" (string_of_int i) ] []
          )
          );
        el "a" [ id "html-a"; Attr.string "href" "#x" ] [ txt "link" ];
        el "button" [ id "toggle"; Attr.on_press Toggle ] [ txt "toggle" ];
        el "button" [ id "grow"; Attr.on_press Grow ] [ txt "grow" ];
      ]

  let component =
    {
      Component.machine =
        simple ~id:"svg" ~init:{ alt = false; circles = 0 } (fun s -> function
          | Toggle ->
              Some ({ s with alt = not s.alt }, [])
          | Grow ->
              Some ({ s with circles = s.circles + 1 }, [])
          );
      view;
    }
end

let mount_svg root = Silica_browser.mount ~root Svg.component

module Portal = struct
  type event = Inc | Toggle | Move

  type state = { count : int; open_ : bool; target : string option }

  let view { count; open_; target } =
    el "main" []
      ([
         el "p" [ id "count" ] [ txt (string_of_int count) ];
         el "button" [ id "toggle"; Attr.on_press Toggle ] [ txt "toggle" ];
         el "button" [ id "move"; Attr.on_press Move ] [ txt "move" ];
         txt "before";
         txt "after";
       ]
      @
      if open_ then
        [
          Node.portal ?target
            [
              el "div"
                [ id "popup" ]
                [
                  el "button"
                    [ id "popup-inc"; Attr.on_press Inc ]
                    [ txt "inc" ];
                ];
            ];
        ]
      else
        []
      )

  let component =
    {
      Component.machine =
        simple ~id:"portal" ~init:{ count = 0; open_ = false; target = None }
          (fun s -> function
          | Inc ->
              Some ({ s with count = s.count + 1 }, [])
          | Toggle ->
              Some ({ s with open_ = not s.open_ }, [])
          | Move ->
              Some ({ s with target = Some "modal-root" }, [])
          );
      view;
    }
end

let mount_portal root = Silica_browser.mount ~root Portal.component

module Gadgets = struct
  type event =
    | Scroll_window
    | Scroll_box
    | Measure_box
    | Measure_missing
    | Copy_text
    | Copy_quiet
    | Load of string
    | Watch
    | Unwatch
    | Got of string

  module States = struct
    type _ t = Idle : string list t | Watching : string list t
  end

  module M = Machine.Make (States)

  let box = Ref.make "gadget-box"

  let rect (r : Rect.t) =
    Printf.sprintf "%.0f,%.0f,%.0f,%.0f" r.x r.y r.width r.height

  let scroll name (s : Scroll.t) =
    Printf.sprintf "%s:%.0f,%.0f,%.0f,%.0f,%.0f,%.0f" name s.left s.top
      s.scroll_width s.scroll_height s.client_width s.client_height

  let command = function
    | Scroll_window ->
        [ Cmd.Scroll_to { target = None; left = 5; top = 7 } ]
    | Scroll_box ->
        [ Cmd.Scroll_to { target = Some box; left = 3; top = 4 } ]
    | Measure_box ->
        [
          Cmd.Measure
            {
              target = box;
              on_rect =
                (fun r ->
                  Got
                    ("rect:" ^ match r with Some r -> rect r | None -> "none")
                );
            };
        ]
    | Measure_missing ->
        [
          Cmd.Measure
            {
              target = Ref.make "missing";
              on_rect =
                (fun r ->
                  Got
                    ( if r = None then
                        "rect:none"
                      else
                        "rect:some"
                    )
                );
            };
        ]
    | Copy_text ->
        [
          Cmd.Copy
            {
              text = "hello";
              on_result = Some (fun ok -> Got (Printf.sprintf "copy:%b" ok));
            };
        ]
    | Copy_quiet ->
        [ Cmd.Copy { text = "quiet"; on_result = None } ]
    | Load src ->
        [
          Cmd.Load_script
            {
              src;
              on_result = (fun ok -> Got (Printf.sprintf "load:%s:%b" src ok));
            };
        ]
    | Watch | Unwatch | Got _ ->
        []

  let handle state log event =
    match event with
    | Got line ->
        M.goto state (line :: log)
    | Watch ->
        M.goto States.Watching log
    | Unwatch ->
        M.goto States.Idle log
    | other ->
        M.stay (command other)

  let machine =
    let to_ = M.[ States.Idle; States.Watching ] in
    M.make ~id:"gadgets" ~init:(M.at States.Idle [])
      [
        M.state States.Idle "idle" ~to_ (handle States.Idle);
        M.state States.Watching "watching" ~to_
          ~invoke:
            [
              Source.scroll (fun s -> Got (scroll "page" s));
              Source.scroll ~target:box (fun s -> Got (scroll "box" s));
              Source.visibility (fun v -> Got (Printf.sprintf "visible:%b" v));
              Source.online (fun v -> Got (Printf.sprintf "online:%b" v));
            ]
          (handle States.Watching);
      ]

  let button name event =
    el "button" [ id name; Attr.on_press event ] [ txt name ]

  let view (M.At (state, data) : M.snapshot) : event Node.t =
    let log : string list =
      match state with States.Idle -> data | States.Watching -> data
    in
    el "main" []
      [
        el "div" [ id "gadget-box"; Attr.ref_ box ] [ txt "box" ];
        button "scroll-window" Scroll_window;
        button "scroll-box" Scroll_box;
        button "measure-box" Measure_box;
        button "measure-missing" Measure_missing;
        button "copy" Copy_text;
        button "copy-quiet" Copy_quiet;
        button "load-a" (Load "/a.js");
        button "load-b" (Load "/b.js");
        button "watch" Watch;
        button "unwatch" Unwatch;
        el "p" [ id "gadget-log" ] [ txt (String.concat "|" (List.rev log)) ];
      ]

  let component = Component.make machine view
end

let mount_gadgets root = Silica_browser.mount ~root Gadgets.component

module Document = struct
  type event =
    | Attr of string option
    | Bad_attr
    | Cookie of string
    | Watch
    | Unwatch
    | Dark of bool

  module States = struct
    type _ t = Idle : string list t | Watching : string list t
  end

  module M = Machine.Make (States)

  let handle state log = function
    | Attr value ->
        M.stay [ Cmd.Set_document_attribute { name = "data-theme"; value } ]
    | Bad_attr ->
        M.stay
          [ Cmd.Set_document_attribute { name = "on click"; value = None } ]
    | Cookie value ->
        M.stay [ Cmd.Set_cookie { name = "silica-theme"; value; max_age = 60 } ]
    | Watch ->
        M.goto States.Watching log
    | Unwatch ->
        M.goto States.Idle log
    | Dark dark ->
        M.goto state (Printf.sprintf "dark:%b" dark :: log)

  let machine =
    let to_ = M.[ States.Idle; States.Watching ] in
    M.make ~id:"document" ~init:(M.at States.Idle [])
      [
        M.state States.Idle "idle" ~to_ (handle States.Idle);
        M.state States.Watching "watching" ~to_
          ~invoke:
            [ Source.media "(prefers-color-scheme: dark)" (fun d -> Dark d) ]
          (handle States.Watching);
      ]

  let button name event =
    el "button" [ id name; Attr.on_press event ] [ txt name ]

  let view (M.At (state, data) : M.snapshot) : event Node.t =
    let log : string list =
      match state with States.Idle -> data | States.Watching -> data
    in
    el "main" []
      [
        button "attr-ocean" (Attr (Some "ocean"));
        button "attr-none" (Attr None);
        button "attr-bad" Bad_attr;
        button "cookie-dark" (Cookie "dark");
        button "watch" Watch;
        button "unwatch" Unwatch;
        el "p" [ id "document-log" ] [ txt (String.concat "|" (List.rev log)) ];
      ]

  let component = Component.make machine view
end

let mount_document root =
  Silica_browser.mount ~dev:true ~root Document.component
let read_cookie name =
  Option.value (Silica_browser.cookie name) ~default:"(none)"

module Twin = struct
  type event = Toggle_first | Toggle_second

  type state = { first : bool; second : bool }

  let popup name =
    Node.portal
      [
        el "div"
          [ id (name ^ "-popup") ]
          [ el "button" [ id (name ^ "-button") ] [ txt name ] ];
      ]

  let view { first; second } =
    el "main" []
      [
        el "button"
          [ id "toggle-first"; Attr.on_press Toggle_first ]
          [ txt "1" ];
        el "button"
          [ id "toggle-second"; Attr.on_press Toggle_second ]
          [ txt "2" ];
        ( if first then
            popup "first"
          else
            Node.list []
        );
        ( if second then
            popup "second"
          else
            Node.list []
        );
      ]

  let component =
    {
      Component.machine =
        simple ~id:"twin" ~init:{ first = false; second = false }
          (fun s -> function
          | Toggle_first ->
              Some ({ s with first = not s.first }, [])
          | Toggle_second ->
              Some ({ s with second = not s.second }, [])
          );
      view;
    }
end

let mount_twin root = Silica_browser.mount ~root Twin.component

module Scoped = struct
  module Theme = Silica_ui.Theme

  type event = Toggle | Switch

  type state = { open_ : bool; outer : Theme.t }

  let popup name =
    Node.portal [ el "div" [ id (name ^ "-popup") ] [ txt name ] ]

  let view { open_; outer } =
    let popups names =
      if open_ then
        List.map popup names
      else
        []
    in
    el "main" []
      ([
         el "button" [ id "scoped-toggle"; Attr.on_press Toggle ] [ txt "t" ];
         el "button" [ id "scoped-switch"; Attr.on_press Switch ] [ txt "s" ];
         Theme.scope outer
           ([
              el "p" [ id "scoped-text" ] [ txt "in scope" ];
              Theme.scope Theme.dark (popups [ "inner" ]);
            ]
           @ popups [ "outer" ]
           );
       ]
      @ popups [ "page" ]
      )

  let component =
    {
      Component.machine =
        simple ~id:"scoped" ~init:{ open_ = false; outer = Theme.ocean }
          (fun s -> function
          | Toggle ->
              Some ({ s with open_ = not s.open_ }, [])
          | Switch ->
              Some ({ s with outer = Theme.high_contrast }, [])
          );
      view;
    }
end

let mount_scoped root = Silica_browser.mount ~root Scoped.component

module Rows = struct
  type event =
    | Create of int
    | Append
    | Clear
    | Swap
    | Select of int
    | Remove of int

  type state = { rows : int array; next : int; selected : int }

  let fresh next count = Array.init count (fun i -> next + i)

  let row selected n =
    let name = string_of_int n in
    Node.keyed name
      (el "tr"
         [
           id ("row-" ^ name);
           Attr.string "class"
             ( if n = selected then
                 "danger"
               else
                 ""
             );
         ]
         [
           el "td" [] [ txt name ];
           el "td" []
             [
               el "a" [ id ("select-" ^ name); Attr.on_press (Select n) ] [];
               el "a" [ id ("remove-" ^ name); Attr.on_press (Remove n) ] [];
             ];
         ]
      )

  let button name event = el "button" [ id name; Attr.on_press event ] []

  let view { rows; selected; _ } =
    el "main" []
      [
        button "create-1000" (Create 1000);
        button "create-10000" (Create 10000);
        button "append" Append;
        button "clear" Clear;
        button "swap" Swap;
        el "table" []
          [
            el "tbody"
              [ id "rows" ]
              (Array.to_list (Array.map (row selected) rows));
          ];
      ]

  let component =
    {
      Component.machine =
        simple ~id:"rows" ~init:{ rows = [||]; next = 1; selected = 0 }
          (fun s -> function
          | Create count ->
              Some
                ({ s with rows = fresh s.next count; next = s.next + count }, [])
          | Append ->
              Some
                ( {
                    s with
                    rows = Array.append s.rows (fresh s.next 1000);
                    next = s.next + 1000;
                  },
                  []
                )
          | Clear ->
              Some ({ s with rows = [||] }, [])
          | Swap when Array.length s.rows > 998 ->
              let rows = Array.copy s.rows in
              rows.(1) <- s.rows.(998);
              rows.(998) <- s.rows.(1);
              Some ({ s with rows }, [])
          | Swap ->
              None
          | Select n ->
              Some ({ s with selected = n }, [])
          | Remove n ->
              Some
                ( {
                    s with
                    rows =
                      Array.of_seq
                        (Seq.filter (fun r -> r <> n) (Array.to_seq s.rows));
                  },
                  []
                )
          );
      view;
    }
end

let mount_rows root = Silica_browser.mount ~root Rows.component

module Clamp = struct
  type event = Typed of string

  let component =
    {
      Component.machine =
        simple ~id:"clamp" ~init:"abc" (fun s -> function
          | Typed v when String.length v > 3 ->
              Some (s, [])
          | Typed v ->
              Some (v, [])
          );
      view =
        (fun s ->
          el "main" []
            [
              el "input"
                [
                  id "clamp";
                  Attr.string "value" s;
                  Attr.on_input (fun v -> Typed v);
                ]
                [];
            ]
        );
    }
end

let mount_clamp root = Silica_browser.mount ~root Clamp.component

let errors : string list ref = ref []
let reported () = Array.of_list (List.rev !errors)

let on_error exn (error : Node.error) =
  errors := (Printexc.to_string exn ^ "|" ^ error.id) :: !errors

let mount_boundary ~guarded ~dev root =
  Silica_browser.mount ~dev ~on_error ~root (App.Shell.component ~guarded)

let mount_mismatch ~dev root =
  Silica_browser.mount ~dev ~on_error ~root Hydrate.component

module Fake = struct
  let log : string list ref = ref []
  let last : Instance.message Node.t ref = ref (Node.text "")
  let dispatch : (Instance.message -> unit) option ref = ref None
  let note line = log := line :: !log

  let command : Instance.message Cmd.t -> string = function
    | Cmd.Focus r ->
        "focus " ^ Ref.name r
    | Cmd.Blur r ->
        "blur " ^ Ref.name r
    | Cmd.Set_title t ->
        "title " ^ t
    | Cmd.Scroll_to { left; top; _ } ->
        Printf.sprintf "scroll_to %d %d" left top
    | Cmd.Measure { target; _ } ->
        "measure " ^ Ref.name target
    | Cmd.Copy { text; _ } ->
        "copy " ^ text
    | Cmd.Load_script { src; _ } ->
        "load_script " ^ src
    | Cmd.Scroll_into_view _
    | Cmd.Navigate _
    | Cmd.Replace _
    | Cmd.Back
    | Cmd.Forward
    | Cmd.Reload
    | Cmd.Resume _
    | Cmd.Revert_navigation
    | Cmd.Preload _
    | Cmd.Set_status _
    | Cmd.Set_document_attribute _
    | Cmd.Set_cookie _
    | Cmd.Load_chunk _
    | Cmd.Remount ->
        "other"

  let renderer : Instance.message Renderer.make =
   fun ~dispatch:send ->
    dispatch := Some send;
    {
      Renderer.paint =
        (fun node ->
          last := node;
          note ("paint " ^ Render.to_string node)
        );
      run = (fun cmd -> note ("run " ^ command cmd));
      start_source =
        (fun ~id source -> note ("start " ^ id ^ " " ^ Cmd.source_name source));
      stop_source = (fun ~id -> note ("stop " ^ id));
    }

  let rec first_press node =
    match Node.view node with
    | Node.Element (_, attrs, children) -> (
        match
          List.find_map (function Node.On_press m -> Some m | _ -> None) attrs
        with
        | Some m ->
            Some m
        | None ->
            List.find_map first_press children
      )
    | Node.List children ->
        List.find_map first_press children
    | Node.Keyed (_, child) ->
        first_press child
    | _ ->
        None

  let press () =
    match (first_press !last, !dispatch) with
    | Some message, Some send ->
        send message
    | _ ->
        failwith "nothing to press"
end

let mount_fake () =
  Fake.log := [];
  Silica_browser.mount_with_renderer ~renderer:Fake.renderer
    (Dialog.component "trap")

let fake_press () = Fake.press ()
let fake_log () = Array.of_list (List.rev !Fake.log)
