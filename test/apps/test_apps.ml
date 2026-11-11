open Silica

let simple (type s e) ~id ~(init : s)
    (step : s -> e -> (s * e Action.t list) option) : (s, e) Machine.t =
  let module Only = struct
    type _ t = Only : s t
  end in
  let module Only_machine = Machine.Make (Only) in
  let table =
    Only_machine.make ~id
      ~init:(Only_machine.at Only.Only init)
      [
        Only_machine.state Only.Only "only" (fun data event ->
            match step data event with
            | None ->
                Only_machine.unhandled
            | Some (data, actions) ->
                Only_machine.goto ~actions Only.Only data
        );
      ]
  in
  Machine.map_snapshot table
    ~get:(fun (Only_machine.At (Only.Only, data) : Only_machine.snapshot) : s ->
      data
    )
    ~set:(fun data -> Only_machine.at Only.Only data)

type event =
  | Inc
  | Double
  | Typed of string
  | Submit
  | Focus_input
  | Reverse
  | Tick
  | Ask of int
  | Ask_timed of int
  | Cancel_ask
  | Answer of (int, Remote.error) result
  | Focused
  | Blurred

type state = {
  count : int;
  doubles : int;
  typed : string;
  submits : int;
  items : string list;
  ticks : int;
  answer : string;
  focus : string;
  asking : int;
}

let double_ep =
  Remote.declare ~path:"/api/double" ~input:Codec.int ~output:Codec.int

let input_ref = Ref.make "name"
let el = Node.element
let txt = Node.text
let id v = Attr.string "id" v

module Phase = struct
  type _ t = Idle : state t | Asking : state t | Hurrying : state t
end

module Phases = Machine.Make (Phase)

let machine =
  let handle self s = function
    | Inc ->
        Phases.goto self { s with count = s.count + 1 }
    | Double ->
        Phases.goto self { s with doubles = s.doubles + 1 }
    | Typed t ->
        Phases.goto self { s with typed = t }
    | Submit ->
        Phases.goto self { s with submits = s.submits + 1 }
    | Focus_input ->
        Phases.stay [ Action.Focus input_ref ]
    | Reverse ->
        Phases.goto self { s with items = List.rev s.items }
    | Tick ->
        Phases.goto self { s with ticks = s.ticks + 1 }
    | Ask n ->
        Phases.reenter Phase.Asking { s with asking = n }
    | Ask_timed n ->
        Phases.reenter Phase.Hurrying { s with asking = n }
    | Cancel_ask ->
        Phases.goto Phase.Idle s
    | Answer result ->
        let answer =
          match result with
          | Ok n ->
              "ok " ^ string_of_int n
          | Error (Remote.Http c) ->
              "http " ^ string_of_int c
          | Error (Remote.Decode _) ->
              "decode"
          | Error (Remote.Network "timeout") ->
              "timeout"
          | Error (Remote.Network _) ->
              "network"
        in
        Phases.goto self { s with answer }
    | Focused ->
        Phases.goto self { s with focus = "in" }
    | Blurred ->
        Phases.goto self { s with focus = "out" }
  in
  let to_ = Phases.[ Phase.Idle; Phase.Asking; Phase.Hurrying ] in
  Phases.make ~id:"app"
    ~init:
      (Phases.at Phase.Idle
         {
           count = 0;
           doubles = 0;
           typed = "";
           submits = 0;
           items = [ "a"; "b"; "c" ];
           ticks = 0;
           answer = "";
           focus = "none";
           asking = 0;
         }
      )
    [
      Phases.state Phase.Idle "idle" ~to_ (handle Phase.Idle);
      Phases.state Phase.Asking "asking" ~to_
        ~after:[ (5, Tick) ]
        ~invoke:
          [ Source.call double_ep (fun s -> s.asking) (fun r -> Answer r) ]
        (handle Phase.Asking);
      Phases.state Phase.Hurrying "hurrying" ~to_
        ~invoke:
          [
            Source.call ~timeout_ms:40 double_ep
              (fun s -> s.asking)
              (fun r -> Answer r);
          ]
        (handle Phase.Hurrying);
    ]

let state_of : Phases.snapshot -> state = function
  | Phases.At (Phase.Idle, s) ->
      s
  | Phases.At (Phase.Asking, s) ->
      s
  | Phases.At (Phase.Hurrying, s) ->
      s

let view snapshot =
  let s = state_of snapshot in
  el "main" []
    [
      el "p" [ id "count" ] [ txt (string_of_int s.count) ];
      el "p" [ id "doubles" ] [ txt (string_of_int s.doubles) ];
      el "p" [ id "typed" ] [ txt s.typed ];
      el "p" [ id "submits" ] [ txt (string_of_int s.submits) ];
      el "p" [ id "ticks" ] [ txt (string_of_int s.ticks) ];
      el "p" [ id "answer" ] [ txt s.answer ];
      el "p" [ id "focus" ] [ txt s.focus ];
      el "button" [ id "inc"; Attr.on_press Inc ] [ txt "inc" ];
      el "div"
        [ id "dbl"; Attr.on_double_press Double ]
        [ el "span" [ id "dbl-child" ] [ txt "double" ] ];
      el "div"
        [ id "fake"; Attr.string "tabindex" "0"; Attr.on_press Inc ]
        [ txt "fake button" ];
      el "form"
        [ id "form"; Attr.on_submit Submit ]
        [
          el "input"
            [
              id "input";
              Attr.ref_ input_ref;
              Attr.string "value" s.typed;
              Attr.on_input (fun v -> Typed v);
              Attr.on_focus Focused;
              Attr.on_blur Blurred;
            ]
            [];
        ];
      el "button" [ id "focus-btn"; Attr.on_press Focus_input ] [ txt "focus" ];
      el "button" [ id "reverse"; Attr.on_press Reverse ] [ txt "reverse" ];
      el "button" [ id "ask"; Attr.on_press (Ask 21) ] [ txt "ask" ];
      el "button"
        [ id "ask-timed"; Attr.on_press (Ask_timed 21) ]
        [ txt "ask timed" ];
      el "button" [ id "cancel-ask"; Attr.on_press Cancel_ask ] [ txt "cancel" ];
      el "ul"
        [ id "list" ]
        (List.map
           (fun i -> Node.keyed i (el "li" [ Attr.string "data-k" i ] [ txt i ]))
           s.items
        );
    ]

let component = { Component.machine; view }

module Pages = struct
  type _ t =
    | Home : unit t
    | About : unit t
    | Item : string t
    | Data : string t
    | Edit : unit t
    | Locked : unit t
    | Not_found : string t

  let path : type data. data t -> data Route.t = function
    | Home ->
        Route.s "home"
    | About ->
        Route.s "about"
    | Item ->
        Route.(s "item" / string)
    | Data ->
        Route.(s "data" / string)
    | Edit ->
        Route.s "edit"
    | Locked ->
        Route.s "locked"
    | Not_found ->
        Route.rest
end

module Pages_router = Router.Make (Pages)

let data_ep =
  Remote.declare ~path:"/api/data" ~input:Codec.string ~output:Codec.string

let routes =
  Pages_router.routes ~not_found:Pages.Not_found
    ~preload:(function
      | Pages_router.At (Pages.Data, name) ->
          [ Remote.request data_ep name ]
      | _ ->
          []
      )
    Pages.[ Home; About; Item; Data; Edit; Locked; Not_found ]

type title_event = Title of string

let title_component =
  Component.make
    (simple ~id:"title" ~init:() (fun () (Title text) ->
         Some ((), [ Action.Set_title text ])
     )
    )
    (fun () -> Node.list [])

type nav_event = Go_about | Go_hash

let nav_component =
  Component.make
    (simple ~id:"nav" ~init:() (fun () -> function
      | Go_about ->
          Some ((), [ Pages_router.navigate routes Pages.About () ])
      | Go_hash ->
          Some ((), [ Action.Navigate (Url.of_string "/about#section") ])
      ))
    (fun () ->
      Node.list
        [
          el "button" [ id "action-nav"; Attr.on_press Go_about ] [ txt "go" ];
          el "button" [ id "action-hash"; Attr.on_press Go_hash ] [ txt "hash" ];
        ]
    )

type data_event =
  | Data_props of string
  | Loaded of (string, Remote.error) result

module Data_state = struct
  type _ t = Idle : unit t | Loading : string t | Shown : (string * string) t
end

module Data_machine = Machine.Make (Data_state)

let data_component =
  let open Data_machine in
  Component.make
    (make ~id:"data" ~init:(at Data_state.Idle ())
       [
         state Data_state.Idle "idle" ~to_:[ Data_state.Loading ]
           (fun () -> function
           | Data_props name ->
               goto Data_state.Loading name
           | Loaded _ ->
               unhandled
           );
         state Data_state.Loading "loading" ~to_:[ Data_state.Shown ]
           ~invoke:[ Source.call data_ep Fun.id (fun r -> Loaded r) ]
           (fun name -> function
             | Data_props next ->
                 reenter Data_state.Loading next
             | Loaded (Ok text) ->
                 goto Data_state.Shown (name, text)
             | Loaded (Error _) ->
                 goto Data_state.Shown (name, "error")
             );
         state Data_state.Shown "shown" ~to_:[ Data_state.Loading ]
           (fun (name, _) -> function
           | Data_props next when next <> name ->
               goto Data_state.Loading next
           | _ ->
               unhandled
           );
       ]
    )
    (function
      | At ((Data_state.Idle | Data_state.Loading), _) ->
          el "p" [ id "data" ] [ txt "loading" ]
      | At (Data_state.Shown, (_, text)) ->
          el "p" [ id "data" ] [ txt text ]
      )

type edit_event =
  | Draft of string
  | Attempt of Nav.t
  | Leave
  | Stay
  | Save
  | Go_home

module Edit_state = struct
  type _ t = Clean : unit t | Dirty : string t | Asking : (string * Nav.t) t
end

module Edit_machine = Machine.Make (Edit_state)

let nav_text = function
  | Nav.Push url ->
      "push " ^ Url.to_string url
  | Nav.Replace url ->
      "replace " ^ Url.to_string url
  | Nav.Traverse { delta; url } ->
      Printf.sprintf "traverse %d %s" delta (Url.to_string url)

let edit_component =
  let open Edit_machine in
  let block () = Source.block (fun nav -> Attempt nav) in
  let home = Pages_router.navigate routes Pages.Home () in
  Component.make
    (make ~id:"edit" ~init:(at Edit_state.Clean ())
       [
         state Edit_state.Clean "clean" ~to_:[ Edit_state.Dirty ]
           (fun () -> function
           | Draft text ->
               goto Edit_state.Dirty text
           | Go_home ->
               stay [ home ]
           | _ ->
               unhandled
           );
         state Edit_state.Dirty "dirty"
           ~to_:[ Edit_state.Asking; Edit_state.Clean ]
           ~invoke:[ block () ]
           (fun draft -> function
             | Draft text ->
                 goto Edit_state.Dirty text
             | Attempt nav ->
                 goto Edit_state.Asking (draft, nav)
             | Save ->
                 goto Edit_state.Clean ()
             | Go_home ->
                 stay [ home ]
             | _ ->
                 unhandled
             );
         state Edit_state.Asking "asking"
           ~to_:[ Edit_state.Dirty; Edit_state.Clean ]
           ~invoke:[ block () ]
           (fun (draft, nav) -> function
             | Leave ->
                 goto ~actions:[ Action.Resume nav ] Edit_state.Clean ()
             | Stay ->
                 goto Edit_state.Dirty draft
             | Attempt next ->
                 goto Edit_state.Asking (draft, next)
             | _ ->
                 unhandled
             );
       ]
    )
    (fun snapshot ->
      let status =
        match snapshot with
        | At (Edit_state.Clean, ()) ->
            "clean"
        | At (Edit_state.Dirty, _) ->
            "dirty"
        | At (Edit_state.Asking, (_, nav)) ->
            "asking " ^ nav_text nav
      in
      Node.list
        [
          el "input" [ id "draft"; Attr.on_input (fun text -> Draft text) ] [];
          el "p" [ id "edit-status" ] [ txt status ];
          el "button" [ id "save"; Attr.on_press Save ] [ txt "save" ];
          el "button" [ id "edit-home"; Attr.on_press Go_home ] [ txt "home" ];
          el "button" [ id "leave"; Attr.on_press Leave ] [ txt "leave" ];
          el "button" [ id "stay"; Attr.on_press Stay ] [ txt "stay" ];
        ]
    )

let render_page (Pages_router.At (state, data) as current) =
  let page title body =
    Node.list [ Component.node title_component ~props:(Title title); body ]
  in
  match (state, data) with
  | Pages.Home, () ->
      page "Home"
        (el "div" []
           [
             el "h1" [] [ txt "home" ];
             Pages_router.link ~current routes Pages.About ()
               [ id "to-about" ]
               [ txt "about" ];
             Pages_router.link routes Pages.Item "7"
               [ id "to-item" ]
               [ txt "item" ];
             Pages_router.link_replace routes Pages.About ()
               [ id "to-about-replace" ]
               [ txt "about, replace" ];
             Pages_router.link routes Pages.Data "7"
               [ id "to-data" ]
               [ txt "data" ];
             Pages_router.link ~preload:Viewport routes Pages.Data "9"
               [ id "to-data-viewport" ]
               [ txt "data, viewport" ];
             Pages_router.link routes Pages.Edit ()
               [ id "to-edit" ]
               [ txt "edit" ];
             Pages_router.link routes Pages.Locked ()
               [ id "to-locked" ]
               [ txt "locked" ];
             Pages_router.link routes Pages.Home ()
               [ id "to-home"; Attr.string "download" "home.html" ]
               [ txt "download" ];
             Component.node nav_component;
           ]
        )
  | Pages.About, () ->
      page "About"
        (el "div" []
           [
             el "h1"
               [ Attr.bool "data-silica-route-focus" true ]
               [ txt "about" ];
             el "div" [ id "section" ] [ txt "section" ];
           ]
        )
  | Pages.Item, name ->
      page ("Item " ^ name) (el "div" [] [ el "h1" [] [ txt ("item " ^ name) ] ])
  | Pages.Data, name ->
      page ("Data " ^ name)
        (el "div" []
           [
             el "h1" [] [ txt ("data " ^ name) ];
             Component.node data_component ~props:(Data_props name);
           ]
        )
  | Pages.Edit, () ->
      page "Edit"
        (el "div" []
           [
             el "h1" [] [ txt "edit" ];
             Pages_router.link routes Pages.About ()
               [ id "edit-about" ]
               [ txt "about" ];
             Component.node edit_component;
           ]
        )
  | Pages.Locked, () ->
      page "Locked"
        (el "div" []
           [
             el "h1" [] [ txt "locked" ];
             Pages_router.link routes Pages.Home ()
               [ id "locked-home" ]
               [ txt "home" ];
           ]
        )
  | Pages.Not_found, _ ->
      page "Not found" (el "h1" [] [ txt "404" ])

let guard ~from ~target =
  match (from, target) with
  | Some _, Pages_router.At (Pages.Locked, ()) ->
      Pages_router.Cancel
  | _ ->
      Pages_router.Allow

let app url = Pages_router.make routes ~guard ~render:render_page url

let live =
  Live.declare ~channel:"counter" ~state:Codec.int ~event:Codec.string
    {
      Component.machine = simple ~id:"live" ~init:0 (fun s _ -> Some (s, []));
      view =
        (fun n ->
          el "div" []
            [
              el "p" [ id "n" ] [ txt (string_of_int n) ];
              el "button"
                [ id "plus"; Attr.ref_ (Ref.make "plus"); Attr.on_press "inc" ]
                [ txt "+" ];
            ]
        );
    }

let gone_live = ref 0
let gone_live_count () = !gone_live

let news = Topic.declare ~name:"news" ~state:Codec.int

type watch_event = Update of int Topic.update | Leave | Return
type watch = { text : string; updates : int }

module Watch_state = struct
  type _ t = Watching : watch t | Away : watch t
end

module Watching_machine = Machine.Make (Watch_state)

let describe (update : int Topic.update) =
  let status =
    match update.status with
    | Status.Idle ->
        "idle"
    | Status.Loading None ->
        "loading"
    | Status.Loading (Some n) ->
        "loading " ^ string_of_int n
    | Status.Ready { data; stale } ->
        ( if stale then
            "stale "
          else
            "ready "
        )
        ^ string_of_int data
    | Status.Failed { error; last } -> (
        let error =
          match error with
          | Remote.Http code ->
              "http " ^ string_of_int code
          | Remote.Decode _ ->
              "decode"
          | Remote.Network code ->
              "network " ^ code
        in
        "failed " ^ error
        ^ match last with Some n -> " last " ^ string_of_int n | None -> ""
      )
  in
  status ^ " seq " ^ string_of_int update.seq

let watch_machine =
  let handle self s = function
    | Update update ->
        Watching_machine.goto self
          { text = describe update; updates = s.updates + 1 }
    | Leave ->
        Watching_machine.goto Watch_state.Away s
    | Return ->
        Watching_machine.goto Watch_state.Watching s
  in
  let to_ = Watching_machine.[ Watch_state.Watching; Watch_state.Away ] in
  Watching_machine.make ~id:"watch"
    ~init:
      (Watching_machine.at Watch_state.Watching { text = "none"; updates = 0 })
    [
      Watching_machine.state Watch_state.Watching "watching" ~to_
        ~invoke:[ Source.live news (fun _ -> "room") (fun u -> Update u) ]
        (handle Watch_state.Watching);
      Watching_machine.state Watch_state.Away "away" ~to_
        (handle Watch_state.Away);
    ]

let watch_state : Watching_machine.snapshot -> watch = function
  | Watching_machine.At (Watch_state.Watching, s) ->
      s
  | Watching_machine.At (Watch_state.Away, s) ->
      s

let watch_view snapshot =
  let s = watch_state snapshot in
  el "main" []
    [
      el "p" [ id "status" ] [ txt s.text ];
      el "p" [ id "updates" ] [ txt (string_of_int s.updates) ];
      el "button" [ id "leave"; Attr.on_press Leave ] [ txt "leave" ];
      el "button" [ id "return"; Attr.on_press Return ] [ txt "return" ];
    ]

let gone = ref 0
let gone_count () = !gone

type host_event = Bump | Toggle

let host_component =
  {
    Component.machine =
      simple ~id:"host" ~init:(0, true) (fun (n, show) -> function
        | Bump ->
            Some ((n + 1, show), [])
        | Toggle ->
            Some ((n, not show), [])
        );
    view =
      (fun (n, show) ->
        el "main" []
          ([
             el "p" [ id "bumps" ] [ txt (string_of_int n) ];
             el "button" [ id "bump"; Attr.on_press Bump ] [ txt "bump" ];
             el "button" [ id "toggle"; Attr.on_press Toggle ] [ txt "toggle" ];
           ]
          @
          if show then
            [
              el "section" [ id "wrap"; Attr.on_press Bump ] [ Live.slot live ];
            ]
          else
            []
          )
      );
  }

type timer_event = Leave | Back | Tick_timer
type timer_state = Waiting | Left | Fired | Late

module Timer_states = struct
  type _ t = Waiting : unit t | Left : unit t | Fired : unit t | Late : unit t
end

module Timer = Machine.Make (Timer_states)

let timer_component =
  let table =
    Timer.make ~id:"timer"
      ~init:(Timer.at Timer_states.Waiting ())
      [
        Timer.state Timer_states.Waiting "waiting"
          ~after:[ (20, Tick_timer) ]
          ~to_:[ Timer_states.Left; Timer_states.Fired ]
          (fun () -> function
            | Leave ->
                Timer.goto Timer_states.Left ()
            | Tick_timer ->
                Timer.goto Timer_states.Fired ()
            | Back ->
                Timer.unhandled
            );
        Timer.state Timer_states.Left "left"
          ~to_:[ Timer_states.Waiting; Timer_states.Late ] (fun () -> function
          | Back ->
              Timer.goto Timer_states.Waiting ()
          | Tick_timer ->
              Timer.goto Timer_states.Late ()
          | Leave ->
              Timer.unhandled
          );
        Timer.state Timer_states.Fired "fired" ~final:true Timer.ignore;
        Timer.state Timer_states.Late "late" ~final:true Timer.ignore;
      ]
  in
  {
    Component.machine =
      Machine.map_snapshot table
        ~get:(fun (Timer.At (state, _) : Timer.snapshot) : timer_state ->
          match state with
          | Timer_states.Waiting ->
              Waiting
          | Timer_states.Left ->
              Left
          | Timer_states.Fired ->
              Fired
          | Timer_states.Late ->
              Late
        )
        ~set:(function
          | Waiting ->
              Timer.at Timer_states.Waiting ()
          | Left ->
              Timer.at Timer_states.Left ()
          | Fired ->
              Timer.at Timer_states.Fired ()
          | Late ->
              Timer.at Timer_states.Late ()
          );
    view =
      (fun s ->
        el "main" []
          [
            el "p"
              [ id "phase" ]
              [
                txt
                  ( match s with
                  | Waiting ->
                      "waiting"
                  | Left ->
                      "left"
                  | Fired ->
                      "fired"
                  | Late ->
                      "late"
                  );
              ];
            el "button" [ id "leave"; Attr.on_press Leave ] [ txt "leave" ];
            el "button" [ id "back"; Attr.on_press Back ] [ txt "back" ];
          ]
      );
  }

type child_event = Child_props of int | Child_press
type child_state = { prop : int; clicks : int }

let child_component =
  Component.make
    (simple ~id:"child" ~init:{ prop = 0; clicks = 0 } (fun s -> function
      | Child_props prop ->
          Some ({ s with prop }, [])
      | Child_press ->
          Some ({ s with clicks = s.clicks + 1 }, [])
      ))
    (fun s ->
      el "div"
        [
          Attr.string "class" "child";
          Attr.string "data-n" (string_of_int s.prop);
        ]
        [
          el "span"
            [ Attr.string "class" "child-clicks" ]
            [ txt (string_of_int s.clicks) ];
          el "span"
            [ Attr.string "class" "child-prop" ]
            [ txt (string_of_int s.prop) ];
          el "button"
            [ Attr.string "class" "child-press"; Attr.on_press Child_press ]
            [ txt "press" ];
        ]
    )

type parent_event = Parent_press | Prop_up | Heard of int | Reverse_children
type parent_state = {
  presses : int;
  prop_value : int;
  heard : int;
  order : int list;
}

let parent_component =
  Component.make
    (simple ~id:"parent"
       ~init:{ presses = 0; prop_value = 0; heard = 0; order = [ 1; 2; 3 ] }
       (fun s -> function
         | Parent_press ->
             Some ({ s with presses = s.presses + 1 }, [])
         | Prop_up ->
             Some ({ s with prop_value = s.prop_value + 1 }, [])
         | Heard heard ->
             Some ({ s with heard }, [])
         | Reverse_children ->
             Some ({ s with order = List.rev s.order }, [])
         )
    )
    (fun s ->
      el "main" []
        [
          el "p" [ id "presses" ] [ txt (string_of_int s.presses) ];
          el "p" [ id "heard" ] [ txt (string_of_int s.heard) ];
          el "button"
            [ id "parent-press"; Attr.on_press Parent_press ]
            [ txt "p" ];
          el "button" [ id "prop-up"; Attr.on_press Prop_up ] [ txt "up" ];
          el "button"
            [ id "reverse-children"; Attr.on_press Reverse_children ]
            [ txt "reverse" ];
          el "section"
            [ id "single" ]
            [
              Component.node child_component ~props:(Child_props s.prop_value)
                ~output:(fun event child ->
                  match event with
                  | Child_press ->
                      [ Heard child.clicks ]
                  | Child_props _ ->
                      []
              );
            ];
          el "section"
            [ id "kids" ]
            (List.map
               (fun n ->
                 Component.node ~key:(string_of_int n) ~props:(Child_props n)
                   child_component
               )
               s.order
            );
        ]
    )

type ticker_event = Tick_child | Label of string

module Ticker_states = struct
  type _ t = Running : (string * int) t
end

module Ticker = Machine.Make (Ticker_states)

let ticker_component =
  Component.make
    (Ticker.make ~id:"ticker"
       ~init:(Ticker.at Ticker_states.Running ("", 0))
       [
         Ticker.state Ticker_states.Running "running"
           ~after:[ (20, Tick_child) ]
           (fun (label, count) -> function
             | Tick_child ->
                 Ticker.reenter Ticker_states.Running (label, count + 1)
             | Label label ->
                 Ticker.goto Ticker_states.Running (label, count)
             );
       ]
    |> Machine.map_snapshot
         ~get:(fun
             (Ticker.At (Ticker_states.Running, data) : Ticker.snapshot)
             :
             (string * int)
           -> data
         )
         ~set:(fun data -> Ticker.at Ticker_states.Running data)
    )
    (fun (label, count) ->
      el "span"
        [ Attr.string "class" "tick"; Attr.string "data-label" label ]
        [ txt (string_of_int count) ]
    )

type ticker_host_event = Toggle_a | Toggle_b

let ticker_host_component =
  Component.make
    (simple ~id:"ticker-host" ~init:(true, true) (fun (a, b) -> function
      | Toggle_a ->
          Some ((not a, b), [])
      | Toggle_b ->
          Some ((a, not b), [])
      ))
    (fun (a, b) ->
      let ticker label =
        Component.node ~key:label ~props:(Label label) ticker_component
      in
      el "main" []
        ([
           el "button" [ id "toggle-a"; Attr.on_press Toggle_a ] [ txt "a" ];
           el "button" [ id "toggle-b"; Attr.on_press Toggle_b ] [ txt "b" ];
         ]
        @ ( if a then
              [ ticker "a" ]
            else
              []
          )
        @
        if b then
          [ ticker "b" ]
        else
          []
        )
    )

module Lookup = struct
  type event = Again | Answered of (int, Remote.error) result

  module States = struct
    type _ t = Loading : int t | Ready : int t | Failed : int t
  end

  module Lookup_machine = Machine.Make (States)

  let machine =
    let to_ = Lookup_machine.[ States.Loading; States.Ready; States.Failed ] in
    let loading input =
      Lookup_machine.state States.Loading "loading" ~to_
        ~invoke:[ Source.call double_ep (fun n -> n) (fun r -> Answered r) ]
        (fun _ -> function
          | Answered (Ok n) ->
              Lookup_machine.goto States.Ready n
          | Answered (Error _) ->
              Lookup_machine.goto States.Failed input
          | Again ->
              Lookup_machine.unhandled
          )
    in
    Lookup_machine.make ~id:"lookup"
      ~init:(Lookup_machine.at States.Loading 4)
      [
        loading 4;
        Lookup_machine.state States.Ready "ready" ~to_ (fun n -> function
          | Again ->
              Lookup_machine.reenter States.Loading n
          | _ ->
              Lookup_machine.unhandled
          );
        Lookup_machine.state States.Failed "failed" ~to_ (fun n -> function
          | Again ->
              Lookup_machine.reenter States.Loading n
          | _ ->
              Lookup_machine.unhandled
          );
      ]

  let view (Lookup_machine.At (state, n) : Lookup_machine.snapshot) :
      event Node.t =
    el "main" []
      [
        el "p"
          [ id "answer" ]
          [
            txt
              ( match state with
              | States.Loading ->
                  "loading"
              | States.Ready ->
                  "ready " ^ string_of_int n
              | States.Failed ->
                  "failed"
              );
          ];
        el "button" [ id "again"; Attr.on_press Again ] [ txt "again" ];
      ]

  let component = Component.make machine view
end

let lookup_server () =
  let tree = Instance.mount Lookup.component in
  let node, actions = Instance.update tree None in
  let table = ref Answers.empty in
  let result = ref (Render.to_string node) in
  List.iter
    (function
      | Action.Start_call { endpoint; input; on_result; _ } -> (
          let encoded = Codec.to_string (Remote.input endpoint) input in
          let output = string_of_int (2 * int_of_string encoded) in
          match Codec.of_string (Remote.output endpoint) output with
          | Ok value ->
              table :=
                Answers.add !table ~path:(Remote.path endpoint) ~input:encoded
                  ~output;
              let node, _ =
                Instance.update tree (Some (on_result (Ok value)))
              in
              result := Render.to_string node
          | Error message ->
              failwith message
        )
      | _ ->
          ()
      )
    actions;
  (!result, !table)

let lookup_html () = fst (lookup_server ())
let lookup_answers () = snd (lookup_server ())

module Bomb = struct
  type event = Inc | Boom | Break_view | Quiet

  module States = struct
    type _ t = Live : int t
  end

  module Bomb_machine = Machine.Make (States)

  let machine =
    Bomb_machine.make ~id:"bomb"
      ~init:(Bomb_machine.at States.Live 0)
      [
        Bomb_machine.state States.Live "live" (fun count -> function
          | Inc ->
              Bomb_machine.goto States.Live (count + 1)
          | Boom ->
              failwith "secret boom"
          | Break_view ->
              Bomb_machine.goto States.Live (-1)
          | Quiet ->
              Bomb_machine.unhandled
          );
      ]

  let view (Bomb_machine.At (States.Live, count) : Bomb_machine.snapshot) :
      event Node.t =
    if count < 0 then failwith "secret view";
    el "div"
      [ id "bomb" ]
      [
        el "p" [ id "bomb-count" ] [ txt (string_of_int count) ];
        el "button" [ id "bomb-inc"; Attr.on_press Inc ] [ txt "inc" ];
        el "button" [ id "bomb-boom"; Attr.on_press Boom ] [ txt "boom" ];
        el "button"
          [ id "bomb-break"; Attr.on_press Break_view ]
          [ txt "break" ];
      ]

  let component = Component.make machine view
end

module Shell = struct
  type event = Bump | Child of Bomb.event

  let view ~guarded (bumps, _) =
    let child =
      Component.node ~output:(fun event _ -> [ Child event ]) Bomb.component
    in
    el "main" []
      [
        el "p" [ id "bumps" ] [ txt (string_of_int bumps) ];
        el "button" [ id "bump"; Attr.on_press Bump ] [ txt "bump" ];
        ( if guarded then
            Node.boundary
              ~fallback:(fun (error : Node.error) ~reset ->
                el "section"
                  [ id "fallback" ]
                  [
                    el "p"
                      [ id "fallback-text" ]
                      [
                        txt
                          (error.message ^ " "
                          ^ Option.value error.detail ~default:"no-detail"
                          );
                      ];
                    el "button" [ id "retry"; reset ] [ txt "retry" ];
                  ]
              )
              child
          else
            child
        );
      ]

  let component ~guarded =
    {
      Component.machine =
        simple ~id:"shell" ~init:(0, ()) (fun (n, ()) -> function
          | Bump ->
              Some ((n + 1, ()), [])
          | Child _ ->
              None
          );
      view = view ~guarded;
    }
end
