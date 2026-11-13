open Silica

let simple ~id ~init step =
  Machine.make ~id ~init
    ~name:(fun _ -> "only")
    ~states:[ "only" ]
    (fun data event ->
      match step data event with
      | None ->
          Ignore
      | Some (data, actions) ->
          Go_with (data, actions)
    )

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
  | Answer of (int, string) Remote.answer
  | Focused
  | Blurred

type state = {
  count : int;
  doubles : int;
  typed : string;
  submits : int;
  items : string list;
  ticks : int;
  ticked : bool;
  answered : bool;
  answer : string;
  focus : string;
  asking : int;
}

type phase = Idle of state | Asking of state | Hurrying of state

let phase_name = function
  | Idle _ ->
      "idle"
  | Asking _ ->
      "asking"
  | Hurrying _ ->
      "hurrying"

let phase_states = [ "idle"; "asking"; "hurrying" ]

let double_ep =
  Remote.declare ~path:"/api/double" ~input:Codec.int ~output:Codec.int
    ~rejected:Codec.string

let input_ref = Ref.make "name"
let el = Node.element
let txt = Node.text
let id v = Attr.string "id" v
let data_of = function Idle s | Asking s | Hurrying s -> s

let with_data phase data =
  match phase with
  | Idle _ ->
      Idle data
  | Asking _ ->
      Asking data
  | Hurrying _ ->
      Hurrying data

let answer_text : (int, string) Remote.answer -> string = function
  | Ok n ->
      "ok " ^ string_of_int n
  | Rejected reason ->
      "rejected " ^ reason
  | Failed (Http code) ->
      "http " ^ string_of_int code
  | Failed (Decode _) ->
      "decode"
  | Failed (Network "timeout") ->
      "timeout"
  | Failed (Network _) ->
      "network"

let machine =
  Machine.make ~id:"app"
    ~init:
      (Idle
         {
           count = 0;
           doubles = 0;
           typed = "";
           submits = 0;
           items = [ "a"; "b"; "c" ];
           ticks = 0;
           ticked = false;
           answered = false;
           answer = "";
           focus = "none";
           asking = 0;
         }
      )
    ~name:phase_name ~states:phase_states
    ~during:(function
      | Idle _ ->
          []
      | Asking s ->
          ( if s.ticked then
              []
            else
              [ Source.after 5 Tick ]
          )
          @
          if s.answered then
            []
          else
            [ Remote.call double_ep s.asking (fun answer -> Answer answer) ]
      | Hurrying s ->
          if s.answered then
            []
          else
            [
              Remote.call ~timeout_ms:40 double_ep s.asking (fun answer ->
                  Answer answer
              );
            ]
      )
    (fun phase event ->
      let s = data_of phase in
      let stay data = Machine.Go (with_data phase data) in
      match event with
      | Inc ->
          stay { s with count = s.count + 1 }
      | Double ->
          stay { s with doubles = s.doubles + 1 }
      | Typed typed ->
          stay { s with typed }
      | Submit ->
          stay { s with submits = s.submits + 1 }
      | Focus_input ->
          Stay [ Action.Focus input_ref ]
      | Reverse ->
          stay { s with items = List.rev s.items }
      | Tick ->
          stay { s with ticks = s.ticks + 1; ticked = true }
      | Ask asking ->
          Reenter (Asking { s with asking; ticked = false; answered = false })
      | Ask_timed asking ->
          Reenter (Hurrying { s with asking; answered = false })
      | Cancel_ask ->
          Go (Idle s)
      | Answer answer ->
          stay { s with answer = answer_text answer; answered = true }
      | Focused ->
          stay { s with focus = "in" }
      | Blurred ->
          stay { s with focus = "out" }
    )

let view phase =
  let s = data_of phase in
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

let component = Component.make machine view

module Pages = struct
  type t =
    | Home
    | About
    | Item of string
    | Data of string
    | Edit
    | Locked
    | Not_found of string
end

let data_ep =
  Remote.declare ~path:"/api/data" ~input:Codec.string ~output:Codec.string
    ~rejected:Codec.string

let routes =
  Router.routes
    ~not_found:(function Pages.Not_found _ -> true | _ -> false)
    ~preload:(function
      | Pages.Data name -> [ Remote.request data_ep name ] | _ -> []
      )
    [
      Router.route (Route.s "home")
        (fun () -> Pages.Home)
        (function Pages.Home -> Some () | _ -> None);
      Router.route (Route.s "about")
        (fun () -> Pages.About)
        (function Pages.About -> Some () | _ -> None);
      Router.route
        Route.(s "item" / string)
        (fun name -> Pages.Item name)
        (function Pages.Item name -> Some name | _ -> None);
      Router.route
        Route.(s "data" / string)
        (fun name -> Pages.Data name)
        (function Pages.Data name -> Some name | _ -> None);
      Router.route (Route.s "edit")
        (fun () -> Pages.Edit)
        (function Pages.Edit -> Some () | _ -> None);
      Router.route (Route.s "locked")
        (fun () -> Pages.Locked)
        (function Pages.Locked -> Some () | _ -> None);
      Router.route Route.rest
        (fun path -> Pages.Not_found path)
        (function Pages.Not_found path -> Some path | _ -> None);
    ]

let title_component =
  Component.make
    (Machine.make_with_props ~id:"title" ~init:()
       ~name:(fun () -> "only")
       ~states:[ "only" ]
       ~on_props:(fun _ title () -> Stay [ Action.Set_title title ])
       (fun _ () () -> Ignore)
    )
    (fun () -> Node.list [])

type nav_event = Go_about | Go_hash

let nav_component =
  Component.make
    (simple ~id:"nav" ~init:() (fun () -> function
      | Go_about ->
          Some ((), [ Router.navigate routes Pages.About ])
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

type data_event = Loaded of (string, string) Remote.answer
type data_state = Loading | Shown of string

let data_state_name = function Loading -> "loading" | Shown _ -> "shown"

let data_state_states = [ "loading"; "shown" ]

let data_component =
  Component.make
    (Machine.make_with_props ~id:"data" ~init:Loading ~name:data_state_name
       ~states:data_state_states
       ~during:(fun name -> function
         | Loading ->
             [ Remote.call data_ep name (fun answer -> Loaded answer) ]
         | Shown _ ->
             []
         )
       ~on_props:(fun previous name _ ->
         match previous with
         | Some previous when previous <> name ->
             Go Loading
         | Some _ | None ->
             Ignore
       )
       (fun _ state (Loaded answer) ->
         match (state, answer) with
         | Loading, Ok text ->
             Go (Shown text)
         | Loading, (Rejected _ | Failed _) ->
             Go (Shown "error")
         | Shown _, _ ->
             Ignore
       )
    )
    (function
      | Loading ->
          el "p" [ id "data" ] [ txt "loading" ]
      | Shown text ->
          el "p" [ id "data" ] [ txt text ]
      )

type edit_event =
  | Draft of string
  | Attempt of Nav.t
  | Leave
  | Stay
  | Save
  | Go_home

type edit_state = Clean | Dirty of string | Asking of string * Nav.t

let edit_state_name = function
  | Clean ->
      "clean"
  | Dirty _ ->
      "dirty"
  | Asking _ ->
      "asking"

let edit_state_states = [ "clean"; "dirty"; "asking" ]

let nav_text = function
  | Nav.Push url ->
      "push " ^ Url.to_string url
  | Nav.Replace url ->
      "replace " ^ Url.to_string url
  | Nav.Traverse { delta; url } ->
      Printf.sprintf "traverse %d %s" delta (Url.to_string url)

let edit_component =
  let home = Router.navigate routes Pages.Home in
  Component.make
    (Machine.make ~id:"edit" ~init:Clean ~name:edit_state_name
       ~states:edit_state_states
       ~during:(function
         | Clean ->
             []
         | Dirty _ | Asking _ ->
             [ Source.block (fun nav -> Attempt nav) ]
         )
       (fun state event ->
         match (state, event) with
         | (Clean | Dirty _), Draft text ->
             Go (Dirty text)
         | (Clean | Dirty _), Go_home ->
             Stay [ home ]
         | Dirty draft, Attempt nav ->
             Go (Asking (draft, nav))
         | Dirty _, Save ->
             Go Clean
         | Asking (_, nav), Leave ->
             Go_with (Clean, [ Action.Resume nav ])
         | Asking (draft, _), Stay ->
             Go (Dirty draft)
         | Asking (draft, _), Attempt next ->
             Go (Asking (draft, next))
         | _ ->
             Ignore
       )
    )
    (fun state ->
      let status =
        match state with
        | Clean ->
            "clean"
        | Dirty _ ->
            "dirty"
        | Asking (_, nav) ->
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

let render_page current =
  let page title body =
    Node.list [ Component.node_with_props title_component ~props:title; body ]
  in
  match current with
  | Pages.Home ->
      page "Home"
        (el "div" []
           [
             el "h1" [] [ txt "home" ];
             Router.link ~current routes Pages.About
               [ id "to-about" ]
               [ txt "about" ];
             Router.link routes (Pages.Item "7") [ id "to-item" ] [ txt "item" ];
             Router.link_replace routes Pages.About
               [ id "to-about-replace" ]
               [ txt "about, replace" ];
             Router.link routes (Pages.Data "7") [ id "to-data" ] [ txt "data" ];
             Router.link ~preload:Viewport routes (Pages.Data "9")
               [ id "to-data-viewport" ]
               [ txt "data, viewport" ];
             Router.link routes Pages.Edit [ id "to-edit" ] [ txt "edit" ];
             Router.link routes Pages.Locked [ id "to-locked" ] [ txt "locked" ];
             Router.link routes Pages.Home
               [ id "to-home"; Attr.string "download" "home.html" ]
               [ txt "download" ];
             Component.node nav_component;
           ]
        )
  | Pages.About ->
      page "About"
        (el "div" []
           [
             el "h1"
               [ Attr.bool "data-silica-route-focus" true ]
               [ txt "about" ];
             el "div" [ id "section" ] [ txt "section" ];
           ]
        )
  | Pages.Item name ->
      page ("Item " ^ name) (el "div" [] [ el "h1" [] [ txt ("item " ^ name) ] ])
  | Pages.Data name ->
      page ("Data " ^ name)
        (el "div" []
           [
             el "h1" [] [ txt ("data " ^ name) ];
             Component.node_with_props data_component ~props:name;
           ]
        )
  | Pages.Edit ->
      page "Edit"
        (el "div" []
           [
             el "h1" [] [ txt "edit" ];
             Router.link routes Pages.About [ id "edit-about" ] [ txt "about" ];
             Component.node edit_component;
           ]
        )
  | Pages.Locked ->
      page "Locked"
        (el "div" []
           [
             el "h1" [] [ txt "locked" ];
             Router.link routes Pages.Home [ id "locked-home" ] [ txt "home" ];
           ]
        )
  | Pages.Not_found _ ->
      page "Not found" (el "h1" [] [ txt "404" ])

let guard ~from ~target =
  match (from, target) with
  | Some _, Pages.Locked ->
      Router.Cancel
  | _ ->
      Router.Allow

let app url = Router.make routes ~guard ~render:render_page url

let live =
  Live.declare ~channel:"counter" ~state:Codec.int ~event:Codec.string
    (Component.make
       (simple ~id:"live" ~init:0 (fun count _ -> Some (count, [])))
       (fun count ->
         el "div" []
           [
             el "p" [ id "n" ] [ txt (string_of_int count) ];
             el "button"
               [ id "plus"; Attr.ref_ (Ref.make "plus"); Attr.on_press "inc" ]
               [ txt "+" ];
           ]
       )
    )

let gone_live = ref 0
let gone_live_count () = !gone_live
let news = Topic.declare ~name:"news" ~state:Codec.int

type watch_event = Update of int Topic.update | Leave | Return
type watch = { text : string; updates : int }
type watch_state = Watching of watch | Away of watch

let watch_state_name = function Watching _ -> "watching" | Away _ -> "away"

let watch_state_states = [ "watching"; "away" ]

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

let watch_data = function Watching watch | Away watch -> watch

let watch_machine =
  Machine.make ~id:"watch"
    ~init:(Watching { text = "none"; updates = 0 })
    ~name:watch_state_name ~states:watch_state_states
    ~during:(function
      | Watching _ ->
          [ Topic.subscribe news "room" (fun update -> Update update) ]
      | Away _ ->
          []
      )
    (fun state event ->
      let watch = watch_data state in
      match event with
      | Update update ->
          let watch = { text = describe update; updates = watch.updates + 1 } in
          Go
            ( match state with
            | Watching _ ->
                Watching watch
            | Away _ ->
                Away watch
            )
      | Leave ->
          Go (Away watch)
      | Return ->
          Go (Watching watch)
    )

let watch_view state =
  let watch = watch_data state in
  el "main" []
    [
      el "p" [ id "status" ] [ txt watch.text ];
      el "p" [ id "updates" ] [ txt (string_of_int watch.updates) ];
      el "button" [ id "leave"; Attr.on_press Leave ] [ txt "leave" ];
      el "button" [ id "return"; Attr.on_press Return ] [ txt "return" ];
    ]

let watch_component = Component.make watch_machine watch_view
let gone = ref 0
let gone_count () = !gone

type host_event = Bump | Toggle

let host_component =
  Component.make
    (simple ~id:"host" ~init:(0, true) (fun (n, show) -> function
      | Bump ->
          Some ((n + 1, show), [])
      | Toggle ->
          Some ((n, not show), [])
      ))
    (fun (n, show) ->
      el "main" []
        ([
           el "p" [ id "bumps" ] [ txt (string_of_int n) ];
           el "button" [ id "bump"; Attr.on_press Bump ] [ txt "bump" ];
           el "button" [ id "toggle"; Attr.on_press Toggle ] [ txt "toggle" ];
         ]
        @
        if show then
          [ el "section" [ id "wrap"; Attr.on_press Bump ] [ Live.slot live ] ]
        else
          []
        )
    )

type timer_event = Leave | Back | Tick_timer

type timer_state = Waiting | Left | Fired | Late

let timer_state_name = function
  | Waiting ->
      "waiting"
  | Left ->
      "left"
  | Fired ->
      "fired"
  | Late ->
      "late"

let timer_state_states = [ "waiting"; "left"; "fired"; "late" ]

let timer_component =
  Component.make
    (Machine.make ~id:"timer" ~init:Waiting ~name:timer_state_name
       ~states:timer_state_states
       ~final:(function Fired | Late -> true | Waiting | Left -> false)
       ~during:(function
         | Waiting -> [ Source.after 20 Tick_timer ] | Left | Fired | Late -> []
         )
       (fun state event ->
         match (state, event) with
         | Waiting, Leave ->
             Go Left
         | Waiting, Tick_timer ->
             Go Fired
         | Left, Back ->
             Go Waiting
         | Left, Tick_timer ->
             Go Late
         | _ ->
             Ignore
       )
    )
    (fun state ->
      el "main" []
        [
          el "p" [ id "phase" ] [ txt (timer_state_name state) ];
          el "button" [ id "leave"; Attr.on_press Leave ] [ txt "leave" ];
          el "button" [ id "back"; Attr.on_press Back ] [ txt "back" ];
        ]
    )

type child_event = Child_press

let child_component =
  Component.make_with_props
    (Machine.make_with_props ~id:"child" ~init:0
       ~name:(fun _ -> "only")
       ~states:[ "only" ]
       (fun _ clicks Child_press -> Go (clicks + 1))
    )
    (fun prop clicks ->
      el "div"
        [
          Attr.string "class" "child"; Attr.string "data-n" (string_of_int prop);
        ]
        [
          el "span"
            [ Attr.string "class" "child-clicks" ]
            [ txt (string_of_int clicks) ];
          el "span"
            [ Attr.string "class" "child-prop" ]
            [ txt (string_of_int prop) ];
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
              Component.node_with_props child_component ~props:s.prop_value
                ~output:(fun Child_press clicks -> [ Heard clicks ]
              );
            ];
          el "section"
            [ id "kids" ]
            (List.map
               (fun n ->
                 Component.node_with_props ~key:(string_of_int n) ~props:n
                   child_component
               )
               s.order
            );
        ]
    )

type ticker_event = Tick_child
type ticker_state = Running of int

let ticker_state_name = function Running _ -> "running"

let ticker_state_states = [ "running" ]

let ticker_component =
  Component.make_with_props
    (Machine.make_with_props ~id:"ticker" ~init:(Running 0)
       ~name:ticker_state_name ~states:ticker_state_states
       ~during:(fun _ (Running _) -> [ Source.after 20 Tick_child ])
       (fun _ (Running count) Tick_child -> Reenter (Running (count + 1)))
    )
    (fun label (Running count) ->
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
        Component.node_with_props ~key:label ~props:label ticker_component
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
  type event = Again | Answered of (int, string) Remote.answer

  type state = Loading of int | Ready of int | Failed of int

  let name = function
    | Loading _ ->
        "loading"
    | Ready _ ->
        "ready"
    | Failed _ ->
        "failed"

  let states = [ "loading"; "ready"; "failed" ]

  let machine =
    Machine.make ~id:"lookup" ~init:(Loading 4) ~name ~states
      ~during:(function
        | Loading n ->
            [ Remote.call double_ep n (fun answer -> Answered answer) ]
        | Ready _ | Failed _ ->
            []
        )
      (fun state event ->
        match (state, event) with
        | Loading _, Answered (Ok n) ->
            Go (Ready n)
        | Loading input, Answered (Rejected _ | Failed _) ->
            Go (Failed input)
        | (Ready n | Failed n), Again ->
            Reenter (Loading n)
        | _ ->
            Ignore
      )

  let view state : event Node.t =
    el "main" []
      [
        el "p"
          [ id "answer" ]
          [
            txt
              ( match state with
              | Loading _ ->
                  "loading"
              | Ready n ->
                  "ready " ^ string_of_int n
              | Failed _ ->
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
  type state = Live of int

  let name = function Live _ -> "live"

  let states = [ "live" ]

  let machine =
    Machine.make ~id:"bomb" ~init:(Live 0) ~name ~states
      (fun (Live count) -> function
      | Inc ->
          Go (Live (count + 1))
      | Boom ->
          failwith "secret boom"
      | Break_view ->
          Go (Live (-1))
      | Quiet ->
          Ignore
      )

  let view (Live count) : event Node.t =
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

  let view ~guarded (bumps, ()) =
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
    Component.make
      (simple ~id:"shell" ~init:(0, ()) (fun (bumps, ()) -> function
        | Bump ->
            Some ((bumps + 1, ()), [])
        | Child _ ->
            None
        ))
      (view ~guarded)
end
