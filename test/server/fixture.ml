open Silica

type ev =
  | Inc
  | Focus_me
  | Mixed
  | Double
  | Boom
  | Ghost
  | Later
  | Set of int
  | Failed
  | Cancel_later
  | Cancel_double
  | Slow

let name = function
  | Inc ->
      "inc"
  | Focus_me ->
      "focus"
  | Mixed ->
      "mixed"
  | Double ->
      "double"
  | Boom ->
      "boom"
  | Ghost ->
      "ghost"
  | Later ->
      "later"
  | Failed ->
      "failed"
  | Cancel_later ->
      "cancel_later"
  | Cancel_double ->
      "cancel_double"
  | Slow ->
      "slow"
  | Set n ->
      "set:" ^ string_of_int n

let of_name = function
  | "inc" ->
      Ok Inc
  | "focus" ->
      Ok Focus_me
  | "mixed" ->
      Ok Mixed
  | "double" ->
      Ok Double
  | "boom" ->
      Ok Boom
  | "ghost" ->
      Ok Ghost
  | "later" ->
      Ok Later
  | "failed" ->
      Ok Failed
  | "cancel_later" ->
      Ok Cancel_later
  | "cancel_double" ->
      Ok Cancel_double
  | "slow" ->
      Ok Slow
  | s -> (
      match String.split_on_char ':' s with
      | [ "set"; n ] -> (
          match int_of_string_opt n with
          | Some n ->
              Ok (Set n)
          | None ->
              Error ("bad event " ^ s)
        )
      | _ ->
          Error ("bad event " ^ s)
    )

let ev_codec =
  Codec.make
    ~encode:(fun e -> Codec.string.encode (name e))
    ~decode:(fun j -> Result.bind (Codec.string.decode j) of_name)

let double =
  Remote.declare ~path:"/api/double" ~input:Codec.int ~output:Codec.int
    ~rejected:Codec.string

let boom =
  Remote.declare ~path:"/api/boom" ~input:Codec.int ~output:Codec.int
    ~rejected:Codec.string

let ghost =
  Remote.declare ~path:"/api/ghost" ~input:Codec.int ~output:Codec.int
    ~rejected:Codec.string

let x = Ref.make "x"
let settle = function
  | Remote.Ok v ->
      Set v
  | Remote.Rejected reason ->
      Set (-String.length reason)
  | Remote.Failed _ ->
      Failed

type phase =
  | Resting of int
  | Mixing of int
  | Doubling of int
  | Booming of int
  | Ghosting of int
  | Waiting of int
  | Slowing of int

let phase_name = function
  | Resting _ ->
      "resting"
  | Mixing _ ->
      "mixing"
  | Doubling _ ->
      "doubling"
  | Booming _ ->
      "booming"
  | Ghosting _ ->
      "ghosting"
  | Waiting _ ->
      "waiting"
  | Slowing _ ->
      "slowing"

let phase_states =
  [
    "resting"; "mixing"; "doubling"; "booming"; "ghosting"; "waiting"; "slowing";
  ]

let count = function
  | Resting n
  | Mixing n
  | Doubling n
  | Booming n
  | Ghosting n
  | Waiting n
  | Slowing n ->
      n

let with_count phase n =
  match phase with
  | Resting _ ->
      Resting n
  | Mixing _ ->
      Mixing n
  | Doubling _ ->
      Doubling n
  | Booming _ ->
      Booming n
  | Ghosting _ ->
      Ghosting n
  | Waiting _ ->
      Waiting n
  | Slowing _ ->
      Slowing n

let machine =
  Machine.make ~id:"counter" ~init:(Resting 0) ~name:phase_name
    ~states:phase_states
    ~during:(function
      | Resting _ ->
          []
      | Mixing n ->
          [ Source.after 10 Inc; Remote.call double n settle ]
      | Doubling n ->
          [ Remote.call double n settle ]
      | Booming n ->
          [ Remote.call boom n settle ]
      | Ghosting n ->
          [ Remote.call ghost n settle ]
      | Waiting _ ->
          [ Source.after 50 Inc ]
      | Slowing n ->
          [ Remote.call ~timeout_ms:20 double n settle ]
      )
    (fun phase event ->
      let n = count phase in
      match event with
      | Inc -> (
          match phase with
          | Waiting _ ->
              Machine.Go (Resting (n + 1))
          | Mixing _ ->
              Machine.Go (Doubling (n + 1))
          | Resting _ | Doubling _ | Booming _ | Ghosting _ | Slowing _ ->
              Machine.Go (with_count phase (n + 1))
        )
      | Focus_me ->
          Machine.Stay [ Action.Focus x ]
      | Mixed ->
          Machine.Go_with
            ( Mixing n,
              [ Action.Focus x; Action.Navigate (Url.of_string "/a?b=c") ]
            )
      | Double ->
          Machine.Go (Doubling n)
      | Boom ->
          Machine.Go (Booming n)
      | Ghost ->
          Machine.Go (Ghosting n)
      | Slow ->
          Machine.Go (Slowing n)
      | Later ->
          Machine.Reenter (Waiting n)
      | Cancel_later | Cancel_double ->
          Machine.Go (Resting n)
      | Set v ->
          Machine.Go (Resting v)
      | Failed ->
          Machine.Go (Resting (-1))
    )

let counter_view n =
  Node.element "p" [] [ Node.text ("count <" ^ string_of_int n ^ ">") ]

let component = Component.make machine (fun phase -> counter_view (count phase))

let state_codec : phase Codec.t =
  Codec.make
    ~encode:(fun phase -> Codec.int.encode (count phase))
    ~decode:(fun _ -> Error "server state only")

let live =
  Live.declare ~channel:"counter" ~state:state_codec ~event:ev_codec component

type route = Home | Missing of string

let routes =
  Router.routes
    ~not_found:(function Missing _ -> true | Home -> false)
    [
      Router.route Route.root
        (fun () -> Home)
        (function Home -> Some () | Missing _ -> None);
      Router.route Route.rest
        (fun path -> Missing path)
        (function Missing path -> Some path | Home -> None);
    ]

let titled ~id title =
  Machine.make_with_props ~id ~init:0
    ~name:(fun _ -> "active")
    ~on_props:(fun _ n _ -> Machine.Go_with (n, [ Action.Set_title (title n) ]))
    (fun _ _ (_ : ev) -> Machine.Ignore)

let page_component : (int, int, ev) Component.t =
  Component.make
    (titled ~id:"page" (fun n -> "Count & <" ^ string_of_int n ^ ">"))
    counter_view

let missing_component : (int, int, ev) Component.t =
  Component.make
    (titled ~id:"missing" (fun _ -> "Not found"))
    (fun _ -> Node.element "h1" [] [ Node.text "Nothing here" ])

let render : route -> Router.event Node.t = function
  | Home ->
      Component.node_with_props ~props:7 page_component
  | Missing _ ->
      Component.node_with_props ~props:0 missing_component

let app = Router.make routes ~render

let status route =
  if Router.is_not_found routes route then
    404
  else
    200
