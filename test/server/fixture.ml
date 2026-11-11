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
let boom = Remote.declare ~path:"/api/boom" ~input:Codec.int ~output:Codec.int
let ghost = Remote.declare ~path:"/api/ghost" ~input:Codec.int ~output:Codec.int
let x = Ref.make "x"

let settle = function Ok v -> Set v | Error _ -> Failed

module Phase = struct
  type _ t =
    | Resting : int t
    | Mixing : int t
    | Doubling : int t
    | Booming : int t
    | Ghosting : int t
    | Waiting : int t
    | Slowing : int t
end

module Phase_machine = Machine.Make (Phase)

let machine =
  let handler self n = function
    | Inc ->
        Phase_machine.goto self (n + 1)
    | Focus_me ->
        Phase_machine.stay [ Cmd.Focus x ]
    | Mixed ->
        Phase_machine.goto Phase.Mixing n
          ~cmds:[ Cmd.Focus x; Cmd.Navigate (Url.of_string "/a?b=c") ]
    | Double ->
        Phase_machine.goto Phase.Doubling n
    | Boom ->
        Phase_machine.goto Phase.Booming n
    | Ghost ->
        Phase_machine.goto Phase.Ghosting n
    | Slow ->
        Phase_machine.goto Phase.Slowing n
    | Later ->
        Phase_machine.reenter Phase.Waiting n
    | Cancel_later | Cancel_double ->
        Phase_machine.goto Phase.Resting n
    | Set v ->
        Phase_machine.goto Phase.Resting v
    | Failed ->
        Phase_machine.goto Phase.Resting (-1)
  in
  let to_ =
    Phase_machine.
      [
        Phase.Resting;
        Phase.Mixing;
        Phase.Doubling;
        Phase.Booming;
        Phase.Ghosting;
        Phase.Waiting;
        Phase.Slowing;
      ]
  in
  Phase_machine.make ~id:"counter"
    ~init:(Phase_machine.at Phase.Resting 0)
    [
      Phase_machine.state Phase.Resting "resting" ~to_ (handler Phase.Resting);
      Phase_machine.state Phase.Mixing "mixing" ~to_
        ~after:[ (10, Inc) ]
        ~invoke:[ Source.call double Fun.id settle ]
        (handler Phase.Mixing);
      Phase_machine.state Phase.Doubling "doubling" ~to_
        ~invoke:[ Source.call double Fun.id settle ]
        (handler Phase.Doubling);
      Phase_machine.state Phase.Booming "booming" ~to_
        ~invoke:[ Source.call boom Fun.id settle ]
        (handler Phase.Booming);
      Phase_machine.state Phase.Ghosting "ghosting" ~to_
        ~invoke:[ Source.call ghost Fun.id settle ]
        (handler Phase.Ghosting);
      Phase_machine.state Phase.Waiting "waiting" ~to_
        ~after:[ (50, Inc) ]
        (handler Phase.Waiting);
      Phase_machine.state Phase.Slowing "slowing" ~to_
        ~invoke:[ Source.call ~timeout_ms:20 double Fun.id settle ]
        (handler Phase.Slowing);
    ]

let count : Phase_machine.snapshot -> int = function
  | Phase_machine.At (Phase.Resting, n) ->
      n
  | Phase_machine.At (Phase.Mixing, n) ->
      n
  | Phase_machine.At (Phase.Doubling, n) ->
      n
  | Phase_machine.At (Phase.Booming, n) ->
      n
  | Phase_machine.At (Phase.Ghosting, n) ->
      n
  | Phase_machine.At (Phase.Waiting, n) ->
      n
  | Phase_machine.At (Phase.Slowing, n) ->
      n

let counter_view n =
  Node.element "p" [] [ Node.text ("count <" ^ string_of_int n ^ ">") ]

let component =
  { Component.machine; view = (fun snapshot -> counter_view (count snapshot)) }

let state_codec : Phase_machine.snapshot Codec.t =
  Codec.make
    ~encode:(fun snapshot -> Codec.int.encode (count snapshot))
    ~decode:(fun _ -> Error "server state only")

let live =
  Live.declare ~channel:"counter" ~state:state_codec ~event:ev_codec component

module State = struct
  type _ t = Home : unit t | Missing : string t

  let path : type data. data t -> data Route.t = function
    | Home ->
        Route.root
    | Missing ->
        Route.rest
end

module Router = Router.Make (State)

let routes =
  Router.routes ~not_found:State.Missing [ State.Home; State.Missing ]

let page_component : (int, ev) Component.t =
  {
    machine =
      Silica_test_support.Simple.machine ~id:"page" ~init:0 (fun _ -> function
        | Set n ->
            Some (n, [ Cmd.Set_title ("Count & <" ^ string_of_int n ^ ">") ])
        | _ ->
            None
        );
    view = counter_view;
  }

let missing_component : (int, ev) Component.t =
  {
    machine =
      Silica_test_support.Simple.machine ~id:"missing" ~init:0
        (fun _ -> function
        | Set _ ->
            Some (0, [ Cmd.Set_title "Not found" ])
        | _ ->
            None
        );
    view = (fun _ -> Node.element "h1" [] [ Node.text "Nothing here" ]);
  }

let render : Router.snapshot -> Silica.Router.event Node.t = function
  | Router.At (State.Home, ()) ->
      Component.node ~props:(Set 7) page_component
  | Router.At (State.Missing, _) ->
      Component.node ~props:(Set 0) missing_component

let app = Router.make routes ~render
let status snapshot =
  if Router.is_not_found routes snapshot then
    404
  else
    200
