type event = Url_changed of Url.t | Preload of Url.t
type preload = Intent | Viewport | Never

module Make (State : sig
  type 'data t

  val path : 'data t -> 'data Route.t
end) =
struct
  module M = Machine.Make (State)

  type snapshot = M.snapshot = At : 'data State.t * 'data -> snapshot

  type any = Any : 'data State.t -> any

  type routes = {
    base : string list;
    listed : any list;
    not_found : any option;
    reenter : any list;
    preload : (snapshot -> Remote.request list) option;
    chunked : any list;
  }

  let rec strip prefix path =
    match (prefix, path) with
    | [], rest ->
        Some rest
    | p :: prefix, s :: path when p = s ->
        strip prefix path
    | _ ->
        None

  let parse routes (url : Url.t) =
    let no_match () =
      failwith
        ("Router: no route matches " ^ Url.to_string url
       ^ "; end the list with a Route.rest route"
        )
    in
    let inner =
      match strip routes.base url.path with
      | Some path ->
          { url with path }
      | None ->
          failwith
            ("Router: " ^ Url.to_string url ^ " is outside the base /"
            ^ String.concat "/" routes.base
            )
    in
    let rec go = function
      | [] ->
          no_match ()
      | Any state :: rest -> (
          match Route.parse (State.path state) inner with
          | Some data ->
              At (state, data)
          | None ->
              go rest
        )
    in
    go routes.listed

  let print routes (At (state, data)) =
    let url = Route.print (State.path state) data in
    { url with path = routes.base @ url.path }

  let is_not_found routes (At (state, _)) =
    match routes.not_found with
    | Some (Any not_found) ->
        M.same not_found state
    | None ->
        false

  let calls routes snapshot =
    match routes.preload with Some f -> f snapshot | None -> []

  let chunked routes (At (state, _)) =
    List.exists (fun (Any listed) -> M.same listed state) routes.chunked

  let url_of ?hash routes snapshot = { (print routes snapshot) with hash }

  let navigate ?hash routes state data =
    Cmd.Navigate (url_of ?hash routes (At (state, data)))

  let replace ?hash routes state data =
    Cmd.Replace (url_of ?hash routes (At (state, data)))

  let current_attrs routes (link : Url.t) = function
    | None ->
        []
    | Some current -> (
        let now = (print routes current).path in
        match strip link.path now with
        | Some [] ->
            [ Attr.string "aria-current" "page"; Attr.bool "data-active" true ]
        | Some _ ->
            [ Attr.bool "data-active" true ]
        | None ->
            []
      )

  let preload_attrs routes snapshot = function
    | Never ->
        []
    | (Intent | Viewport) as mode ->
        if calls routes snapshot = [] && not (chunked routes snapshot) then
          []
        else
          [
            Attr.string "data-silica-preload"
              ( if mode = Intent then
                  "intent"
                else
                  "viewport"
              );
          ]

  let anchor extra ?hash ?(preload = Intent) ?current routes state data attrs
      children =
    let snapshot = At (state, data) in
    let url = url_of ?hash routes snapshot in
    Node.element "a"
      (Attr.string "href" (Url.to_string url)
      :: Attr.bool "data-silica-link" true
      :: (extra
         @ current_attrs routes url current
         @ preload_attrs routes snapshot preload
         @ attrs
         )
      )
      children

  let link ?hash ?preload ?current routes state data attrs children =
    anchor [] ?hash ?preload ?current routes state data attrs children

  let link_replace ?hash ?preload ?current routes state data attrs children =
    anchor
      [ Attr.bool "data-silica-replace" true ]
      ?hash ?preload ?current routes state data attrs children

  type targets = M.targets = [] | ( :: ) : 'data State.t * targets -> targets

  let rec any_list : targets -> any list = function
    | [] ->
        []
    | state :: rest ->
        Any state :: any_list rest

  let routes ?(base = "") ?not_found ?(reenter = ([] : targets)) ?preload
      ?(chunked = ([] : targets)) listed =
    {
      base = (Url.of_string base).path;
      listed = any_list listed;
      not_found = Option.map (fun state -> Any state) not_found;
      reenter = any_list reenter;
      preload;
      chunked = any_list chunked;
    }

  type decision = Allow | Redirect of snapshot | Cancel

  let make routes ?(guard = fun ~from:_ ~target:_ -> Allow)
      ?(exit = fun _ -> ([] : event Cmd.t list)) ?title
      ?(chunks = fun _ -> ([] : Chunk.any list)) ~render =
    let titled snapshot (cmds : event Cmd.t list) =
      match title with
      | Some title ->
          List.cons (Cmd.Set_title (title snapshot)) cmds
      | None ->
          cmds
    in
    let all : targets =
      List.fold_right
        (fun (Any state) rest -> state :: rest)
        routes.listed
        ([] : targets)
    in
    let restart state =
      List.exists (fun (Any listed) -> M.same listed state) routes.reenter
    in
    let move ?(cmds : event Cmd.t list = []) (At (state, data) as target) =
      let cmds = titled target cmds in
      if restart state then
        M.reenter ~cmds:(Cmd.Remount :: cmds) state data
      else
        M.goto ~cmds state data
    in
    let same_url a b =
      Url.to_string (print routes a) = Url.to_string (print routes b)
    in
    let handle current = function
      | Preload url ->
          let target = parse routes url in
          M.stay
            (List.map (fun request -> Cmd.Preload request) (calls routes target)
            @ List.map
                (fun chunk -> Cmd.Load_chunk { chunk; on_ready = None })
                (chunks target)
            )
      | Url_changed url -> (
          let target = parse routes url in
          if same_url target current then
            M.stay []
          else
            match guard ~from:(Some current) ~target with
            | Allow ->
                move target
            | Redirect other ->
                move ~cmds:[ Cmd.Replace (print routes other) ] other
            | Cancel ->
                M.stay [ Cmd.Revert_navigation ]
        )
    in
    let cases =
      List.map
        (fun (Any state) ->
          M.state state
            (Route.describe (State.path state))
            ~to_:all
            ~exit:(fun data -> exit (At (state, data)))
            (fun data event -> handle (At (state, data)) event)
        )
        routes.listed
    in
    fun url ->
      let target = parse routes url in
      let init, (start : event Cmd.t list) =
        match guard ~from:None ~target with
        | Allow | Cancel ->
            (target, [])
        | Redirect other ->
            (other, [ Cmd.Replace (print routes other) ])
      in
      let start = titled init start in
      {
        Component.machine = M.make ~id:"router" ~init ~start cases;
        view = render;
      }
end
