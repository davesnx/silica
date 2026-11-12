type event = Url_changed of Url.t | Preload of Url.t
type preload = Intent | Viewport | Never

type 'r route =
  | Route : {
      path : 'data Route.t;
      make : 'data -> 'r;
      data : 'r -> 'data option;
    }
      -> 'r route

let route path make data = Route { path; make; data }

type 'r routes = {
  base : string list;
  listed : 'r route list;
  not_found : 'r -> bool;
  reenter : 'r -> bool;
  preload : 'r -> Remote.request list;
  chunked : 'r -> bool;
}

let rec strip prefix path =
  match (prefix, path) with
  | [], rest ->
      Some rest
  | p :: prefix, s :: path when p = s ->
      strip prefix path
  | _ :: _, _ ->
      None

let parse routes (url : Url.t) =
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
  match
    List.find_map
      (fun (Route route) -> Option.map route.make (Route.parse route.path inner))
      routes.listed
  with
  | Some route ->
      route
  | None ->
      failwith
        ("Router: no route matches " ^ Url.to_string url
       ^ "; end the list with a Route.rest route"
        )

let find routes r =
  List.find_map
    (fun (Route route) ->
      Option.map
        (fun data ->
          (Route.describe route.path, fun () -> Route.print route.path data)
        )
        (route.data r)
    )
    routes.listed

let found routes r =
  match find routes r with
  | Some found ->
      found
  | None ->
      failwith "Router: no listed route gives data for this route"

let print routes r =
  let url = (snd (found routes r)) () in
  { url with path = routes.base @ url.path }

let name routes r = fst (found routes r)
let is_not_found routes r = routes.not_found r
let url_of ?hash routes r = { (print routes r) with hash }
let navigate ?hash routes r = Action.Navigate (url_of ?hash routes r)
let replace ?hash routes r = Action.Replace (url_of ?hash routes r)

let current_attrs routes (link : Url.t) = function
  | None ->
      []
  | Some current -> (
      let now = (print routes current).path in
      match strip link.path now with
      | Some [] ->
          [ Attr.string "aria-current" "page"; Attr.bool "data-active" true ]
      | Some (_ :: _) when link.path = routes.base ->
          []
      | Some (_ :: _) ->
          [ Attr.bool "data-active" true ]
      | None ->
          []
    )

let preload_attrs routes r = function
  | Never ->
      []
  | (Intent | Viewport) as mode ->
      if routes.preload r = [] && not (routes.chunked r) then
        []
      else
        [
          Attr.string "data-silica-preload"
            ( match mode with
            | Intent ->
                "intent"
            | Viewport | Never ->
                "viewport"
            );
        ]

let anchor extra ?hash ?(preload = Intent) ?current routes r attrs children =
  let url = url_of ?hash routes r in
  Node.element "a"
    (Attr.string "href" (Url.to_string url)
    :: Attr.bool "data-silica-link" true
    :: (extra
       @ current_attrs routes url current
       @ preload_attrs routes r preload
       @ attrs
       )
    )
    children

let link ?hash ?preload ?current routes r attrs children =
  anchor [] ?hash ?preload ?current routes r attrs children

let link_replace ?hash ?preload ?current routes r attrs children =
  anchor
    [ Attr.bool "data-silica-replace" true ]
    ?hash ?preload ?current routes r attrs children

let never _ = false
let no_calls _ = []

let routes ?(base = "") ?(not_found = never) ?(reenter = never)
    ?(preload = no_calls) ?(chunked = never) listed =
  {
    base = (Url.of_string base).path;
    listed;
    not_found;
    reenter;
    preload;
    chunked;
  }

type 'r decision = Allow | Redirect of 'r | Cancel

let url_codec : Url.t Codec.t =
  {
    encode = (fun url -> Codec.string.encode (Url.to_string url));
    decode = (fun json -> Result.map Url.of_string (Codec.string.decode json));
  }

let event_codec : event Codec.t =
  {
    encode =
      (function
      | Url_changed url ->
          Codec.constructor "Url_changed" [ url_codec.encode url ]
      | Preload url ->
          Codec.constructor "Preload" [ url_codec.encode url ]
      );
    decode =
      (fun json ->
        Result.bind (Codec.constructor_of json) (function
          | "Url_changed", [ url ] ->
              Result.map (fun url -> Url_changed url) (url_codec.decode url)
          | "Preload", [ url ] ->
              Result.map (fun url -> Preload url) (url_codec.decode url)
          | name, _ ->
              Error ("unknown constructor " ^ name)
          )
      );
  }

let state_codec routes : 'r Codec.t =
  {
    encode = (fun route -> url_codec.encode (print routes route));
    decode = (fun json -> Result.map (parse routes) (url_codec.decode json));
  }

type made = { made : 'r. 'r routes -> (unit, 'r, event) Machine.t -> unit }

let on_make = ref { made = (fun _ _ -> ()) }

let event_name = function
  | Url_changed _ ->
      "url changed"
  | Preload _ ->
      "preload"

let make routes ?(guard = fun ~from:_ ~target:_ -> Allow)
    ?(exit = fun _ -> ([] : event Action.t list)) ?title
    ?(chunks = fun _ -> ([] : Chunk.any list)) ~render =
  let titled r (actions : event Action.t list) =
    match title with
    | Some title ->
        Action.Set_title (title r) :: actions
    | None ->
        actions
  in
  let move ?(actions : event Action.t list = []) current target :
      (_, event) Machine.next =
    let actions = titled target actions in
    if routes.reenter target && name routes current = name routes target then
      Go_with (target, exit current @ (Action.Remount :: actions))
    else
      Go_with (target, actions)
  in
  let same_url a b =
    Url.to_string (print routes a) = Url.to_string (print routes b)
  in
  let update current = function
    | Preload url ->
        let target = parse routes url in
        Machine.Stay
          (List.map
             (fun request -> Action.Preload request)
             (routes.preload target)
          @ List.map
              (fun chunk -> Action.Load_chunk { chunk; on_ready = None })
              (chunks target)
          )
    | Url_changed url -> (
        let target = parse routes url in
        if same_url target current then
          Machine.Stay []
        else
          match guard ~from:(Some current) ~target with
          | Allow ->
              move current target
          | Redirect other ->
              move
                ~actions:[ Action.Replace (print routes other) ]
                current other
          | Cancel ->
              Machine.Stay [ Action.Revert_navigation ]
      )
  in
  fun url ->
    let target = parse routes url in
    let init, (start : event Action.t list) =
      match guard ~from:None ~target with
      | Allow | Cancel ->
          (target, [])
      | Redirect other ->
          (other, [ Action.Replace (print routes other) ])
    in
    let machine =
      Machine.make ~id:"router" ~init ~name:(name routes) ~event_name ~exit
        ~start:(titled init start) update
    in
    !on_make.made routes machine;
    Component.make machine render
