open Silica

type route = Home | About [@@deriving state_shape]

let parse url = match Url.to_string url with "/about" -> About | _ -> Home

let print = function
  | Home ->
      Url.of_string "/"
  | About ->
      Url.of_string "/about"

let events =
  Machine.shape
    ~name:(function
      | Router.Url_changed _ -> "url changed" | Preload _ -> "preload"
      )
    ()

let machine =
  Machine.make ~id:"pages" ~init:Home ~shape:route_shape ~events (fun _ event ->
      match event with
      | Router.Url_changed url ->
          Machine.Go (parse url)
      | Preload _ ->
          Machine.Ignore
  )

let app _ =
  Component.make machine (function
    | Home ->
        Node.element "main" []
          [
            Node.element "h1" [] [ Node.text "Home" ];
            Node.element "a"
              [ Attr.string "href" "/about"; Attr.bool "data-silica-link" true ]
              [ Node.text "About" ];
          ]
    | About ->
        Node.element "main" []
          [
            Node.element "h1" [] [ Node.text "About" ];
            Node.element "img" [ Attr.string "src" "/team.png" ] [];
          ]
    )

let check =
  Check.app ~routes:[ Home; About ] ~parse ~print
    ~url_event:(fun url -> Router.Url_changed url)
    ~properties:
      [
        Check.never_after ~name:"home stays behind"
          ~after:(Check.state_named "about")
          ~never:(Check.state_named "home") ();
      ]
    app
