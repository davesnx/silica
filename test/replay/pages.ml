open Silica

type route = Home | About | Secret

let route_name = function
  | Home ->
      "home"
  | About ->
      "about"
  | Secret ->
      "secret"

let route_states = [ "home"; "about"; "secret" ]

let parse url =
  match Url.to_string url with
  | "/about" ->
      About
  | "/secret" ->
      Secret
  | _ ->
      Home

let print = function
  | Home ->
      Url.of_string "/"
  | About ->
      Url.of_string "/about"
  | Secret ->
      Url.of_string "/secret"

let event_name = function
  | Router.Url_changed _ ->
      "url changed"
  | Preload _ ->
      "preload"

let machine =
  Machine.make ~id:"pages" ~init:Home ~name:route_name ~states:route_states
    ~event_name (fun _ event ->
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
    | Secret ->
        Node.element "main" [] [ Node.element "h1" [] [ Node.text "Secret" ] ]
    | About ->
        Node.element "main" []
          [
            Node.element "h1" [] [ Node.text "About" ];
            Node.element "img" [ Attr.string "src" "/team.png" ] [];
          ]
    )

let check =
  Check.app ~routes:[ Home; About; Secret ] ~urls:[ "/secret" ]
    ~parse:(fun url -> Ok (parse url))
    ~print
    ~url_event:(fun url -> Router.Url_changed url)
    ~properties:
      [
        Check.never_after ~name:"home stays behind"
          ~after:(Check.state_named "about")
          ~never:(Check.state_named "home") ();
        Check.never_after ~name:"forward stays back"
          ~after:
            (Check.state (fun (app : _ Check.app_state) ->
                 app.index < List.length app.history - 1
                 && List.exists
                      (fun url -> Url.to_string url = "/secret")
                      app.history
             )
            )
          ~never:(Check.state_named "secret")
          ();
      ]
    app
