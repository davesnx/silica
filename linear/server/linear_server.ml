open Linear_model
module S = Silica_example_adapter.Server

let user_key = "user"

let assigned_user ~(config : Silica_server.config) ~next_user
    (session : Silica_server.Session.session) =
  match List.assoc_opt user_key session.data with
  | Some name -> (
      match List.find_opt (fun u -> User.name u = name) User.all with
      | Some user ->
          user
      | None ->
          failwith ("unknown user in session: " ^ name)
    )
  | None ->
      let user = next_user () in
      config.sessions.set
        { session with data = (user_key, User.name user) :: session.data };
      user

let create ?count (config : Silica_server.config) ~assets ~read () =
  let store = Linear_store.create ?count () in
  let next_user = Linear_store.next_user () in
  let server = ref None in
  let me (context : S.context) () =
    Ok (assigned_user ~config ~next_user context.session)
  in
  let list _ () = Ok (Linear_store.issues store) in
  let create _ draft =
    match
      Linear_store.create_issue store
        ~now:(int_of_float (Unix.gettimeofday ()))
        draft
    with
    | Error message ->
        Error message
    | Ok issue ->
        S.publish (Option.get !server) issues_topic ~key:issues_key
          (Linear_store.issues store);
        Ok issue
  in
  let update _ request =
    let answer =
      Linear_store.update_issue store
        ~now:(int_of_float (Unix.gettimeofday ()))
        request
    in
    ( match answer with
    | Ok (Change.Saved _) ->
        S.publish (Option.get !server) issues_topic ~key:issues_key
          (Linear_store.issues store)
    | Ok (Change.Conflict _) | Error _ ->
        ()
    );
    answer
  in
  let viewer (context : S.context) =
    assigned_user ~config ~next_user context.session
  in
  let publish_comments number =
    S.publish (Option.get !server) comments_topic ~key:(string_of_int number)
      (Linear_store.comments store number)
  in
  let thread context number =
    if
      List.exists
        (fun (i : Issue.t) -> i.number = number)
        (Linear_store.issues store)
    then
      Ok
        {
          Thread.viewer = viewer context;
          comments = Linear_store.comments store number;
        }
    else
      Error ("There is no issue ENG-" ^ string_of_int number)
  in
  let add_comment context input =
    let added =
      Linear_store.add_comment store
        ~now:(int_of_float (Unix.gettimeofday ()))
        ~author:(viewer context) input
    in
    Result.iter (fun (c : Comment.t) -> publish_comments c.number) added;
    added
  in
  let edit_comment context input =
    let edited =
      Linear_store.edit_comment store ~author:(viewer context) input
    in
    Result.iter (fun (c : Comment.t) -> publish_comments c.number) edited;
    edited
  in
  let handlers =
    [
      S.implement me_endpoint me;
      S.implement issues_endpoint list;
      S.implement create_endpoint create;
      S.implement update_endpoint update;
      S.implement comments_endpoint thread;
      S.implement add_comment_endpoint add_comment;
      S.implement edit_comment_endpoint edit_comment;
    ]
  in
  let t =
    S.create config ~assets ~read ~handlers ~islands:[]
      ~topics:
        [
          S.Topic_def
            {
              topic = issues_topic;
              init =
                (fun _ ~key ->
                  if key = issues_key then
                    Ok (Linear_store.issues store)
                  else
                    Error ("no such key: " ^ key)
                );
            };
          S.Topic_def
            {
              topic = comments_topic;
              init =
                (fun _ ~key ->
                  match int_of_string_opt key with
                  | Some number
                    when List.exists
                           (fun (i : Issue.t) -> i.number = number)
                           (Linear_store.issues store) ->
                      Ok (Linear_store.comments store number)
                  | Some _ | None ->
                      Error ("no such key: " ^ key)
                );
            };
        ]
      ~page:(fun (page : S.page) ->
        S.render_page_settled
          (Linear_app.make
             (Linear_theme.choice
                (Silica_server.cookie page.context.request
                   Silica_ui.Theme.cookie_name
                )
             )
          )
          ~status:Linear_app.status
          ~scripts:[ page.asset "client_main.js" ]
          ~head:
            (Printf.sprintf "<link rel=\"stylesheet\" href=\"%s\">"
               (page.asset "app.css")
            )
          ~csrf:page.csrf ~nonce:page.nonce ~islands:[] ~handlers
          ~context:page.context page.url
      )
      ()
  in
  server := Some t;
  t
