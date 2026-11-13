module Server = Silica_adapter.Server

let create config ~assets ~read =
  let item _ id =
    if id < 0 then
      Error (Printf.sprintf "no item %d" id)
    else
      Ok
        {
          Example_model.Item.id;
          title = Printf.sprintf "Item %d" id;
          price_cents = 100 * (id + 1);
          tags = [ "demo" ];
        }
  in
  let votes = Atomic.make 0 in
  let server = ref None in
  let vote _ () =
    let n = Atomic.fetch_and_add votes 1 + 1 in
    ignore
      (Server.publish (Option.get !server) Example_model.votes_topic ~key:"main"
         n
      );
    Ok n
  in
  let handlers =
    [
      Server.implement Example_model.item_endpoint item;
      Server.implement Example_model.vote_endpoint vote;
    ]
  in
  let islands = [ Server.Island Example_counter.live ] in
  let t =
    Server.create config ~assets ~read ~handlers ~islands
      ~topics:
        [
          Server.Topic_def
            {
              topic = Example_model.votes_topic;
              init = (fun _ ~key:_ -> Ok (Atomic.get votes));
            };
        ]
      ~page:(fun (page : Server.page) ->
        Server.render_page_settled Example_app.app ~status:Example_app.status
          ~scripts:[ page.asset "client_main.js" ]
          page
      )
      ()
  in
  server := Some t;
  t
