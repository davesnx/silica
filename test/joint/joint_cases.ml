let contains text needle =
  let length = String.length needle in
  let rec scan index =
    index + length <= String.length text
    && (String.sub text index length = needle || scan (index + 1))
  in
  scan 0

let bumped =
  Check.component
    ~children:
      [
        Check.child ~max_instances:3 Row.component;
        Check.child
          ~samples:[ Check.answers Job.endpoint [ "ok" ] ]
          Job.component;
      ]
    ~properties:
      [
        Check.leads_to ~name:"a bumped row finishes"
          ~from:
            (Check.view (fun view ->
                 contains (Silica.View_text.to_string view) "clicks 1"
             )
            )
          ~to_:(Check.state (fun (state : Board.state) -> state.finished <> []))
          ();
      ]
    Board.component
