open Silica
open Example_model

let count = ref 0

let round_trip name codec value =
  match codec with
  | Error message ->
      failwith (name ^ ": " ^ message)
  | Ok codec -> (
      let text = Codec.to_string codec value in
      incr count;
      match Codec.of_string codec text with
      | Ok decoded when decoded = value ->
          ()
      | Ok _ ->
          failwith (name ^ ": " ^ text ^ " decodes to another value")
      | Error error ->
          failwith (name ^ ": " ^ text ^ " does not decode: " ^ error)
    )

let events name machine values =
  List.iter (round_trip name (Machine.event_codec machine)) values

let states name machine values =
  List.iter (round_trip name (Machine.state_codec machine)) values

let item : Item.t =
  { id = 1; title = "A title"; price_cents = 1999; tags = [ "a"; "b" ] }

let page : Site_doc.page =
  {
    slug = "router";
    title = "Router";
    blocks =
      [
        Paragraph
          [
            Text "A ";
            Strong [ Code "route" ];
            Page_link { page = "data"; anchor = None; label = [ Emph [] ] };
          ];
        Items { ordered = true; items = [ [ Paragraph [] ]; [] ] };
      ];
  }

let () =
  Example_codecs.register ();
  Site_codecs.register ();
  states "doc page" Site_doc_page.machine
    [ Site_doc_page.Ready page; Failed "down" ];
  events "doc page" Site_doc_page.machine
    [ Site_doc_page.Loaded (Ok Missing); Loaded (Ok (Page page)); Retry ];
  events "item" Example_item.machine
    [
      Example_item.Settled (Ok item);
      Settled (Rejected "no item");
      Settled (Failed (Network "down"));
    ];
  states "item" Example_item.machine
    [ Example_item.Loading; Loaded item; Missing "gone"; Failed (Http 500) ];
  states "todo" Example_todo.machine
    [
      {
        Example_todo.todos = [ { id = 1; text = "milk"; completed = true } ];
        draft = "";
        next_id = 2;
      };
    ];
  events "todo" Example_todo.machine
    [ Example_todo.Draft "milk"; Add; Toggle 1 ];
  events "votes" Example_votes.machine
    [
      Example_votes.Update
        { seq = 2; status = Ready { data = 3; stale = true } };
      Update { seq = 0; status = Loading None };
      Reconnect;
    ];
  states "votes" Example_votes.machine
    [ Example_votes.Watching (Failed { error = Http 503; last = Some 2 }) ];
  events "vote" Example_vote_button.machine
    [ Remote_machine.Load (); Settled (Rejected "closed") ];
  print_endline
    ("codecs: " ^ string_of_int !count ^ " values round trip under Melange")
