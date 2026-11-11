open Jsonkit.Primitives

module Item = struct
  type t = {
    id : int;
    title : string;
    tags : string list;
    note : string option;
  }
  [@@deriving json]
end

module Shape = struct
  type t = Dot | Line of int | Box of { w : int; h : int } [@@deriving json]
end

module Event = struct
  type t =
    | Loaded of (Item.t, string) Silica_jsonkit.answer
    | Updated of int Silica_jsonkit.update
    | Failed of Silica_jsonkit.error
  [@@deriving json]
end

let event_codec =
  Silica_jsonkit.codec ~to_json:Event.to_json ~of_json:Event.of_json

let item_codec =
  Silica_jsonkit.codec ~to_json:Item.to_json ~of_json:Item.of_json

let shape_codec =
  Silica_jsonkit.codec ~to_json:Shape.to_json ~of_json:Shape.of_json

let item =
  {
    Item.id = 3;
    title = "A \"quoted\" \xc3\xa9";
    tags = [ "a"; "b" ];
    note = None;
  }

let tests =
  [
    Alcotest.test_case "derived answers and updates are Silica JSON" `Quick
      (fun () ->
        let answer = Silica.Remote.answer_codec item_codec Silica.Codec.string
        and update = Silica.Topic.update_codec Silica.Codec.int in
        let loaded : (Item.t, string) Silica.Remote.answer = Ok item
        and updated : int Silica.Topic.update =
          { seq = 2; status = Ready { data = 5; stale = false } }
        in
        Alcotest.(check (list string))
          "text"
          [
            {|["Loaded",|} ^ Silica.Codec.to_string answer loaded ^ "]";
            {|["Updated",|} ^ Silica.Codec.to_string update updated ^ "]";
            {|["Failed",["Http",500]]|};
          ]
          (List.map
             (Silica.Codec.to_string event_codec)
             [ Event.Loaded loaded; Updated updated; Failed (Http 500) ]
          );
        List.iter
          (fun event ->
            Alcotest.(check bool)
              "round trip" true
              (Silica.Codec.of_string event_codec
                 (Silica.Codec.to_string event_codec event)
              = Ok event
              )
          )
          [
            Event.Loaded (Rejected "no");
            Loaded (Failed (Network "down"));
            Updated { seq = 0; status = Loading None };
          ]
    );
    Alcotest.test_case "a bad derived answer is an error value" `Quick
      (fun () ->
        match Silica.Codec.of_string event_codec {|["Loaded",["Maybe"]]|} with
        | Error _ ->
            ()
        | Ok _ ->
            Alcotest.fail "expected an error"
    );
    Alcotest.test_case "a record round trips through the wire text" `Quick
      (fun () ->
        let text = Silica.Codec.to_string item_codec item in
        Alcotest.(check bool)
          "same" true
          (Silica.Codec.of_string item_codec text = Ok item)
    );
    Alcotest.test_case "the wire text is the Jsonkit text" `Quick (fun () ->
        Alcotest.(check string)
          "text"
          (Jsonkit.to_string (Item.to_json item))
          (Silica.Codec.to_string item_codec item)
    );
    Alcotest.test_case "variants round trip" `Quick (fun () ->
        List.iter
          (fun shape ->
            Alcotest.(check bool)
              "same" true
              (Silica.Codec.of_string shape_codec
                 (Silica.Codec.to_string shape_codec shape)
              = Ok shape
              )
          )
          [ Shape.Dot; Shape.Line 4; Shape.Box { w = 1; h = 2 } ]
    );
    Alcotest.test_case "a decode failure is an error value" `Quick (fun () ->
        match Silica.Codec.of_string item_codec {|{"id":"x"}|} with
        | Error _ ->
            ()
        | Ok _ ->
            Alcotest.fail "expected an error"
    );
    Alcotest.test_case "invalid JSON is an error value" `Quick (fun () ->
        match Silica.Codec.of_string item_codec "{" with
        | Error _ ->
            ()
        | Ok _ ->
            Alcotest.fail "expected an error"
    );
    Alcotest.test_case "an unknown variant is an error value" `Quick (fun () ->
        match Silica.Codec.of_string shape_codec {|["Circle",1]|} with
        | Error _ ->
            ()
        | Ok _ ->
            Alcotest.fail "expected an error"
    );
  ]

let () = Alcotest.run "jsonkit" [ ("silica_jsonkit", tests) ]
