open Silica

let json ~to_json ~of_json = Silica_jsonkit.codec ~to_json ~of_json

let register () =
  Machine.register_codecs Example_counter.machine
    ~state:Example_model.counter_state ~event:Example_model.counter_event ();
  Machine.register_codecs Example_todo.machine
    ~state:
      (json ~to_json:Example_todo.state_to_json
         ~of_json:Example_todo.state_of_json
      )
    ~event:
      (json ~to_json:Example_todo.event_to_json
         ~of_json:Example_todo.event_of_json
      )
    ();
  Machine.register_codecs Example_item.machine
    ~state:
      (json ~to_json:Example_item.state_to_json
         ~of_json:Example_item.state_of_json
      )
    ~event:
      (json ~to_json:Example_item.event_to_json
         ~of_json:Example_item.event_of_json
      )
    ();
  Machine.register_codecs Example_about.machine
    ~state:
      (json ~to_json:Example_about.state_to_json
         ~of_json:Example_about.state_of_json
      )
    ~event:
      (json ~to_json:Example_about.event_to_json
         ~of_json:Example_about.event_of_json
      )
    ();
  Machine.register_codecs Example_votes.machine
    ~state:
      (json ~to_json:Example_votes.state_to_json
         ~of_json:Example_votes.state_of_json
      )
    ~event:
      (json ~to_json:Example_votes.event_to_json
         ~of_json:Example_votes.event_of_json
      )
    ();
  Machine.register_codecs Example_vote_button.machine
    ~state:(Remote_machine.state_codec Example_model.vote_endpoint)
    ~event:(Remote_machine.event_codec Example_model.vote_endpoint)
    ()
