open Silica

let json ~to_json ~of_json = Silica_jsonkit.codec ~to_json ~of_json

let register () =
  Machine.register_codecs Linear_me.machine
    ~state:
      (json ~to_json:Linear_me.state_to_json ~of_json:Linear_me.state_of_json)
    ~event:
      (json ~to_json:Linear_me.event_to_json ~of_json:Linear_me.event_of_json)
    ();
  Machine.register_codecs Linear_comment.machine
    ~state:
      (json ~to_json:Linear_comment.state_to_json
         ~of_json:Linear_comment.state_of_json
      )
    ~event:
      (json ~to_json:Linear_comment.event_to_json
         ~of_json:Linear_comment.event_of_json
      )
    ();
  Machine.register_codecs Linear_comments.machine
    ~state:
      (json ~to_json:Linear_comments.state_to_json
         ~of_json:Linear_comments.state_of_json
      )
    ~event:
      (json ~to_json:Linear_comments.event_to_json
         ~of_json:Linear_comments.event_of_json
      )
    ();
  Machine.register_codecs Linear_composer.machine
    ~state:
      (json ~to_json:Linear_composer.state_to_json
         ~of_json:Linear_composer.state_of_json
      )
    ~event:
      (json ~to_json:Linear_composer.event_to_json
         ~of_json:Linear_composer.event_of_json
      )
    ();
  Machine.register_codecs Linear_create.machine
    ~state:
      (json ~to_json:Linear_create.state_to_json
         ~of_json:Linear_create.state_of_json
      )
    ~event:
      (json ~to_json:Linear_create.event_to_json
         ~of_json:Linear_create.event_of_json
      )
    ();
  Machine.register_codecs Linear_list.machine
    ~event:
      (json ~to_json:Linear_list.event_to_json
         ~of_json:Linear_list.event_of_json
      )
    ();
  Machine.register_codecs Linear_detail.machine
    ~event:
      (json ~to_json:Linear_detail.event_to_json
         ~of_json:Linear_detail.event_of_json
      )
    ()
