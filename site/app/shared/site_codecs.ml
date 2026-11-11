open Silica

let json ~to_json ~of_json = Silica_jsonkit.codec ~to_json ~of_json

let register () =
  Machine.register_codecs Site_doc_page.machine
    ~state:
      (json ~to_json:Site_doc_page.state_to_json
         ~of_json:Site_doc_page.state_of_json
      )
    ~event:
      (json ~to_json:Site_doc_page.event_to_json
         ~of_json:Site_doc_page.event_of_json
      )
    ();
  Machine.register_codecs Site_landing.machine
    ~state:
      (json ~to_json:Site_landing.state_to_json
         ~of_json:Site_landing.state_of_json
      )
    ~event:
      (json ~to_json:Site_landing.event_to_json
         ~of_json:Site_landing.event_of_json
      )
    ();
  Machine.register_codecs Site_search.machine
    ~state:
      (json ~to_json:Site_search.state_to_json
         ~of_json:Site_search.state_of_json
      )
    ~event:
      (json ~to_json:Site_search.event_to_json
         ~of_json:Site_search.event_of_json
      )
    ()
