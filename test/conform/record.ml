open Silica
open Example_model
module Memory = Silica_memory

let body = "Purple elephant memo"
let note = "Quarterly zebra migration"

let item id : Item.t =
  { id; title = "Item " ^ string_of_int id; price_cents = 1999; tags = [] }

let reply request endpoint value =
  Memory.reply request (Codec.to_string (Remote.output endpoint) value)

let rec answer t =
  let pending = Memory.pending t in
  List.iter
    (fun request ->
      match Memory.path request with
      | "/api/item" ->
          reply request item_endpoint (item 1)
      | "/api/vote" ->
          reply request vote_endpoint 1
      | path ->
          failwith ("unexpected call " ^ path)
    )
    pending;
  if pending <> [] then answer t

let link t name = Memory.press (Memory.by_role ~name t Accessibility.Link)

let add t text =
  let field = Memory.by_role ~name:"New todo" t Accessibility.Textbox in
  Memory.input field text;
  Memory.submit field

let visit ~edit () =
  Example_codecs.register ();
  let t = Memory.mount_app ~url:"/" Example_app.app in
  answer t;
  add t body;
  if edit then add t note;
  Memory.press (Memory.by_id t "vote");
  answer t;
  link t "Item 1";
  answer t;
  link t "Item 2";
  answer t;
  if edit then (
    link t "Edit";
    Memory.input (Memory.by_role ~name:"Draft" t Accessibility.Textbox) "note";
    Memory.press (Memory.by_role ~name:"Save" t Accessibility.Button)
  );
  Memory.trace t
