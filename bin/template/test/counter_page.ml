open Silica
module Memory = Silica_memory

let () =
  let page = Memory.mount Counter.component in
  let button name = Memory.by_role ~name page Accessibility.Button in
  let count () = Memory.text (Memory.by_role page Accessibility.Status) in
  Memory.press (button "Increment");
  Memory.press (button "Increment");
  Expect.string "count" "2" (count ());
  Memory.press (button "Save");
  ( match Memory.pending page with
  | [ request ] ->
      Expect.string "path" "/api/save" (Memory.path request);
      Expect.string "body" "2" (Memory.body request);
      Memory.reply request "null"
  | requests ->
      Expect.equal ~show:string_of_int "pending requests" 1
        (List.length requests)
  );
  Expect.string "view"
    {|main
  heading "Counter" level=1
  status
    text "2"
  button "Decrement" [press]
  button "Increment" [press]
  button "Save" disabled [press]
  text "Saved"
|}
    (Memory.view_text page)
