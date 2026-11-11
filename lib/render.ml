let is_void = function
  | "area"
  | "base"
  | "br"
  | "col"
  | "embed"
  | "hr"
  | "img"
  | "input"
  | "link"
  | "meta"
  | "param"
  | "source"
  | "track"
  | "wbr" ->
      true
  | _ ->
      false

let entity = function
  | '&' ->
      "&amp;"
  | '<' ->
      "&lt;"
  | '>' ->
      "&gt;"
  | '"' ->
      "&quot;"
  | '\'' ->
      "&#39;"
  | _ ->
      ""

let escape b s =
  let n = String.length s in
  let start = ref 0 in
  for i = 0 to n - 1 do
    match String.unsafe_get s i with
    | ('&' | '<' | '>' | '"' | '\'') as c ->
        Buffer.add_substring b s !start (i - !start);
        Buffer.add_string b (entity c);
        start := i + 1
    | _ ->
        ()
  done;
  Buffer.add_substring b s !start (n - !start)

let attr b = function
  | Node.String (k, v) ->
      Buffer.add_char b ' ';
      Buffer.add_string b k;
      Buffer.add_string b "=\"";
      escape b v;
      Buffer.add_char b '"'
  | Bool (k, true) ->
      Buffer.add_char b ' ';
      Buffer.add_string b k
  | Bool (_, false) ->
      ()
  | Ref r ->
      Buffer.add_string b " data-silica-ref=\"";
      escape b (Ref.name r);
      Buffer.add_char b '"'
  | On_press _
  | On_double_press _
  | On_input _
  | On_submit _
  | On_focus _
  | On_blur _
  | On_key_down _
  | On_key_up _
  | On_pointer_enter _
  | On_pointer_leave _
  | On_reset
  | Rated _ ->
      ()

let rec node : type e.
    island:(string -> string option) ->
    last_text:bool ref ->
    Buffer.t ->
    e Node.t ->
    unit =
 fun ~island ~last_text b n ->
  match Node.view n with
  | Text "" ->
      ()
  | Text s ->
      if !last_text then Buffer.add_string b "<!-- -->";
      escape b s;
      last_text := true
  | List l ->
      List.iter (node ~island ~last_text b) l
  | Keyed (_, n) ->
      node ~island ~last_text b n
  | Portal _ ->
      ()
  | Boundary { body; _ } ->
      node ~island ~last_text b body
  | Chunk chunk ->
      node ~island ~last_text b (Node.chunk_view chunk)
  | Component child ->
      let tree = Instance.mount_with_props ~props:child.props child.component in
      node ~island ~last_text b (fst (Instance.update tree None))
  | Element (tag, attrs, children) ->
      if tag = "html" then Buffer.add_string b "<!DOCTYPE html>";
      Buffer.add_char b '<';
      Buffer.add_string b tag;
      List.iter (attr b) attrs;
      if is_void tag then
        Buffer.add_string b " />"
      else (
        Buffer.add_char b '>';
        let inner = ref false in
        let hosted =
          match children with
          | [] ->
              Option.bind
                (List.find_map
                   (function[@warning "-4"]
                     | Node.String ("data-silica-live", ch) ->
                         Some ch
                     | _ ->
                         None
                     )
                   attrs
                )
                island
          | _ ->
              None
        in
        ( match hosted with
        | Some html ->
            Buffer.add_string b html
        | None ->
            List.iter (node ~island ~last_text:inner b) children
        );
        Buffer.add_string b "</";
        Buffer.add_string b tag;
        Buffer.add_char b '>'
      );
      last_text := false

let to_buffer ?(island = fun _ -> None) b n =
  node ~island ~last_text:(ref false) b n

let to_string ?island n =
  let b = Buffer.create 4096 in
  to_buffer ?island b n;
  Buffer.contents b
