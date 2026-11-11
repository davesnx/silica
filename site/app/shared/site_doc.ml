open Jsonkit.Primitives

type inline =
  | Text of string
  | Code of string
  | Strong of inline list
  | Emph of inline list
  | Link of link
  | Page_link of page_link

and link = { href : string; content : inline list }

and page_link = { page : string; anchor : string option; label : inline list }
[@@deriving json]

type block =
  | Heading of heading
  | Paragraph of inline list
  | Items of items
  | Code_block of code
  | Table of table

and heading = { level : int; anchor : string; text : inline list }
and items = { ordered : bool; items : block list list }
and code = { lang : string; file : string option; source : string }
and table = { head : inline list list; rows : inline list list list }
[@@deriving json]

type page = { slug : string; title : string; blocks : block list }
[@@deriving json]

type answer = Page of page | Missing [@@deriving json]

let endpoint =
  Silica.Remote.declare ~path:"/api/doc" ~input:Silica.Codec.string
    ~output:
      (Silica_jsonkit.codec ~to_json:answer_to_json ~of_json:answer_of_json)
    ~rejected:Silica.Codec.string

let rec plain inlines = String.concat "" (List.map plain_inline inlines)

and plain_inline = function
  | Text text | Code text ->
      text
  | Strong content | Emph content | Link { content; _ } ->
      plain content
  | Page_link { label; _ } ->
      plain label
