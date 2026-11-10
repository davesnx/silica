(** Write the pages of a server as files for a static host, such as Cloudflare
    Pages or Vercel.

    Each page is the server HTML with the [silica-csrf] meta element replaced by
    [<meta name="silica-static" content="prefix">]. In a page with that element,
    the browser host sends no Remote call: it reads the file
    [prefix ^ answer_file ~path ~input] with a [GET] instead. The export writes
    one such file for each answer that the server wrote into a page (see
    {!Silica.Answers}). A call that no page answered gets status 404 from the
    host, so its machine gets [Error (Http 404)]. *)

val meta_name : string
(** ["silica-static"]. *)

val answer_file : path:string -> input:string -> string
(** [path] without its leading slashes, a slash, the FNV-1a 32-bit hash of the
    bytes of [input] in 8 lower-case hex digits, and [.json]. For example
    [answer_file ~path:"/api/doc" ~input:"\"router\""]. *)

val page_file : string -> string
(** The file of a page path: ["/"] is [index.html], ["/docs/router"] is
    [docs/router.html]. Static hosts serve [a.html] at [/a]. *)

val run :
  handle:(Silica_server.request -> Silica_server.response) ->
  pages:string list ->
  not_found:string ->
  files:string list ->
  prefix:string ->
  out:string ->
  (int, string) result
(** Renders each of [pages] with a [GET] through [handle] (status 200 needed)
    and writes it to [out] at {!page_file}. Renders [not_found] (status 404
    needed) to [404.html]. Writes each answer under [out ^ prefix], and each of
    [files] (public paths, such as the hashed assets) as the server answers it.
    Writes [_headers] (read by Cloudflare Pages and Netlify) with a one-year
    immutable cache for every file of [files] whose name is hashed. Gives the
    number of files written, or an error that names the page or file. *)
