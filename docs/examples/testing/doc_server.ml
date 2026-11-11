module Io = struct
  type 'a t = 'a

  let return value = value
  let bind value next = next value
  let sleep _ = ()
  let tasks : (unit -> unit) Queue.t = Queue.create ()
  let async task = Queue.add task tasks
  let now () = 0.

  type 'a promise = 'a option ref

  let promise () = ref None
  let resolve promise value = if !promise = None then promise := Some value

  let await promise ~timeout:_ =
    while not (Queue.is_empty tasks) do
      (Queue.take tasks) ()
    done;
    !promise
end

module Server = Silica_server.Make (Io)

let handlers =
  [
    Server.implement Tutorial_api.greeting (fun _context name ->
        Ok ("Hello, " ^ name)
    );
  ]

let config =
  Silica_server.default_config
    ~origins:(Silica_server.Origins [ "https://app.example" ])
    ~secret:"a secret of more than 16 bytes"

let server =
  Server.create config
    ~read:(fun _ -> None)
    ~handlers ~islands:[] ~topics:[]
    ~page:(fun (page : Server.page) ->
      Server.render_page_settled
        (fun _url -> Doc_hello.component)
        ~status:(fun _ -> 200)
        ~scripts:[] page
    )
    ()

let request ?(headers = []) ?(body = "") meth path =
  { Silica_server.meth; path; query = []; headers; body }

let after marker text =
  let length = String.length marker in
  let rec find i =
    if String.sub text i length = marker then
      i + length
    else
      find (i + 1)
  in
  let start = find 0 in
  String.sub text start (String.index_from text start '"' - start)

let visit () =
  let page = Server.handle server (request "GET" "/") in
  let cookie =
    List.hd (String.split_on_char ';' (List.assoc "set-cookie" page.headers))
  in
  (cookie, after "name=\"silica-csrf\" content=\"" page.body)

let call ?(origin = "https://app.example") ?token path body =
  let cookie, page_token = visit () in
  Server.handle server
    (request "POST" path ~body
       ~headers:
         [
           ("cookie", cookie);
           ("origin", origin);
           ("content-type", "application/json");
           ("x-silica-csrf", Option.value token ~default:page_token);
         ]
    )
