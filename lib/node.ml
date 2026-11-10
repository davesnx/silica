type ('p, 'e) handler = { run : 'p -> 'e option; prevent : bool }

type error = { id : string; message : string; detail : string option }

type 'e attr =
  | String of string * string
  | Bool of string * bool
  | Ref of Ref.t
  | On_press of 'e
  | On_double_press of 'e
  | On_input of (string -> 'e)
  | On_submit of 'e
  | On_focus of 'e
  | On_blur of 'e
  | On_key_down of (Key.t, 'e) handler
  | On_key_up of (Key.t, 'e) handler
  | On_pointer_enter of (Pointer.t, 'e) handler
  | On_pointer_leave of (Pointer.t, 'e) handler
  | On_reset
  | Rated of Rate.t * 'e attr

type ('s, 'e) component = { machine : ('s, 'e) Machine.t; view : 's -> 'e t }

and 'e t =
  | Element of string * 'e attr list * 'e t list
  | Text of string
  | List of 'e t list
  | Keyed of string * 'e t
  | Portal of string option * 'e t list
  | Boundary of 'e boundary
  | Component : ('s, 'ce, 'e) child -> 'e t
  | Chunk of 'e chunk

and 'e chunk =
  | Chunk_view : {
      chunk : 'a Chunk.t;
      loading : 'e t;
      render : 'a -> 'e t;
    }
      -> 'e chunk

and 'e boundary = { fallback : error -> reset:'e attr -> 'e t; body : 'e t }

and ('s, 'ce, 'e) child = {
  key : string option;
  component : ('s, 'ce) component;
  props : 'ce option;
  output : 'ce -> 's -> 'e list;
  on_final : 's -> 'e list;
}

type 'e shape = 'e t

let element tag attrs children = Element (tag, attrs, children)
let text s = Text s
let list l = List l
let keyed k n = Keyed (k, n)
let portal ?target children = Portal (target, children)
let boundary ~fallback body = Boundary { fallback; body }

let component ?key ?props ?(output = fun _ _ -> []) ?(on_final = fun _ -> [])
    component =
  Component { key; component; props; output; on_final }

let chunk chunk ~loading render = Chunk (Chunk_view { chunk; loading; render })

exception Chunk_failed of string

let chunk_view (Chunk_view { chunk; loading; render }) =
  match Chunk.get chunk with Some value -> render value | None -> loading

let view t = t

let map_handler f { run; prevent } =
  { run = (fun p -> Option.map f (run p)); prevent }

let rec map_attr f : _ attr -> _ attr = function
  | String (k, v) ->
      String (k, v)
  | Bool (k, v) ->
      Bool (k, v)
  | Ref r ->
      Ref r
  | On_press e ->
      On_press (f e)
  | On_double_press e ->
      On_double_press (f e)
  | On_input g ->
      On_input (fun s -> f (g s))
  | On_submit e ->
      On_submit (f e)
  | On_focus e ->
      On_focus (f e)
  | On_blur e ->
      On_blur (f e)
  | On_key_down h ->
      On_key_down (map_handler f h)
  | On_key_up h ->
      On_key_up (map_handler f h)
  | On_pointer_enter h ->
      On_pointer_enter (map_handler f h)
  | On_pointer_leave h ->
      On_pointer_leave (map_handler f h)
  | On_reset ->
      On_reset
  | Rated (rate, attr) ->
      Rated (rate, map_attr f attr)

let rec map f : _ t -> _ t = function
  | Element (tag, attrs, children) ->
      Element (tag, List.map (map_attr f) attrs, List.map (map f) children)
  | Text s ->
      Text s
  | List l ->
      List (List.map (map f) l)
  | Keyed (k, n) ->
      Keyed (k, map f n)
  | Portal (target, children) ->
      Portal (target, List.map (map f) children)
  | Boundary { fallback; body } ->
      Boundary
        {
          fallback =
            (fun error ~reset:_ -> map f (fallback error ~reset:On_reset));
          body = map f body;
        }
  | Chunk (Chunk_view { chunk; loading; render }) ->
      Chunk
        (Chunk_view
           {
             chunk;
             loading = map f loading;
             render = (fun v -> map f (render v));
           }
        )
  | Component { key; component; props; output; on_final } ->
      Component
        {
          key;
          component;
          props;
          output = (fun event state -> List.map f (output event state));
          on_final = (fun state -> List.map f (on_final state));
        }
