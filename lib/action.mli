(** Requests that a machine asks its host to carry out. App code builds {!t}
    values only. Timers and calls are not actions: they run during a state, in
    the [during] list of {!Machine.make}, and hosts see them as {!host} values
    that only the library builds. *)

type 'e t =
  | Focus of Ref.t
      (** Move keyboard focus to the element. The browser does not scroll to an
          element inside a portal: an anchor resource places a popup. *)
  | Blur of Ref.t
  | Scroll_into_view of Ref.t
      (** Scroll the element into view by the smallest distance, as
          [scrollIntoView({ block: "nearest" })]. The browser does it after the
          other actions and the resources of the same step, so an {!Anchor}
          places a new popup first and the page does not scroll to where the
          popup was before. *)
  | Scroll_to of { target : Ref.t option; left : int; top : int }
      (** Scroll the element with the ref [target], or the window when [target]
          is [None], so that the point ([left], [top]) of its content is at its
          top left corner. *)
  | Measure of { target : Ref.t; on_rect : Rect.t option -> 'e }
      (** Read the size and position of the element with the ref [target] in the
          viewport, and raise [on_rect] with it, or with [None] when no element
          has the ref. *)
  | Copy of { text : string; on_result : (bool -> 'e) option }
      (** Write [text] to the clipboard, and raise [on_result] with [true] when
          the write succeeded. *)
  | Load_script of { src : string; on_result : bool -> 'e }
      (** Load the script at the URL [src] and raise [on_result] with [true]
          when it ran, or with [false] when it failed to load. A [src] loads
          once per page: a second action for a loaded [src] raises
          [on_result true] at once, and one for a [src] that is still loading
          waits for the same load. *)
  | Navigate of Url.t
  | Replace of Url.t
  | Back
  | Forward
  | Reload
  | Resume of Nav.t
      (** Do this navigation past every {!Host.block}: the answer of a page that
          stopped a navigation with a block and then lets it happen. *)
  | Revert_navigation
      (** Undo the last URL change that the renderer reported: back one entry
          after a push, the old URL after a replace, the other way after a back
          or forward. The renderer does not report the undo. The router raises
          it when its guard answers [Cancel]. *)
  | Preload of Endpoint.request
      (** Fetch the call now and keep the answer for a later call with the same
          endpoint path and input (see {!Runtime}). The runtime handles it: a
          renderer never receives it. *)
  | Load_chunk of { chunk : Chunk.any; on_ready : 'e option }
      (** Start loading the chunk (see {!Chunk.load}) and raise [on_ready] when
          the load ends. {!Instance} raises it for a {!Node.val-chunk} whose
          value is not here; a router raises it, with no event, to preload the
          code of a route. The runtime handles it: a renderer never receives it.
      *)
  | Set_title of string
      (** Set the document title. A server render uses the last one. *)
  | Set_status of int
      (** The HTTP status of a server render. The server uses the last one, in
          place of the status that its [status] function gives. A browser
          ignores it. *)
  | Set_document_attribute of { name : string; value : string option }
      (** Set the attribute [name] of the document element ([<html>]) to
          [value], or remove it when [value] is [None]. A server render writes
          the last value of each name on its [<html>] tag, so the first HTML has
          it. [name] is lower case ASCII letters, digits and [-], and starts
          with a letter; a host raises [Invalid_argument] for another name. *)
  | Set_cookie of { name : string; value : string; max_age : int }
      (** Store the cookie [name] with [value] for [max_age] seconds, with
          [Path=/] and [SameSite=Lax], readable by scripts. [name] and [value]
          are ASCII letters, digits, [-], [_] and [.]; a host raises
          [Invalid_argument] for other text. A server render ignores it. *)
  | Remount
      (** End all the components that the view of this machine renders, children
          first. They start again, with new state, when the view renders them
          again. {!Instance} handles it: a host never receives it. *)

(** A held resource that runs during a state: it starts when the state is
    entered and stops when the state is left. Build them with the constructors
    in {!Host}. A renderer carries them out. [rate] limits how often the
    renderer delivers the event (see {!Rate}). *)
type 'e held =
  | Focus_trap of { container : Ref.t; return_focus : bool }
      (** Keep keyboard focus inside the element with the ref [container]: focus
          moves into it, Tab and Shift+Tab wrap, and focus that leaves it comes
          back. When [return_focus] is true, leaving the state gives focus back
          to the element that had it at the start. *)
  | Scroll_lock
      (** The page does not scroll while the state is active. Locks nest: the
          page scrolls again when the last one stops. *)
  | Outside_press of { container : Ref.t; event : 'e }
      (** A press that starts outside the element with the ref [container]
          raises [event]. *)
  | Global_keys of {
      bindings : (Key.t * 'e) list;
      prevent : bool;
      rate : Rate.t option;
    }
      (** A key press anywhere in the page that equals a key of [bindings]
          raises its event. With [prevent] the default behavior of that key is
          prevented. A key that an element handler prevented does not count.
          While focus is in a text field (an input, a textarea, a select or
          editable content), only keys with Ctrl or Meta, and Escape, count:
          other keys are text. *)
  | Resize of { target : Ref.t; rate : Rate.t option; on_resize : Rect.t -> 'e }
      (** The element with the ref [target] changes size. *)
  | Intersection of {
      target : Ref.t;
      rate : Rate.t option;
      on_change : bool -> 'e;
    }  (** The element with the ref [target] enters or leaves the viewport. *)
  | Scroll of {
      target : Ref.t option;
      rate : Rate.t option;
      on_scroll : Scroll.t -> 'e;
    }
      (** The element with the ref [target], or the page when [target] is
          [None], scrolls. The renderer raises [on_scroll] once at the start
          with the position then, and again for each scroll. *)
  | Visibility of (bool -> 'e)
      (** The document becomes visible ([true]) or hidden ([false]), for example
          when the tab changes. Raised once at the start with the value then. *)
  | Online of (bool -> 'e)
      (** The browser goes online ([true]) or offline ([false]). Raised once at
          the start with the value then. *)
  | Media of { query : string; on_change : bool -> 'e }
      (** The media query [query], such as ["(prefers-color-scheme: dark)"],
          starts ([true]) or stops ([false]) to match. Raised once at the start
          with the value then. *)
  | Dismiss of {
      containers : Ref.t list;
      on_escape : 'e option;
      on_outside : 'e option;
      on_focus_out : 'e option;
    }
      (** A dismiss layer of a popup. [on_escape] is raised by an Escape key
          press in the page, but only for the layer that started last of those
          with an [on_escape]. [on_outside] is raised by a press that starts
          outside every element of [containers], unless the press is inside a
          layer that started later (a nested popup). [on_focus_out] is raised
          when focus moves to an element outside every element of [containers],
          with the same rule for a later layer. A [Focus_trap] is a layer too.
      *)
  | Block of { on_attempt : Nav.t -> 'e }
      (** Stop every navigation (a link press, {!Navigate}, {!Replace}, back and
          forward) before the URL changes, and raise [on_attempt] with it
          instead. When more than one block is held, the one that started last
          gets it. {!Resume} does the navigation past every block. While a block
          is held, closing or reloading the page asks the user first. *)
  | Anchor of { anchor : Ref.t; popup : Ref.t; place : Place.t }
      (** Keep the element with the ref [popup] next to the element with the ref
          [anchor], as {!Place.compute} says, while the page scrolls or either
          element changes size. The renderer writes the position, the
          [data-side] and [data-align] attributes and the CSS variables
          [--anchor-width], [--anchor-height], [--available-width] and
          [--available-height] on the popup, so its view must not set [style].
      *)

val check : 'e t -> unit
(** Raises [Invalid_argument] for a {!Set_document_attribute} or {!Set_cookie}
    with a name or value outside the rules of those actions. Hosts call it
    before they run either. *)

val held_name : 'e held -> string
(** ["focus_trap"], ["scroll_lock"], ["outside_press"], ["keys"], ["resize"],
    ["intersection"], ["dismiss"], ["anchor"], ["scroll"], ["visibility"],
    ["online"], ["media"] or ["block"]. *)

val map_held : ('a -> 'b) -> 'a held -> 'b held
(** Change the events that a resource sends. *)

val map_refs : (Ref.t -> Ref.t) -> 'e held -> 'e held
(** The same resource with each ref replaced. *)

(** What a machine gives to its host. Only {!Machine} and {!Instance} build
    [Start_timer], [Start_call] and [Stop], so no timer or call lives outside
    the state that it runs during. *)
type 'e host =
  | Run of 'e t
  | Start_held of { id : string; held : 'e held }
      (** Start the held resource. A [Stop] with the same id ends it. Raised
          when a state starts to own that resource. *)
  | Start_timer of { id : string; ms : int; event : 'e }
      (** Raise [event] after [ms] milliseconds, unless stopped first. Raised
          when a state starts to own a {!Host.after} timer. *)
  | Start_call : {
      id : string;
      endpoint : ('i, 'o, 'x) Endpoint.t;
      input : 'i;
      timeout_ms : int option;
      on_result : ('o, 'x) Endpoint.answer -> 'e;
    }
      -> 'e host
      (** Call the endpoint and raise [on_result] with the answer, unless
          stopped first. Raised when a state starts to own a {!Remote.call}.
          When [timeout_ms] passes before the answer, the host raises
          [on_result (Failed (Network "timeout"))] and drops the answer. *)
  | Start_live : {
      id : string;
      topic : 's Live_topic.t;
      key : string;
      on_update : 's Live_topic.update -> 'e;
    }
      -> 'e host
      (** Subscribe to the topic under the key and raise [on_update] for each
          state, and for each change of the connection (see {!Topic.update}),
          unless stopped first. Raised when a state with a {!Topic.subscribe} is
          entered. A [Stop] with the same id ends the subscription. *)
  | Stop of string
      (** Stop the timer, call or subscription with this id. A stopped timer
          never fires and the answer of a stopped call is dropped. Stopping an
          id that is not running does nothing. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Change the events of an action, for example to put a child action into a
    parent machine. *)

val map_host : ('a -> 'b) -> 'a host -> 'b host
(** Change the events of a host action, as {!map} does. *)
