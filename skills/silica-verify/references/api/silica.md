# API: the library silica

## Silica.Accessibility

The role, the accessible name and the states of the elements of a view, computed from the view as data. The checker, `View_text` and the queries of `Silica_memory` use this one definition, so "the button named Save" means the same element in each tool.

The computation follows WAI-ARIA 1.2, HTML-AAM and the Accessible Name and Description Computation 1.2, for the subset of HTML that Silica views use:

**Role.** An explicit `role` attribute wins: its first token that is a `role` is used, and `none` or `presentation` gives no role. Otherwise the tag gives the role: `button`; `a` and `area` with `href` (`Link`); `h1` to `h6` (`Heading`); `ul`, `ol`, `menu` (`List`); `li` (`Listitem`); `input` by its `type` (`button`, `submit`, `reset`, `image`: `Button`; `checkbox`; `radio`; `range`: `Slider`; `number`: `Spinbutton`; `search`: `Searchbox`; `text`, `email`, `tel`, `url` or no type: `Textbox`; `Combobox` for a text or search field with `list`; other types: no role); `textarea` (`Textbox`); `select` (`Combobox`, or `Listbox` with `multiple` or `size` above 1); `option`; `optgroup`, `fieldset`, `details` (`Group`); `img` (`Img`, no role with `alt=""`); `nav`, `main`, `aside`, `form`, `search`, `dialog`; `header` and `footer` (`Banner`, `Contentinfo`) when no `article`, `aside`, `main`, `nav` or `section` is above them; `section` (`Region`) when it has a name; `table`, `tr`, `td` (`Cell`), `th` (`Columnheader`, or `Rowheader` with `scope="row"`); `hr` (`Separator`); `progress` (`Progressbar`); `output` (`Status`). Elements that ARIA maps to `generic` (`div`, `span`, `a` with no `href`, and the rest) have no role here; `role="generic"` gives `Generic`.

**Name.** The first of these that is not empty, with white space collapsed and trimmed:

1. `aria-labelledby`: the names of the elements with those ids, joined by a space. A referenced element counts even when it is hidden, and its own `aria-labelledby` is not followed.
2. `aria-label`.
3. For a field (`input` that is not a button, `textarea`, `select`): the text of each `label` whose `for` is its id, then of the `label` around it. For an `input` button: `value`, or `"Submit"` and `"Reset"` for those types. For `img`, `area` and an `input` image: `alt`. For `fieldset`: its `legend` child. For `table`: its `caption` child.
4. The content, for the roles that take their name from it (see `name_from_content`), and for every element reached while the content of another element is read: the text and the names of the children, in order, with no space added between them. Hidden children give nothing. A field inside the content gives nothing (ARIA would use its value).
5. `title`.
6. `placeholder`, for `input` and `textarea`.

**Hidden.** An element is hidden when it or an element above it has `hidden`, `inert` or `aria-hidden="true"`, or when it is an `input` of type `hidden`. CSS (`display: none`) and closed `details` are not seen.

**Tree.** Component nodes have no elements (a view of a parent shows its children as `Component`); a chunk shows its value, or `Loading`; a boundary shows its body. The children of a portal are elements with nothing above them, as in the page, where they are painted at the end of the body.

```ocaml
type role =
  | Alert
  | Alertdialog
  | Banner
  | Button
  | Cell
  | Checkbox
  | Columnheader
  | Combobox
  | Complementary
  | Contentinfo
  | Dialog
  | Form
  | Generic
  | Grid
  | Gridcell
  | Group
  | Heading
  | Img
  | Link
  | List
  | Listbox
  | Listitem
  | Main
  | Menu
  | Menubar
  | Menuitem
  | Menuitemcheckbox
  | Menuitemradio
  | Navigation
  | Option
  | Progressbar
  | Radio
  | Radiogroup
  | Region
  | Row
  | Rowheader
  | Search
  | Searchbox
  | Separator
  | Slider
  | Spinbutton
  | Status
  | Switch
  | Tab
  | Table
  | Tablist
  | Tabpanel
  | Textbox
  | Toolbar
  | Tooltip
  | Tree
  | Treeitem
```

```ocaml
val role_of_string : string -> role option
```

The role with this ARIA name, such as `"button"`.

```ocaml
val role_to_string : role -> string
```

The ARIA name of the role, such as `"button"`.

```ocaml
val name_from_content : role -> bool
```

The role takes its name from its content when nothing else names it, such as `Button`, `Link`, `Heading`, `Option` and `Tab`.

```ocaml
type 'e element
```

An element of a view, with the elements above it and the document it is in.

```ocaml
type 'e item =
  | Element of 'e element
  | Text of string  (** A text node that is not empty. *)
  | Component of string  (** A component node, with the id of its machine. *)
  | Loading
```

```ocaml
val items : 'e Node.t -> 'e item list
```

The top of the view, in view order. The children of a portal are in place of the portal.

```ocaml
val children : 'e element -> 'e item list
```

The children of the element, in view order, as `items` gives them.

```ocaml
val elements : 'e Node.t -> 'e element list
```

Every element of the view in document order: first the elements outside portals, then the elements of each portal. A portal inside a portal comes before the portal around it.

```ocaml
val tag : 'e element -> string
```

The tag name.

```ocaml
val attributes : 'e element -> 'e Node.attr list
```

The attributes, as the view gives them.

```ocaml
val attribute : 'e element -> string -> string option
```

The value of an attribute; `Some ""` for a `true` boolean attribute.

```ocaml
val role : 'e element -> role option
```

The explicit `role`, else the role of the tag.

```ocaml
val name : 'e element -> string
```

The accessible name.

```ocaml
val hidden : 'e element -> bool
```

The element is not in the accessibility tree.

```ocaml
val path : 'e element -> string
```

Where the element is: the tags from the top down, joined by `" > "`, each with its position among the children of the same tag when there are several, such as `"main > form > button[2]"`. The children of a portal start with `"portal > "`.

**States**

```ocaml
val level : 'e element -> int option
```

`aria-level`, else the level of an `h1` to `h6`.

```ocaml
val checked : 'e element -> bool option
```

`aria-checked` `"true"` or `"false"`, else `checked` on an `input` checkbox or radio. `None` when the element cannot be checked, and for `aria-checked="mixed"`.

```ocaml
val selected : 'e element -> bool option
```

`aria-selected` `"true"` or `"false"`, else `selected` on an `option`.

```ocaml
val expanded : 'e element -> bool option
```

`aria-expanded` `"true"` or `"false"`.

```ocaml
val disabled : 'e element -> bool
```

`aria-disabled="true"`, or `disabled` on a `button`, `input`, `select`, `textarea`, `option`, `optgroup` or `fieldset`.

```ocaml
val current : 'e element -> string option
```

`aria-current`, when it is not `"false"`.

```ocaml
val value : 'e element -> string option
```

`aria-valuetext`, else `aria-valuenow`, else the `value` of an `input`, `textarea` or `select`.

```ocaml
val modal : 'e element -> bool
```

`aria-modal="true"`.

## Silica.Action

Requests that a machine asks its host to carry out. App code builds `t` values only. Timers and calls are not actions: a state owns them through the `owns` list of `Machine.make`, and hosts see them as `host` values that only the library builds.

```ocaml
type 'e t =
  | Focus of Ref.t
      (** Move keyboard focus to the element. The browser does not scroll to an
          element inside a portal: an anchor source places a popup. *)
  | Blur of Ref.t
  | Scroll_into_view of Ref.t
      (** Scroll the element into view by the smallest distance, as
          [scrollIntoView({ block: "nearest" })]. The browser does it after the
          other actions and the sources of the same step, so an {!Anchor} places
          a new popup first and the page does not scroll to where the popup was
          before. *)
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
      (** Do this navigation past every {!Source.block}: the answer of a page
          that stopped a navigation with a block and then lets it happen. *)
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
```

```ocaml
type 'e source =
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
```

A held resource that a state owns: it starts when the state is entered and stops when the state is left. Build them with the constructors in `Source`. A renderer carries them out. `rate` limits how often the renderer delivers the event (see `Rate`).

```ocaml
val check : 'e t -> unit
```

Raises `Invalid_argument` for a `Set_document_attribute` or `Set_cookie` with a name or value outside the rules of those actions. Hosts call it before they run either.

```ocaml
val source_name : 'e source -> string
```

`"focus_trap"`, `"scroll_lock"`, `"outside_press"`, `"keys"`, `"resize"`, `"intersection"`, `"dismiss"`, `"anchor"`, `"scroll"`, `"visibility"`, `"online"`, `"media"` or `"block"`.

```ocaml
val map_source : ('a -> 'b) -> 'a source -> 'b source
```

Change the events that a source sends.

```ocaml
val map_refs : (Ref.t -> Ref.t) -> 'e source -> 'e source
```

The same source with each ref replaced.

```ocaml
type 'e host =
  | Run of 'e t
  | Start_source of { id : string; source : 'e source }
      (** Start the held resource. A [Stop] with the same id ends it. Raised
          when a state starts to own that source. *)
  | Start_timer of { id : string; ms : int; event : 'e }
      (** Raise [event] after [ms] milliseconds, unless stopped first. Raised
          when a state starts to own a {!Source.after} timer. *)
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
      topic : 's Topic.t;
      key : string;
      on_update : 's Topic.update -> 'e;
    }
      -> 'e host
      (** Subscribe to the topic under the key and raise [on_update] for each
          state, and for each change of the connection (see {!Topic.update}),
          unless stopped first. Raised when a state with a {!Source.live} is
          entered. A [Stop] with the same id ends the subscription. *)
  | Stop of string
```

What a machine gives to its host. Only `Machine` and `Instance` build `Start_timer`, `Start_call` and `Stop`, so no timer or call lives outside the state that owns it.

```ocaml
val map : ('a -> 'b) -> 'a t -> 'b t
```

Change the events of an action, for example to put a child action into a parent machine.

```ocaml
val map_host : ('a -> 'b) -> 'a host -> 'b host
```

Change the events of a host action, as `map` does.

## Silica.Answers

The answers of Remote calls that the server got while it rendered a page. The server writes them into the page as JSON. The client answers the same calls from this table before its first paint, so no call runs twice and the first render has the same state as the server HTML.

```ocaml
type t
```

```ocaml
val empty : t
```

A table with no answers.

```ocaml
val add : t -> path:string -> input:string -> output:string -> t
```

`input` and `output` are the encoded JSON bodies of the call (see `Codec.to_string`). Adding the same `path` and `input` again replaces the answer.

```ocaml
val is_empty : t -> bool
```

`true` when the table has no answers.

```ocaml
val find : t -> path:string -> input:string -> string option
```

The encoded output of a call.

```ocaml
val to_list : t -> (string * string * string) list
```

The answers as `(path, input, output)`, in the order they were added.

```ocaml
val remove : t -> path:string -> input:string -> t
```

The client uses each answer once.

```ocaml
val to_script : ?nonce:string -> t -> string
```

An HTML element `<script type="application/json" id="silica-answers">` with the table as JSON. The character `<` is written as `<`, so the text cannot end the script element or start a comment. A `nonce` that is not empty becomes the `nonce` attribute; an empty one writes no attribute.

```ocaml
val script_id : string
```

`"silica-answers"`.

```ocaml
val of_string : string -> (t, string) result
```

Read the JSON text that `to_script` put into the element.

## Silica.Attr

Attribute constructors.

```ocaml
val string : string -> string -> 'e Node.attr
```

`string name value` sets the attribute `name` to `value`.

```ocaml
val bool : string -> bool -> 'e Node.attr
```

`bool name b` sets the attribute `name` with no value when `b` is `true`, and leaves it out when `b` is `false`.

```ocaml
val ref_ : Ref.t -> 'e Node.attr
```

Connects the element to a `Ref.t`, so the machine can read its size and position or move focus to it.

```ocaml
val style : string -> 'e Node.attr
```

Sets the `style` attribute to the given CSS text.

```ocaml
val on_press : 'e -> 'e Node.attr
```

A press (click, or Enter or Space on a button) on the element sends the event.

```ocaml
val on_double_press : 'e -> 'e Node.attr
```

A double press on the element sends the event.

```ocaml
val on_input : ?rate:Rate.t -> (string -> 'e) -> 'e Node.attr
```

Each change of the field value sends the event made from the new text.

```ocaml
val on_submit : 'e -> 'e Node.attr
```

A form submit sends the event. The browser does not load a new page.

```ocaml
val on_focus : 'e -> 'e Node.attr
```

The element gets focus.

```ocaml
val on_blur : 'e -> 'e Node.attr
```

The element loses focus.

```ocaml
val rated : Rate.t -> 'e Node.attr -> 'e Node.attr
```

Rate limit any event handler attribute.

```ocaml
val on_key_down :
  ?prevent:bool -> ?rate:Rate.t -> (Key.t -> 'e option) -> 'e Node.attr
```

A key goes down while the element or a descendant has focus. With `prevent`, the default behavior of the key is cancelled when the handler returns `Some _`.

```ocaml
val on_key_up :
  ?prevent:bool -> ?rate:Rate.t -> (Key.t -> 'e option) -> 'e Node.attr
```

A key goes up while the element or a descendant has focus. `prevent` works as in `on_key_down`.

```ocaml
val on_pointer_enter :
  ?prevent:bool -> ?rate:Rate.t -> (Pointer.t -> 'e option) -> 'e Node.attr
```

The pointer moves onto the element.

```ocaml
val on_pointer_leave :
  ?prevent:bool -> ?rate:Rate.t -> (Pointer.t -> 'e option) -> 'e Node.attr
```

The pointer moves off the element.

```ocaml
val on_reset : 'e Node.attr
```

See `Node.On_reset`.

## Silica.Backoff

*For hosts: the browser runtime, `Silica_memory` and the server use it. App code does not call it.*

Delays between reconnect attempts of a Live socket.

```ocaml
val delay_ms : attempt:int -> jitter:float -> int
```

The wait before attempt number `attempt` (0 is the first retry). The base delay is 100 ms, doubled for each attempt, capped at 5000 ms. `jitter` is a value from 0 to 1 that adds up to 25 percent. The result never exceeds 5000 ms plus that 25 percent, and does not fall under the base delay.

```ocaml
val cap_ms : int
```

The cap of the base delay, `5000` ms.

## Silica.Chunk

A value whose code may arrive later: the page of a lazy route.

On a native server the code is linked in, so a chunk is `ready`. In the browser, `deferred` wraps a loader such as a dynamic import (see `Silica_browser.import`): the bundler puts the code in its own file, and the file loads the first time a view shows the chunk with `Node.chunk`, or earlier when a link preloads its route (`Router.routes`). The same shared view code uses either, because each platform library defines the module that holds the chunks.

```ocaml
type 'a t
```

```ocaml
val ready : 'a -> 'a t
```

A chunk whose value is here.

```ocaml
val deferred : ((('a, string) result -> unit) -> unit) -> 'a t
```

A chunk that a loader brings. The loader runs at the first `load`, at most once at a time, and calls its argument once with the value or an error message. After an error, the next `load` runs the loader again.

```ocaml
val get : 'a t -> 'a option
```

The value, when it is here.

```ocaml
val load : 'a t -> (unit -> unit) -> unit
```

Start the loader, or join the load in progress, and call the function once when the load ends, with a value or with an error. It is called at once when the value is here or the last load failed.

```ocaml
val failure : 'a t -> string option
```

The message of the last load, when it failed.

```ocaml
val forget_failure : 'a t -> unit
```

Make a failed chunk loadable again: `failure` gives `None` and the next `load` runs the loader.

```ocaml
type any = Any : 'a t -> any
```

## Silica.Class

Class names for `class_`, with conditions.

Write each class as a complete string literal, so that the Tailwind scan of the source files finds it. A name built at run time, such as `"bg-" ^ colour`, is not found and gets no CSS.

```ocaml
val join : string list -> string
```

The non-empty strings, separated by one space. A condition is an empty string: `Class.join [ "row"; (if active then "bg-gray-100" else "") ]`.

## Silica.Codec

Encoders and decoders to JSON, identical on native and Melange.

```ocaml
type 'a t = {
  encode : 'a -> Silica_json.Json.t;
  decode : Silica_json.Json.t -> ('a, string) result;
}
```

```ocaml
val make :
  encode:('a -> Silica_json.Json.t) ->
  decode:(Silica_json.Json.t -> ('a, string) result) ->
  'a t
```

A codec from an encoder and a decoder.

```ocaml
val string : string t
```

A JSON string.

```ocaml
val int : int t
```

A JSON number with no fraction.

```ocaml
val bool : bool t
```

A JSON boolean.

```ocaml
val float : float t
```

A JSON number.

```ocaml
val unit : unit t
```

JSON `null`.

```ocaml
val list : 'a t -> 'a list t
```

A JSON array; each item goes through the item codec.

```ocaml
val option : 'a t -> 'a option t
```

`None` is JSON `null`. `Some v` is the encoding of `v`, so `Some None` and `None` give the same JSON.

```ocaml
val pair : 'a t -> 'b t -> ('a * 'b) t
```

A JSON array of two items.

```ocaml
val to_string : 'a t -> 'a -> string
```

The JSON text of the value.

```ocaml
val of_string : 'a t -> string -> ('a, string) result
```

Parse JSON text and decode it. `Error` holds the parse or decode error.

```ocaml
val constructor : string -> Silica_json.Json.t list -> Silica_json.Json.t
```

`constructor "Saved" [ json ]` is `["Saved", json]` and `constructor "Retry" []` is `["Retry"]`: a variant constructor as `[@@deriving json]` writes it.

```ocaml
val constructor_of :
  Silica_json.Json.t -> (string * Silica_json.Json.t list, string) result
```

The name and the arguments of a constructor written by `constructor`.

```ocaml
val field : string -> 'a t -> Silica_json.Json.t -> ('a, string) result
```

Decode the field of a JSON object with the codec.

## Silica.Component

A machine together with the view of its states. Each rendered component runs its own machine while it is rendered, keyed by its position, its machine id and an optional key. The parent gives props to the child, and the child gives output to the parent through function props.

A component with props is written as a module with a `make` function, so that it works as a tag in `.mlx`:

```
  let component = Component.make_with_props machine view

  let make ?key ~id ~on_pick () =
    Component.node_with_props ?key component ~props:id
      ~output:(fun event _ ->
        match event with Picked hit -> [ on_pick hit ] | _ -> []
    )
```

and is used as `<Row id on_pick=(fun hit -> Pick hit) />`.

```ocaml
type ('p, 's, 'e) t = ('p, 's, 'e) Node.component = {
  machine : ('p, 's, 'e) Machine.t;
  view : 'p -> 's -> 'e Node.t;
}
```

```ocaml
val make : ('p, 's, 'e) Machine.t -> ('s -> 'e Node.t) -> ('p, 's, 'e) t
```

`make machine view` is a component whose view does not read the props. Define it once, at the top level.

```ocaml
val make_with_props :
  ('p, 's, 'e) Machine.t -> ('p -> 's -> 'e Node.t) -> ('p, 's, 'e) t
```

`make_with_props machine view`: the view gets the props of the moment.

```ocaml
val node :
  ?key:string ->
  ?output:('ce -> 's -> 'e list) ->
  ?on_final:('s -> 'e list) ->
  (unit, 's, 'ce) t ->
  'e Node.t
```

The node that renders a component with no props (a machine made by `Machine.make`). See `Node.child`.

```ocaml
val node_with_props :
  ?key:string ->
  props:'p ->
  ?output:('ce -> 's -> 'e list) ->
  ?on_final:('s -> 'e list) ->
  ('p, 's, 'ce) t ->
  'e Node.t
```

The node that renders the component, with its props (`()` for a machine made by `Machine.make`). See `Node.child`.

## Silica.Early_input

Presses made on a page before its code runs.

The server puts `script` in the head of each page that loads scripts. Until the browser host mounts, it keeps each click on the page, except on links, form fields, labels and summaries (the browser does those itself), and each form submit made with the Enter key, which it stops. When the host has mounted and painted, it presses them again, in order, against the adopted page: at most 16, and none older than 10 seconds.

```ocaml
val script : string
```

The JavaScript of the script element, with no `<script>` tag.

## Silica.Endpoint

The types of `Remote`, in a module below `Source` and `Action`. Use them through `Remote`.

```ocaml
type ('i, 'o, 'x) t
```

An endpoint with input `'i`, output `'o` and application error `'x`.

```ocaml
type error =
  | Http of int  (** The server answered with this status. *)
  | Decode of string  (** The answer did not decode. *)
  | Network of string
```

```ocaml
type ('o, 'x) answer =
  | Ok of 'o  (** The output. *)
  | Rejected of 'x
      (** The handler answered [Error x]: an application error, such as a failed
          validation. *)
  | Failed of error
```

```ocaml
val declare :
  path:string ->
  input:'i Codec.t ->
  output:'o Codec.t ->
  rejected:'x Codec.t ->
  ('i, 'o, 'x) t
```

```ocaml
val path : ('i, 'o, 'x) t -> string
```

```ocaml
val input : ('i, 'o, 'x) t -> 'i Codec.t
```

```ocaml
val output : ('i, 'o, 'x) t -> 'o Codec.t
```

```ocaml
val rejected : ('i, 'o, 'x) t -> 'x Codec.t
```

```ocaml
val answer_id : ('i, 'o, 'x) t -> ('o * 'x) Type.Id.t
```

```ocaml
type request = Request : ('i, 'o, 'x) t * 'i -> request
```

```ocaml
val error_codec : error Codec.t
```

```ocaml
val answer_codec : 'o Codec.t -> 'x Codec.t -> ('o, 'x) answer Codec.t
```

## Silica.Fingerprint

*For development tools only: hot reload under `silica dev` uses it. App code and tests do not call it.*

The fingerprint of a type: what a hot reload compares before it gives a snapshot of the old page to the new code (see `Handoff`).

Two equal fingerprints mean that the two builds compiled the same declarations, with the same compiler, for the type and every type it mentions. The derivers of `silica.ppx` make them: `[@@deriving state_shape]` gives `state_fingerprint` next to `state_shape` and puts it in the shape; `[@@deriving fingerprint]` gives one to any other type, so that a state type in another file can mention it. A type with no fingerprint can not be handed over: a machine whose state mentions it starts again at a hot reload, and the reason names the type.

A state type that `state_shape` does not take, such as a record, gets its fingerprint from `[@@deriving fingerprint]`, and the shape written by hand passes it: `Machine.shape ~name:(fun _ -> "todo") ~fingerprint:state_fingerprint ()`. A shape written by hand with no `~fingerprint` starts again at each hot reload, unless its initial snapshot equals the old one (see `Handoff.hook`).

A fingerprint written by hand, such as the one of a type made by a functor, goes in `[%fingerprint ...]`: the expression in a development build, `off` in a release build, so a release bundle computes nothing.

The derivers give fingerprints only in a development build: the release profile of a project sets `SILICA_FINGERPRINTS=false` for them (see the root `dune` file), so a release bundle holds none; there `[@@deriving fingerprint]` gives `off`.

```ocaml
type part =
  | Declared of string * part list
      (** The digest of the printed declarations that a deriver saw, with the
          fingerprints of the types from other modules that they mention. *)
  | Silica of string * part list
      (** A type of Silica, named as in ["Remote.error"], with the fingerprints
          of its parameters. It stands for the interfaces of this build of
          Silica and the Melange version. *)
  | Unknown of string
      (** A type that has no fingerprint, named as it is written. *)
  | Refused of string
```

```ocaml
type 'a t = Fingerprint of part [@@unboxed]
```

The fingerprint of the type `'a`. The type parameter lets the compiler check that a fingerprint found by name is the one of the type with that name.

```ocaml
val part : 'a t -> part
```

The fingerprint with no type, as a part of another one.

```ocaml
val declared : string -> part list -> 'a t
```

`declared digest parts`, for the derivers.

```ocaml
val unknown : string -> 'a t
```

The fingerprint of a type that has none, named as it is written.

```ocaml
val refused : string -> 'a t
```

The fingerprint of a type that can not be handed over, such as a function. The text says why, as in `"its state type holds a function"`.

```ocaml
val off : 'a t
```

What `[@@deriving fingerprint]` gives in a release build, where fingerprints are off: it gives no digest. A shape written by hand can then pass `~fingerprint:state_fingerprint` in every build.

```ocaml
val digest : 'a t -> (string, string) result
```

The text that two builds compare, or the reason there is none, as in `"its state type mentions Issue.t, which has no fingerprint"`.

## Silica.Handoff

*For development tools only: hot reload under `silica dev` uses it. App code and tests do not call it.*

What a hot reload hands from the running page to the new code: the snapshot of each mounted instance, by its instance path, with the fingerprint of its state type (see `Fingerprint`), the instance ids of the Live islands, and the URL.

Only development code uses this module: `silica dev` and its browser hook, and tests. A release bundle does not reference it.

```ocaml
type entry = {
  machine : string;  (** The {!Machine.id}. *)
  fingerprint : (string, string) result option;
      (** The {!Fingerprint.digest} of the state type, or [None] when the shape
          has no fingerprint. *)
  snapshot : Obj.t;  (** The snapshot, as the old code made it. *)
}
```

```ocaml
type t = {
  instances : (string * entry) list;  (** By instance path. *)
  islands : (string * string) list;
      (** The channel and the instance id of each Live island, in page order. *)
  url : Url.t;
}
```

```ocaml
val of_running :
  islands:(string * string) list -> url:Url.t -> Instance.running list -> t
```

The handoff of the instances that `Instance.running` gives.

```ocaml
type reason =
  | Other_machine of string
      (** Another machine, with this id, ran at the path before. *)
  | No_fingerprint  (** The shape of the machine has no fingerprint. *)
  | Not_handed_over of string
      (** The fingerprint of the new code gives this reason (see
          {!Fingerprint.digest}). *)
  | Changed  (** The fingerprints differ: the state type changed. *)
  | Holds_function
      (** The snapshot holds a function, which would run the old code. *)
  | Render_failed of string
```

```ocaml
val reason_text : reason -> string
```

As in `"its state type changed"`.

```ocaml
val take :
  holds_function:(Obj.t -> bool) ->
  t ->
  path:string ->
  ('p, 's, 'e) Machine.t ->
  ('s, reason) result option
```

`take ~holds_function handoff ~path machine` is the snapshot that the instance at `path` had, for `machine` in the new code: `None` when no instance ran at the path, `Error` when the snapshot can not be given.

This function holds the only cast of a hot reload, from `Obj.t` to `'s`. It happens only when the machine id at the path is the same, both fingerprints are `Ok` and equal, and `holds_function` finds no function in the snapshot. Why that is safe: an equal fingerprint is the same digest of the same printed declarations of the state type and of every type it mentions, transitively, made by the same deriver with the same Melange and OCaml versions (see `Fingerprint`). The deriver prints the types declared in the same file, and checks at compile time, with a re-export, that each one is the type with that name where the state type is declared; it takes the fingerprint of a type of another module from that module, found by name and checked by the type parameter of `Fingerprint.t`; a type of Silica counts by the digest of the interfaces of Silica. A type that it can not see (abstract, extensible, from an `open`, or from a module with no fingerprint for it), and functions, objects, `exn` and `lazy`, give no digest, so their machines are never given a snapshot. Two builds that compiled the same declarations with the same compiler give the values of that type the same JavaScript representation, so the snapshot of the old build is a valid value of `'s` in the new one. A function inside it would still be code of the old build, which `holds_function` refuses.

```ocaml
type restarted = { path : string; machine : string; reason : reason }
```

```ocaml
type outcome = { kept : string list; restarted : restarted list }
```

The instances that resumed, by path, and the ones that started again. An instance with no entry in the handoff is in neither list.

```ocaml
val hook :
  holds_function:(Obj.t -> bool) -> t -> Instance.resume * (unit -> outcome)
```

The `Instance.resume` of the handoff, and what it did so far. An instance whose path is not in the handoff takes the entry of the same machine id when the handoff has exactly one, not taken yet: a view edit that moves a component keeps its state.

When a snapshot can not be given because the machine has no fingerprint, or its fingerprint gives a reason, the instance starts from its initial snapshot (with `Machine.start` on its props, as for a new instance); when that snapshot is structurally equal to the old one, the instance counts as kept and resumes there, with no entry. The old value is only compared, never used: a router, whose state comes from the URL, keeps its route.

## Silica.Instance

*For hosts: the browser runtime, `Silica_memory` and the server use it. App code does not call it.*

The instance tree: what runs while components are rendered.

A tree has a root component. Rendering a view that holds component nodes (see `Node.component`) mounts, updates and unmounts child instances:

- A child is identified by its position in the view of its parent, by the `Machine.id` of its machine and by its key, not by the machine value. A key keeps the child when the siblings are reordered, a key that is removed ends the child, and a key that comes back starts a new one.
- One id is one machine in a tree. The state of a child is kept only when its machine has the same type as before, and the types are known equal only for the same `Machine.make` call (`Machine.type_id`). So a tree that meets two machine values with one id, such as a machine made again in a view or two machines that share an id, raises `Invalid_argument` with that id, rather than starting the child again at each render. Make each machine once, at the top level.
- A new child starts its machine with its props. A child that stays gets its new props when they changed (see `Machine.set_props`).
- A child that is no longer rendered ends: the actions of leaving its state, and a `Action.Stop` for every timer and source it still owns, come first for its own children, then for it.
- Each entry of a state gets a number. Timer and source ids are `"<instance>.<entry>/<machine>/<state>/<key>"` (see `Source.key`), so two instances of one machine never share an id. A message from a timer or source of an earlier entry, or of an ended instance, is dropped.
- The result event of an action (such as `Action.Measure`) goes to the instance that ran the action, in any state. It is dropped when that instance has ended.
- A view must be a function of its snapshot. An instance whose snapshot is physically the same as at its last render, whose props did not change, under which no instance stepped and no boundary failed or was reset, and that was not remounted, is not rendered again: its last resolved view is used.

Server render, the browser host and the tests use this one tree. The checker explores one machine at a time and does not use it.

```ocaml
type message
```

An event addressed to one instance. Hosts keep it as it is, and give it back to `update` when the DOM event, timer or answer it stands for happens.

```ocaml
type ('p, 's, 'e) t
```

A tree whose root component has props `'p`, snapshots `'s` and events `'e`.

**Observing a tree**

A tree, or the Live runner of the server, can give each start, step and end of each instance to an `observer`. The values it gives hold the typed state of the moment and cost nothing to make; `inspect` reads them, and `Transition.observer` turns them into records. All the code that reads them is reached only through an observer, so an app with no observer carries none of it.

```ocaml
type ('p, 's, 'e) cell
```

A running instance in a tree.

```ocaml
type change =
  | Started  (** The instance started. *)
  | Resumed
      (** The instance started again at a snapshot it reached before (see
          {!Machine.resume}). *)
  | Unhandled  (** The machine ignored the event or the props. *)
  | Kept  (** The step kept the state name. *)
  | Entered  (** The step entered a state. *)
  | Ended
```

```ocaml
type cause =
  | Start
  | Props  (** The parent gave new props. *)
  | End
  | Message  (** A message of the host for the root (see {!val-message}). *)
  | Handler
      (** A handler of the view whose message is made with the view, such as a
          press: the element is the one that holds the delivered message (see
          {!inspected}). *)
  | Element of message Node.attr * string option * Key.t option
      (** The handler attribute, physically as it is in the view the input
          happened on; the text of an input; the key of a key handler. *)
  | Timer of string * int  (** The id and the delay of a timer. *)
  | Answer :
      string * ('i, 'o, 'x) Endpoint.t * ('o, 'x) Endpoint.answer
      -> cause  (** The id of a call, its endpoint and its answer. *)
  | Live : string * 's Topic.t * 's Topic.update -> cause
      (** The id of a subscription, its topic and the update. *)
  | Source : string * 'm Action.source -> cause
      (** The id of a held source and the source. *)
  | Result : 'm Action.t -> cause  (** The action whose result this is. *)
  | Child : ('p, 's, 'e) cell -> cause
      (** The [output] or [on_final] of this child. *)
  | Route of Router.event
```

What made a step, with the values the host had.

```ocaml
type observed =
  | Observed :
      ('p, 's, 'e) cell
      * Machine.how
      * 's
      * 'e option
      * message Action.host list
      * cause
      -> observed
      (** A start ({!Start}), step, props change or end ({!End}) in a tree, with
          the snapshot before. Read it with {!inspect} while the observer runs:
          the tree changes after. *)
  | Detached : {
      instance : string;
      machine : ('p, 's, 'e) Machine.t;
      props : 'p;
      entry : int;
      from : 's option;
      snapshot : 's;
      change : change;
      event : 'e option;
      actions : 'e Action.host list;
      cause : cause;
    }
      -> observed
```

```ocaml
type observer = {
  observe : observed -> unit;
  handler : 'e. (cause -> 'e -> message) -> 'e Node.attr -> message Node.attr;
      (** Resolve an attribute of a view with the function that makes the
          message of an event with its cause. An observer gives an {!Element}
          cause to the handlers whose message is made at the time of the input
          (input, keys, pointer), and {!Handler} to the others. *)
}
```

```ocaml
type inspected =
  | Inspected : {
      instance : string;
          (** The instance path (see [Transition.t.instance]). *)
      number : int;  (** As in timer ids. *)
      entry : int;  (** The entry after the step, as in timer ids. *)
      machine : ('p, 's, 'e) Machine.t;
      props : 'p;
      from : 's option;  (** [None] for [Started] and [Resumed]. *)
      snapshot : 's;  (** After the step. *)
      change : change;
      event : 'e option;
      actions : 'm Action.host list;  (** The actions of the step, in order. *)
      cause : cause;
      view : message Node.t option;
          (** The last view of the tree, the one an input happened on. *)
      message : message option;
          (** The message that the last {!update} delivered: for a [Handler]
              step, the element of [view] that holds it. *)
    }
      -> inspected
```

```ocaml
val inspect : observed -> inspected
```

The values of the step. Call it while the observer runs.

```ocaml
val cell_path : ('p, 's, 'e) cell -> string
```

The instance path of a cell.

```ocaml
type ('p, 's, 'e) asked
```

An instance that starts (see `asked_path`, `asked_component` and `asked_props`).

```ocaml
val asked_path : ('p, 's, 'e) asked -> string
```

The instance path of the instance (see `cell_path`).

```ocaml
val asked_component : ('p, 's, 'e) asked -> ('p, 's, 'e) Component.t
```

```ocaml
val asked_props : ('p, 's, 'e) asked -> 'p
```

```ocaml
type resume = {
  take : 'p 's 'e. ('p, 's, 'e) asked -> ('s * 'e Action.host list) option;
}
```

How a tree starts from the snapshots of an earlier run, at a hot reload (see `Handoff.hook`). While the first `update` runs, each instance that starts asks `take`. `Some (snapshot, actions)` starts it there with those actions (the observer sees `Resumed`); `None` starts it as usual.

```ocaml
val mount :
  ?dev:bool ->
  ?on_error:(exn -> Node.error -> unit) ->
  ?observe:observer ->
  ?resume:resume ->
  (unit, 's, 'e) Component.t ->
  (unit, 's, 'e) t
```

Create the root and start its machine. The actions are returned by the first `update`.

An exception from the machine step or the view of a component under a `Node.boundary` does not leave `update`: the boundary shows its fallback. `on_error` gets the exception and the `Node.error` the fallback gets; with no hook the error goes to stderr. `dev` (default `false`) puts the details in `Node.error.detail`. An exception with no boundary above it leaves `update`; the tree is then not safe to use and the host should drop it.

`observe` gets one `observed` for each start, step (also an ignored event), props change and end of each instance of the tree, at the time it happens, with its cause: the element of a handler (only with a hook are handler messages made with their element), the timer, the answer, the Live update, the held source, the action, the props, the child or the message of the host. Give it `Transition.observer` or `Trace.Recorder.observer`. With no observer the cost is one test of an option per step. A step that raises is not observed.

`resume` starts the tree from the snapshots of an earlier run (see `resume`).

```ocaml
val mount_with_props :
  ?dev:bool ->
  ?on_error:(exn -> Node.error -> unit) ->
  ?observe:observer ->
  ?resume:resume ->
  props:'p ->
  ('p, 's, 'e) Component.t ->
  ('p, 's, 'e) t
```

Like `mount`, for a root component with props.

```ocaml
val set_props : ('p, 's, 'e) t -> 'p -> unit
```

Give the root new props, as a parent gives a child new props: the `on_props` of the machine runs and the sources are compared by key. The actions come with the next `update`. Props equal to the current ones do nothing.

```ocaml
val failure :
  ?dev:bool -> ?on_error:(exn -> Node.error -> unit) -> exn -> Node.error
```

Like `report` for an exception that happened with no tree, such as while the root machine starts.

```ocaml
val perform : message -> unit
```

Run a message made by `callback`, or any message, with no tree.

```ocaml
val report : ('p, 's, 'e) t -> exn -> Node.error
```

Make the `Node.error` for an exception that no boundary caught, and send it to `on_error` (or stderr), as a boundary does. The error names the root machine and its state.

```ocaml
val callback : (unit -> unit) -> message
```

A message that runs the function when it is delivered. A host uses it for the handlers of its own views, such as a top-level error page.

```ocaml
val update :
  ('p, 's, 'e) t -> message option -> message Node.t * message Action.host list
```

Deliver the message, if any. Then process the events that components raised for their parents, and render until nothing changes. Returns the resolved view (it has no component nodes) and the actions raised since the last call, in order. Timer and source ids in the actions are the ids described above.

```ocaml
val waiting : ('p, 's, 'e) t -> bool
```

The last `update` rendered a `Node.chunk` whose value is not here. A host that adopts a server screen waits for the chunks before its first paint. An instance that shows a chunk that is loading renders again at each update, with its parents, until the value is here.

```ocaml
val message : ('p, 's, 'e) t -> 'e -> message
```

An event for the root component, such as a URL change.

```ocaml
val route : ('p, 's, 'e) t -> Router.event -> 'e -> message
```

Like `message`, for the event that the root makes of a URL change or a preload: an observer gets the cause `Route`.

```ocaml
val path : ('p, 's, 'e) t -> string
```

The instance path of the root: the id of its machine. Children have the paths that `Transition.t.instance` describes.

```ocaml
val unmount : ('p, 's, 'e) t -> message Action.host list
```

End the root, children first. Later messages are dropped.

```ocaml
val snapshot : ('p, 's, 'e) t -> 's
```

The snapshot of the root machine.

```ocaml
val count : ('p, 's, 'e) t -> int
```

The number of instances now mounted, the root included.

```ocaml
type running =
  | Running : {
      path : string;  (** The instance path (see {!cell_path}). *)
      machine : ('p, 's, 'e) Machine.t;
      snapshot : 's;
    }
      -> running
```

```ocaml
val running : ('p, 's, 'e) t -> running list
```

Every mounted instance, parents before their children, with its snapshot. A hot reload hands them to the next run (see `Handoff`).

## Silica.Key

A keyboard event as a portable value. Hosts build it from the platform event; machines and the checker match on it.

```ocaml
type t = {
  key : string;  (** The value of [KeyboardEvent.key], such as ["Escape"]. *)
  ctrl : bool;
  meta : bool;
  shift : bool;
  alt : bool;
  repeat : bool;
}
```

```ocaml
val make :
  ?ctrl:bool ->
  ?meta:bool ->
  ?shift:bool ->
  ?alt:bool ->
  ?repeat:bool ->
  string ->
  t
```

A key with no modifier unless given.

```ocaml
val escape : t
```

The Escape key with no modifier.

```ocaml
val enter : t
```

The Enter key with no modifier.

```ocaml
val tab : t
```

The Tab key with no modifier.

```ocaml
val arrow_up : t
```

The Up arrow key with no modifier.

```ocaml
val arrow_down : t
```

The Down arrow key with no modifier.

```ocaml
val equal : t -> t -> bool
```

Two keys are equal when the key name and every modifier are equal.

```ocaml
val to_string : t -> string
```

`"Ctrl+Shift+k"` style text for traces and logs.

## Silica.Live

A component whose state lives on the server, one instance per connection. The client sends encoded events and receives encoded state.

```ocaml
type ('s, 'e) t = {
  channel : string;
  state : 's Codec.t;
  event : 'e Codec.t;
  component : (unit, 's, 'e) Component.t;
}
```

```ocaml
val declare :
  channel:string ->
  state:'s Codec.t ->
  event:'e Codec.t ->
  (unit, 's, 'e) Component.t ->
  ('s, 'e) t
```

Declare a Live component in shared code. `channel` is the same on the server and in the browser.

```ocaml
val slot : ('s, 'e) t -> 'a Node.t
```

An island: an empty `div` with `data-silica-live` set to the channel. It has no handlers, so it fits any event type. Hosts never diff its children and connect the session while it exists.

## Silica.Live_client

*For hosts: the browser runtime, `Silica_memory` and the server use it. App code does not call it.*

One live subscription over a WebSocket, as Live protocol v1 describes it (`Protocol`). The client sends the hello, keeps the heartbeat, drops old states, and connects again after a loss. It reaches the platform only through `Platform.io`.

```ocaml
type config = { ping_ms : int; pong_ms : int }
```

`ping_ms` is the wait before each ping. `pong_ms` is the wait for the pong before the client closes the socket and connects again.

```ocaml
val default_config : config
```

The values of `Protocol.ping_interval_ms` and `Protocol.pong_timeout_ms`.

```ocaml
type fatal =
  | Close_code of int
      (** The server closed the socket with {!Protocol.close_version},
          {!Protocol.close_session}, {!Protocol.close_csrf} or
          {!Protocol.close_unknown}. *)
  | Server_error of string
```

```ocaml
type t
```

```ocaml
val start :
  ?config:config ->
  io:Platform.io ->
  sub:Protocol.subscription ->
  on_state:
    (seq:int -> state:Silica_json.Json.t -> actions:Protocol.host list -> unit) ->
  on_down:(unit -> unit) ->
  on_fatal:(fatal -> unit) ->
  unit ->
  t
```

Connect with `io.connect` and keep the connection. `on_state` runs for each state with a `seq` higher than the last one of the same socket. `on_down` runs once when a socket that had no loss before is lost; the client then connects again, and the next state runs `on_state`. `on_fatal` runs once, and the client does not connect again. After a close with `Protocol.close_session`, `io.session_gone` runs first. After `stop`, no callback runs.

```ocaml
val send_event : t -> 'e Codec.t -> 'e -> unit
```

Send the event with the next event id. Before the first state of the socket (no `Hello_ok` yet, no state yet, or lost and not back), the client holds the event and sends it, in order, right after the next first state: at most 16 events, the oldest dropped first, and an event held for more than 10 seconds is dropped. The ids continue across reconnects, and never go below the `ack` of a state plus one: a new client for an island that the server already knows, such as after a hot reload, does not reuse ids the server has seen. After `stop` an event is dropped.

```ocaml
val stop : t -> unit
```

Close the socket with code 1000, cancel all timers, and never connect again.

```ocaml
val subscribe :
  ?config:config ->
  io:Platform.io ->
  'a Topic.t ->
  string ->
  deliver:('a Topic.update -> unit) ->
  unit ->
  unit
```

Subscribe to the topic under the key. `deliver` gets the updates that `Action.Start_live` describes. The result stops the subscription.

## Silica.Machine

Statecharts over a plain variant.

The states are the constructors of a plain OCaml variant, with their data inside, and one function decides every step:

```
  type event = Save | Answer of (string, string) Remote.answer | Reset
  [@@deriving event_shape]

  type state = Idle | Saving | Saved of string | Failed of Remote.error
  [@@deriving state_shape]

  let machine =
    Machine.make ~id:"save" ~init:Idle ~shape:state_shape
      ~events:event_shape
      ~owns:(function
        | Saving ->
            [ Remote.call save "draft" (fun answer -> Answer answer) ]
        | Saved _ ->
            [ Source.after 2000 Reset ]
        | Idle | Failed _ ->
            []
        )
      (fun state event ->
        match (state, event) with
        | (Idle | Failed _), Save ->
            Go Saving
        | Saving, Answer (Ok time) ->
            Go (Saved time)
        | _ ->
            Ignore
      )
```

The machine enters a state when its `name` changes. A step to a state with the same name keeps its timers and sources running, except those whose `Source.key` changed; `Reenter` stops and starts them all again. Hosts carry this out through the actions `Action.host`: `Start_timer`, `Start_call`, `Start_live`, `Start_source` and `Stop`; a host drops a timer or answer whose id was stopped.

A component with props uses `make_with_props`: the update, `owns`, `entry` and `exit` get the props, and the props are not an event.

```ocaml
type ('s, 'e) next =
  | Ignore  (** The event is not handled: the snapshot does not change. *)
  | Stay of 'e Action.t list
      (** Handled: keep the snapshot and run the actions. *)
  | Go of 's
      (** Go to the snapshot. When its name is the name of the current one, only
          the data changes: no exit, no entry, and the timers and sources whose
          key did not change keep running. *)
  | Go_with of 's * 'e Action.t list
      (** Like [Go], and run the actions between the exit and the entry. *)
  | Reenter of 's
```

```ocaml
val map_next : ('a -> 'b) -> ('a, 'e) next -> ('b, 'e) next
```

Change the snapshot of a `Go`, `Go_with` or `Reenter`. A helper that decides the step of a part of the state, such as a record field, gives a `next` of that part; `map_next` puts the part back into the whole state.

```ocaml
type ('p, 's, 'e) t
```

A machine with props `'p`, snapshots `'s` and events `'e`.

```ocaml
type 'a shape
```

What a machine knows about its state type or its event type: the name of each value and the list of every name. Make it with `shape`, or with the derivers `[@@deriving state_shape]` and `[@@deriving event_shape]` of `silica.ppx`:

```
  type event = Save | Answer of (string, string) Remote.answer
  [@@deriving event_shape]
  (* event_shape : event Machine.shape *)

  type state = Idle | Saving | Saved of string [@@deriving state_shape]
  (* state_shape : state Machine.shape *)
```

The derivers name each constructor in lower case (`Saved` is `"saved"`); `state_shape` also lists every name, `event_shape` lists none; `[@state_name "x"]` or `[@event_name "x"]` on a constructor gives another name, and two constructors with one name are a compile error. `[@event_name_of f]` on an event constructor with tuple data names the event by its data: the name is `f` applied to the items of the tuple, such as `[@event_name_of fun value -> "select " ^ value]`.

```ocaml
val shape :
  name:('a -> string) ->
  ?names:string list ->
  ?fingerprint:'a Fingerprint.t ->
  unit ->
  'a shape
```

`shape ~name ?names ?fingerprint ()`. For states, `name` names the state of a snapshot in the spec, the checker and the ids of timers and sources, and `names` lists every name, for the checker to report the states that a run never reached. For events, `name` names the event on the edges of the spec and in traces.

`fingerprint` lets a hot reload keep the snapshots of the machine (see `Fingerprint`); `[@@deriving state_shape]` gives it in a development build, and a shape written by hand takes the one of `[@@deriving fingerprint]` on its state type. A machine with none starts again at each hot reload, unless its initial snapshot equals the old one.

```ocaml
val shape_name : 'a shape -> 'a -> string
```

The name of the value.

```ocaml
val shape_names : 'a shape -> string list
```

Every name, or `[]` when the shape has no `names`.

```ocaml
val make :
  id:string ->
  init:'s ->
  shape:'s shape ->
  ?events:'e shape ->
  ?owns:('s -> 'e Source.t list) ->
  ?entry:('s -> 'e Action.t list) ->
  ?exit:('s -> 'e Action.t list) ->
  ?final:('s -> bool) ->
  ?start:'e Action.t list ->
  ('s -> 'e -> ('s, 'e) next) ->
  (unit, 's, 'e) t
```

`make ~id ~init ~shape update` is a machine with no props, for example `Machine.make ~id:"save" ~init:Idle ~shape:state_shape ~events:event_shape ~owns update`.

`id` names the machine in the spec, in the checker and in the ids of timers and sources. `shape` names its states and `events` its events (see `shape`). `owns` lists the timers (`Source.after`), calls (`Remote.call`) and sources of a snapshot (see `Source.key`). `entry` and `exit` give the actions of entering and leaving a state. A `final` snapshot handles no event. `start` are actions that run once, before the entry of `init`.

The update ends with `| _ -> Ignore` in most machines; the checker reports an event that the view offers and the machine ignores (see `Check`).

```ocaml
val make_with_props :
  id:string ->
  init:'s ->
  shape:'s shape ->
  ?events:'e shape ->
  ?owns:('p -> 's -> 'e Source.t list) ->
  ?entry:('p -> 's -> 'e Action.t list) ->
  ?exit:('p -> 's -> 'e Action.t list) ->
  ?final:('s -> bool) ->
  ?start:'e Action.t list ->
  ?on_props:('p option -> 'p -> 's -> ('s, 'e) next) ->
  ('p -> 's -> 'e -> ('s, 'e) next) ->
  ('p, 's, 'e) t
```

Like `make`, for a component with props: the parent gives them with `Component.node`, and the update, `owns`, `entry` and `exit` get the props of the moment. `on_props previous props state` runs when the component starts (`previous = None`) and when the parent gives other props; it defaults to `Ignore`. After it, the sources are compared by key, as after a step, so a source that depends on the props starts again when they change.

```ocaml
val id : ('p, 's, 'e) t -> string
```

The id of the machine, as given to `make`.

```ocaml
val init : ('p, 's, 'e) t -> 's
```

The snapshot that the machine starts in.

```ocaml
val name : ('p, 's, 'e) t -> 's -> string
```

The name of the state of the snapshot.

```ocaml
val states : ('p, 's, 'e) t -> string list option
```

The `names` of the shape of the machine.

```ocaml
val fingerprint : ('p, 's, 'e) t -> 's Fingerprint.t option
```

The `fingerprint` of the shape of the machine.

```ocaml
val event_name : ('p, 's, 'e) t -> 'e -> string
```

The name of the event by the `events` of the machine, or `""` when it has none.

```ocaml
val register_codecs :
  ('p, 's, 'e) t ->
  ?props:'p Codec.t ->
  ?state:'s Codec.t ->
  ?event:'e Codec.t ->
  unit ->
  unit
```

`register_codecs machine ?props ?state ?event ()` gives the machine JSON codecs of its props, snapshots and events, for traces, replay and development tools. A second call for the same machine replaces the first.

The codecs are not part of the machine definition on purpose: only the code that calls `register_codecs` references them. Call it from development entry points (tests, the checker suite, a development client), and a release bundle that never calls it carries no codec. Leave out a codec when the values hold closures or other data that is not plain data.

```ocaml
val props_codec : ('p, 's, 'e) t -> ('p Codec.t, string) result
```

The props codec given to `register_codecs`, or an `Error` as for `state_codec`.

```ocaml
val state_codec : ('p, 's, 'e) t -> ('s Codec.t, string) result
```

The state codec given to `register_codecs`. `Error` names the machine and says how to register it.

```ocaml
val event_codec : ('p, 's, 'e) t -> ('e Codec.t, string) result
```

The event codec given to `register_codecs`, or an `Error` as for `state_codec`.

```ocaml
val registered_codecs : unit -> string list
```

The ids of the machines with registered codecs, the latest first. A test of an app checks with it that the codec module registers every machine whose events are plain data.

```ocaml
val is_final : ('p, 's, 'e) t -> 's -> bool
```

A final state ignores every event.

```ocaml
val default_props : ('p, 's, 'e) t -> 'p option
```

`Some ()` for a machine made by `make`, `None` for one made by `make_with_props`.

```ocaml
val type_id : ('p, 's, 'e) t -> ('p * 's * 'e) Type.Id.t
```

A type id made by `make`, one for each machine. Hosts use it to know that two machines with the same id have the same types.

```ocaml
type resource =
  | Timer of { id : string; ms : int }
  | Call of { id : string; path : string }
  | Subscription of { id : string; topic : string }
  | Held of { id : string; name : string }
```

```ocaml
val active : ('p, 's, 'e) t -> 'p -> 's -> resource list
```

The timers and sources that the snapshot owns. After `start`, `step` or `set_props`, the actions and this list agree: a resource is running if and only if its id was started and not stopped.

```ocaml
val held : ('p, 's, 'e) t -> 'p -> 's -> 'e Action.source list
```

The held sources (see `Source.focus_trap`) that the snapshot owns. The checker reads them to know which presses can happen.

```ocaml
val can : ('p, 's, 'e) t -> 'p -> 's -> 'e -> bool
```

The machine handles the event in this snapshot: it is not final and the update does not give `Ignore`. A view can disable a button with it.

```ocaml
type how = Unhandled | Kept | Entered
```

```ocaml
val start : ('p, 's, 'e) t -> 'p -> 's * 'e Action.host list
```

The initial snapshot after the `start` actions of the machine, the actions of entering it, and `on_props None props`.

```ocaml
val resume : ('p, 's, 'e) t -> 'p -> 's -> 'e Action.host list
```

`resume machine props snapshot` gives the actions that run the machine again at a snapshot it reached before, in another process or bundle: the starts of the timers, calls, subscriptions and held sources that the snapshot owns, in the order of `owns`, with the ids of `active`. A timer starts with its full delay and a call is sent again.

It gives no `start` actions, no `entry` and no `on_props`: the snapshot entered its state before, when they ran, and their effects (a focus, a title, a navigation, a call made by an action) must not happen twice. After it, the actions and `active` agree, as after `start`.

```ocaml
val step :
  ?finished:string ->
  ('p, 's, 'e) t ->
  'p ->
  's ->
  'e ->
  's * 'e Action.host list * how
```

Run the update. A final state handles nothing; `Ignore` leaves the snapshot unchanged (`Unhandled`). On a change of state the actions are: `exit` and stops of the old state, the actions of the step, `entry` and starts of the new state (`Entered`). A step that keeps the name stops the sources whose key went away, runs the actions and starts the new sources (`Kept`). `Reenter` is `Entered` even when the name stays. Hosts use `Entered` to give each entry of a state its own instance (see `Instance`).

`finished` is the id of the timer or call that raised the event: it ended when it delivered. A handled step that keeps the name and still owns a source with that id starts it again, so a state that owns `Source.after 1000 Tick` and handles `Tick` with `Go` ticks every second. Hosts give it for timers and calls; subscriptions and held sources do not end when they deliver.

```ocaml
val set_props :
  ('p, 's, 'e) t -> previous:'p -> 'p -> 's -> 's * 'e Action.host list * how
```

The parent gave new props: run `on_props (Some previous) props`, then compare the sources by key. `Unhandled` when nothing changed.

```ocaml
val exit : ('p, 's, 'e) t -> 'p -> 's -> 'e Action.host list
```

The actions of leaving the snapshot: `exit`, then the stops of its timers and sources.

## Silica.Nav

One navigation, as a `Source.block` sees it.

```ocaml
type t =
  | Push of Url.t
      (** A link press or {!Action.Navigate}: a new history entry. *)
  | Replace of Url.t
      (** A replace link or {!Action.Replace}: the current entry changes. *)
  | Traverse of { delta : int; url : Url.t }
```

```ocaml
val fingerprint : t Fingerprint.t
```

The fingerprint of `t` (see `Fingerprint`).

## Silica.Node

Immutable view trees. Handlers are data: an event, or a function from the input value to an event. A tree can hold component nodes: each one runs its own machine while it is rendered (see `Instance`).

```ocaml
type ('p, 'e) handler = { run : 'p -> 'e option; prevent : bool }
```

A handler for an event with payload `'p`. `run` sees the payload at the moment the event happens. It returns `None` to ignore the event. `prevent` asks the renderer to cancel the default behavior of the platform event at that moment, but only when `run` returned `Some _`. The answer to `prevent` does not wait for the machine.

```ocaml
type error = { id : string; message : string; detail : string option }
```

What a boundary fallback gets. `id` names the failure in the console and in `on_error` hooks. `message` is short and safe to show. `detail` is the exception text, the machine, its state and a trace: it is `Some _` only when the host runs in dev mode.

```ocaml
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
      (** A press on the element resets the {!val-boundary} whose fallback holds
          it. Outside a fallback it does nothing. *)
  | Rated of Rate.t * 'e attr
```

```ocaml
type ('p, 's, 'e) component = {
  machine : ('p, 's, 'e) Machine.t;
  view : 'p -> 's -> 'e t;
}
(** A machine together with the view of its props and states. Define it once, at
    the top level. A rendered component is the same one as before when it is at
    the same place and its machine has the same {!Machine.id} and types. *)

and 'e t =
  | Element of string * 'e attr list * 'e t list
  | Text of string
  | List of 'e t list
  | Keyed of string * 'e t
  | Portal of string option * 'e t list
      (** The children are painted into the element with this id, or into the
          end of the document body when it is [None], and not at the place of
          the node. A server render leaves them out. With [None], the browser
          renderer gives the container in the body the [data-theme] attribute of
          the nearest element above the node that has one, so the children keep
          the theme of their place (see [Silica_ui.Theme.scope]). *)
  | Boundary of 'e boundary
  | Component : ('p, 's, 'ce, 'e) child -> 'e t
  | Chunk of 'e chunk

and 'e chunk =
  | Chunk_view : {
      chunk : 'a Chunk.t;
      loading : 'e t;
      render : 'a -> 'e t;
    }
      -> 'e chunk
      (** [render value] when the value of [chunk] is here; [loading] while it
          loads (see {!val-chunk}). *)

and 'e boundary = { fallback : error -> reset:'e attr -> 'e t; body : 'e t }
(** The components under [body] run as usual. When an exception escapes the
    machine step or the view of one of them, the boundary ends all of them,
    shows [fallback error ~reset] in place of [body], and keeps the state of the
    components above it. [reset] is {!On_reset}: pressing an element that has it
    renders [body] again, and the components under it start from their initial
    state. An exception in [fallback] goes to the next boundary above. *)

and ('p, 's, 'ce, 'e) child = {
  key : string option;
  component : ('p, 's, 'ce) component;
  props : 'p;
  output : 'ce -> 's -> 'e list;
  on_final : 's -> 'e list;
}
```

A machine together with the view of its props and states. Define it once, at the top level. A rendered component is the same one as before when it is at the same place and its machine has the same `Machine.id` and types.

```ocaml
type 'e shape = 'e t
```

```ocaml
val element : string -> 'e attr list -> 'e t list -> 'e t
```

`element tag attrs children` is an HTML or SVG element.

```ocaml
val text : string -> 'e t
```

A text node. Hosts escape the text.

```ocaml
val list : 'e t list -> 'e t
```

A group of nodes with no element around them.

```ocaml
val keyed : string -> 'e t -> 'e t
```

Identity for the keyed diff in hosts.

```ocaml
val portal : ?target:string -> 'e t list -> 'e t
```

`target` is the id of an element of the page.

```ocaml
val boundary : fallback:(error -> reset:'e attr -> 'e t) -> 'e t -> 'e t
```

A `boundary` around a part of the view. See `boundary`.

```ocaml
val component :
  ?key:string ->
  ?output:('ce -> 's -> 'e list) ->
  ?on_final:('s -> 'e list) ->
  (unit, 's, 'ce) component ->
  'e t
```

A component node for a component with no props. `key` keeps the component through reordering. Without `output` the parent hears nothing.

```ocaml
val component_with_props :
  ?key:string ->
  props:'p ->
  ?output:('ce -> 's -> 'e list) ->
  ?on_final:('s -> 'e list) ->
  ('p, 's, 'ce) component ->
  'e t
```

Like `component`, for a component with props.

```ocaml
val chunk : 'a Chunk.t -> loading:'e t -> ('a -> 'e t) -> 'e t
```

`chunk c ~loading render` shows `render value` once the value of `c` is here, and `loading` until then. The host starts the load when it renders the node, and renders again when the load ends. In the browser, the first paint waits for every chunk of the first view, so the server HTML is adopted with the code of its page. A load that fails raises `Chunk_failed` in the view: the nearest `boundary` shows its fallback, and its reset loads again.

```ocaml
exception Chunk_failed of string
```

```ocaml
val chunk_view : 'e chunk -> 'e t
```

The node a chunk shows now: `render value` when the value is here, `loading` otherwise. For tools that walk a view, such as a checker.

```ocaml
val view : 'e t -> 'e shape
```

The node as a value to match on. A host uses it to walk the tree.

```ocaml
val map : ('a -> 'b) -> 'a t -> 'b t
```

Change the events of a view, for example to put a child view into a parent view.

```ocaml
val map_attr : ('a -> 'b) -> 'a attr -> 'b attr
```

Change the event of an attribute, as `map` does for a node.

## Silica.Optimistic

An optimistic view of data: the last confirmed value with the changes that the user made and the server has not yet confirmed. Pure: no host is involved. A machine keeps one in its snapshot data: the value is first-order (no functions), so the checker can compare and hash it.

For a Remote mutation: `OPTIMISTIC.apply` when the call starts, `OPTIMISTIC.settle` when the answer arrives, `OPTIMISTIC.drop` when it fails. For Live data: `OPTIMISTIC.apply` when the change is sent, and `OPTIMISTIC.settle` when a state with a higher `seq` arrives.

```ocaml
module type CHANGE = sig
  type value
  type change

  val apply : value -> change -> value
  (** [apply value change] is the value with the change on it. *)
end
```

How a change goes on a value.

```ocaml
type ('value, 'change) state
```

The confirmed value and the pending changes.

```ocaml
val fingerprint :
  'value Fingerprint.t ->
  'change Fingerprint.t ->
  ('value, 'change) state Fingerprint.t
```

The fingerprint of a `state` from the ones of the value and the change, for a state type that holds one (see `Fingerprint`).

```ocaml
module type OPTIMISTIC = sig
  type value
  type change
  type t = (value, change) state

  val make : value -> t
  (** [make confirmed]: no pending change. *)

  val apply : id:int -> change -> t -> t
  (** Add a pending change. An [id] that is already pending replaces its change.
  *)

  val settle : ?confirmed:value -> id:int -> t -> t
  (** The server confirmed the change [id]: it is no longer pending. With
      [confirmed], that value becomes the confirmed value, and the changes that
      are still pending go on top of it. Without it, the change is folded into
      the confirmed value. An unknown [id] changes nothing. *)

  val drop : id:int -> t -> t
  (** The change failed or was cancelled: it is removed and the value goes back.
      An unknown [id] changes nothing. *)

  val rebase : value -> t -> t
  (** A new confirmed value, for example a Live state. Pending changes stay. *)

  val value : t -> value
  (** The confirmed value with the pending changes applied, oldest first. *)

  val confirmed : t -> value
  (** The confirmed value, with no pending change applied. *)

  val pending : t -> int
  (** The number of pending changes. *)

  val is_pending : id:int -> t -> bool
  (** The change [id] is pending. *)
end
```

```ocaml
module Make (Change : CHANGE) :
  OPTIMISTIC with type value = Change.value and type change = Change.change
```

## Silica.Place

Where a popup goes next to its anchor. Pure: a renderer measures the rectangles and writes the result.

```ocaml
type side = Top | Bottom | Left | Right
```

```ocaml
type align = Start | Center | End
```

```ocaml
type t = {
  side : side;  (** The preferred side of the anchor. *)
  align : align;  (** The alignment along that side. *)
  offset : float;  (** The gap between the anchor and the popup. *)
  padding : float;  (** The smallest gap between the popup and the viewport. *)
}
```

```ocaml
val default : t
```

Below the anchor, aligned at its start, 4 pixels away, 8 pixels from the edges of the viewport.

```ocaml
type result = {
  x : float;
  y : float;
  side : side;  (** The side used: the preferred one, or its opposite. *)
  align : align;
  available_width : float;
  available_height : float;
      (** The room on the used side, along the main axis and across it, inside
          the padding. *)
}
```

```ocaml
val compute : t -> anchor:Rect.t -> popup:Rect.t -> viewport:Rect.t -> result
```

The popup goes on the preferred side when it fits there. When it does not fit and the opposite side has more room, it goes on the opposite side. Along the side it is aligned with the anchor and then moved inside the viewport, less the padding. Only the `width` and `height` of `popup` are read.

```ocaml
val side_name : side -> string
```

`"top"`, `"bottom"`, `"left"` or `"right"`, as in `data-side`.

```ocaml
val align_name : align -> string
```

`"start"`, `"center"` or `"end"`, as in `data-align`.

## Silica.Platform

*For hosts: the browser runtime, `Silica_memory` and the server use it. App code does not call it.*

The contract between a host runtime (`Runtime`) and the platform it runs on: a browser, or the in-memory platform of the tests.

The runtime keeps the queue of events, the instance tree, timers, calls, subscriptions, the error page and the replay of server answers. It reaches the platform only through a value of `t`:

- `renderer` paints views, adopts a server-painted screen, turns platform input into events (element handlers and held sources), runs the platform actions of `Action.t` (focus, scroll, history, title, ...) and paints portals. See `Renderer`.
- `location` and the `on_route` argument of `renderer` are the navigation part: the URL at the start, each URL change (a navigation action, a link press, back and forward) and each preload of a link, which the runtime gives to the root component.
- `io` holds what the runtime itself needs: a clock, one HTTP request form for Remote calls, a socket for Live subscriptions, and a log.

A platform for a new screen writes one value of this type and no runtime code.

```ocaml
type cancel = unit -> unit
```

Stops what was started. Calling it again, or after the end, does nothing.

```ocaml
type reply = { status : int; body : string }
```

An HTTP answer.

```ocaml
type socket = { send : string -> unit; close : int -> unit }
```

An open Live socket: send a text message, close with a code.

```ocaml
type socket_events = {
  on_open : unit -> unit;
  on_message : string -> unit;
  on_close : int -> unit;  (** The close code, [0] when there is none. *)
  on_error : unit -> unit;
}
```

What a socket reports. A platform calls `on_message` with text: binary frames are decoded as UTF-8. It never calls a handler inside `io.connect`.

```ocaml
type io = {
  schedule : int -> (unit -> unit) -> cancel;
      (** Run the function after the milliseconds, unless cancelled first. It
          never runs it at once, inside the call. *)
  post : path:string -> body:string -> ((reply, string) result -> unit) -> unit;
      (** Send a Remote call: a POST of the JSON body to the path of the page
          origin, with the CSRF token of the page. [Error] is a network failure
          with a message. *)
  connect : socket_events -> socket;
      (** Open the Live socket of the page origin ([/_silica/live]). *)
  csrf : unit -> string;  (** The CSRF token that the server gave the page. *)
  session_gone : unit -> unit;
      (** The server no longer knows the session (see
          {!Protocol.close_session}). A browser reloads the page once. *)
  log : string -> unit;  (** An error for the developer, such as the console. *)
}
```

```ocaml
type 'e t = {
  renderer : on_route:(Router.event -> 'e) option -> 'e Renderer.make;
      (** Make the renderer. With [on_route], each URL change raises the event
          that [on_route] makes from {!Router.event.Url_changed}, and each
          preload of a link the event made from {!Router.event.Preload}. *)
  location : unit -> Url.t;  (** The URL of the page now. *)
  io : io;
}
```

## Silica.Pointer

A pointer event as a portable value. Positions are in CSS pixels, relative to the viewport.

```ocaml
type t = { x : float; y : float; button : int }
```

```ocaml
val origin : t
```

A press with the main button at `(0, 0)`.

## Silica.Protocol

*For hosts: the browser runtime, `Silica_memory` and the server use it. App code does not call it.*

Live protocol, version 2. Wire messages between a live client and the server, shared by both sides. One socket serves one subscription: an island or a topic key. Every message is one JSON object with a `"t"` tag.

The client sends first `{"t":"hello","v":2,"csrf":...,"sub":...}` where `sub` is `{"island":channel,"id":instance}` or `{"topic":name,"key":key}`. The server answers `{"t":"hello","v":2}`, or closes the socket with a close code. Then: `{"t":"state","seq":n,"ack":n,"state":...,"actions":[...]}` from the server, `{"t":"event","id":n,"event":...}` from the client, `{"t":"error","code":...}` from the server, and `{"t":"ping"}` from the client, answered by `{"t":"pong"}`.

```ocaml
val version : int
```

The protocol version, `2`. A hello with another version closes the socket.

```ocaml
type host =
  | Focus of Ref.t
  | Blur of Ref.t
  | Scroll_into_view of Ref.t
  | Scroll_to of { target : Ref.t option; left : int; top : int }
  | Navigate of Url.t
  | Replace of Url.t
  | Back
  | Forward
  | Reload
```

```ocaml
val host_of_host_action : 'e Action.host -> host option
```

`None` for `Start_timer`, `Start_call`, `Start_live`, `Start_source`, `Stop`, `Set_title` and `Remount`, which never cross the wire, and for the actions with a result event (`Measure`, `Copy`, `Load_script`): the event would be made by a function on the server, and the protocol carries no answer from the client to an action. `None` too for the navigation and page actions that only make sense for the page that owns them: `Resume` answers a `Block` source, which a Live machine cannot hold; `Revert_navigation` belongs to the router of the page; `Preload` fills the call cache of the page runtime, which a Live machine does not use; `Set_status` is for a server render, and an island renders from its initial state. The checker (`Check.live`) reports a Live machine that runs one of these.

```ocaml
val host_to_action : host -> 'e Action.t
```

The action that a host message stands for.

```ocaml
val encode_host : host -> Silica_json.Json.t
```

The JSON form of a host action.

```ocaml
val decode_host : Silica_json.Json.t -> (host, string) result
```

Read a host action from JSON.

```ocaml
type subscription =
  | Island of { channel : string; instance : string }
      (** [instance] is chosen by the client and kept across reconnects, so the
          server finds the state again. *)
  | Topic of { name : string; key : string }
```

```ocaml
type client =
  | Hello of { v : int; csrf : string; sub : subscription }
  | Event of { id : int; event : Silica_json.Json.t }
      (** [id] grows by one for each event of one subscription. *)
  | Ping
```

```ocaml
type server =
  | Hello_ok of { v : int }
  | State of {
      seq : int;
      ack : int;
      state : Silica_json.Json.t;
      actions : host list;
    }
      (** The full state. [seq] grows with each state of the subscription; [ack]
          is the id of the last event the server handled (0 for none). *)
  | Error of { code : string }
  | Pong
```

```ocaml
val encode_client : client -> string
```

The JSON text of a client message.

```ocaml
val decode_client : string -> (client, string) result
```

Read a client message. `Error` holds the reason.

```ocaml
val encode_server : server -> string
```

The JSON text of a server message.

```ocaml
val decode_server : string -> (server, string) result
```

Read a server message. `Error` holds the reason.

```ocaml
val encode_event : 'e Codec.t -> id:int -> 'e -> string
```

`encode_client (Event ...)` with the event encoded by the codec.

```ocaml
val encode_state : 's Codec.t -> seq:int -> ack:int -> 's -> host list -> string
```

`encode_server (State ...)` with the state encoded by the codec.

```ocaml
val decode_state : 's Codec.t -> Silica_json.Json.t -> ('s, string) result
```

The state of a `State` message.

```ocaml
val decode_event_payload :
  'e Codec.t -> Silica_json.Json.t -> ('e, string) result
```

The event of an `Event` message, decoded with the codec.

```ocaml
type close_code = int
```

```ocaml
val close_version : close_code
```

4001: the server does not speak the version of the hello.

```ocaml
val close_hello : close_code
```

4002: the first message is not a valid hello.

```ocaml
val close_csrf : close_code
```

4003: the CSRF token is missing or wrong.

```ocaml
val close_unknown : close_code
```

4004: no island or topic with that name.

```ocaml
val close_session : close_code
```

4005: the request has no live session, for example after the server restarted. The server accepts the upgrade and closes at once, because a browser cannot read the status of a refused upgrade. The client does not connect again: it reloads the page to get a new session and token.

```ocaml
val close_idle : close_code
```

4008: no traffic for two heartbeats.

```ocaml
val close_too_big : close_code
```

1009: a message above the limit.

```ocaml
val close_server_error : close_code
```

1011: a step of the server raised.

```ocaml
val max_message : int
```

\65536. An incoming message above it is dropped and answered with `{"t":"error","code":"too_big"}`.

```ocaml
val hard_limit : int
```

\1048576. A message above it closes the socket with `close_too_big`.

```ocaml
val ping_interval_ms : int
```

\30000.

```ocaml
val pong_timeout_ms : int
```

\10000.

## Silica.Rate

A limit on how often a host delivers an input to a machine. The last value always wins: a value that is held back is replaced by a newer one.

```ocaml
type t =
  | Throttle of int
      (** The first value goes through at once. Values that arrive in the next
          [ms] milliseconds are held back, and only the last one is delivered
          when the window ends. *)
  | Debounce of int
```

```ocaml
type scheduler = int -> (unit -> unit) -> unit -> unit
```

`schedule ms callback` runs `callback` after `ms` milliseconds and returns a function that cancels it. A host gives its own timer.

```ocaml
val limiter : schedule:scheduler -> t -> ('a -> unit) -> 'a -> unit
```

`limiter ~schedule rate deliver` is a function that calls `deliver` under the rate. It keeps its own state: make one for each element and event.

## Silica.Rect

The size and position of an element, as a portable value.

```ocaml
type t = { x : float; y : float; width : float; height : float }
```

```ocaml
val zero : t
```

All four values are `0.`.

## Silica.Ref

Names for nodes that actions can target (focus, blur, scroll).

```ocaml
type t
```

```ocaml
val make : string -> t
```

A ref with this name. Use a name that is unique in the page. The host writes it to the `data-silica-ref` attribute.

```ocaml
val name : t -> string
```

The name given to `make`.

## Silica.Remote

Typed HTTP endpoints declared in shared code, and calls to them.

```ocaml
type ('i, 'o, 'x) endpoint = ('i, 'o, 'x) Endpoint.t
```

An endpoint with input `'i`, output `'o` and application error `'x`.

```ocaml
type error = Endpoint.error =
  | Http of int  (** The server answered with this status. *)
  | Decode of string  (** The answer did not decode. *)
  | Network of string
```

```ocaml
type ('o, 'x) answer = ('o, 'x) Endpoint.answer =
  | Ok of 'o  (** The output. *)
  | Rejected of 'x
      (** The handler answered [Error x]: an application error, such as a failed
          validation. The server answers it with status 422, not 500. *)
  | Failed of error
```

```ocaml
val declare :
  path:string ->
  input:'i Codec.t ->
  output:'o Codec.t ->
  rejected:'x Codec.t ->
  ('i, 'o, 'x) endpoint
```

Declare an endpoint in shared code. The client sends the input to `path` with POST. The server answers with the output, or with an application error encoded by `rejected`.

```ocaml
val path : ('i, 'o, 'x) endpoint -> string
```

The URL path of the endpoint.

```ocaml
val input : ('i, 'o, 'x) endpoint -> 'i Codec.t
```

The codec of the input.

```ocaml
val output : ('i, 'o, 'x) endpoint -> 'o Codec.t
```

The codec of the output.

```ocaml
val rejected : ('i, 'o, 'x) endpoint -> 'x Codec.t
```

The codec of the application error.

```ocaml
val answer_id : ('i, 'o, 'x) endpoint -> ('o * 'x) Type.Id.t
```

A type id made by `declare`, one for each endpoint. The checker uses it to find the sample answers of an endpoint.

```ocaml
val call :
  ?timeout_ms:int ->
  ('i, 'o, 'x) endpoint ->
  'i ->
  (('o, 'x) answer -> 'e) ->
  'e Source.t
```

One request to the endpoint with this input, as a source that a state owns (see `Machine.make`). The answer is raised as an event unless the call was stopped first. The key of the call is its path and its encoded input: a step that keeps the state but changes the input stops the old call and starts a new one. There is no timeout unless `timeout_ms` is given. When it passes before the answer, the call ends with `Failed (Network "timeout")`, and a later answer is dropped.

```ocaml
val error_codec : error Codec.t
```

The error as `[@@deriving json]` writes a variant: `["Http", 500]`, `["Decode", "reason"]` or `["Network", "reason"]`.

```ocaml
val answer_codec : 'o Codec.t -> 'x Codec.t -> ('o, 'x) answer Codec.t
```

`answer_codec output rejected`: `["Ok", output]`, `["Rejected", error]` or `["Failed", error]`. For an endpoint: `answer_codec (Remote.output endpoint) (Remote.rejected endpoint)`. A machine whose events hold answers uses it in its event codec (see `Machine.shape`).

```ocaml
val error_to_string : error -> string
```

A short English sentence for the error, for a view: `"The server answered 500"`, `"The server sent data that cannot be read"` or `"The network failed"`.

```ocaml
type request = Endpoint.request =
  | Request : ('i, 'o, 'x) endpoint * 'i -> request
```

```ocaml
val request : ('i, 'o, 'x) endpoint -> 'i -> request
```

`request endpoint input` is `Request (endpoint, input)`.

```ocaml
val error_fingerprint : error Fingerprint.t
```

The fingerprint of `error` (see `Fingerprint`).

```ocaml
val answer_fingerprint :
  'o Fingerprint.t -> 'x Fingerprint.t -> ('o, 'x) answer Fingerprint.t
```

The fingerprint of `('o, 'x) answer` from the ones of `'o` and `'x`.

## Silica.Remote_machine

A ready-made machine for one Remote endpoint. Its states hold the input of the last `Load`, so `Retry` can use it, and the data from before. A parent reads the state through function props (see `Component`).

```ocaml
type ('i, 'o, 'x) state =
  | Idle  (** Nothing was asked for. *)
  | Loading of { input : 'i; last : 'o option }
      (** The call runs. [last] is the data from before. *)
  | Ready of { input : 'i; data : 'o }
  | Rejected of { input : 'i; reason : 'x; last : 'o option }
      (** The handler answered with an application error. *)
  | Failed of { input : 'i; error : Remote.error; last : 'o option }
```

```ocaml
type ('i, 'o, 'x) event =
  | Load of 'i
      (** Start a call, or restart the one in flight. A restarted call stops the
          old one, so a host never delivers an old answer. *)
  | Retry
      (** In [Rejected] and [Failed]: call again with the same input. Ignored
          elsewhere. *)
  | Settled of ('o, 'x) Remote.answer
      (** The answer. Handled only while loading. *)
  | Reset
```

```ocaml
val data : ('i, 'o, 'x) state -> 'o option
```

The data of `Ready`, or the `last` data of the other states.

```ocaml
val state_codec : ('i, 'o, 'x) Remote.endpoint -> ('i, 'o, 'x) state Codec.t
```

The states as `[@@deriving json]` writes them, with the codecs of the endpoint, for `Machine.register_codecs`.

```ocaml
val event_codec : ('i, 'o, 'x) Remote.endpoint -> ('i, 'o, 'x) event Codec.t
```

The events, with the codecs of the endpoint.

```ocaml
val make :
  ?timeout_ms:int ->
  id:string ->
  ('i, 'o, 'x) Remote.endpoint ->
  (unit, ('i, 'o, 'x) state, ('i, 'o, 'x) event) Machine.t
```

`Loading` owns a `Remote.call` on the endpoint, with `timeout_ms` when given. The events are named `"load"`, `"retry"`, `"settled ok"`, `"settled rejected"`, `"settled error"` and `"reset"`. There is no automatic retry: a `Retrying` state that owns a `Source.after` timer and counts the attempts is a pattern for the app.

## Silica.Render

Server-side HTML5 rendering. Text and attribute values are escaped, void elements self-close, `true` boolean attributes print the bare name, `false` ones are omitted, refs print as `data-silica-ref`, handlers are not printed. An `html` root is prefixed with the doctype. A component node renders as its machine starts with its props; its actions are dropped.

```ocaml
val to_string : ?island:(string -> string option) -> 'e Node.t -> string
```

`island channel` gives the HTML that an island with `data-silica-live` set to `channel` and no children prints inside its element.

```ocaml
val to_buffer :
  ?island:(string -> string option) -> Buffer.t -> 'e Node.t -> unit
```

Like `to_string`, adding the HTML to the buffer.

## Silica.Renderer

*For hosts: the browser runtime, `Silica_memory` and the server use it. App code does not call it.*

The boundary between a host and the platform it paints on.

A renderer paints views, turns platform input into events and carries out the platform actions and held sources. It is the only part of a browser host that touches the DOM. The queue of events, timers, calls, the error boundary and the instance tree stay in the host runtime, and are the same for any renderer. A renderer for another platform replaces this record and nothing else.

A renderer is made for one host with a `dispatch` function: it calls `dispatch` for each event that platform input produces, and the host queues it. Rate limits and `prevent` are applied by the renderer, when it receives the platform input.

```ocaml
type 'e t = {
  paint : 'e Node.t -> unit;
      (** Make the screen show the view. The view has no component nodes. The
          first call may adopt a screen that the server already painted (see the
          browser renderer); later calls change only what differs. *)
  run : 'e Action.t -> unit;
      (** Carry out an action. {!Action.Remount} and {!Action.Preload} never
          reach a renderer. *)
  start_source : id:string -> 'e Action.source -> unit;
      (** Start a held resource under the id. The view is already painted. Start
          with an id that is running replaces it. *)
  stop_source : id:string -> unit;
      (** Stop the held resource. Stopping an id that is not running does
          nothing. *)
}
```

```ocaml
type 'e make = dispatch:('e -> unit) -> 'e t
```

What a host needs from a renderer: a renderer wired to `dispatch`.

## Silica.Route

Typed conversion between URLs and the data of a route: path segments and a typed query with defaults. A route is built from `s`, `int`, `string`, `rest` and the operators, for example `s "issues" / int // s "edit"`. One value gives both parse and print.

```ocaml
type 'a t
```

A pattern for the data `'a`.

```ocaml
val s : string -> unit t
```

One literal path segment.

```ocaml
val root : unit t
```

The empty path.

```ocaml
val int : int t
```

One path segment that prints as an integer: `"007"` and `"+7"` do not match.

```ocaml
val string : string t
```

One path segment.

```ocaml
val rest : string t
```

The whole remaining path, as a string such as `"/a/b"`. It matches every path, so a route that uses it, placed last, is the not-found route.

```ocaml
val ( / ) : unit t -> 'a t -> 'a t
```

A literal segment first, then the pattern.

```ocaml
val ( // ) : 'a t -> unit t -> 'a t
```

The pattern, then a literal segment.

```ocaml
val ( /+ ) : 'a t -> 'b t -> ('a * 'b) t
```

Two patterns in a row.

```ocaml
type 'a query
```

Typed query parameters with defaults.

```ocaml
val query :
  name:string ->
  default:'a ->
  parse:(string -> 'a option) ->
  print:('a -> string) ->
  'a query
```

A value that `parse` cannot read falls back to `default`. A value equal to `default` is not printed.

```ocaml
val string_query : string -> default:string -> string query
```

A text query parameter.

```ocaml
val int_query : string -> default:int -> int query
```

An integer query parameter. Text that is not an integer gives `default`.

```ocaml
val bool_query : string -> default:bool -> bool query
```

A boolean query parameter, `"true"` or `"false"`.

```ocaml
val choice_query : string -> default:'a -> (string * 'a) list -> 'a query
```

A value from a fixed list of names.

```ocaml
val map_query : ('a -> 'b) -> ('b -> 'a) -> 'a query -> 'b query
```

`map_query read write query`: the same keys with the value turned into another type, such as a record of the values of `( &? )`. `write (read v)` must equal `v`.

```ocaml
val ( &? ) : 'a query -> 'b query -> ('a * 'b) query
```

Two query parameters. They print in this order, which is the canonical one.

```ocaml
val with_query : 'a t -> 'q query -> ('a * 'q) t
```

The path pattern with a query. Query keys that the route does not know are ignored and not printed.

```ocaml
val ( +? ) : unit t -> 'q query -> 'q t
```

A path with no data, such as `s "issues"`, with a query: the data is the query.

```ocaml
val parse : 'a t -> Url.t -> 'a option
```

`None` when the path does not match. The hash is ignored.

```ocaml
val print : 'a t -> 'a -> Url.t
```

The canonical URL: default query values are left out, there is no hash.

```ocaml
val describe : 'a t -> string
```

A name for the pattern, such as `"/issues/:int"`.

## Silica.Router

The router: a machine whose states are the routes of the app.

The routes are a plain variant with the data of each route (its path parameters and its typed query). Each `route` gives the `Route.t` of one constructor, how to build the constructor from its data and how to read the data back:

```
  type route =
    | Issues of filters [@route Route.(s "issues" +? filters)]
    | Issue of int [@route Route.(s "issues" / int)]
    | Not_found of string [@route Route.rest]
  [@@deriving routes]

  let routes =
    Router.routes
      ~not_found:(function Not_found _ -> true | _ -> false)
      route_routes
```

The deriver `[@@deriving routes]` of `silica.ppx` makes `route_routes`, one `route` for each constructor, in order, from its `[@route]` path. By hand, the same table is `Router.route Route.(s "issues" / int) (fun n -> Issue n) (function Issue n -> Some n | _ -> None)` for each constructor; the check `route-round-trip` of `Check.app` finds a pair that does not agree.

Any machine can navigate with `Router.navigate routes (Issue 42)`. The app root is `Router.make routes ~render`, defined after the pages.

The snapshot of the router machine is the current route. The URL is that route printed by its `Route.t`, after the `base` of the routes. A navigation action, a press on a `link`, the browser back and forward buttons and a typed URL all reach the machine as `event` `Url_changed`. The route is found by trying the listed routes in order, so the last one must match every path (use `Route.rest`): a bad path goes to that route. A query value that does not parse falls back to its default, so it never makes a bad route.

```ocaml
type event =
  | Url_changed of Url.t
      (** Sent by a host when the URL changes: at a navigation action, a link
          press, and when the user moves in the history. *)
  | Preload of Url.t
```

```ocaml
type preload =
  | Intent
      (** Preload when the pointer enters the link or the link gets focus. *)
  | Viewport  (** Preload when the link enters the viewport. *)
  | Never
```

```ocaml
type 'r route
```

One route of an app whose routes are `'r`.

```ocaml
val route : 'data Route.t -> ('data -> 'r) -> ('r -> 'data option) -> 'r route
```

`route path make data`: `make` builds the route from the data that `path` parses, and `data` gives the data back for a route of this kind, `None` for the other routes. `data (make d)` must be `Some d`.

```ocaml
type 'r routes
```

```ocaml
val routes :
  ?base:string ->
  ?not_found:('r -> bool) ->
  ?reenter:('r -> bool) ->
  ?preload:('r -> Remote.request list) ->
  ?chunked:('r -> bool) ->
  'r route list ->
  'r routes
```

The listed routes, in the order they are tried. `base`, such as `"/app"`, is the path that every URL of the app starts with: `parse` needs it and takes it off, `print` puts it first. `not_found` tells the route that `is_not_found` and the server status refer to. A route in `reenter` starts its page again when only its parameters change; any other route keeps its page, which gets the new parameters as new props. `preload` gives the Remote calls that the page of a route makes when it starts, so that a link can fetch them early (see `link`). `chunked` tells the lazy routes: those whose page is a `Node.chunk`. A link to one of them preloads its code (the `chunks` of `make`) as it preloads the calls. The code itself is named in `make`, at the app root, because a page links to routes and so depends on `routes`.

```ocaml
val parse : 'r routes -> Url.t -> 'r
```

Raises `Failure` when the path does not start with the `base` of the routes, or when no listed route matches the path.

```ocaml
val print : 'r routes -> 'r -> Url.t
```

The URL of a route, after the `base` of the routes. Raises `Failure` when no listed route gives data for it.

```ocaml
val name : 'r routes -> 'r -> string
```

The pattern of the route, as `Route.describe` gives it: the name of the state of the router machine.

```ocaml
val is_not_found : 'r routes -> 'r -> bool
```

The `not_found` function of the routes; `false` when there is none.

```ocaml
val navigate : ?hash:string -> 'r routes -> 'r -> 'e Action.t
```

Push a history entry. With `hash`, the URL has that hash, and the browser scrolls to the element with that id after the paint. The router ignores the hash.

```ocaml
val replace : ?hash:string -> 'r routes -> 'r -> 'e Action.t
```

Replace the current history entry. Use it for a change of the query only. `hash` is as in `navigate`.

```ocaml
val link :
  ?hash:string ->
  ?preload:preload ->
  ?current:'r ->
  'r routes ->
  'r ->
  'e Node.attr list ->
  'e Node.t list ->
  'e Node.t
```

An `a` with `href` and `data-silica-link`. The browser host turns a press into a history push without a page load. `hash` is as in `navigate`.

`preload` (default `Intent`) says when the host fetches the calls that the `preload` function of the routes gives for the route of the link, and loads the code of the route when it is `chunked`: the link has `data-silica-preload="intent"` or `"viewport"`. A link with `Never`, or whose route has no calls and is not `chunked`, has no such attribute.

`current` is the route now. The link has `aria-current="page"` when its path equals the path of `current`, and `data-active` when its path is a prefix of the path of `current` (by whole segments), as for the link of a parent page. A link to the root (the path of the routes, `"/"` with no base) is `data-active` only on the root itself, as every path starts with it. The query is ignored for both.

```ocaml
val link_replace :
  ?hash:string ->
  ?preload:preload ->
  ?current:'r ->
  'r routes ->
  'r ->
  'e Node.attr list ->
  'e Node.t list ->
  'e Node.t
```

Like `link`, but the press replaces the history entry. For a link that changes only the query.

```ocaml
type 'r decision =
  | Allow
  | Redirect of 'r  (** Go to this route instead, and replace the URL. *)
  | Cancel
```

```ocaml
val state_codec : 'r routes -> 'r Codec.t
```

A route as its printed URL, for `Machine.register_codecs` on the machine of the router component.

```ocaml
val event_codec : event Codec.t
```

`["Url_changed", url]` or `["Preload", url]`.

```ocaml
val make :
  'r routes ->
  ?guard:(from:'r option -> target:'r -> 'r decision) ->
  ?exit:('r -> event Action.t list) ->
  ?title:('r -> string) ->
  ?chunks:('r -> Chunk.any list) ->
  render:('r -> event Node.t) ->
  Url.t ->
  (unit, 'r, event) Component.t
```

The router component for the app root, starting at the URL. `render` shows the page of the current route, as a component node. `guard` runs before a URL change to another URL takes effect, and at the start with `from = None`: use it for "go to login". At the start, `Redirect` starts in the other route and raises `Action.Replace` with its URL (a server render answers it with a redirect), and `Cancel` starts in the route of the URL, as there is no route to stay in. For unsaved data inside a page, use `Source.block`. `exit` runs when a route is left, as the `exit` of its state, and before the page starts again when a `reenter` route gets new parameters. `title` gives the document title of a route: the router raises `Action.Set_title` with it at the start and at each change of route or parameters, before the actions of the page, so a page machine that sets its own title wins. A change to the same printed URL does nothing. `chunks` gives the code that `render` shows with `Node.chunk` for a route: the router loads it when a link to that route preloads (see `routes`, `chunked`). The router names its events `"url changed"` and `"preload"`.

## Silica.Runtime

*For hosts: the browser runtime, `Silica_memory` and the server use it. App code does not call it.*

The host runtime: the queue of events, the instance tree, timers, Remote calls, Live subscriptions, the top-level error page and the replay of the answers of a server render. It reaches the platform only through `Platform.t`, so the browser and the in-memory platform of the tests run this same code.

Preload. The runtime carries out `Action.Preload`: it sends the call at once and keeps a `200` answer for 30 seconds from its arrival, for one use. The next call of the same endpoint path with the same encoded input takes the kept answer with no request, or waits for the preload that is still in flight. A failed or non-`200` preload is not kept: a call that waited for it sends its own request. A preload of a call that is kept or in flight does nothing.

Chunks. The runtime carries out `Action.Load_chunk`. The first paint waits until the first view has no `Node.chunk` that is loading, so a renderer that adopts the server screen adopts it with the code of its page; timers, calls and sources start at once.

```ocaml
val start :
  ?dev:bool ->
  ?on_error:(exn -> Node.error -> unit) ->
  ?answers:Answers.t ->
  ?live_config:Live_client.config ->
  ?on_route:(Router.event -> 'e) ->
  platform:Instance.message Platform.t ->
  create:
    (dev:bool ->
    on_error:(exn -> Node.error -> unit) option ->
    ('p, 's, 'e) Instance.t
    ) ->
  unit ->
  unit
```

Create the tree with `create`, answer its calls from `answers` before the first paint, paint, and then run every event that the platform raises, one at a time: events raised while one runs are queued. `on_route` makes the event for the root component from each URL change and each link preload that the renderer reports.

Transition log: `create` gives the tree its hook (see `Instance.mount`). The runtime gives the root each URL change and preload with the cause `Instance.Route` (see `Instance.route`). After an error page, "Try again" calls `create` again, and the new tree gets the same hook.

Errors. The runtime is a top-level boundary: an exception that no `Node.boundary` catches paints an error page (with "Try again", which starts the root again with fresh state, and "Reload the page"), and goes to `on_error` and `platform.io.log`. A step that raises changes nothing: its state, view and actions are dropped with the tree. In `dev` mode the page shows the details, and an exception during the first paint is raised again after the error page shows.

## Silica.Scroll

The scroll position and sizes of the page or of an element, as a portable value, in CSS pixels.

```ocaml
type t = {
  left : float;  (** The distance scrolled from the left edge. *)
  top : float;  (** The distance scrolled from the top edge. *)
  scroll_width : float;  (** The width of the content, scrolled or not. *)
  scroll_height : float;  (** The height of the content, scrolled or not. *)
  client_width : float;  (** The width of the visible part. *)
  client_height : float;  (** The height of the visible part. *)
}
```

```ocaml
val zero : t
```

All six values are `0.`.

## Silica.Source

Timers and sources of events that a state owns: the `owns` list of `Machine.make`. A source starts when a state starts to own it and stops when the state no longer owns it.

Each source has a `key`. After each step the machine compares the keys of the old and the new `owns` list: it stops the sources whose key went away and starts the new ones, and the others keep running. So one state can own a set of calls that changes with its data. Calls are made with `Remote.call`.

```ocaml
type 'e t
```

A timer or a source that raises events `'e`.

```ocaml
val after : int -> 'e -> 'e t
```

A timer: the event is raised once, this many milliseconds after the timer starts, unless it was stopped first.

```ocaml
val live : 's Topic.t -> string -> ('s Topic.update -> 'e) -> 'e t
```

Subscribe to the topic under the key. The event is raised once with `Loading None` while the socket connects, and again for every state the server publishes, for every loss of the connection (the old data, `stale = true`) and for every failure. The client reconnects by itself.

```ocaml
val focus_trap : ?return_focus:bool -> Ref.t -> 'e t
```

Keep focus inside the element with this ref while the source runs. `return_focus` defaults to `true`. See `Action.Focus_trap`.

```ocaml
val scroll_lock : 'e t
```

The page does not scroll while the source runs.

```ocaml
val outside_press : Ref.t -> 'e -> 'e t
```

A press outside the element with this ref raises the event.

```ocaml
val keys : ?prevent:bool -> ?rate:Rate.t -> (Key.t * 'e) list -> 'e t
```

A key press anywhere in the page, from the table, raises its event. The table is data, so the checker presses each key of the active state. Keys typed in a text field and keys that an element handler prevented do not count (see `Action.Global_keys`).

```ocaml
val dismiss : ?escape:'e -> ?outside:'e -> ?focus_out:'e -> Ref.t list -> 'e t
```

A dismiss layer for a popup whose elements have these refs: Escape raises `escape` in the newest layer only, a press outside the elements raises `outside`, and focus that moves outside the elements raises `focus_out`, unless the press or the focus is inside a newer layer. See `Action.Dismiss`.

```ocaml
val anchor : ?place:Place.t -> anchor:Ref.t -> Ref.t -> 'e t
```

Keep the popup with this ref next to the `anchor` element while the source runs. `place` defaults to `Place.default`. See `Action.Anchor`.

```ocaml
val resize : ?rate:Rate.t -> Ref.t -> (Rect.t -> 'e) -> 'e t
```

The element changes size.

```ocaml
val intersection : ?rate:Rate.t -> Ref.t -> (bool -> 'e) -> 'e t
```

The element enters (`true`) or leaves (`false`) the viewport.

```ocaml
val scroll : ?rate:Rate.t -> ?target:Ref.t -> (Scroll.t -> 'e) -> 'e t
```

The element with the ref `target`, or the page when `target` is not given, scrolls. The event is raised once at the start and for each scroll. See `Action.Scroll`.

```ocaml
val visibility : (bool -> 'e) -> 'e t
```

The document becomes visible (`true`) or hidden (`false`). The event is raised once at the start with the value then.

```ocaml
val online : (bool -> 'e) -> 'e t
```

The browser goes online (`true`) or offline (`false`). The event is raised once at the start with the value then.

```ocaml
val media : string -> (bool -> 'e) -> 'e t
```

The media query, such as `"(prefers-color-scheme: dark)"`, matches (`true`) or not (`false`). The event is raised once at the start with the value then and again at each change. See `Action.Media`.

```ocaml
val block : (Nav.t -> 'e) -> 'e t
```

While the source runs, every navigation stops before the URL changes and raises the event instead: a page with unsaved data asks, then sends `Action.Resume` with the navigation to let it happen. Closing or reloading the page asks the user first. See `Action.Block`.

```ocaml
val map_refs : (Ref.t -> Ref.t) -> 'e t -> 'e t
```

The same held source with each of its refs replaced. A component that can have more than one instance in a page uses it to give each instance its own refs, for example from an id in its props. Other sources are returned unchanged.

```ocaml
val key : 'e t -> string
```

The identity of the source in an `owns` list: the path and encoded input of a call, the topic and key of a subscription, `"after <ms>ms"` for a timer, the kind and refs of a held source, and the place of an `anchor`. When an `owns` list holds two sources with the same key, the second one gets `"#2"` after its key, and so on.

```ocaml
val same : 'e t -> 'e t -> bool
```

Two sources with the same key also have the same data: the event of a timer, the table of `keys`, the events of `outside_press` and `dismiss`. Compared with `=`. Functions cannot be compared: a part that is a function counts as the same.

The rule after a step that keeps the state: a source whose key went away stops, a new key starts, and a source whose key stays but whose data is not `same` stops and starts again, so it raises the event of the current `owns` list. A function, such as the answer function of a call, is the one of its first start while the key and the data stay: put the data it needs in the state, or in the input of the call.

```ocaml
val name : 'e t -> string
```

The endpoint path of a call, `"live:" ^ topic name` for a `live`, `"after <ms>ms"` for a timer, or the name of a held source as `Action.source_name` gives it.

```ocaml
val delay : 'e t -> int option
```

The milliseconds of an `after` timer.

```ocaml
val topic : 'e t -> string option
```

The topic name of a `live` source.

```ocaml
val is_held : 'e t -> bool
```

A source that is not a call, a subscription or a timer.

```ocaml
val held : 'e t -> 'e Action.source option
```

The held resource of a source that is not a call, a subscription or a timer.

```ocaml
val start : string -> 'e t -> 'e Action.host
```

`start id source` is the action that starts `source` under `id`.

/\*

```ocaml
val remote_call :
  ?timeout_ms:int ->
  ('i, 'o, 'x) Endpoint.t ->
  'i ->
  (('o, 'x) Endpoint.answer -> 'e) ->
  'e t
```

## Silica.Spec

The shape of a machine as a checker run found it: the states it reached, the steps it took with the names of their events, and the timers and sources of each state. `Check.run` gives one in the `specs` of its report.

```ocaml
type state = {
  name : string;
  final : bool;
  timers : int list;
      (** Delays in milliseconds of the {!Source.after} timers. *)
  sources : string list;
      (** Endpoint paths of the calls and ["live:" ^ topic] of the
          subscriptions. *)
  held : string list;
      (** Names of the other sources, such as ["focus_trap"]: they are resources
          of a client renderer. *)
}
```

```ocaml
type label = { from : string; target : string; event : string; kept : bool }
```

A step that a checker run took, with the name of the event that took it. `event` is `""` when the machine gives no event names. `kept` is `true` for a step that kept the state (`Go` to a state with the same name, or `Stay`: the data may change, the timers and sources keep running); then `target = from`.

```ocaml
type t = {
  id : string;
  init : string;
  states : state list;
  edges : label list;
      (** Sorted, without duplicates: the steps of the run. A step that entered
          a state, from another state or from the same one ([Reenter]), has
          [kept = false]; a step that kept the state has [kept = true]. *)
}
```

```ocaml
val arrows : t -> (string * string) list
```

The pairs `(from, target)` of the steps that entered a state, sorted, without duplicates: the arrows of a diagram. Kept steps are not arrows.

```ocaml
type kind = Unreachable | Dead_end
```

```ocaml
type issue = { machine : string; state : string; kind : kind }
```

```ocaml
val lint : t -> issue list
```

`Unreachable`: no path from the initial state. `Dead_end`: a state that is not final and has no arrow to another state, in a machine with more than one state. Self arrows do not count for either.

```ocaml
val issue_to_string : issue -> string
```

`"machine.state: unreachable"` or `"machine.state: non-final dead end"`.

```ocaml
val details : state -> string list
```

The timers and sources of a state as lines: `"after 300ms"`, `"invoke /api/save"`, `"hold focus_trap"`.

```ocaml
val to_dot : t -> string
```

A Graphviz digraph. Timers and sources are listed in the state label; final states are double circles. An arrow shows the events of its edges; kept steps are not shown.

```ocaml
val to_quint : t -> string
```

A Quint module named after the machine id, with `'-'` as `'_'`. Variables: `state` (one constructor for each state), `running` (the timers and sources that run, as strings `"state/after 300ms"`, `"state/invoke /api/path"` and `"state/hold name"`) and `last` (the event of the last step: `Start` at init, the event of the edge, or `Any` for an edge with no event name). One transition for each arrow and event, and one for each kept step; each transition is written as a Quint `action`. `enter` starts the resources of the target and stops the others. The transition of a kept step is named `<state>_stays_on_<event>` and changes only `last`. Invariants: `resources_owned` (every running resource belongs to the current state), `can_move` (the state is final, has an edge or a kept step) and `inv`, both together. Handler code and data are not exported: the model holds the edges only, so it allows more runs than the machine.

```ocaml
val to_text : t -> string
```

A line based text form to read and diff. One line for each item, in the order of `t`:

```
machine toast
initial empty
state empty
state done final
state running
  after 100ms
  invoke /api/save
  hold focus_trap
edge running -> done on tick
edge running -> empty
stay running on props tick
```

One `edge` line for each arrow, with the names of the events of its edges; one `stay` line for each state with kept steps, with their event names. An edge whose only event name is `""` has no `on`. A word with a space, a quote, a backslash or no character is written in double quotes with backslash escapes. Kept steps of a state that is not in `states` are left out.

```ocaml
val of_text : string -> (t, string) result
```

Reads `to_text`. The edges come back sorted, without duplicates, as `Check` gives them. `Error` names the line it cannot read.

```ocaml
val to_mermaid : t -> string
```

A Mermaid `stateDiagram-v2` block, left to right. A state shows its `details` in parentheses; an arrow shows the events of its edges; final states lead to `[*]`. Kept steps are not shown.

```ocaml
val to_json : t -> Silica_json.Json.t
```

An object with the fields `"id"`, `"init"`, `"states"` (objects with `"name"`, `"final"`, `"timers"`, `"sources"`, `"held"`) and `"edges"` (objects with `"from"`, `"target"`, `"event"`, `"kept"`).

```ocaml
val of_json : Silica_json.Json.t -> (t, string) result
```

Reads `to_json`. `Error` names the field it cannot read.

```ocaml
type change =
  | Initial_changed of { before : string; after : string }
  | State_added of state
  | State_removed of state
  | State_changed of { before : state; after : state }
      (** The same name, with another [final], timers or sources. *)
  | Edge_added of label
  | Edge_removed of label
```

A difference between two specs of one machine.

```ocaml
val diff : t -> t -> change list
```

`diff before after`: what changed from `before` to `after`, in this order: the initial state, the removed states, the added and changed states (in the order of `after`), the removed edges, the added edges. `[]` when the specs are equal. A step that now goes to another target is one removed and one added edge.

```ocaml
val change_to_string : change -> string
```

The change in words, with the names as `to_text` writes them:

```
changed initial state from idle to loading
added state archived
removed state draft
changed state saving: now final; added after 300ms; removed invoke /api/save
removed edge idle -> done on save
added edge idle -> saving on save
added stay running on tick
```

**Conformance**

Does a run of the app, as a trace file holds it, stay inside the specs that a check found? This level reads names only, so it works on a trace with no user data (see `Trace.names_only`).

```ocaml
type problem =
  | No_spec  (** No spec is of the machine of the record. *)
  | Unknown_state of string  (** No spec of the machine has this state. *)
  | Unknown_edge of label
```

```ocaml
type deviation = {
  seq : int;  (** The [seq] of the record in the trace. *)
  machine : string;
  instance : string;  (** The instance path of the record. *)
  problem : problem;
}
```

```ocaml
val conforms : t list -> Trace.t -> deviation list
```

`conforms specs trace`: the records of `trace` that `specs` do not allow, in the order of the trace. A start, a resume and an end must be in a state of a spec of the machine; an unhandled event in a state of it; a step that entered a state must be an edge, and a step that kept it a kept step, with the same event name (`props` for new props). A machine can have several specs, such as the specs of the joint checks of a parent with each of its children: a record conforms when one of them allows it. `[]` when the whole trace conforms.

```ocaml
val deviation_to_string : deviation -> string
```

One line, such as `"transition 14, machine comment-composer, instance app/0:comment-composer: no spec has the edge drafting -> archived on send"`.

## Silica.Statechart_layout

A left to right layered drawing of a `Spec.t`, computed in OCaml with no external tool. The same spec gives the same drawing on every host, native and Melange.

Coordinates are in pixels from the top left corner, y downwards. Sizes assume a monospace font: 13 pixels for state names, 11 pixels for details and edge labels.

```ocaml
type node = {
  name : string;
  x : float;  (** Left side. *)
  y : float;  (** Top side. *)
  width : float;
  height : float;
  final : bool;
  initial : bool;
  details : string list;  (** {!Spec.details} of the state. *)
}
```

```ocaml
type label = {
  lines : string list;  (** The events, wrapped at about 24 characters. *)
  center : float;  (** The x of the middle of the lines. *)
  bottom : float;  (** The y under the last line. *)
}
```

```ocaml
type edge = {
  from : string;
  target : string;
  events : string list;
  points : (float * float) list;
      (** From the border of [from] to the border of [target]. Between two
          points with the same y the edge is a straight line; between two points
          with different y it is a cubic curve with horizontal tangents at both
          ends, which stays in the box of the two points. For a self edge
          ([from = target]) the four points are the start, the two control
          points and the end of one cubic curve above the node. *)
  back : bool;  (** The edge goes right to left, against the main direction. *)
  label : label option;  (** [None] when the edge has no events. *)
}
```

```ocaml
type t = {
  machine : string;  (** The id of the spec. *)
  width : float;
  height : float;
  nodes : node list;  (** In the order of the spec states. *)
  edges : edge list;  (** In the order of {!Spec.arrows}. *)
}
```

```ocaml
val layout : Spec.t -> t
```

Layers by longest path after the back edges of a depth first search from the initial state are turned around. Edge labels and long edges get places of their own in the columns between the layers, so labels and edges do not cover nodes. The order in each column comes from barycenter sweeps and adjacent swaps that reduce crossings; heights come from the median of the neighbours. Edges whose states are not in the spec are left out.

```ocaml
val to_string : t -> string
```

One line for the size, each node and each edge, with positions rounded to whole pixels. For golden tests and to compare hosts.

## Silica.Statechart_view

An SVG drawing of a `Statechart_layout.t`. Colors come from the silica-ui theme variables (`--ui-text`, `--ui-muted`, `--ui-border`, `--ui-surface`, `--ui-accent`, `--ui-accent-text`), with the light theme as fallback, so the same node works in a themed page and as a standalone SVG file (render it with `Render.to_string`).

Class names for styles and tests: `chart` on the `svg`; `chart-node` on each state group, with `is-active`, `is-final` and `is-initial`, and the state name in `data-state`; `chart-edge` on each edge group, with `is-taken` and `is-back`, and the states in `data-from` and `data-target`; `chart-start` on the start mark.

```ocaml
val view :
  ?current:string -> ?taken:string * string -> Statechart_layout.t -> 'e Node.t
```

`current` fills the node of that state with the accent color. `taken` is the `(from, target)` of an edge to draw in the accent color, such as the last step.

## Silica.Status

Where Remote or Live data is. A plain value that a machine keeps in its snapshot data. The old data stays when there is some.

```ocaml
type 'a t =
  | Idle  (** Nothing was asked for. *)
  | Loading of 'a option
      (** An answer is on its way. [Some old] while a reload runs. *)
  | Ready of { data : 'a; stale : bool }
      (** [stale] is always [false] for Remote data. For Live data it is [true]
          while the socket is down and the old data is kept. *)
  | Failed of { error : Endpoint.error; last : 'a option }
```

```ocaml
val data : 'a t -> 'a option
```

The data of `Ready`, the old data of `Loading`, or the `last` of `Failed`.

```ocaml
val is_loading : 'a t -> bool
```

`true` for `Loading`.

```ocaml
val is_stale : 'a t -> bool
```

`true` for `Ready` with `stale = true`.

```ocaml
val map : ('a -> 'b) -> 'a t -> 'b t
```

Change the data, old data included, and keep the status.

```ocaml
val codec : 'a Codec.t -> 'a t Codec.t
```

The status as `[@@deriving json]` writes a variant, with the codec of the data: `["Idle"]`, `["Loading", old]`, `["Ready", {"data": .., "stale": ..}]` or `["Failed", {"error": .., "last": ..}]`. A missing `last` or `old` is `null`.

```ocaml
val fingerprint : 'a Fingerprint.t -> 'a t Fingerprint.t
```

The fingerprint of `'a t` from the one of `'a` (see `Fingerprint`).

## Silica.Topic

A named, keyed piece of server data with a state type. The server publishes the full state under a key; every subscriber of that name and key gets it with a sequence number. The server holds no machine for a topic.

```ocaml
type 's t
```

```ocaml
val declare : name:string -> state:'s Codec.t -> 's t
```

Declare a topic in shared code. `name` is the same on the server and in the browser.

```ocaml
val name : 's t -> string
```

The name of the topic.

```ocaml
val state : 's t -> 's Codec.t
```

The codec of the topic state.

```ocaml
val state_id : 's t -> 's Type.Id.t
```

A type id made by `declare`, one for each topic. The checker uses it to find the sample states of a topic.

```ocaml
type 's update = { seq : int; status : 's Status.t }
```

What a subscription raises as an event. `seq` is the sequence number of the state in `status`; it is `0` when the status holds no new state: connecting, offline (the old data with `stale = true`) or a failure. A client drops a state whose `seq` is not higher than the last one it got on the socket.

```ocaml
val update_codec : 's Codec.t -> 's update Codec.t
```

`update_codec state` writes `{"seq": n, "status": ..}`, the status by `Status.codec`. A machine whose events hold updates uses it in its event codec (see `Machine.shape`). For a topic: `update_codec (Topic.state topic)`.

## Silica.Trace

A trace: the transitions of one run, as a file that a program can read.

The file is JSON Lines: one header object on the first line, then one `Transition.t` per line (see `Transition.codec`), in `seq` order. Lines can be appended and streamed, and a file cut after any line still parses.

```
{"root":"app","url":"/","build":"dev","origin":{"type":"runtime","platform":"memory"},"dropped":0}
{"seq":1,"time_ms":0,"instance":"app","number":1,"entry":0,"machine":"app","from":"","to":"loading","change":"started","event":"","actions":["start call 1.0/app/loading//api/issues null /api/issues"],"cause":{"type":"start"}}
{"seq":2,"time_ms":40,"instance":"app","number":1,"entry":1,"machine":"app","from":"loading","to":"ready","change":"entered","event":"loaded","actions":["stop 1.0/app/loading//api/issues null"],"cause":{"type":"answer","id":"1.0/app/loading//api/issues null","path":"/api/issues","answer":["Ok",[]]}}
```

What a trace holds, for replay: the start of each instance with its props and snapshot, each step with its event and its cause (the element and its accessible name, the timer, the answer of a call, the Live update, the URL), and the time of each step. The data of events, props and snapshots is there only when the machine registered those codecs (see `Machine.register_codecs`) and the recorder keeps data.

What a trace does not hold: values with no registered codec; the view (only the element of each handler, by role, name and path); focus, scroll and other browser state that is not in a snapshot; the HTTP status and body of an answer (only the answer as the machine got it); the Live socket frames; the server state behind calls and topics; the events of a step that raised (the step is not recorded); the records dropped by a full recorder (only their number).

A trace with names only (see `names_only`) holds no event data, props, snapshots, answers, Live updates, input text, accessible names, URL queries or page titles, and its ids of timers, calls and sources are numbers: the id of a call no longer holds its encoded input (see `Source.key`).

```ocaml
type origin =
  | Checker of { rule : string; start : int; choices : int list }
      (** A counterexample of the checker that breaks [rule]. [start] is the
          position of its start among the starts of the check (one for each of
          its [props]). [choices] has one number for each step after the start:
          the position of the move of the step among the moves that the check
          offered at that point (the handlers of the view, the inputs, the
          answers of each call, the timers, in the order of [Check.component]).
          With the same check, it replays each step exactly, with no codec. *)
  | Runtime of { platform : string }
```

```ocaml
type header = {
  root : string;  (** The machine id of the root instance. *)
  url : string option;  (** The URL of the page, when there is one. *)
  build : string;  (** The version or commit of the app, given by the app. *)
  origin : origin;
  dropped : int;  (** The records that a full recorder dropped. *)
  loop_from : int option;
      (** For a run that loops forever: the [seq] where the loop starts. *)
}
```

```ocaml
type t = { header : header; transitions : Transition.t list }
```

```ocaml
val to_jsonl : t -> string
```

The file: the header and each transition, each on one line ending with a newline.

```ocaml
val of_jsonl : string -> (t, string) result
```

Read a file. Empty lines are skipped. The error names the line number and the reason, such as `"line 3: field \"seq\": expected an integer"`.

```ocaml
val to_text : t -> string
```

One line for the header, then one line for each transition, for a person or an agent to read: `12 +840ms issue-list loading -> ready (entered) on loaded <- answer /api/issues`. Unhandled steps are marked `(unhandled)`; actions follow on the same line after `=>`.

```ocaml
val names_only : t -> t
```

The trace with names only: `Transition.names_only` of its transitions, and the URL of the header without query and fragment. A recorder without `data` gives this form.

```ocaml
module Recorder : sig
  type trace := t
  type t

  val create :
    ?capacity:int ->
    ?data:bool ->
    ?clock:(unit -> float) ->
    ?platform:string ->
    build:string ->
    unit ->
    t
  (** [capacity] (default 1000) is the number of transitions kept; older ones
      are dropped and counted. With [data] (default [false]) the records keep
      event data, props, snapshots, answers, Live updates and input text;
      without it they keep names only (see {!names_only}). [clock] gives the
      time of each record in milliseconds; with no clock records have no time.
      [platform] (default ["browser"]) goes in the header. *)

  val observer : t -> Instance.observer
  (** The observer to give a host, such as {!Instance.mount},
      [Silica_browser.mount ~observe] or [Server.live ~observe]. *)

  val record : t -> Transition.t -> unit
  (** Keep a record made elsewhere. *)

  val transitions : t -> Transition.t list
  (** The records kept, oldest first. Their elements are computed now (see
      {!Transition.pending}). *)

  val trace : t -> url:string option -> trace
  (** The trace of the records kept. The root is the machine of the first record
      whose instance has no parent. *)

  val clear : t -> unit
  (** Drop every record and set [dropped] to 0. The numbering goes on. *)
end
```

A ring buffer of transitions, to keep the last ones of a run.

## Silica.Transition

The transition log: one record for each start, step and end of each instance.

A host gives an `Instance.observer` to `Instance.mount` (in the `create` function of `Runtime.start`), to the Live runner of the server, or to `Silica_browser.mount ~observe`. `observer` makes one: it turns each step into a `t` with the names of the states and of the event, the data of the event, the props and the snapshot by the codecs of the machine (see `Machine.register_codecs`), the actions as text and the cause. `Trace.Recorder` keeps the records and writes them as a trace file.

A record holds names always, and data only when the machine has registered the codec for it. A release bundle that registers no codec gives records with names only.

**Records**

```ocaml
type change = Instance.change =
  | Started  (** The instance started: [from] is [""]. *)
  | Resumed
      (** The instance started again at a snapshot it reached before, such as a
          Live island after a server restart: [from] is [""], [target] is the
          state of that snapshot, [actions] are only the starts of what it owns
          (see {!Machine.resume}), and the cause is [Start]. *)
  | Unhandled  (** The machine ignored the event: nothing changed. *)
  | Kept  (** The step kept the state name. *)
  | Entered  (** The step entered a state, with the same name or another. *)
  | Ended
```

```ocaml
type cause =
  | Start  (** The start of an instance. *)
  | Element of {
      handler : string;
          (** ["press"], ["double_press"], ["input"], ["submit"], ["focus"],
              ["blur"], ["key_down"], ["key_up"], ["pointer_enter"] or
              ["pointer_leave"]. *)
      role : string option;
          (** The role of the element, by {!Accessibility}. *)
      name : string;  (** The accessible name of the element. *)
      element : string;
          (** Where the element is, as {!Accessibility.path} gives it. *)
      value : string option;
          (** The text of an [input], or the name of the key of a [key_down] or
              [key_up] (see {!Key.to_string}). *)
    }  (** A handler of an element of the view. *)
  | Timer of { id : string; ms : int }  (** A timer that the state owned. *)
  | Answer of { id : string; path : string; answer : Silica_json.Json.t option }
      (** The answer of a call to the endpoint at [path], by
          {!Endpoint.answer_codec}. [None] when data is not recorded. *)
  | Live of { id : string; topic : string; update : Silica_json.Json.t option }
      (** A state or a change of connection of a Live subscription, by
          {!Topic.update_codec}. [None] when data is not recorded. *)
  | Held of { id : string; source : string }
      (** A held source, by its {!Action.source_name}. *)
  | Action_result of { action : string }
      (** The result of an action, such as {!Action.Measure}. *)
  | Props  (** The parent gave new props. *)
  | Child of { instance : string }
      (** The [output] or the [on_final] of the child at this instance path. *)
  | Url of string  (** The URL changed: the root got [Router.Url_changed]. *)
  | Preload of string  (** The root got [Router.Preload] for this URL. *)
  | Message
      (** The host gave the root an event (see {!Instance.val-message}). *)
  | Ended
```

```ocaml
type t = {
  seq : int;  (** From 1, one higher for each record of a hook. *)
  time_ms : float option;
      (** The clock of the observer when the record was made, in milliseconds.
      *)
  instance : string;
      (** The instance path: the root is its machine id; a child adds
          ["/<slot>:<machine id>"] to the path of its parent, where the slot is
          the positions of the component node in the view of the parent, joined
          by ["."], and a key is [#key]. [%], [/], [:], [#] and [.] in a key are
          percent encoded. Example: [app/0.2:router/#inbox:issue-list]. *)
  number : int;
      (** The number of the instance in its tree, as in timer and source ids. *)
  entry : int;
      (** The number of the entry of the state after the step, as in timer and
          source ids. *)
  machine : string;  (** {!Machine.id}. *)
  from : string;
      (** The state name before; [""] for [Started] and [Resumed]. *)
  target : string;  (** The state name after. *)
  change : change;
  event : string;
      (** {!Machine.event_name} of the event; [""] when there is no event
          ([Started], [Resumed], [Ended], [Props]). *)
  data : Silica_json.Json.t option;
      (** The event, by the event codec of the machine. *)
  props : Silica_json.Json.t option;
      (** The props, by the props codec of the machine, for [Started], [Resumed]
          and for a [Props] cause. *)
  snapshot : Silica_json.Json.t option;
      (** The snapshot after, by the state codec of the machine. *)
  actions : string list;
      (** The actions given to the host, in order, as {!describe} writes them.
      *)
  cause : cause;
}
```

```ocaml
val codec : t Codec.t
```

One JSON object. Optional fields that are `None` and empty `actions` are left out.

```ocaml
val describe : 'm Action.host -> string
```

One action as short text, such as `"start timer 1.2/save/saved/after 2000ms 2000ms"`, `"start call 1.1/save/saving//api/save null /api/save"` (the id, then the path), `"stop 1.1/save/saving//api/save null"`, `"focus title"` or `"navigate /issues"`. The text of a `Copy` and the value of a cookie are left out.

```ocaml
val strip : t -> t
```

The record with no user data: `data`, `props`, `snapshot`, the `answer` of an `Answer`, the `update` of a `Live` and the `value` of an `input` are `None`.

```ocaml
val names_only : t list -> t list
```

The records of one trace with names only, as a recorder without data keeps them: each record is `strip`ped, and also:

- Each id of a timer, call, subscription or held source, in causes and in actions, becomes `"<number>.<entry>/<machine id>/#<n>"`, where `n` counts the distinct ids of the list from 1. The same id gets the same `n`, so a start, its answer and its stop still match, but the id no longer holds the state, the path, the encoded input of a call or the key of a subscription (see `Source.key`).
- The accessible `name` of the element of a handler is `""`: it can be text that a user typed, such as the title of an issue in a link.
- A URL keeps its path only, without query and fragment: in `Url` and `Preload` causes and in `navigate` and `replace` actions.
- `set_title` actions lose their title, `start live` actions their key, and `set_document_attribute` actions their value.

What stays: the sequence numbers, times, instance paths (with the keys that the app gave component nodes), machine ids, state and event names, changes, handler names, roles and element paths, timer delays, endpoint paths, topic and source names, and the other actions as `describe` writes them.

**Observers**

```ocaml
type pending
```

A record whose element may not be known yet. The role and name of the element of a handler are computed from the view, which costs a walk of it, so they wait until the record is read. At the input the observer only finds the element and its path; the record keeps that element and the tag and attributes of the elements above it, which are immutable data, so the answer is the same later. When the name depends on other parts of the page (`aria-labelledby` in the element, or a field with an `id` or inside a `label`), the record keeps the whole view.

```ocaml
val resolve : pending -> t
```

The record, with its element. The first call does the work.

```ocaml
val peek : pending -> t
```

The record as it is now, with no work: the element of a handler may still have no role, an empty name and an empty path.

```ocaml
val ready : t -> pending
```

A record that is known.

```ocaml
val observer : ?clock:(unit -> float) -> (pending -> unit) -> Instance.observer
```

`observer ?clock record` numbers the steps from 1, reads the `clock` (no time when it is not given) and gives each record to `record`. The work happens only in the observer: a host with no observer does none, and an app that never calls this function carries none of its code.

## Silica.Url

Application URLs: path segments, query pairs and hash, percent-encoded.

```ocaml
type t = {
  path : string list;
  query : (string * string) list;
  hash : string option;
}
```

```ocaml
val of_string : string -> t
```

Empty path segments are dropped. Invalid escapes are kept as written.

```ocaml
val to_string : t -> string
```

The empty path prints as `"/"`. Everything except `A-Za-z0-9-._~` is percent-encoded.

```ocaml
val fingerprint : t Fingerprint.t
```

The fingerprint of `t` (see `Fingerprint`).

## Silica.View_text

A view as text that a person or a program can read and compare, with no browser. The checker, `Silica_memory` and the golden files print views with this one function.

One line per element that has a role or a handler, indented by two spaces for each such element above it. A line is:

- the role (see `Accessibility.role`), or the tag when the element has a handler and no role;
- the accessible name in quotes, when it is not empty;
- the states that are set, in this order: `level=2`, `value="..."`, `checked` or `checked=false`, `selected`, `expanded` or `expanded=false`, `disabled`, `current=page`, `href="..."`, `modal`;
- the handlers in brackets, such as `[press, key_down]`.

Other elements are left out and their children move up. The text that is not in such a line prints as `text "..."`: the text of the elements left out side by side is joined into one line, with a space where an element starts or ends and white space collapsed. Text inside an element whose role takes its name from its content (see `Accessibility.name_from_content`) is its name and does not print again.

A component node prints as `component <id>`, an island as `island <channel>`, a chunk that loads as `loading`. Hidden elements (see `Accessibility.hidden`) and their children are left out. The children of a portal print at the place of the portal. In a quoted string, a double quote and a backslash are escaped with a backslash. Each line ends with a newline.

```ocaml
val to_string : 'e Node.t -> string
```

```ocaml
val visible : 'e Node.t -> string
```

The text of the view that a person can see: every text node, side by side with no separator, as the `textContent` of the page, without the text of hidden elements (see `Accessibility.hidden`), of component nodes and of loading chunks.

```ocaml
val shows : 'e Node.t -> string -> bool
```

`shows view text`: `text` is a part of `visible` `view`.
