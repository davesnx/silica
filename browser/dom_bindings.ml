type node
type event

external document : node = "document"
external location : node = "location"
external history : node = "history"
external window : node = "window"

external get_by_id : node -> string -> node Js.nullable = "getElementById"
[@@mel.send]

external create_element : node -> string -> node = "createElement" [@@mel.send]
external create_text : node -> string -> node = "createTextNode" [@@mel.send]
external append_child : node -> node -> unit = "appendChild" [@@mel.send]

external insert_before : node -> node -> node Js.nullable -> unit
  = "insertBefore"
[@@mel.send]

external remove_child : node -> node -> unit = "removeChild" [@@mel.send]
external first_child : node -> node Js.nullable = "firstChild" [@@mel.get]
external next_sibling : node -> node Js.nullable = "nextSibling" [@@mel.get]
external last_child : node -> node Js.nullable = "lastChild" [@@mel.get]

external previous_sibling : node -> node Js.nullable = "previousSibling"
[@@mel.get]

external parent_node : node -> node Js.nullable = "parentNode" [@@mel.get]
external node_value : node -> string = "nodeValue" [@@mel.get]
external set_node_value : node -> string -> unit = "nodeValue" [@@mel.set]
external tag_name : node -> string = "tagName" [@@mel.get]

external get_attribute : node -> string -> string Js.nullable = "getAttribute"
[@@mel.send]

external set_attribute : node -> string -> string -> unit = "setAttribute"
[@@mel.send]

external remove_attribute : node -> string -> unit = "removeAttribute"
[@@mel.send]

external value : node -> string = "value" [@@mel.get]
external set_value : node -> string -> unit = "value" [@@mel.set]
external default_value : node -> string = "defaultValue" [@@mel.get]
external set_checked : node -> bool -> unit = "checked" [@@mel.set]
external set_title : node -> string -> unit = "title" [@@mel.set]
external focus : node -> unit = "focus" [@@mel.send]
external blur : node -> unit = "blur" [@@mel.send]
external closest : node -> string -> node Js.nullable = "closest" [@@mel.send]
type node_list

external query_all : node -> string -> node_list = "querySelectorAll"
[@@mel.send]

external array_from : node_list -> node array = "from" [@@mel.scope "Array"]

external add_listener : node -> string -> (event -> unit) -> unit
  = "addEventListener"
[@@mel.send]

external target : event -> node = "target" [@@mel.get]
external prevent_default : event -> unit = "preventDefault" [@@mel.send]
external default_prevented : event -> bool = "defaultPrevented" [@@mel.get]
external key : event -> string = "key" [@@mel.get]
external button : event -> int = "button" [@@mel.get]
external ctrl_key : event -> bool = "ctrlKey" [@@mel.get]
external meta_key : event -> bool = "metaKey" [@@mel.get]
external shift_key : event -> bool = "shiftKey" [@@mel.get]
external alt_key : event -> bool = "altKey" [@@mel.get]
external origin : node -> string = "origin" [@@mel.get]
external pathname : node -> string = "pathname" [@@mel.get]
external search : node -> string = "search" [@@mel.get]
external hash : node -> string = "hash" [@@mel.get]
external href : node -> string = "href" [@@mel.get]

let write_history : bool -> int -> int -> string -> unit =
  [%mel.raw
    {|function (replace, key, index, url) { history[replace ? "replaceState" : "pushState"]({ silicaKey: key, silicaIndex: index }, "", url) }|}]

let history_key : unit -> int Js.nullable =
  [%mel.raw
    {|function () { var s = history.state; return s && typeof s.silicaKey === "number" ? s.silicaKey : null }|}]

let history_index : unit -> int Js.nullable =
  [%mel.raw
    {|function () { var s = history.state; return s && typeof s.silicaIndex === "number" ? s.silicaIndex : null }|}]

let history_go : int -> unit =
  [%mel.raw {|function (delta) { history.go(delta) }|}]

let session_get : string -> string Js.nullable =
  [%mel.raw
    {|function (key) { try { return window.sessionStorage.getItem(key) } catch (e) { return null } }|}]

let session_set : string -> string -> unit =
  [%mel.raw
    {|function (key, value) { try { window.sessionStorage.setItem(key, value) } catch (e) {} }|}]

let ask_before_unload : unit -> unit -> unit =
  [%mel.raw
    {|function () {
  var ask = function (ev) { ev.preventDefault(); ev.returnValue = "" };
  window.addEventListener("beforeunload", ask);
  return function () { window.removeEventListener("beforeunload", ask) };
}|}]

let after_task : (unit -> unit) -> unit =
  [%mel.raw {|function (f) { Promise.resolve().then(function () { f() }) }|}]

let create_announcer : node -> node =
  [%mel.raw
    {|function (parent) {
  var region = document.createElement("div");
  region.setAttribute("data-silica-announcer", "");
  region.setAttribute("aria-live", "polite");
  region.setAttribute("aria-atomic", "true");
  region.setAttribute("style", "position:absolute;width:1px;height:1px;margin:-1px;padding:0;overflow:hidden;clip:rect(0,0,0,0);white-space:nowrap;border:0");
  parent.appendChild(region);
  return region;
}|}]

external root_node : node -> node = "getRootNode" [@@mel.send]

let active_in : node -> node Js.nullable =
  [%mel.raw {|function (scope) { return scope.activeElement || null }|}]

let find_by_id_in : node -> string -> node Js.nullable =
  [%mel.raw
    {|function (scope, id) {
  if (typeof scope.getElementById === "function") return scope.getElementById(id);
  var all = scope.querySelectorAll("[id]");
  for (var i = 0; i < all.length; i++) if (all[i].id === id) return all[i];
  return null;
}|}]

let watch_visible : (node -> unit) -> node -> unit =
  [%mel.raw
    {|function (callback) {
  if (typeof IntersectionObserver === "undefined") return function () {};
  var observer = null;
  return function (el) {
    if (!observer) observer = new IntersectionObserver(function (entries) {
      entries.forEach(function (entry) {
        if (entry.isIntersecting) { observer.unobserve(entry.target); callback(entry.target) }
      });
    });
    observer.observe(el);
  };
}|}]

external title : node -> string = "title" [@@mel.get]

let manual_scroll_restoration : unit -> unit =
  [%mel.raw
    {|function () { try { history.scrollRestoration = "manual" } catch (e) {} }|}]

let scroll_x : unit -> int =
  [%mel.raw {|function () { return Math.round(window.scrollX || 0) }|}]

let scroll_y : unit -> int =
  [%mel.raw {|function () { return Math.round(window.scrollY || 0) }|}]

let scroll_to : int -> int -> unit =
  [%mel.raw
    {|function (x, y) { if (typeof window.scrollTo === "function") { try { window.scrollTo(x, y) } catch (e) {} } }|}]

external back : node -> unit = "back" [@@mel.send]
external forward : node -> unit = "forward" [@@mel.send]
external reload : node -> unit = "reload" [@@mel.send]
let has_scroll : node -> bool =
  [%mel.raw {|function (n) { return typeof n.scrollIntoView === 'function' }|}]

external scroll_into_view : node -> unit = "scrollIntoView" [@@mel.send]

let microtask : (unit -> unit) -> unit =
  [%mel.raw {|function (run) { queueMicrotask(function () { run() }) }|}]

let scroll_into_view_nearest : node -> unit =
  [%mel.raw {|function (n) { n.scrollIntoView({ block: "nearest" }) }|}]

external set_title_ : node -> string -> unit = "title" [@@mel.set]

external node_type : node -> int = "nodeType" [@@mel.get]

external create_element_ns : node -> string -> string -> node
  = "createElementNS"
[@@mel.send]

external set_attribute_ns : node -> string -> string -> string -> unit
  = "setAttributeNS"
[@@mel.send]

external remove_attribute_ns : node -> string -> string -> unit
  = "removeAttributeNS"
[@@mel.send]

external get_attribute_ns : node -> string -> string -> string Js.nullable
  = "getAttributeNS"
[@@mel.send]

external namespace_uri : node -> string Js.nullable = "namespaceURI" [@@mel.get]
external attribute_names : node -> string array = "getAttributeNames"
[@@mel.send]
external contains : node -> node -> bool = "contains" [@@mel.send]
external is_connected : node -> bool = "isConnected" [@@mel.get]
external document_element : node -> node = "documentElement" [@@mel.get]
external body : node -> node = "body" [@@mel.get]
external active_element : node -> node Js.nullable = "activeElement" [@@mel.get]
external query_one : node -> string -> node Js.nullable = "querySelector"
[@@mel.send]
external node_name : node -> string = "nodeName" [@@mel.get]
external related_target : event -> node Js.nullable = "relatedTarget"
[@@mel.get]
external stop_propagation : event -> unit = "stopPropagation" [@@mel.send]

external remove_listener : node -> string -> (event -> unit) -> unit
  = "removeEventListener"
[@@mel.send]

external add_listener_capture :
  node -> string -> (event -> unit) -> bool -> unit = "addEventListener"
[@@mel.send]

external remove_listener_capture :
  node -> string -> (event -> unit) -> bool -> unit = "removeEventListener"
[@@mel.send]

external get_property : node -> string -> 'a Js.nullable = "" [@@mel.get_index]
external set_property : node -> string -> 'a -> unit = "" [@@mel.set_index]

let client_x : event -> float =
  [%mel.raw {|function (ev) { return ev.clientX || 0 }|}]

let client_y : event -> float =
  [%mel.raw {|function (ev) { return ev.clientY || 0 }|}]

let event_button : event -> int =
  [%mel.raw {|function (ev) { return ev.button || 0 }|}]

let event_repeat : event -> bool =
  [%mel.raw {|function (ev) { return ev.repeat === true }|}]

let style_get : node -> string -> string =
  [%mel.raw {|function (n, k) { return n.style[k] || "" }|}]

let style_set : node -> string -> string -> unit =
  [%mel.raw {|function (n, k, v) { n.style[k] = v }|}]

let scrollbar_width : unit -> int =
  [%mel.raw
    {|function () { return Math.max(0, (window.innerWidth || 0) - document.documentElement.clientWidth) }|}]

let observe_resize : node -> (float array -> unit) -> unit -> unit =
  [%mel.raw
    {|function (el, callback) {
  if (typeof ResizeObserver === "undefined") return function () {};
  var observer = new ResizeObserver(function (entries) {
    var r = entries[entries.length - 1].contentRect;
    callback([r.x, r.y, r.width, r.height]);
  });
  observer.observe(el);
  return function () { observer.disconnect() };
}|}]

let observe_intersection : node -> (bool -> unit) -> unit -> unit =
  [%mel.raw
    {|function (el, callback) {
  if (typeof IntersectionObserver === "undefined") return function () {};
  var observer = new IntersectionObserver(function (entries) {
    callback(entries[entries.length - 1].isIntersecting);
  });
  observer.observe(el);
  return function () { observer.disconnect() };
}|}]

let create_portal_root : node -> node =
  [%mel.raw
    {|function (parent) { var d = document.createElement("div"); d.setAttribute("data-silica-portal", ""); parent.appendChild(d); return d }|}]

let bounding_rect : node -> float array =
  [%mel.raw
    {|function (n) { var r = n.getBoundingClientRect(); return [r.left, r.top, r.width, r.height] }|}]

let viewport_size : unit -> float array =
  [%mel.raw
    {|function () { return [window.innerWidth || 0, window.innerHeight || 0] }|}]

let style_property : node -> string -> string -> unit =
  [%mel.raw {|function (n, k, v) { n.style.setProperty(k, v) }|}]

let on_layout_change : node array -> (unit -> unit) -> unit -> unit =
  [%mel.raw
    {|function (nodes, callback) {
  var options = { capture: true, passive: true };
  window.addEventListener("scroll", callback, options);
  window.addEventListener("resize", callback);
  var observer = typeof ResizeObserver === "undefined" ? null : new ResizeObserver(function () { callback() });
  if (observer) nodes.forEach(function (n) { observer.observe(n) });
  return function () {
    window.removeEventListener("scroll", callback, options);
    window.removeEventListener("resize", callback);
    if (observer) observer.disconnect();
  };
}|}]

let focus_without_scroll : node -> unit =
  [%mel.raw {|function (n) { n.focus({ preventScroll: true }) }|}]

let dispatch_input : node -> unit =
  [%mel.raw
    {|function (n) { n.dispatchEvent(new n.ownerDocument.defaultView.Event("input", { bubbles: true })) }|}]

let console_warn : string -> unit =
  [%mel.raw {|function (m) { console.warn(m) }|}]

external text_content : node -> string = "textContent" [@@mel.get]
external set_text_content : node -> string -> unit = "textContent" [@@mel.set]

let opt n = Js.Nullable.toOption n

external protocol : node -> string = "protocol" [@@mel.get]
external host : node -> string = "host" [@@mel.get]

let scroll_element : node -> int -> int -> unit =
  [%mel.raw
    {|function (n, left, top) { n.scrollLeft = left; n.scrollTop = top }|}]

let page_scroll : unit -> float array =
  [%mel.raw
    {|function () { var d = document.documentElement; return [window.scrollX, window.scrollY, d.scrollWidth, d.scrollHeight, d.clientWidth, d.clientHeight] }|}]

let element_scroll : node -> float array =
  [%mel.raw
    {|function (n) { return [n.scrollLeft, n.scrollTop, n.scrollWidth, n.scrollHeight, n.clientWidth, n.clientHeight] }|}]

let visible : unit -> bool =
  [%mel.raw {|function () { return document.visibilityState === "visible" }|}]

let online : unit -> bool =
  [%mel.raw {|function () { return window.navigator.onLine }|}]

let watch_media : string -> (bool -> unit) -> unit -> unit =
  [%mel.raw
    {|function (query, report) {
  var list = window.matchMedia(query);
  var changed = function () { report(list.matches) };
  report(list.matches);
  list.addEventListener("change", changed);
  return function () { list.removeEventListener("change", changed) };
}|}]

let set_document_attribute : string -> string Js.Nullable.t -> unit =
  [%mel.raw
    {|function (name, value) {
  var root = document.documentElement;
  if (value == null) root.removeAttribute(name); else root.setAttribute(name, value);
}|}]

let set_cookie : string -> string -> int -> unit =
  [%mel.raw
    {|function (name, value, maxAge) {
  document.cookie = name + "=" + value + "; Path=/; Max-Age=" + maxAge + "; SameSite=Lax";
}|}]

let cookie : string -> string Js.Nullable.t =
  [%mel.raw
    {|function (name) {
  var parts = document.cookie ? document.cookie.split("; ") : [];
  for (var i = 0; i < parts.length; i++) {
    var at = parts[i].indexOf("=");
    if (at > 0 && parts[i].slice(0, at) === name) return parts[i].slice(at + 1);
  }
  return null;
}|}]

let copy_text : string -> (bool -> unit) -> unit =
  [%mel.raw
    {|function (text, done) {
  var clipboard = window.navigator.clipboard;
  if (!clipboard || typeof clipboard.writeText !== "function") { done(false); return }
  clipboard.writeText(text).then(function () { done(true) }, function () { done(false) });
}|}]

let load_script : string -> (bool -> unit) -> unit =
  [%mel.raw
    {|function (src, done) {
  var scripts = document.querySelectorAll("script[data-silica-script]");
  for (var i = 0; i < scripts.length; i++) {
    var known = scripts[i];
    if (known.getAttribute("src") === src && known.__silicaScript) {
      if (known.__silicaScript.loaded) done(true);
      else known.__silicaScript.waiting.push(done);
      return;
    }
  }
  var script = document.createElement("script");
  var state = { loaded: false, waiting: [done] };
  script.__silicaScript = state;
  script.async = true;
  script.setAttribute("data-silica-script", "");
  script.setAttribute("src", src);
  var finish = function (ok) {
    var waiting = state.waiting;
    state.waiting = [];
    waiting.forEach(function (f) { f(ok) });
  };
  script.addEventListener("load", function () { state.loaded = true; finish(true) });
  script.addEventListener("error", function () { script.remove(); finish(false) });
  document.head.appendChild(script);
}|}]
