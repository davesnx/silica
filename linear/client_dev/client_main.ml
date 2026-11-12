open Silica

let has_meta : string -> bool =
  [%mel.raw
    {|function (name) { return document.querySelector('meta[name="' + name + '"]') !== null }|}]

let location : unit -> string =
  [%mel.raw {|function () { return location.pathname + location.search }|}]

let post : string -> string -> unit =
  [%mel.raw
    {|function (path, body) {
  var headers = { "content-type": "application/json" };
  var meta = document.querySelector('meta[name="silica-csrf"]');
  if (meta) headers["x-silica-csrf"] = meta.getAttribute("content") || "";
  var failed = function (reason) { console.error("mini Linear: the trace was not saved: " + reason) };
  fetch(path, { method: "POST", headers: headers, body: body }).then(function (response) {
    if (!response.ok) response.text().then(failed);
  }, failed);
}|}]

let on_hidden : (unit -> unit) -> unit =
  [%mel.raw
    {|function (send) {
  document.addEventListener("visibilitychange", function () {
    if (document.visibilityState === "hidden") send();
  });
}|}]

let () = Linear_codecs.register ()

let observe =
  if has_meta Linear_trace.meta then (
    let recorder = Silica_browser.recorder ~data:true ~build:"dev" () in
    on_hidden (fun () ->
        post
          (Remote.path Linear_trace.endpoint)
          (Codec.to_string
             (Remote.input Linear_trace.endpoint)
             (Trace.to_jsonl
                (Trace.Recorder.trace recorder ~url:(Some (location ())))
             )
          )
    );
    Some (Trace.Recorder.observer recorder)
  ) else
    None

let () =
  Silica_browser.mount_app ?observe ~root:"silica-root"
    (Linear_app.make
       (Linear_theme.choice (Silica_browser.cookie Silica_ui.Theme.cookie_name))
    )
