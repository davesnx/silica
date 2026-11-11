include Test_apps

let mount_basic root = Silica_browser.mount ~root component

let mount_router root = Silica_browser.mount_app ~root app

let fast = { Silica_browser.ping_ms = 30; pong_ms = 30 }

let mount_live root = Silica_browser.mount_live ~root live

let mount_live_gone root =
  Silica_browser.mount_live
    ~on_session_gone:(fun () -> incr gone_live)
    ~root live

let mount_live_fast root =
  Silica_browser.mount_live ~live_config:fast ~root live

let mount_topic root = Silica_browser.mount ~root watch_component

let mount_topic_gone root =
  Silica_browser.mount
    ~on_session_gone:(fun () -> incr gone)
    ~root watch_component

let mount_island root =
  Silica_browser.mount ~lives:[ Silica_browser.Live live ] ~root host_component

let mount_unknown_island root = Silica_browser.mount ~root host_component

let mount_timer root = Silica_browser.mount ~root timer_component

let mount_parent root = Silica_browser.mount ~root parent_component

let mount_tickers root = Silica_browser.mount ~root ticker_host_component
