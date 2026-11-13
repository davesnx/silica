let app =
  Silica_browser.mount
    ~root:(Silica_browser.by_id "silica-root")
    Tutorial_greeting.component
