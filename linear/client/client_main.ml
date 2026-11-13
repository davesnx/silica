let app =
  Silica_browser.mount_app
    ~root:(Silica_browser.by_id "silica-root")
    (Linear_app.make
       (Linear_theme.choice (Silica_browser.cookie Silica_ui.Theme.cookie_name))
    )
