let app =
  Silica_browser.mount_app
    ~root:(Silica_browser.by_id "silica-root")
    (Site_app.make
       (Site_theme.choice (Silica_browser.cookie Silica_ui.Theme.cookie_name))
    )
