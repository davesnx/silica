let () =
  Silica_browser.mount_app ~root:"silica-root"
    (Site_app.make
       (Site_theme.choice (Silica_browser.cookie Silica_ui.Theme.cookie_name))
    )
