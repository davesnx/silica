let () =
  Silica_browser.mount_app ~root:"silica-root"
    (Linear_app.make
       (Linear_theme.choice (Silica_browser.cookie Silica_ui.Theme.cookie_name))
    )
