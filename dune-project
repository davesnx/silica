(lang dune 3.20)
(using melange 1.0)
(using directory-targets 0.1)
(using mdx 0.4)
(generate_opam_files true)

(name silica)

(version 0.1.0)
(maintainers "David Sancho <dsnxmoreno@gmail.com>")
(authors "David Sancho <dsnxmoreno@gmail.com>")
(license MIT)

(package
 (name silica)
 (synopsis "View framework where all behaviour is statecharts")
 (depends
  (ocaml (>= 5.1))
  (melange (>= 7.0))
  jsonkit
  jsonkit-melange
  tiny_httpd
  (alcotest :with-test)
  (odoc :with-doc)
  (mdx :with-test)))

(package
 (name silica-jsonkit-melange)
 (synopsis "Silica codecs from jsonkit-melange, for Melange code")
 (depends
  (silica (= :version))
  (melange (>= 7.0))
  jsonkit-melange))

(package
 (name silica-ui)
 (synopsis "Headless components for Silica, after Base UI")
 (depends
  (silica (= :version))))

(dialect
 (name mlx)
 (implementation
  (extension mlx)
  (merlin_reader mlx)
  (format
   (run ocamlformat-mlx %{input-file}))
  (preprocess
   (run mlx-pp %{input-file}))))
