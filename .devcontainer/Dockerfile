FROM node:22-bookworm-slim AS node

FROM ocaml/opam:debian-12-ocaml-5.4
COPY --from=node /usr/local/bin /usr/local/bin
COPY --from=node /usr/local/lib/node_modules /usr/local/lib/node_modules
COPY --chown=opam:opam silica.opam /home/opam/deps/silica.opam
RUN opam update \
 && opam install -y dune.3.24.2 melange.7.0.1-54 mlx.0.12 ocamlmerlin-mlx.0.12 \
      ocamlformat.0.28.1 ocamlformat-mlx.0.29.0.2 ocaml-lsp-server.1.26.0 \
      jsonkit.1.1.0 jsonkit-melange.1.1.0 tiny_httpd.0.22 \
      odoc-parser.3.2.2 ppxlib.0.39.0 \
 && opam install -y --deps-only --with-test --with-doc /home/opam/deps/silica.opam \
 && opam clean -a -c -s --logs
ENV OPAM_SWITCH_PREFIX=/home/opam/.opam/5.4 \
    CAML_LD_LIBRARY_PATH=/home/opam/.opam/5.4/lib/stublibs:/home/opam/.opam/5.4/lib/ocaml/stublibs:/home/opam/.opam/5.4/lib/ocaml \
    OCAML_TOPLEVEL_PATH=/home/opam/.opam/5.4/lib/toplevel \
    OCAMLTOP_INCLUDE_PATH=/home/opam/.opam/5.4/lib/toplevel \
    MANPATH=:/home/opam/.opam/5.4/man \
    PATH=/home/opam/.opam/5.4/bin:$PATH
