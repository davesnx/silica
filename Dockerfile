FROM node:22-bookworm-slim AS node

FROM ocaml/opam:debian-12-ocaml-5.4 AS build
RUN opam update \
 && opam install -y dune.3.24.2 melange.7.0.1-54 mlx.0.12 \
      jsonkit.1.1.0 jsonkit-melange.1.1.0 tiny_httpd.0.22 \
      odoc-parser.3.2.2 ppxlib.0.39.0
COPY --from=node /usr/local/bin/node /usr/local/bin/node
WORKDIR /home/opam/silica
COPY --chown=opam:opam . .
RUN opam exec -- dune build --profile release \
      site/server/main.exe site/client/meta.json site/export/export.exe

FROM build AS export
RUN ./_build/default/site/export/export.exe --out /home/opam/static

FROM scratch AS static
COPY --from=export /home/opam/static /

FROM debian:12-slim
RUN useradd --system --uid 10001 --no-create-home silica
COPY --from=build /home/opam/silica/_build/default/site/server/main.exe /app/site
COPY --from=build /home/opam/silica/_build/default/site/client/dist /app/dist
COPY --from=build /home/opam/silica/_build/default/site/client/meta.json /app/meta.json
ENV PORT=8080 \
    SILICA_ADDR=0.0.0.0 \
    SILICA_STATIC=/app/dist \
    SILICA_METAFILE=/app/meta.json
USER silica
EXPOSE 8080
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s \
  CMD ["bash", "-c", "exec 3<>/dev/tcp/127.0.0.1/$PORT && printf 'GET /_silica/health HTTP/1.0\\r\\n\\r\\n' >&3 && head -n 1 <&3 | grep -q ' 200 '"]
CMD ["/app/site"]
