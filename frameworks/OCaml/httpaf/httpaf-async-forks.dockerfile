# -*- mode: dockerfile -*-

FROM ocurrent/opam:alpine-3.12-ocaml-4.11

# Core fails without this
RUN sudo apk add -u tzdata
ENV TZ=UTC

RUN \
  opam depext dune httpaf httpaf-async async core yojson && \
  opam install dune httpaf httpaf-async async core yojson

COPY . /app

WORKDIR /app

RUN \
  sudo chown -R opam: . && \
  eval $(opam env) && \
  dune build --release async/httpaf_async.exe

EXPOSE 8080

CMD _build/default/async/httpaf_async.exe supervisor
