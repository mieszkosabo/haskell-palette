FROM docker.io/library/haskell:9 AS build-haskell

WORKDIR /build

COPY docker.cabal.config /build/cabal.config
ENV CABAL_CONFIG /build/cabal.config

RUN cabal v2-update

COPY haskell-palette.cabal /build/
RUN cabal v2-build -v1 -j4 --dependencies-only all

COPY /src /build/src
COPY /app /build/app
RUN mkdir /build/artifacts
RUN cabal v2-install -v1 --installdir='/build/artifacts'

FROM docker.io/library/ubuntu:20.04 AS build-assets

SHELL ["/usr/bin/bash", "-euExo", "pipefail", "-O", "inherit_errexit", "-c"]

RUN apt-get update && \
    apt-get install -y --no-install-recommends curl ca-certificates && \
    apt-get clean all && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /build

RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
RUN gunzip elm.gz
RUN ln ./elm /usr/bin/elm
RUN chmod +x /usr/bin/elm

COPY /frontend /build
RUN /usr/bin/elm make --optimize ./src/Main.elm

FROM docker.io/library/ubuntu:20.04

WORKDIR /haskell-palette

COPY --from=build-haskell /build/artifacts/haskell-palette /usr/bin/
COPY --from=build-assets /build/index.html .
EXPOSE 3000/tcp
ENV SOURCE_PATH=/haskell-palette/

ENTRYPOINT ["/usr/bin/haskell-palette"]
