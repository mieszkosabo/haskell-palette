FROM docker.io/library/haskell@sha256:51cbc46421f75088ea9ff2ab367d37c99ded755a0472da80b898bdf1e738813a AS build-haskell

RUN wget -nv -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN apt-get update && \
    apt-get install -y --no-install-recommends lsb-release wget software-properties-common && \
    apt-get clean all && \
    rm -rf /var/lib/apt/lists/*

RUN wget -nv https://apt.llvm.org/llvm.sh
RUN chmod +x llvm.sh
RUN ./llvm.sh 11
RUN ln /usr/bin/llc-11 /usr/bin/llc
RUN ln /usr/bin/opt-11 /usr/bin/opt

WORKDIR /build

COPY Makefile /build/Makefile
COPY docker.cabal.config /build/cabal.config
ENV CABAL_CONFIG /build/cabal.config

RUN make cabal-update

COPY haskell-palette.cabal /build/
RUN make build-dependencies

COPY /src /build/src
COPY /app /build/app
RUN mkdir /build/artifacts
RUN INSTALL_DIR='/build/artifacts' make install

FROM docker.io/library/ubuntu:20.04 AS build-assets

SHELL ["/usr/bin/bash", "-euExo", "pipefail", "-O", "inherit_errexit", "-c"]

RUN apt-get update && \
    apt-get install -y --no-install-recommends make curl ca-certificates && \
    apt-get clean all && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /build

RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
RUN gunzip elm.gz
RUN ln ./elm /usr/bin/elm
RUN chmod +x /usr/bin/elm

COPY /frontend /build
RUN ELM=/usr/bin/elm make build

FROM docker.io/library/ubuntu:20.04

WORKDIR /haskell-palette

COPY --from=build-haskell /build/artifacts/haskell-palette /usr/bin/
COPY --from=build-assets /build/index.html .
EXPOSE 3000/tcp
ENV SOURCE_PATH=/haskell-palette/

ENTRYPOINT ["/usr/bin/haskell-palette", "+RTS", "-N"]
