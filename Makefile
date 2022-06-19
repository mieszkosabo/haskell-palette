all: image

SHELL :=/bin/bash -euEo pipefail -O inherit_errexit

IMAGE_TAG ?= latest
IMAGE_REF ?= docker.io/rzetelskik/haskell-palette:$(IMAGE_TAG)
INSTALL_DIR ?= $(shell pwd)

ELM_BIN ?= $(shell which elm)

cabal-update:
	cabal v2-update
.PHONY: cabal-update

build-dependencies: cabal-update
	cabal v2-build -v1 --dependencies-only all
.PHONY: build-dependencies

install: build-dependencies
	cabal v2-install -v1 --overwrite-policy=always --installdir=$(INSTALL_DIR) 
.PHONY: install

build-frontend:
	$(MAKE) -C frontend build

run-dev: install
	cabal v2-run haskell-palette -- +RTS -N
.PHONY: run-dev

image:
	docker build . -t $(IMAGE_REF)
.PHONY: image

run-image: image
	docker run -it -p 3000:3000 $(IMAGE_REF)
.PHONY: run-image