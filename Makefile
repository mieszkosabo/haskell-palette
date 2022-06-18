all: run

SHELL :=/bin/bash -euEo pipefail -O inherit_errexit

IMAGE_TAG ?= latest
IMAGE_REF ?= docker.io/rzetelskik/haskell-palette:$(IMAGE_TAG)

image:
	docker build . -t $(IMAGE_REF)

run: image
	docker run -it -p 3000:3000 $(IMAGE_REF)

.PHONY: image