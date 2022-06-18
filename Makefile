all: image

SHELL :=/bin/bash -euEo pipefail -O inherit_errexit

IMAGE_TAG ?= latest
IMAGE_REF ?= docker.io/rzetelskik/haskell-palette:$(IMAGE_TAG)

image:
	docker build . -t $(IMAGE_REF)
.PHONY: image