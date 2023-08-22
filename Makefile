# --------------------------------------------------
# standard Makefile preamble
# see https://tech.davis-hansson.com/p/make/
SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules
ifeq ($(origin .RECIPEPREFIX), undefined)
  $(error Your Make does not support .RECIPEPREFIX. Use GNU Make 4.0 or later)
endif
.RECIPEPREFIX = >
# --------------------------------------------------

cur_dir = $(shell pwd)

SRC = src
TEX = Paper

docs: .sentinel-make-docs
.PHONY: docs

.sentinel-make-docs: $(shell find R -type f)
> R -e 'devtools::document()'
> R -e 'pkgdown::build_site()'
> touch $@

clean:
> rm -rf docs
> rm -rf man
> rm .sentinel*
.PHONY: clean
