THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))
SRCROOT := $(realpath $(dir $(THIS_MAKEFILE))/../..)
include $(SRCROOT)/config.mk

LIBALLOCS ?= $(SRCROOT)/../liballocs
LIBALLOCS_BASE ?= $(realpath $(LIBALLOCS))

OCAMLFIND ?= ocamlfind
CIL_INSTALL ?= $(LIBALLOCS_BASE)/tools/lang/c/cil
CILLY ?= $(CIL_INSTALL)/bin/cilly

OCAMLFLAGS +=  -package findlib -I $(CIL_INSTALL)/lib/cil
OCAMLFLAGS += -I $(dir $(THIS_MAKEFILE))/lib -I $(LIBALLOCS_BASE)/tools/lang/c/cilallocs #-I $(dir $(wildcard $(shell which $(CILLY))/../lib/ocaml/*/cil))

export LIBALLOCS_BASE
export LIBALLOCS
export OCAMLFLAGS
export OCAMLFIND
export CIL_INSTALL

.PHONY: default
default: src

.PHONY: src
src:
	$(MAKE) -C src

.PHONY: clean
clean:
	$(MAKE) -C src clean
