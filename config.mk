-include $(dir $(lastword $(MAKEFILE_LIST)))/local-config.mk

SRCROOT ?= $(dir $(lastword $(MAKEFILE_LIST)))
LIBALLOCS ?= $(realpath $(SRCROOT)/../liballocs)
$(info LIBALLOCS is $(LIBALLOCS))
LIBALLOCSTOOL ?= $(LIBALLOCS)/contrib/liballocstool
LIBCXXGEN ?= $(LIBALLOCS)/contrib/dwarfidl/contrib/libcxxgen
LIBDWARFPP ?= $(LIBALLOCS)/contrib/dwarfidl/contrib/libdwarfpp
LIBSRK31CXX ?= $(LIBALLOCS)/contrib/dwarfidl/contrib/libdwarfpp/contrib/libsrk31c++
LIBCXXFILENO ?= $(LIBALLOCS)/contrib/dwarfidl/contrib/libdwarfpp/contrib/libc++fileno
LIBDLBIND ?= $(LIBALLOCS)/contrib/libdlbind
LIBSYSTRAP ?= $(LIBALLOCS)/contrib/libsystrap
LIBRUNT ?= $(LIBALLOCS)/contrib/libsystrap/contrib/librunt
