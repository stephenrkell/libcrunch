-include $(dir $(lastword $(MAKEFILE_LIST)))/local-config.mk

LIBALLOCS ?= $(realpath $(SRCROOT)/../liballocs)
LIBCXXGEN ?= $(LIBALLOCS)/contrib/dwarfidl/contrib/libcxxgen
LIBDWARFPP ?= $(LIBALLOCS)/contrib/dwarfidl/contrib/libdwarfpp
LIBSRK31CXX ?= $(LIBALLOCS)/contrib/dwarfidl/contrib/libdwarfpp/contrib/libsrk31c++
LIBCXXFILENO ?= $(LIBALLOCS)/contrib/dwarfidl/contrib/libdwarfpp/contrib/libc++fileno
LIBDLBIND ?= $(LIBALLOCS)/contrib/libdlbind
LIBSYSTRAP ?= $(LIBALLOCS)/contrib/libsystrap
LIBRUNT ?= $(LIBALLOCS)/contrib/libsystrap/contrib/librunt
