-include $(dir $(lastword $(MAKEFILE_LIST)))/local-config.mk

LIBALLOCS ?= $(realpath $(SRCROOT)/../liballocs)
LIBCXXGEN ?= $(LIBALLOCS)/contrib/libcxxgen
LIBDWARFPP ?= $(LIBALLOCS)/contrib/libdwarfpp
LIBSRK31CXX ?= $(LIBALLOCS)/contrib/libsrk31cxx
LIBCXXFILENO ?= $(LIBALLOCS)/contrib/libcxxfileno
LIBDLBIND ?= $(LIBALLOCS)/contrib/libdlbind
TRAP_SYSCALLS ?= $(LIBALLOCS)/contrib/trap-syscalls
LIBSYSTRAP ?= $(TRAP_SYSCALLS)/libsystrap
LIBRUNT ?= $(LIBALLOCS)/contrib/librunt
