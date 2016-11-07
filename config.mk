THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))
SRCROOT := $(dir $(THIS_MAKEFILE))

-include local-config.mk

LIBALLOCS ?= $(realpath $(SRCROOT)/../liballocs)
LIBCXXGEN ?= $(LIBALLOCS)/contrib/libcxxgen
LIBDWARFPP ?= $(LIBALLOCS)/contrib/libdwarfpp
LIBSRK31CXX ?= $(LIBALLOCS)/contrib/libsrk31cxx
LIBCXXFILENO ?= $(LIBALLOCS)/contrib/libcxxfileno
LIBDLBIND ?= $(LIBALLOCS)/contrib/libdlbind

export CXXFLAGS += \
-I$(LIBCXXGEN)/include \
-I$(LIBDWARFPP)/include \
-I$(LIBSRK31CXX)/include \
-I$(LIBCXXFILENO)/include \
-I$(LIBDLBIND)/include \

export LDFLAGS += \
-L$(LIBCXXGEN)/lib -Wl,-rpath,$(LIBCXXGEN)/lib \
-L$(LIBDWARFPP)/lib -Wl,-rpath,$(LIBDWARFPP)/lib \
-L$(LIBSRK31CXX)/lib -Wl,-rpath,$(LIBSRK31CXX)/lib \
-L$(LIBCXXFILENO)/lib -Wl,-rpath,$(LIBCXXFILENO)/lib \
-L$(LIBDLBIND)/lib -Wl,-rpath,$(LIBDLBIND)/lib
