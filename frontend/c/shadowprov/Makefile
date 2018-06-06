# This will build any one-file C program as a test

-include $(dir $(lastword $(MAKEFILE_LIST)))/../../../config.mk
LIBCRUNCH_BASE ?= ../../..

default: runtime.so stubs.o gold-plugin.so wrap.ldopts

CPPFLAGS += -I$(LIBRUNT)/include

CXXFLAGS += -g -fPIC -std=c++14

gold-plugin.so: LDFLAGS += -Wl,--export-dynamic
gold-plugin.so: gold-plugin.cpp
	$(CXX) -shared -o "$@" $(CPPFLAGS) $(CXXFLAGS) -o "$@" $+ $(LDFLAGS) $(LDLIBS)

stubs.o: shadowprov_libc_wrappers.o
	ln -sf "$<" "$@"
shadowprov_libc_wrappers.o: shadowprov_libc_wrappers.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c -o "$@" "$<"
shadowprov_libc_wrappers.o: CFLAGS += -g -fPIC -std=c11
shadowprov_libc_wrappers.o: CFLAGS += -I$(LIBCRUNCH_BASE)/include \
  -I$(LIBCRUNCH_BASE)/src \
  -I$(LIBALLOCS)/include \
  -I$(LIBALLOCS)/src \
  -I$(LIBSYSTRAP)

runtime.so: $(LIBCRUNCH_BASE)/lib/libcrunch_stubs.so
	ln -sf "$<" "$@"

wrap.ldopts: stubs.o
	nm --format=posix "$<" | grep __wrap_ | \
            sed -r 's/__wrap_([^[:blank:]]*).*/--wrap \1/' > "$@" \
            || (rm -f "$@"; false)

CFLAGS += -g

export CIL_PLUGINS := $(LIBALLOCS)/tools/lang/c/cilallocs/cilallocs.cmxs \
                $(LIBCRUNCH_BASE)/frontend/c/shadow/shadow.cmxs \
                $(LIBCRUNCH_BASE)/frontend/c/shadowprov/shadowprov.cmxs"
export CIL_PASSES := shadowprov

tests := $(wildcard test-*.c)
$(patsubst %.c,%.o,$(tests)): CFLAGS += -wrapper $(LIBCRUNCH_BASE)/frontend/cilpp/bin/wrapper \
   -include $(LIBCRUNCH_BASE)/frontend/c/shadowprov/shadowprov_helpers.h \
   -save-temps
$(patsubst %.c,%,$(tests)): LDFLAGS += -Wl,-plugin,$(LIBCRUNCH_BASE)/frontend/c/shadowprov/gold-plugin.so \
   -Wl,-rpath,$(LIBCRUNCH_BASE)/lib \
   -Wl,@$(LIBCRUNCH_BASE)/frontend/c/shadowprov/wrap.ldopts

# never build straight from .c
%: %.c

#%.o: %.c
#	OCAMLRUNPARAM=b \
#~/work/devel/liballocs.hg/tools/lang/c/cil/bin/cilly \
#--bytecode \
#--load=${HOME}/work/devel/liballocs.hg/tools/lang/c/cilallocs/cilallocs.cma \
#--load=${HOME}/work/devel/libcrunch.hg/frontend/c/shadow/shadow.cma \
#--load=${HOME}/work/devel/libcrunch.hg/frontend/c/shadowprov/shadowprov.cma \
#--doshadowtest \
#$(CFLAGS) $(CPPFLAGS) \
#-c -o "$@" \
#--keepunused \
#-save-temps \
#--live_debug \
#-include shadowtest_helpers.h \
#"$<" \

#%: %.o
#	$(CC) -o "$@" "$<" \
#shadowtest_weak.so \
#-Wl,-rpath,`pwd` \
#-Wl,-Map=$*.map \
#$(LDFLAGS) $(LDLIBS)

.PHONY: gdbrun-%
gdbrun-%: %
	LIBCRUNCH_DELAY_STARTUP=1 LD_PRELOAD=$(realpath ../../../src/libcrunch_preload.so) ./$* & gdb -p $$!
