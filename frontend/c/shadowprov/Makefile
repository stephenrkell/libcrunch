# This will build any one-file C program as a test

-include $(dir $(lastword $(MAKEFILE_LIST)))/../../../config.mk
LIBCRUNCH_BASE ?= ../../..

export USE_LD=ld.gold

default: runtime.so stubs.o gold-plugin.so wrap.ldopts

C_SRC := shadowprov_libc_wrappers.c
C_DEPS := $(patsubst %.c,.%.d,$(C_SRC))
$(C_DEPS): .%.d : %.c
	$(CC) -MM $(CFLAGS) $(CPPFLAGS) "$<"  > "$@" || (rm -f "$@"; false)
-include $(C_DEPS)

CPPFLAGS += -I$(LIBRUNT)/include

CXXFLAGS += -g -fPIC -std=c++14

gold-plugin.so: LDFLAGS += -Wl,--export-dynamic
gold-plugin.so: gold-plugin.cpp
	$(CXX) -shared -o "$@" $(CPPFLAGS) $(CXXFLAGS) -o "$@" $+ $(LDFLAGS) $(LDLIBS)

stubs.o: shadowprov_libc_wrappers.o
	ln -sf "$<" "$@"
shadowprov_libc_wrappers.o: shadowprov_libc_wrappers.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c -o "$@" "$<"
shadowprov_libc_wrappers.o .shadowprov_libc_wrappers.d: CFLAGS += -g -fPIC -std=c11
shadowprov_libc_wrappers.o .shadowprov_libc_wrappers.d: CFLAGS += -I$(LIBCRUNCH_BASE)/include \
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
vpath %.c defacto_tarball/tests/de_facto_memory_model
extra_tests := $(patsubst defacto_tarball/tests/de_facto_memory_model/%.c,%.c,$(wildcard defacto_tarball/tests/de_facto_memory_model/*.c))
$(info found extra tests $(extra_tests))
# sort to remove duplicates, for copied .c files that show up twice
tests := $(sort $(tests) $(patsubst %.c,test-%.c,$(extra_tests)))
$(patsubst %.c,test-%.c,$(extra_tests)): test-%.c: %.c
	cp "$<" "$@"
# never build straight from .c
%: %.c

$(patsubst %.c,%.o,$(tests)): CFLAGS += -wrapper $(LIBCRUNCH_BASE)/frontend/cilpp/bin/wrapper \
   -include $(LIBCRUNCH_BASE)/frontend/c/shadowprov/shadowprov_helpers.h \
   -save-temps
$(patsubst %.c,%,$(tests)): LDFLAGS += -Wl,-plugin,$(LIBCRUNCH_BASE)/frontend/c/shadowprov/gold-plugin.so \
   -Wl,-rpath,$(LIBCRUNCH_BASE)/lib \
   -Wl,@$(LIBCRUNCH_BASE)/frontend/c/shadowprov/wrap.ldopts

test-fail-materialise: LDFLAGS += -Wl,-Ttext,0x410000

#./charon-run/charon_run --compile_only --tools tools.gcc8 --tests tests.charon \
#                        --test_src_dir tests/de_facto_memory_model/ --test_bin_dir tests.bin/ --test_int_dir tests.int \
#                        --int tests.int/int_gcc8.log

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
