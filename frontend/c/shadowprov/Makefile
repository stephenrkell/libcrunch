# This will build any one-file C program as a test

-include $(dir $(lastword $(MAKEFILE_LIST)))/../../../config.mk
LIBCRUNCH_BASE ?= ../../..

export USE_LD=ld.gold

default: runtime.so stubs.o gold-plugin.so wrap.ldopts shadowprov_weak.so

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
  -I$(LIBSYSTRAP) \
  -DSHADOW_STACK_TRACE_LEVEL=0

runtime.so: $(LIBCRUNCH_BASE)/lib/libcrunch_stubs.so
	ln -sf "$<" "$@"

wrap.ldopts: stubs.o
	nm --format=posix "$<" | grep __wrap_ | \
            sed -r 's/__wrap_([^[:blank:]]*).*/--wrap \1/' > "$@" \
            || (rm -f "$@"; false)

CFLAGS += -g

tests := $(wildcard test-*.c)
vpath %.c defacto_tarball/tests/de_facto_memory_model
extra_tests := $(patsubst defacto_tarball/tests/de_facto_memory_model/%.c,%.c,$(wildcard defacto_tarball/tests/de_facto_memory_model/*.c))
$(info found extra tests $(extra_tests))
# sort to remove duplicates, for copied .c files that show up twice
tests := $(sort $(tests) $(patsubst %.c,test-%.c,$(extra_tests)))
$(patsubst %.c,test-%.c,$(extra_tests)): test-%.c: %.c
	cp "$<" "$@"
# never build straight from .c -- go via .o
%: %.c

$(patsubst %.c,%.o,$(tests)): CFLAGS += $(shell ./shadowprov-cflags) -save-temps
$(patsubst %.c,%,$(tests)): LDFLAGS += $(shell ./shadowprov-ldflags)

test-fail-materialise: LDFLAGS += -Wl,-Ttext,0x410000

.PHONY: gdbrun-%
gdbrun-%: %
	LIBCRUNCH_DELAY_STARTUP=1 LD_PRELOAD=$(realpath ../../../src/libcrunch_preload.so) ./$* & gdb -p $$!
run-%: %
	LD_PRELOAD=$(realpath ../../../src/libcrunch_preload.so) ./$*
