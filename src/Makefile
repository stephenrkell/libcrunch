CFLAGS += -std=gnu99 -g -fPIC -I../include
# debugging version
ifneq ($(DEBUG),)
CFLAGS += -O0 -flto
else
# experiment to see what's faster -- plain old -O4 -flto seems marginally faster than with -Os
#CFLAGS += -O4 -flto -DNDEBUG
#CFLAGS += -Os -flto -DNDEBUG
CFLAGS += -O3 -flto -DNDEBUG
FINAL_LDFLAGS += -O4 -flto -Bsymbolic -Wl,-O2
endif
#CFLAGS += -O0 

CXXFLAGS := $(filter-out -std=%,$(CFLAGS)) -std=c++11

C_SRC := $(wildcard *.c)
CC_SRC := $(wildcard *.cc)

# Don't use crunchcc to compile libcrunch!
# if CC is crunchcc, unset it
ifeq ($(notdir $(CC)),crunchcc)
export CC := cc
endif

default: libcrunch_preload.so libcrunch_preload.a libcrunch_noop.so libcrunch_noop.o

ifeq ($(USE_REAL_LIBUNWIND),)
CFLAGS += -fno-omit-frame-pointer
LIBUNWIND_LDLIBS :=
else
LIBUNWIND_LDLIBS := -lunwind -lunwind-`uname -m` 
CFLAGS += -DUSE_REAL_LIBUNWIND
endif

C_DEPS := $(patsubst %.c,.%.d,$(C_SRC))
CC_DEPS := $(patsubst %.cc,.%.d,$(CC_SRC))

DEPS := $(C_DEPS) $(CC_DEPS)

$(C_DEPS): .%.d : %.c
	$(CC) -MM $(CFLAGS) "$<"  > "$@"
$(CC_DEPS): .%.d : %.cc
	$(CXX) -MM $(CXXFLAGS) "$<"  > "$@"

-include $(DEPS)

# We want to make sure that the libcrunch--liballocs interface is 
# linked in the fastest possible way.
libcrunch_preload.so libcrunch_nomemtable_preload.so libcrunch.so: LDFLAGS += $(FINAL_LDFLAGS)
libcrunch_preload.so libcrunch_nomemtable_preload.so: LDFLAGS += -Wl,--wrap,malloc_usable_size

uniqtypes.o: uniqtypes.c
	$(CC) -o "$@" $(filter-out -flto,$(CFLAGS)) -c "$<" && \
	$(LIBALLOCS)/tools/lang/c/bin/link-used-types "$@" || (rm -f "$@"; false)

# FIXME: in preload case, can be clever with stubs? 
# HMM. We still have to --wrap the allocation funcs, but we could put the wrappers in the preload lib...
# BUT since they're generated per-build from the LIBCRUNCH_ALLOC_FNS content, this doesn't work --
# stick with stubs!
libcrunch_preload.so: uniqtypes.o libcrunch.o $(LIBALLOCS)/lib/liballocs_preload.a
	$(CC) $(CFLAGS) -shared -o "$@" $(filter-out %allocs_preload.a,$+) \
$(LDFLAGS) -ldl $(LIBUNWIND_LDLIBS) \
-Wl,--whole-archive -Wl,-Bstatic -lallocs_preload \
-Wl,--no-whole-archive -Wl,-Bdynamic 

libcrunch_preload.a: libcrunch.o $(LIBALLOCS)/lib/liballocs_preload.a
	$(AR) r "$@" $+

libdumpsmaps.so: libdumpsmaps.o
	$(CC) $(CFLAGS) -shared -o "$@" $(filter-out %hooks.a,$^) $(LDFLAGS) 

libcrunch_nomemtable_preload.so: libcrunch_nomemtable.o dummy_thread_locals.o
	$(CC) $(CFLAGS) -shared -o "$@" $(filter-out %hooks.a,$^) $(LDFLAGS) -ldl $(LIBUNWIND_LDLIBS)

# this version lacks preload stuff
libcrunch.so: libcrunch.o $(LIBALLOCS)/lib/liballocs.a
	$(CC) $(CFLAGS) -shared -o "$@" $^ $(LDFLAGS) -ldl $(LIBUNWIND_LDLIBS) -Wl,--whole-archive -Wl,-Bstatic -lallocs -Wl,--no-whole-archive -Wl,-Bdynamic #-lheap_index_hooks

noop.o: CFLAGS += -fPIC 

libcrunch_nomemtable.o: libcrunch.c
	$(CC) $(CFLAGS) -DNO_MEMTABLE -c -o "$@" "$<" 

libcrunch_noop.so: noop.o $(LIBALLOCS)/lib/liballocs_noop.o
	$(CC) $(CFLAGS) -shared -o "$@" $+ $(LDFLAGS)

libcrunch_noop.o: noop.o $(LIBALLOCS)/lib/liballocs_noop.o
	$(LD) -r -o "$@" $+

# have to preload the hooks yourself if you want them to hook!

clean: 
	rm -f *.o *.so .*.d
