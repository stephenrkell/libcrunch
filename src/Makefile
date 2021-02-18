THIS_MAKEFILE := $(realpath $(lastword $(MAKEFILE_LIST)))
SRCROOT := $(dir $(THIS_MAKEFILE))../
srcdir := $(dir $(THIS_MAKEFILE))

include $(SRCROOT)/config.mk
TRAP_SYSCALLS ?= $(realpath $(SRCROOT)/../trap-syscalls)
LIBSYSTRAP ?= $(TRAP_SYSCALLS)/libsystrap
LIBRUNT ?= $(realpath $(SRCROOT)/../librunt)
LIBALLOCS ?= $(realpath $(SRCROOT)/../liballocs)
# HACK around lazy expansion: something unsets LIBALLOCS later? FIXME
LIBALLOCS := $(LIBALLOCS)
LIBDLBIND ?= $(realpath $(SRCROOT)/../libdlbind)

default: libcrunch_preload.so libcrunch_preload.a libcrunch_stubs.so libcrunch_stubs.o \
    libcrunch_wrappers.o

CFLAGS += $(shell cat $(SRCROOT)/frontend/c/lib/bounds-abi | \
  tr -s '[:space:]' ' ')
libcrunch_preload.so: LDFLAGS += -Wl,--defsym,__libcrunch_bounds_abi_md5sum=0x$(shell cat $(SRCROOT)/frontend/c/lib/bounds-abi | md5sum | head -c16 )

CFLAGS += -I$(LIBALLOCS)/include -I$(LIBALLOCS)/src -I$(LIBSYSTRAP)/include -I$(LIBRUNT)/include -I$(LIBDLBIND)/include -I$(LIBALLOCS)/contrib/liballocstool/include
LDFLAGS += -L$(LIBALLOCS)/lib -L$(LIBALLOCS)/libsystrap -fno-lto

#CFLAGS += -fkeep-inline-functions
CFLAGS += -std=gnu99 -g -fPIC -I../include
#CFLAGS += -Wall -Wextra -Wno-unused-parameter
# debugging version
ifneq ($(DEBUG),)
CFLAGS += -O0 -fno-lto
else
# experiment to see what's faster -- plain old -O4 -flto seems marginally faster than with -Os
#CFLAGS += -O4 -flto -DNDEBUG
#CFLAGS += -Os -flto -DNDEBUG
CFLAGS += -O3 -fno-lto -ffat-lto-objects -DNDEBUG
FINAL_LDFLAGS += -O4 -fno-lto -Bsymbolic -Wl,-O2
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

ifeq ($(USE_FAKE_LIBUNWIND),)
CFLAGS += -DUSE_REAL_LIBUNWIND
LIBUNWIND_LDLIBS := -lunwind -lunwind-$(shell uname -m)
else
CFLAGS += -fno-omit-frame-pointer
LIBUNWIND_LDLIBS :=
endif

# until binutils bug 13600 is fixed, gold is the linker we need
LDFLAGS += -Wl,-fuse-ld=gold

C_DEPS := $(patsubst %.c,.%.d,$(C_SRC))
CC_DEPS := $(patsubst %.cc,.%.d,$(CC_SRC))

DEPS := $(C_DEPS) $(CC_DEPS)

ifneq ($(realpath .),$(realpath $(srcdir)))
$(warning Build directory is $(realpath .))
VPATH := $(srcdir):$(VPATH)
endif

$(C_DEPS): .%.d : %.c
	$(CC) -MM $(CFLAGS) "$<"  > "$@" || (rm -f "$@"; false)
$(CC_DEPS): .%.d : %.cc
	$(CXX) -MM $(CXXFLAGS) "$<"  > "$@" || (rm -f "$@"; false)

-include $(DEPS)

# We want to make sure that the libcrunch--liballocs interface is 
# linked in the fastest possible way.
libcrunch_preload.so libcrunch_nomemtable_preload.so libcrunch.so: LDFLAGS += $(FINAL_LDFLAGS)
libcrunch_preload.so libcrunch_nomemtable_preload.so: LDFLAGS += -Wl,--wrap,malloc_usable_size -Wl,--wrap,__notify_copy
libcrunch_preload.so libcrunch_nomemtable_preload.so libcrunch.so: LDLIBS += -L$(LIBSYSTRAP)/lib -lsystrap -lz -Wl,--no-as-needed -ldl

vpath dlmalloc.c $(LIBSYSTRAP)/contrib/
# We put dlmalloc in a library so that --exclude-libs=dlmalloc.a will hide its symbols
# in our output .so, so that they don't override anything in the rest of the program.
dlmalloc.a: dlmalloc.o
	$(AR) r "$@" $^
dlmalloc.o: CFLAGS += -DHAVE_MORECORE=0 -DUSE_DL_PREFIX -fPIC
libcrunch_preload.so: dlmalloc.a
libcrunch_preload.so: LDFLAGS += -Wl,--wrap,dlmalloc -Wl,--wrap,dlcalloc \
-Wl,--wrap,dlfree -Wl,--wrap,dlrealloc -Wl,--wrap,dlmemalign -Wl,--wrap,dlposix_memalign \
-Wl,--whole-archive dlmalloc.a  -Wl,--no-whole-archive \
 -Wl,--defsym,__private_malloc=__wrap_dlmalloc \
 -Wl,--defsym,__private_calloc=__wrap_dlcalloc \
 -Wl,--defsym,__private_free=__wrap_dlfree \
 -Wl,--defsym,__private_realloc=__wrap_dlrealloc \
 -Wl,--defsym,__private_memalign=__wrap_dlmemalign \
 -Wl,--defsym,__private_posix_memalign=__wrap_dlposix_memalign \
 -Wl,--exclude-libs=dlmalloc.a \
 -Bsymbolic # important for _etext in preload hooks
# -Wl,--wrap,__liballocs_nudge_mmap \

# FIXME: all this guff is because librunt is logically incomplete.
# Can we package it into some kind of librunt.lds linker script
# that pulls in the library but also sets up all this other stuff?
libcrunch_stubs.so: LDFLAGS += -Wl,--whole-archive dlmalloc.a  -Wl,--no-whole-archive \
 -Wl,--defsym,__private_malloc=dlmalloc \
 -Wl,--defsym,__private_calloc=dlcalloc \
 -Wl,--defsym,__private_free=dlfree \
 -Wl,--defsym,__private_realloc=dlrealloc \
 -Wl,--defsym,__private_memalign=dlmemalign \
 -Wl,--defsym,__private_posix_memalign=dlposix_memalign \
 -Wl,--exclude-libs=dlmalloc.a \
 -Bsymbolic # important for _etext in preload hooks
# + see the giant __private_strdup hack in liballocs/src/dummyweaks.c

## Direct copy paste s/allocs/crunch/g from liballocs/src/Makefile
RUNT_LDFLAGS := -Wl,--wrap,__runt_files_notify_load \
-Wl,--wrap,__runt_files_notify_unload \
-Wl,--wrap,__runt_files_metadata_by_addr \
-Wl,--wrap,__runt_segments_notify_define_segment \
-Wl,--wrap,__runt_segments_notify_destroy_segment \
-Wl,--wrap,__runt_sections_notify_define_section # there is no 'destroy section'
# if we don't do --whole-archive, then the wrapping of __runt_* doesn't work...
# ... though with a behaviour that makes no sense: librunt still calls itself rather
# than its wrapper __static_file_allocator notify_load.
libcrunch_preload.so: LDFLAGS += $(RUNT_LDFLAGS) -Wl,-O0
libcrunch_preload.so: LDLIBS +=  -Wl,--whole-archive $(LIBRUNT)/lib/librunt_preload.a -Wl,--no-whole-archive
# dependency for make
libcrunch_preload.so: $(LIBRUNT)/lib/librunt_preload.a
# NOTE that libcrunch_stubs.so is a little different: it doesn't need to wrap anything
# in librunt (because it doesn't contain a full liballocs), so just link it as a normal archive.
# BUT we need to sew up the wrappable seam in librunt.
libcrunch_stubs.so: $(LIBRUNT)/lib/librunt.a
libcrunch_stubs.so: LDLIBS += $(LIBRUNT)/lib/librunt.a
libcrunch_stubs.so: LDFLAGS += -Wl,--defsym,__wrap___runt_files_notify_load=__runt_files_notify_load \
-Wl,--defsym,__wrap___runt_files_notify_unload=__runt_files_notify_unload \
-Wl,--defsym,__wrap___runt_files_metadata_by_addr=__runt_files_metadata_by_addr \
-Wl,--defsym,__wrap___runt_segments_notify_define_segment=__runt_segments_notify_define_segment\
-Wl,--defsym,__wrap___runt_segments_notify_destroy_segment=__runt_segments_notify_destroy_segment \
-Wl,--defsym,__wrap___runt_sections_notify_define_section=__runt_sections_notify_define_section # there is no 'destroy section'

# (This matters because we don't want multiple copies of the liballocs/librunt preload
# functions in the process! It breaks RTLD_NEXT, which we use to locate the 'real'
# dl_iterate_phdr and friends... we end up finding our own stub version.)

# FIXME: in preload case, can be clever with stubs? 
# HMM. We still have to --wrap the allocation funcs, but we could put the wrappers in the preload lib...
# BUT since they're generated per-build from the LIBCRUNCH_ALLOC_FNS content, this doesn't work --
# stick with stubs!

libcrunch_preload.so libcrunch_stubs.so: LDFLAGS += -L$(LIBDLBIND)/lib
libcrunch_preload.so: LDFLAGS += $(LIBALLOCS)/lib/liballocs_preload.a
libcrunch_preload.so libcrunch_stubs.so: LDLIBS += $(LIBUNWIND_LDLIBS) -ldlbind -ldl
libcrunch_preload.so: libcrunch.o $(LIBALLOCS)/lib/liballocs_preload.a $(LIBDLBIND)/lib/libdlbind.a $(LIBALLOCS)/lib/liballocs_systrap.o $(LIBSYSTRAP)/lib/libsystrap.a
	$(CC) $(CFLAGS) -shared -o "$@" $(filter-out dlmalloc.a,$(filter-out %librunt_preload.a,$(filter-out %libdlbind.a,$(filter-out %allocs_preload.a,$+)))) \
$(LDFLAGS) \
-Wl,--no-whole-archive -Wl,-Bdynamic \
$(LDLIBS)
#-Wl,--wrap,__liballocs_nudge_mmap \

libcrunch_preload.a: libcrunch.o $(LIBALLOCS)/lib/liballocs_preload.a
	$(AR) r "$@" $+

libdumpsmaps.so: libdumpsmaps.o
	$(CC) $(CFLAGS) -shared -o "$@" $(filter-out %hooks.a,$^) $(LDFLAGS) 

libcrunch_nomemtable_preload.so: libcrunch_nomemtable.o dummy_thread_locals.o
	$(CC) $(CFLAGS) -shared -o "$@" $(filter-out %hooks.a,$^) $(LDFLAGS) -ldl $(LIBUNWIND_LDLIBS)

# this version lacks preload stuff
libcrunch.so: libcrunch.o $(LIBALLOCS)/lib/liballocs.a
	$(CC) $(CFLAGS) -shared -o "$@" $^ $(LDFLAGS) -ldl $(LIBUNWIND_LDLIBS) -Wl,-Bdynamic

stubs.o: CFLAGS += -fPIC 

libcrunch_nomemtable.o: libcrunch.c
	$(CC) $(CFLAGS) -DNO_MEMTABLE -c -o "$@" $+

libcrunch_stubs.so: stubs.o shadow.o $(LIBALLOCS)/lib/liballocs_dummyweaks.o $(LIBALLOCS)/lib/liballocs_systrap.o $(LIBSYSTRAP)/lib/libsystrap.a
	$(CC) $(CFLAGS) -Wl,-Map,$@.map -shared -o "$@" $(filter-out %librunt_preload.a,$+) $(LDFLAGS) $(LDLIBS) -lz -Wl,--wrap,__liballocs_nudge_mmap -ldlbind

libcrunch_stubs.o: stubs.o shadow.o $(LIBALLOCS)/lib/liballocs_dummyweaks.o $(LIBALLOCS)/lib/liballocs_systrap.o $(LIBSYSTRAP)/lib/libsystrap.a
	$(LD) -r -o "$@" $+ --wrap __liballocs_nudge_mmap

libcrunch_wrappers.o: softbound-libc-wrappers.c
	$(CC)  -o "$@" $(CFLAGS) -c "$<"

# have to preload the hooks yourself if you want them to hook!

clean: 
	rm -f *.o *.so .*.d
