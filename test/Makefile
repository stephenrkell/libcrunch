CFLAGS += -D_GNU_SOURCE # HACK
CFLAGS += -g3 -gstrict-dwarf -std=c99 -fno-eliminate-unused-debug-types -O2
CFLAGS += -DUSE_STARTUP_BRK

THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))

export CFLAGS

UNIQTYPES_BASE ?= /usr/lib/allocsites
ALLOCSITES_BASE ?= /usr/lib/allocsites
LIBALLOCS ?= $(realpath $(dir $(THIS_MAKEFILE))/../../liballocs)

export UNIQTYPES_BASE
export ALLOCSITES_BASE

export LIBCRUNCH_BASE := $(realpath $(dir $(THIS_MAKEFILE))/..)
export LIBCRUNCH_ALWAYS_PRINT_EXIT_SUMMARY := 1

# To support differential testing against implementations, each test
# is allowed to provide a list of the frontends it wants to test.
# It should do this in mk.inc.
# What does run-% or checkrun-% do in these circumstances?
# What do we do with the %/check-stamp files?
# Where do the builds live?
# First step: each test is built and run in a FRONTEND dir.
FRONTEND := crunchcc

CRUNCHCC ?= $(LIBCRUNCH_BASE)/frontend/c/bin/crunchcc
#$(warning in Makefile, CRUNCHCC is $(CRUNCHCC))
CRUNCHCXX ?= $(LIBCRUNCH_BASE)/frontend/c++/bin/crunchc++
# use lazy expansion for these, so that changes to crunchcc get reflected
CC = $(CRUNCHCC)
#$(warning in Makefile, CC is $(CC))
CXX = $(CRUNCHCXX)
export CRUNCHCC
export CRUNCHCXX
export CC
export CXX

export CFLAGS += -I$(realpath $(dir $(THIS_MAKEFILE)))/../include -I$(LIBALLOCS)/include
export LDFLAGS += -L$(LIBALLOCS)/lib

ifneq ($(USE_FAKE_LIBUNWIND),)
CFLAGS += -fno-omit-frame-pointer
CXXFLAGS += -fno-omit-frame-pointer
LIBUNWIND_LDLIBS :=
else
LIBUNWIND_LDLIBS := -lunwind -lunwind-`uname -m` 
endif

ifeq ($(CC),)
$(error Could not find crunchcc)
endif
ifeq ($(CXX),)
$(error Could not find crunchc++)
endif

cases := $(filter-out checks.sh,$(filter-out rename-case.sh,$(wildcard [-a-z]*)))

LIBCRUNCH := $(realpath $(dir $(THIS_MAKEFILE))/../lib/libcrunch_preload.so)

ifneq ($(MAKECMDGOALS),clean)
ifeq ($(wildcard $(LIBCRUNCH)),)
        $(error You must first build libcrunch{.so,_preload.so} and link them from ../lib)
endif
endif

export PRELOAD := "$(LIBCRUNCH)"

INCLUDE_MK_INC = `if test -e $(dir $(realpath $(THIS_MAKEFILE)))/$*/mk.inc; then /bin/echo -f ../mk.inc; else true; fi`

default: checkalltests

checkalltests: $(patsubst %,checkrun-%,$(cases))
.PRECIOUS: $(patsubst %,%/$(FRONTEND)/check-stamp,$(cases))
ifeq ($(MAKECMDGOAL),)
$(warning cases are $(cases))
endif

RERUN_TESTS_ON_CHANGE := $(LIBCRUNCH) $(shell find $(dir $(THIS_MAKEFILE))/../frontend | grep '\.cmxs$$' )

# Some cases are expected to fail, i.e. generate libcrunch "check failed" stats.
# Note that bounds errors never do this. HMM. Soon, type errors might not either.
checkrun-fail-bounds-type-%: fail-bounds-type-%/$(FRONTEND)/check-stamp
	true # remembered pass of fail-bounds-type-$*
fail-bounds-type-%/$(FRONTEND)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	@echo "expected-fail case is fail-bounds-type-$*" && \
	out=$$( echo -n "out is " 1>&2; mktemp | tee /dev/stderr ) && \
	err=$$( echo -n "err is " 1>&2; mktemp | tee /dev/stderr ) && \
	$(MAKE) cleanrun-fail-bounds-type-$* 1>$$out 2>$$err && \
		. ./checks.sh && tail -n26 "$$err" | \
		    match "       failed otherwise" '[1-9][0-9]*' >/dev/null && \
		    touch "$@" || \
		(tail -n26 $$err; false)
checkrun-fail-bounds-%: fail-bounds-%/$(FRONTEND)/check-stamp
	true # remembered pass of fail-bounds-$*
fail-bounds-%/$(FRONTEND)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	@echo "expected-fail case is fail-bounds-$*" && \
	out=$$( echo -n "out is " 1>&2; mktemp | tee /dev/stderr ) && \
	err=$$( echo -n "err is " 1>&2; mktemp | tee /dev/stderr ) && \
	$(MAKE) cleanrun-fail-bounds-$* 1>$$out 2>$$err && \
		. ./checks.sh && tail -n26 "$$err" | \
		    match "out-of-bounds pointers created" '[1-9][0-9]*' >/dev/null && \
		    touch "$@" || \
		(tail -n26 $$err; false)

# Some cases are expected to fail, i.e. generate libcrunch "check failed" stats.
checkrun-fail-%: fail-%/$(FRONTEND)/check-stamp
	true # remembered pass of fail-$*
fail-%/$(FRONTEND)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	@echo "expected-fail case is fail-$*" && \
	out=$$( echo -n "out is " 1>&2; mktemp | tee /dev/stderr ) && \
	err=$$( echo -n "err is " 1>&2; mktemp | tee /dev/stderr ) && \
	$(MAKE) cleanrun-fail-$* 1>$$out 2>$$err && \
		. ./checks.sh && tail -n26 "$$err" | \
		    match "       failed otherwise" '[1-9][0-9]*' >/dev/null && \
		    touch "$@" || \
		(tail -n26 $$err; false)

# some cases are ignored
checkrun-ignore-%:
	true # remembered pass of ignore-$*
# Some cases are expected to abort the program (assertion failed, segfault, etc.)
checkrun-abort-%: abort-%/$(FRONTEND)/check-stamp
	true # remembered pass of abort-$*
abort-%/$(FRONTEND)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	@echo "expected-fail case is abort-$*" && \
	out=$$( echo -n "fout is " 1>&2; mktemp | tee /dev/stderr ) && \
	err=$$( echo -n "ferr is " 1>&2; mktemp | tee /dev/stderr ) && \
	if $(MAKE) cleanrun-abort-$* 1>$$out 2>$$err; then false; else touch "$@"; fi

# Some cases are expected to succeed without liballocs being involved.
checkrun-noquery-%: noquery-%/$(FRONTEND)/check-stamp
	true # remembered pass of noquery-$*
noquery-%/$(FRONTEND)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	@echo "no-query case is noquery-$*" && \
	out=$$( echo -n "out is " 1>&2; mktemp | tee /dev/stderr ) && \
	err=$$( echo -n "err is " 1>&2; mktemp | tee /dev/stderr ) && \
	if $(MAKE) cleanrun-noquery-$* 1>$$out 2>$$err && \
           ! grep '^queries handled.* [1-9]\+[0-9]*$$' $$err; then touch "$@"; else false; fi

# SoftBound should also never call liballocs
checkrun-softbound-%: softbound-%/$(FRONTEND)/check-stamp
	true # remembered pass of softbound-$*
softbound-%/$(FRONTEND)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	@echo "case is softbound-$*" && \
	out=$$( echo -n "out is " 1>&2; mktemp | tee /dev/stderr ) && \
	err=$$( echo -n "err is " 1>&2; mktemp | tee /dev/stderr ) && \
	if $(MAKE) cleanrun-softbound-$* 1>$$out 2>$$err && \
           . ./checks.sh && tail -n26 "$$err" | \
                match "out-of-bounds pointers created" '0' >/dev/null && \
                ! grep 'calls to __fetch_bounds:.* [1-9]\+[0-9]*$$' $$err; then touch "$@"; else false; fi

# Bounds cases might not report passed type checks
checkrun-bounds-%: bounds-%/$(FRONTEND)/check-stamp
	true # remembered pass of bounds-$*
bounds-%/$(FRONTEND)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	@echo "case is bounds-$*" && \
	out=$$( echo -n "out is " 1>&2; mktemp | tee /dev/stderr ) && \
	err=$$( echo -n "err is " 1>&2; mktemp | tee /dev/stderr ) && \
	$(MAKE) cleanrun-bounds-$* 1>$$out 2>$$err && \
		. ./checks.sh && tail -n26 "$$err" | \
		    touch "$@" || \
		(tail -n26 $$err; false)

# Trap bounds cases should report OOB pointers
checkrun-trap-bounds-%: trap-bounds-%/$(FRONTEND)/check-stamp
	true # remembered pass of trap-bounds-$*
trap-bounds-%/$(FRONTEND)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	@echo "case is trap-bounds-$*" && \
	out=$$( echo -n "tout is " 1>&2; mktemp | tee /dev/stderr ) && \
	err=$$( echo -n "terr is " 1>&2; mktemp | tee /dev/stderr ) && \
	$(MAKE) cleanrun-trap-bounds-$* 1>$$out 2>$$err && \
		. ./checks.sh && tail -n26 "$$err" | \
		    match "out-of-bounds pointers created" '[1-9][0-9]*' >/dev/null && \
		    touch "$@" || \
		(tail -n26 $$err; false)

# Most test cases should output a libcrunch summary in which 
# -- zero or more checks passed (trivially or by lazy typing)
# -- zero checks aborted
# -- zero checks failed
checkrun-%: %/$(FRONTEND)/check-stamp
	true # remembered pass of $*
%/$(FRONTEND)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	@echo "case is $*" && \
	out=$$( echo -n "out is " 1>&2; mktemp | tee /dev/stderr ) && \
	err=$$( echo -n "err is " 1>&2; mktemp | tee /dev/stderr ) && \
	$(MAKE) cleanrun-$* 1>$$out 2>$$err && \
		. ./checks.sh && tail -n32 "$$err" | \
		    match "       nontrivially passed" '[1-9][0-9]*' | \
		    match "       failed inside"        '0' | \
		    match "       failed otherwise"     '0' >/dev/null && \
		    touch "$@" || \
		(tail -n32 $$err; false)

# Some cases are special and require only an exit status of zero
checkrun-hello: hello/$(FRONTEND)/check-stamp
hello/$(FRONTEND)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	$(MAKE) cleanrun-hello >/dev/null 2>&1 && touch "$@"
checkrun-hello-c++: hello-c++/$(FRONTEND)/check-stamp
hello-c++/$(FRONTEND)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	$(MAKE) cleanrun-hello-c++ >/dev/null 2>&1 && touch "$@"
checkrun-hello-errno: hello-errno/$(FRONTEND)/check-stamp
hello-errno/$(FRONTEND)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	$(MAKE) cleanrun-hello-errno >/dev/null 2>&1 && touch "$@"
checkrun-hello-qualified-char: hello-qualified-char/$(FRONTEND)/check-stamp
hello-qualified-char/$(FRONTEND)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	$(MAKE) cleanrun-hello-qualified-char >/dev/null 2>&1 && touch "$@"
checkrun-random: random/$(FRONTEND)/check-stamp
random/$(FRONTEND)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	$(MAKE) cleanrun-random >/dev/null 2>&1 && touch "$@"
checkrun-pointer-degree: pointer-degree/$(FRONTEND)/check-stamp
pointer-degree/$(FRONTEND)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	$(MAKE) cleanrun-pointer-degree >/dev/null 2>&1 && touch "$@"
checkrun-bounds-simple: bounds-simple/$(FRONTEND)/check-stamp
bounds-simple/$(FRONTEND)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	$(MAKE) cleanrun-bounds-simple >/dev/null 2>&1 && touch "$@"
checkrun-bounds-diff: bounds-diff/$(FRONTEND)/check-stamp
bounds-diff/$(FRONTEND)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	$(MAKE) cleanrun-bounds-diff >/dev/null 2>&1 && touch "$@"

# Some cases are special and have their own matching criteria
checkrun-lazy-typing: lazy-typing/$(FRONTEND)/check-stamp
lazy-typing/$(FRONTEND)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	@echo "case is lazy-typing" && \
	out=$$( echo -n "out is " 1>&2; mktemp | tee /dev/stderr ) && \
	err=$$( echo -n "err is " 1>&2; mktemp | tee /dev/stderr ) && \
	$(MAKE) cleanrun-lazy-typing 1>$$out 2>$$err && \
		. ./checks.sh && tail -n26 "$$err" | \
		    match "of which did lazy heap type assignment" '[1-9][0-9]*' | \
		    match "       failed inside"        '0' | \
		    match "       failed otherwise"     '0' >/dev/null && \
		    touch "$@" || \
		(tail -n26 $$err; false)

# These targets run with pwd of the test build dir
_onlyrun-%:
	LD_PRELOAD=$(PRELOAD) ./$*

_onlygdbrun-%:
	gdb --eval-command "set environment LD_PRELOAD=$(PRELOAD)" --args ./$* ./$*

vpath %.c ..
vpath %.cc ..
vpath %.cpp ..

# These are the main targets
build-%:
	mkdir -p "$*"/$(FRONTEND)
	$(MAKE) -C "$*"/$(FRONTEND) $(INCLUDE_MK_INC) -f ../../Makefile "$*" 

run-%:
	$(MAKE) build-$* && ( $(MAKE) -C "$*"/$(FRONTEND) $(INCLUDE_MK_INC) -f ../../Makefile _onlyrun-$* )

cleanbuild-%:
	$(MAKE) clean-$*
	$(MAKE) build-$*

cleanrun-%: 
	$(MAKE) clean-$*
	$(MAKE) run-$*

gdbrun-%: # run the test case with itself as input
	$(MAKE) -C "$*"/$(FRONTEND) $(INCLUDE_MK_INC) "$*"
	$(MAKE) -C "$*"/$(FRONTEND) $(INCLUDE_MK_INC) -f ../../Makefile _onlygdbrun-$*

gdbcleanrun-%:
	$(MAKE) clean-$*
	$(MAKE) gdbrun-$*

clean-%:
	if [ -d "$*"/$(FRONTEND) ]; then $(MAKE) -C "$*"/$(FRONTEND) $(INCLUDE_MK_INC) -f $(realpath $(THIS_MAKEFILE)) clean; fi

# generic clean rule that we can run from test dirs too (with $(MAKE) -f ../Makefile)
clean: # (delete anything whose name is a prefix of a .c file's and doesn't contain a dot)
	rm -f $(filter-out .,$(patsubst %.c,%,$(shell find -name '*.c')))
	rm -f $(filter-out .,$(patsubst %.cc,%,$(shell find -name '*.cc')))
	find -name '*.cil.*'  -o -name '*.i' -o -name '*.o' -o -name '*.s' -o -name '*.allocs' | xargs rm -f
	find -name '*.ii' | xargs rm -f
	rm -f */*/check-stamp
