CFLAGS += -D_GNU_SOURCE # HACK
CFLAGS += -g3 -gstrict-dwarf -std=c99 -fno-eliminate-unused-debug-types -O2 #-no-pie
CFLAGS += -DUSE_STARTUP_BRK

THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))
TESTDIR := $(realpath $(dir $(THIS_MAKEFILE)))
$(warning TESTDIR is $(TESTDIR))

export CFLAGS

UNIQTYPES_BASE ?= /usr/lib/allocsites
ALLOCSITES_BASE ?= /usr/lib/allocsites
LIBALLOCS ?= $(realpath $(dir $(THIS_MAKEFILE))/../../liballocs)
LIBALLOCS_BASE ?= $(LIBALLOCS)

export UNIQTYPES_BASE
export ALLOCSITES_BASE
export LIBALLOCS
export LIBALLOCS_BASE

export LIBCRUNCH := $(realpath $(dir $(THIS_MAKEFILE))/..)
export LIBCRUNCH_BASE := $(LIBCRUNCH)
export LIBCRUNCH_ALWAYS_PRINT_EXIT_SUMMARY := 1

ifneq ($(USE_FAKE_LIBUNWIND),)
CFLAGS += -fno-omit-frame-pointer
CXXFLAGS += -fno-omit-frame-pointer
LIBUNWIND_LDLIBS :=
else
LIBUNWIND_LDLIBS := -lunwind -lunwind-`uname -m` 
endif

cases := $(filter-out checks.sh,$(filter-out rename-case.sh,$(notdir $(wildcard $(TESTDIR)/[-a-z]*))))

LIBCRUNCH := $(realpath $(dir $(THIS_MAKEFILE))/../lib/libcrunch_preload.so)

ifneq ($(MAKECMDGOALS),clean)
ifeq ($(wildcard $(LIBCRUNCH)),)
        $(error You must first build libcrunch{.so,_preload.so} and link them from ../lib)
endif
endif

export PRELOAD := "$(LIBCRUNCH)"

INCLUDE_MK_INC = `if test -e $(dir $(realpath $(THIS_MAKEFILE)))/$*/mk.inc; then /bin/echo -f ../mk.inc; else true; fi`

default: checkalltests

ifeq ($(MAKECMDGOAL),)
#$(warning cases are $(cases))
endif

# If we don't have a frontend in mind, loop over all frontends.
# What about configs? Each test defines one or more configs in which it's supposed to pass.
frontends = c c++ cilpp clang
configs = default b Sb x # Pb mb Mb tb Tb sb
ifeq ($(FRONTEND)$(CONFIG),)
checkalltests:
	for f in $(frontends); do \
		for c in $(configs); do \
			$(MAKE) FRONTEND=$$f CONFIG=$$c checkalltests || break 2; \
		done; \
	done
else
FRONTEND ?= c
CONFIG ?= default
# everything else in the whole rest of the file assumes we have a unique FRONTEND and CONFIG
# set CC according to our config
DEFAULT_CC = $(LIBCRUNCH_BASE)/frontend/$(FRONTEND)/bin/$(CONFIG:default=)cc
ifeq ($(CC),cc)
CC := $(DEFAULT_CC)
endif
ifeq ($(CC),)
CC := $(DEFAULT_CC)
endif
#$(warning in $(MAKEFILE_LIST), CC is $(CC))
# set CXX according to our config
DEFAULT_CXX = $(LIBCRUNCH_BASE)/frontend/$(FRONTEND)/bin/$(CONFIG:default=)c++
ifeq ($(CXX),g++) # i.e. GNU make's "default" CXX value
CXX := $(DEFAULT_CXX)
endif
ifeq ($(CXX),)
CXX := $(DEFAULT_CXX)
endif
#$(warning in $(MAKEFILE_LIST), CXX is $(CXX))
export CC
export CXX
export CFLAGS += -I$(realpath $(dir $(THIS_MAKEFILE)))/../include -I$(LIBALLOCS)/include
export LDFLAGS += -L$(LIBALLOCS)/lib

# We want to filter the list of cases, to include only those
# relevant to the current FRONTEND.
# What defines a frontend? One idea: it's a directory in frontend/.
# It's partly a language thing, but not always.
# The common case is C-language tests,
# where we might want "c, clang" but occasionally just "c".
# Some, like Fortran, might want "Fortran".
# Use a kernel-style makevar solution:
# But that's not enough, because each frontend (say the CIL frontend)
# may define multiple compilers (crunchcc, crunchxcc, ...).

# Give each test a chance to say something about its frontends and configs
#$(foreach c,$(cases),-include $(c)/frontend.mk)

# set CASES_fe and CASES_CONFIG_c for each test's {( fe, c )}
-include */frontend.mk

# some cases don't set 
explicit_fe_cases := $(foreach c,$(cases),$(notdir $(realpath $(dir $(wildcard $(TESTDIR)/$(c)/frontend.mk)))))
no_explicit_fe_cases := $(filter-out $(explicit_fe_cases),$(cases))
#$(warning explicit_fe_cases is $(explicit_fe_cases))
$(warning no_explicit_fe_cases is $(no_explicit_fe_cases))
# Common-case rules are:
# - assume C language, so frontends "c" and "clang".
# - assume default config
# For all cases which don't define a frontend.mk, add to CASES_c and CASES_clang.
CASES_c += $(no_explicit_fe_cases)
CASES_clang += $(no_explicit_fe_cases)
CASES_CONFIG_default += $(no_explicit_fe_cases)

# This frontend cases, and this config cases
fcases := $(filter $(CASES_$(FRONTEND)),$(cases))
ccases := $(filter $(CASES_CONFIG_$(CONFIG)),$(cases))
fccases := $(filter $(CASES_CONFIG_$(CONFIG)),$(fcases))
$(info frontend $(FRONTEND), config $(CONFIG))
#$(info fcases is $(fcases))
#$(info ccases is $(ccases))
$(info fccases is $(fccases))

# We used to specify "config" by setting CC in mk.inc.
# Now we have frontend.mk do it.
# Big question: should be support multiple configs per test,
# or just a single one?
# E.g. a subset of bounds tests will run on Sb or b.
# It needs to be multiple configs, because we have so many bounds configs.
# If we've specified CC, it should match our frontend/config.
ifneq ($(CC),$(DEFAULT_CC))
$(error "Specified CC as $(CC), but frontend $(FRONTEND) and config $(CONFIG) makes $(DEFAULT_CC)")
endif

# Our default target: do a checkrun of all cases for the current frontend.
# Do we also have a "current config"?
checkalltests: $(patsubst %,checkrun-%,$(fccases))
# Our stamp files should not be removed by make, even though they are not explicit targets
.PRECIOUS: $(patsubst %,%/$(FRONTEND)+$(CONFIG)/check-stamp,$(fccases))

# List of everything which, if changed, should prompt re-running tests.
RERUN_TESTS_ON_CHANGE := $(LIBCRUNCH) $(shell find $(dir $(THIS_MAKEFILE))/../frontend | grep '\.cmxs$$' )

# Some cases are expected to fail, i.e. generate libcrunch "check failed" stats.
# Note that bounds errors never do this. HMM. Soon, type errors might not either.
checkrun-fail-bounds-type-%: fail-bounds-type-%/$(FRONTEND)+$(CONFIG)/check-stamp
	true # remembered pass of fail-bounds-type-$*
fail-bounds-type-%/$(FRONTEND)+$(CONFIG)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	@echo "frontend $(FRONTEND), config $(CONFIG), expected-fail case is fail-bounds-type-$*" && \
	out=$$( echo -n "out is " 1>&2; mktemp | tee /dev/stderr ) && \
	err=$$( echo -n "err is " 1>&2; mktemp | tee /dev/stderr ) && \
	$(MAKE) cleanrun-fail-bounds-type-$* 1>$$out 2>$$err && \
		. ./checks.sh && tail -n26 "$$err" | \
		    match "       failed otherwise" '[1-9][0-9]*' >/dev/null && \
		    touch "$@" || \
		(tail -n26 $$err; false)
checkrun-fail-bounds-%: fail-bounds-%/$(FRONTEND)+$(CONFIG)/check-stamp
	true # remembered pass of fail-bounds-$*
fail-bounds-%/$(FRONTEND)+$(CONFIG)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	@echo "frontend $(FRONTEND), config $(CONFIG), expected-fail case is fail-bounds-$*" && \
	out=$$( echo -n "out is " 1>&2; mktemp | tee /dev/stderr ) && \
	err=$$( echo -n "err is " 1>&2; mktemp | tee /dev/stderr ) && \
	$(MAKE) cleanrun-fail-bounds-$* 1>$$out 2>$$err && \
		. ./checks.sh && tail -n26 "$$err" | \
		    match "out-of-bounds pointers created" '[1-9][0-9]*' >/dev/null && \
		    touch "$@" || \
		(tail -n26 $$err; false)

# Some cases are expected to fail, i.e. generate libcrunch "check failed" stats.
checkrun-fail-%: fail-%/$(FRONTEND)+$(CONFIG)/check-stamp
	true # remembered pass of fail-$*
fail-%/$(FRONTEND)+$(CONFIG)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	@echo "frontend $(FRONTEND), config $(CONFIG), expected-fail case is fail-$*" && \
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
checkrun-abort-%: abort-%/$(FRONTEND)+$(CONFIG)/check-stamp
	true # remembered pass of abort-$*
abort-%/$(FRONTEND)+$(CONFIG)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	@echo "frontend $(FRONTEND), config $(CONFIG), expected-fail case is abort-$*" && \
	out=$$( echo -n "fout is " 1>&2; mktemp | tee /dev/stderr ) && \
	err=$$( echo -n "ferr is " 1>&2; mktemp | tee /dev/stderr ) && \
	if $(MAKE) cleanrun-abort-$* 1>$$out 2>$$err; then false; else touch "$@"; fi

# Some cases are expected to succeed without liballocs being involved.
checkrun-noquery-%: noquery-%/$(FRONTEND)+$(CONFIG)/check-stamp
	true # remembered pass of noquery-$*
noquery-%/$(FRONTEND)+$(CONFIG)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	@echo "frontend $(FRONTEND), config $(CONFIG), no-query case is noquery-$*" && \
	out=$$( echo -n "out is " 1>&2; mktemp | tee /dev/stderr ) && \
	err=$$( echo -n "err is " 1>&2; mktemp | tee /dev/stderr ) && \
	if $(MAKE) cleanrun-noquery-$* 1>$$out 2>$$err && \
           ! grep '^queries handled.* [1-9]\+[0-9]*$$' $$err; then touch "$@"; else false; fi

# SoftBound should also never call liballocs -- alloc wrappers return bounds
checkrun-softbound-%: softbound-%/$(FRONTEND)+$(CONFIG)/check-stamp
	true # remembered pass of softbound-$*
softbound-%/$(FRONTEND)+$(CONFIG)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	@echo "frontend $(FRONTEND), config $(CONFIG), case is softbound-$*" && \
	out=$$( echo -n "out is " 1>&2; mktemp | tee /dev/stderr ) && \
	err=$$( echo -n "err is " 1>&2; mktemp | tee /dev/stderr ) && \
	if $(MAKE) cleanrun-softbound-$* 1>$$out 2>$$err && \
           . ./checks.sh && tail -n26 "$$err" | \
                match "out-of-bounds pointers created" '0' >/dev/null && \
                ! grep 'calls to __fetch_bounds:.* [1-9]\+[0-9]*$$' $$err; then touch "$@"; else false; fi

# Bounds cases might not report passed type checks
checkrun-bounds-%: bounds-%/$(FRONTEND)+$(CONFIG)/check-stamp
	true # remembered pass of bounds-$*
bounds-%/$(FRONTEND)+$(CONFIG)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	@echo "frontend $(FRONTEND), config $(CONFIG), case is bounds-$*" && \
	out=$$( echo -n "out is " 1>&2; mktemp | tee /dev/stderr ) && \
	err=$$( echo -n "err is " 1>&2; mktemp | tee /dev/stderr ) && \
	$(MAKE) cleanrun-bounds-$* 1>$$out 2>$$err && \
		. ./checks.sh && tail -n26 "$$err" | \
		    touch "$@" || \
		(tail -n26 $$err; false)

# Trap bounds cases should report OOB pointers
checkrun-trap-bounds-%: trap-bounds-%/$(FRONTEND)+$(CONFIG)/check-stamp
	true # remembered pass of trap-bounds-$*
trap-bounds-%/$(FRONTEND)+$(CONFIG)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	@echo "frontend $(FRONTEND), config $(CONFIG), case is trap-bounds-$*" && \
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
checkrun-%: %/$(FRONTEND)+$(CONFIG)/check-stamp
	true # remembered pass of $*
%/$(FRONTEND)+$(CONFIG)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	@echo "frontend $(FRONTEND), config $(CONFIG), case is $*" && \
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
checkrun-hello: hello/$(FRONTEND)+$(CONFIG)/check-stamp
hello/$(FRONTEND)+$(CONFIG)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	$(MAKE) cleanrun-hello >/dev/null 2>&1 && touch "$@"
checkrun-hello-c++: hello-c++/$(FRONTEND)+$(CONFIG)/check-stamp
hello-c++/$(FRONTEND)+$(CONFIG)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	$(MAKE) cleanrun-hello-c++ >/dev/null 2>&1 && touch "$@"
checkrun-hello-errno: hello-errno/$(FRONTEND)+$(CONFIG)/check-stamp
hello-errno/$(FRONTEND)+$(CONFIG)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	$(MAKE) cleanrun-hello-errno >/dev/null 2>&1 && touch "$@"
checkrun-hello-qualified-char: hello-qualified-char/$(FRONTEND)+$(CONFIG)/check-stamp
hello-qualified-char/$(FRONTEND)+$(CONFIG)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	$(MAKE) cleanrun-hello-qualified-char >/dev/null 2>&1 && touch "$@"
checkrun-random: random/$(FRONTEND)+$(CONFIG)/check-stamp
random/$(FRONTEND)+$(CONFIG)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	$(MAKE) cleanrun-random >/dev/null 2>&1 && touch "$@"
checkrun-pointer-degree: pointer-degree/$(FRONTEND)+$(CONFIG)/check-stamp
pointer-degree/$(FRONTEND)+$(CONFIG)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	$(MAKE) cleanrun-pointer-degree >/dev/null 2>&1 && touch "$@"
checkrun-bounds-simple: bounds-simple/$(FRONTEND)+$(CONFIG)/check-stamp
bounds-simple/$(FRONTEND)+$(CONFIG)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	$(MAKE) cleanrun-bounds-simple >/dev/null 2>&1 && touch "$@"
checkrun-bounds-diff: bounds-diff/$(FRONTEND)+$(CONFIG)/check-stamp
bounds-diff/$(FRONTEND)+$(CONFIG)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	$(MAKE) cleanrun-bounds-diff >/dev/null 2>&1 && touch "$@"

# Some cases are special and have their own matching criteria
#checkrun-lazy-typing: lazy-typing/$(FRONTEND)+$(CONFIG)/check-stamp
XXXdisabed-lazy-typing/$(FRONTEND)+$(CONFIG)/check-stamp: $(RERUN_TESTS_ON_CHANGE)
	@echo "frontend $(FRONTEND), config $(CONFIG), case is lazy-typing" && \
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
	mkdir -p "$*"/$(FRONTEND)+$(CONFIG)
	$(MAKE) -C "$*"/$(FRONTEND)+$(CONFIG) $(INCLUDE_MK_INC) -f ../../Makefile "$*" 

run-%:
	$(MAKE) build-$*
	$(MAKE) -C "$*"/$(FRONTEND)+$(CONFIG) $(INCLUDE_MK_INC) -f ../../Makefile _onlyrun-$*

cleanbuild-%:
	$(MAKE) clean-$*
	$(MAKE) build-$*

cleanrun-%: 
	$(MAKE) clean-$*
	$(MAKE) run-$*

gdbrun-%: # run the test case with itself as input
	$(MAKE) -C "$*"/$(FRONTEND)+$(CONFIG) $(INCLUDE_MK_INC) "$*"
	$(MAKE) -C "$*"/$(FRONTEND)+$(CONFIG) $(INCLUDE_MK_INC) -f ../../Makefile _onlygdbrun-$*

gdbcleanrun-%:
	$(MAKE) clean-$*
	$(MAKE) gdbrun-$*

endif # end assuming we have FRONTEND

clean-%:
	if [ -z "$(FRONTEND)" ]; then frontends="$(frontends)"; else frontends=$(FRONTEND); fi; \
	if [ -z "$(CONFIG)" ]; then configs="$(configs)"; else configs=$(CONFIG); fi; \
	for c in $$configs; do \
	for f in $$frontends; do \
		if [ -d "$*"/$$f+$$c ]; then \
			$(MAKE) -C "$*"/$$f+$$c $(INCLUDE_MK_INC) -f $(realpath $(THIS_MAKEFILE)) clean; \
			rm -f "$*"/$$f+$$c/"$*"; \
		fi; \
	done; \
    done

# generic clean rule that we can run from test dirs too (with $(MAKE) -f ../Makefile)
clean: $(foreach c,$(cases),clean-$(c)) # (delete anything whose name is a prefix of a .c file's and doesn't contain a dot)
	rm -f $(filter-out .,$(patsubst %.c,%,$(shell find -name '*.c')))
	rm -f $(filter-out .,$(patsubst %.cc,%,$(shell find -name '*.cc')))
	find -name '*.cil.*'  -o -name '*.i' -o -name '*.o' -o -name '*.s' -o -name '*.allocs' | xargs rm -f
	find -name '*.ii' | xargs rm -f
	rm -f */*/check-stamp
