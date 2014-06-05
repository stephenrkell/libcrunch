# This makefile handles all the CIL building for our subdirs
# (currently dumpallocs and trumptr)
CIL_TOOLS := dumpallocs trumptr
CIL ?= $(realpath $(dir $(lastword $(MAKEFILE_LIST))))/../../../cil

LIBALLOCS_BASE ?= $(realpath ../../../liballocs.hg)
CIL_EXTRASRC :=  $(LIBALLOCS_BASE)/tools/lang/c/dumpallocs $(realpath $(dir $(THIS_MAKEFILE)))/trumptr 
CIL_TOOLS_SRC := $(shell find $(CIL_EXTRASRC) -name '*.ml')
THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))

default: src lib $(CIL)/bin/cilly sanity-tests

%.cil.o: %.c $(CIL)/bin/cilly
	cd "$(dir $<)" && $(CIL)/bin/cilly --do$$( echo $(dir $<) | tr -d '/' ) --save-temps -c -o "$(notdir $@)" "$(notdir $<)"

clean:
	for dir in $(CIL_TOOLS); do (cd $$dir && rm -f *.o *.cil.c *.i ); done
	$(MAKE) -C src clean

print_cilly_path:
	@echo $(CIL)/bin/cilly

$(CIL)/bin/cilly: $(CIL_TOOLS_SRC)
	cd $(CIL) && \
	(grep '.$(foreach subpat,$(CIL_EXTRASRC) $(CIL_TOOLS),\?$(subpat).*)' config.log || \
	    ./configure EXTRASRCDIRS="$(CIL_EXTRASRC)" EXTRAFEATURES="$(CIL_TOOLS)" ) && \
	COMPILE_FLAGS=-g $(MAKE) && \
	touch $(CIL)/bin/cilly

.PHONY: sanity-tests
sanity-tests: | default
	$(MAKE) -C ../../test cleanrun-hello cleanrun-hello-errno cleanrun-hello-static cleanrun-hello-stack cleanrun-hello-heap

.PHONY: src
src:
	$(MAKE) -C src
