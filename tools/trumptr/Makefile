CIL ?= $(realpath $(dir $(lastword $(MAKEFILE_LIST))))/../cil

default: $(patsubst %.c,%.cil.o,$(filter-out %.cil.c,$(wildcard *.c)))

%.cil.o: %.c $(CIL)/bin/cilly
	$(CIL)/bin/cilly --dodumpallocs --dotrumptr --save-temps -c -o "$@" "$<"

clean:
	rm -f *.o *.cil.c *.i

$(CIL)/bin/cilly: trumptr.ml dumpallocs.ml
	#(cd $(CIL) && ./configure EXTRASRC=$(dir $(lastword $(MAKEFILE_LIST))) EXTRAFEATURES=trumptr)
	$(MAKE) -C $(CIL) && touch $(CIL)/bin/cilly
