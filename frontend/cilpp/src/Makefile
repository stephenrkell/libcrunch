.PHONY: default
default: cilpp

# FIXME
LIBALLOCS ?= /home/stephen/work/devel/liballocs.hg
CIL_INSTALL ?= $(LIBALLOCS)/tools/lang/c/cil
#vpath %.cma $(CIL_INSTALL)/lib/cil
OCAMLFLAGS += -I $(CIL_INSTALL)/lib/cil

OCAMLOPTFLAGS += -fPIC
CFLAGS += -fPIC

OCAMLLIBS := str.cmxa nums.cmxa unix.cmxa cil.cmxa #frontc.cmxa
cilpp: cilpp.ml mkstemp_stubs.o
	$(OCAMLFIND) ocamlopt -o "$@" $(OCAMLOPTFLAGS) $(OCAMLFLAGS) $(OCAMLLIBS) $+
	
%.cmxs: %.cmx
	$(OCAMLFIND) ocamlopt -shared -o "$@" $(OCAMLOPTFLAGS) $(OCAMLFLAGS) $+
%.cmx %.cmi: %.ml
	$(OCAMLFIND) ocamlopt -o "$@" $(OCAMLOPTFLAGS) $(OCAMLFLAGS) -c "$<"
%.cmo %.cmi: %.ml
	$(OCAMLFIND) ocamlc -o "$@" $(OCAMLFLAGS) -c "$<"
%.cma: %.cmo
	$(OCAMLFIND) ocamlc -o "$@" $(OCAMLFLAGS) -a $+

clean:
	rm -f *.o *.cmxa *.cmx *.cmo *.cmxs
