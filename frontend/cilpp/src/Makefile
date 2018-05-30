.PHONY: default
default: cilpp

# FIXME
LIBALLOCS ?= /home/stephen/work/devel/liballocs.hg
CIL_INSTALL ?= $(LIBALLOCS)/tools/lang/c/cil
#vpath %.cma $(CIL_INSTALL)/lib/cil
OCAMLFLAGS += -I $(CIL_INSTALL)/lib/cil

OCAMLOPTFLAGS += -fPIC
CFLAGS += -fPIC

# IMPORTANT: only list cmxa files here iff they are *not*
# covered by a -package argument to ocamlfind -- check the
# Makefile in our parent directory too. The effect of
# -package is to delegate to ocamlfind the inclusion by -I
# of the relevant package. The effect of -linkpkg is to delegate
# to ocamlfind the inclusion of the package cmxa on the command
# line. If we both use -linkpkg with -package of a given package,
# and give the cmxa on the command line, bad things can happen.
# List only the packages whose cmxa files live in either
# (1) locations we can predict from this Makefile, or
# (2) locations that are "standard" don't need specifying.
# That means standard OCaml libs are okay, and CIL libs are okay,
# but findlib is not.
OCAMLLIBS := str.cmxa nums.cmxa unix.cmxa cil.cmxa #frontc.cmxa
# we use -linkall to include the whole cmxa, so that plugins can
# dynlink against any of its symbols.
cilpp: cilpp.ml mkstemp_stubs.o
	$(OCAMLFIND) ocamlopt -o "$@" $(OCAMLOPTFLAGS) $(OCAMLFLAGS) $(OCAMLLIBS) $+ -linkpkg -linkall
	
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
