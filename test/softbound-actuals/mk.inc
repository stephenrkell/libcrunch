CRUNCHCC := $(LIBCRUNCH_BASE)/frontend/c/bin/crunchsbcc
export CRUNCHCC
CC := $(CRUNCHCC)
$(warning in mk.inc, CRUNCHCC is $(CRUNCHCC))
$(warning in mk.inc, CC is $(CC))
