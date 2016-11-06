CRUNCHCC := $(LIBCRUNCH_BASE)/frontend/c/bin/crunchsbcc
export CRUNCHCC
CC := $(CRUNCHCC)

LDFLAGS += -Wl,--defsym,other_vs=vs
