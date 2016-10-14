CRUNCHCC := $(LIBCRUNCH_BASE)/frontend/c/bin/crunchxcc
export CRUNCHCC
CC := $(CRUNCHCC)

LDFLAGS += -Wl,--defsym,other_vs=vs
