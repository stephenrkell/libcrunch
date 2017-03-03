export CC := $(LIBCRUNCH_BASE)/frontend/c/bin/crunchxcc

LDFLAGS += -Wl,--defsym,other_vs=vs
