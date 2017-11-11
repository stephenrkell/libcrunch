export CC := $(LIBCRUNCH_BASE)/frontend/c/bin/crunchbcc

LDFLAGS += -Wl,--defsym,other_vs=vs
