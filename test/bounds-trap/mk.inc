export CC := $(LIBCRUNCH_BASE)/frontend/c/bin/crunchbcc
export CFLAGS := $(CFLAGS) -save-temps -DLIBCRUNCH_DEBUG_PRIMARY_SECONDARY_TRANSITIONS
