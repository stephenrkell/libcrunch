export CC := $(LIBCRUNCH_BASE)/frontend/c/bin/crunchbcc
export CFLAGS := $(CFLAGS) -save-temps -std=c11
export LIBALLOCS_ALLOC_FNS := alloc(Z)p
