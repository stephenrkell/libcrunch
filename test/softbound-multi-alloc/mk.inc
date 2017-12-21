export CC := $(LIBCRUNCH_BASE)/frontend/c/bin/crunchSbcc
export CFLAGS := $(CFLAGS) -save-temps -std=c11 -DLIBCRUNCH_TRACE_BOUNDS_STACK
export LIBALLOCS_ALLOC_FNS := alloc(Z)p
