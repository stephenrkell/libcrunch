export CC := $(LIBCRUNCH_BASE)/frontend/c/bin/crunchsbcc

export CFLAGS := $(CFLAGS) -DLIBCRUNCH_ABORT_ON_OOB=1
