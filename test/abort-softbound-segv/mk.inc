CRUNCHCC := $(LIBCRUNCH_BASE)/frontend/c/bin/crunchsbcc
export CRUNCHCC
CC := $(CRUNCHCC)

export CFLAGS := $(CFLAGS) -DLIBCRUNCH_ABORT_ON_OOB=1

$(warning in mk.inc, CRUNCHCC is $(CRUNCHCC))
$(warning in mk.inc, CC is $(CC))
