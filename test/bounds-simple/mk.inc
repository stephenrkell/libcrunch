#CRUNCHCC := $(LIBCRUNCH_BASE)/frontend/c/bin/crunchAcc
CRUNCHCC := $(LIBCRUNCH_BASE)/frontend/c/bin/crunchxcc
export CRUNCHCC
$(warning in mk.inc, CRUNCHCC is $(CRUNCHCC))
$(warning in mk.inc, CC is $(CC))
