export LIBCRUNCH_ALLOC_FNS := xmalloc(Z)p
LDFLAGS += -Wl,--defsym,xmalloc=__my_xmalloc
