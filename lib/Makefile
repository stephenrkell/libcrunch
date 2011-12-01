CFLAGS += -std=gnu99 -g -O3 -fPIC

default: libcrunch.so

libcrunch.so: libcrunch.c 
	$(CC) $(CFLAGS) -shared -o "$@" "$<" $(LDFLAGS) -lheap_index_fast_hooks
