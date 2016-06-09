default: src lib frontend test

.PHONY: src
src:
	$(MAKE) -C src

.PHONY: frontend
frontend: lib
	$(MAKE) -C frontend

.PHONY: lib
lib: src
	mkdir -p lib && cd lib && \
    ln -sf ../src/libcrunch.so ../src/libcrunch_stubs.so ../src/libcrunch_preload.so ../src/libcrunch_preload.a . && \
    ln -sf ../src/libcrunch_stubs.o libcrunch_stubs.o

.PHONY: clean
clean:
	$(MAKE) -C src clean
	rm -f lib/*.so lib/*.o lib/.??*
	$(MAKE) -C frontend clean
	$(MAKE) -C test clean
