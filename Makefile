default: src lib frontend test #kernel

.PHONY: src
src:
	$(MAKE) -C src

.PHONY: frontend
frontend: lib
	$(MAKE) -C frontend

.PHONY: lib
lib: src
	$(MAKE) -C lib

.PHONY: kernel
kernel:
	$(MAKE) -C kernel

.PHONY: clean
clean:
	$(MAKE) -C src clean
	$(MAKE) -C frontend clean
	$(MAKE) -C kernel clean
	$(MAKE) -C lib clean
