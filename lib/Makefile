.PHONY: default symlinks

default: symlinks wrap.ldopts

symlinks:
	ln -sf ../src/libcrunch.so ../src/libcrunch_stubs.so ../src/libcrunch_preload.so ../src/libcrunch_preload.a . 
	ln -sf ../src/libcrunch_stubs.o libcrunch_stubs.o
	ln -sf ../src/libcrunch_wrappers.o libcrunch_wrappers.o

wrap.ldopts: symlinks
	nm --format=posix libcrunch_wrappers.o | grep __wrap_ | \
	        sed -r 's/__wrap_([^[:blank:]]*).*/--wrap \1/' > "$@" \
	        || (rm -f "$@"; false)
