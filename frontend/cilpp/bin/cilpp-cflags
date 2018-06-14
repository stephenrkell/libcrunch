#!/bin/bash

# FIXME: do these properly
LIBCRUNCH_BASE="$( readlink -f "$(dirname "$0" )"/../../.. )"
LIBALLOCS_BASE="$( readlink -f "$(dirname "$0" )"/../../../../liballocs )"

case "$0" in
	(*cflags|*cppflags)
		/bin/echo -no-integrated-cpp -wrapper "${LIBCRUNCH_BASE}"/frontend/cilpp/bin/wrapper
	;;
	(*ldflags) # HACK: this is the liballocs-specific config, unlike the above
		/bin/echo -Wl,-plugin,"${LIBALLOCS_BASE}"/tools/.libs/gold-plugin.so \
	   -Wl,-rpath,"${LIBCRUNCH_BASE}"/lib -Wl,-rpath,"${LIBALLOCS_BASE}"/lib
	;;
	(*)	echo "Did not understand \$0: $0" 1>&2
	;;
esac
