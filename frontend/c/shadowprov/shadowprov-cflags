#!/bin/bash

LIBCRUNCH_BASE="$( readlink -f "$(dirname "$0" )"/../../.. )"
LIBALLOCS_BASE="$( readlink -f "$(dirname "$0" )"/../../../../liballocs )"

case "$0" in
    (*cflags|*cppflags)
        "${LIBCRUNCH_BASE}"/frontend/cilpp/bin/cilpp-cflags | tr '\n' ' '
        /bin/echo \
         -Wp,-plugin,${LIBALLOCS_BASE}/tools/lang/c/cilallocs/cilallocs.cmxs \
         -include "${LIBALLOCS_BASE}"/include/liballocs_cil_inlines.h \
         -Wp,-plugin,${LIBCRUNCH_BASE}/frontend/c/alloclocals/alloclocals.cmxs \
         -Wp,-fpass-alloclocals \
         -Wp,-plugin,${LIBALLOCS_BASE}/tools/lang/c/monalloca/monalloca.cmxs \
         -Wp,-fpass-monalloca \
         -Wp,-plugin,"${LIBCRUNCH_BASE}"/frontend/c/shadow/shadow.cmxs \
         -Wp,-plugin,"${LIBCRUNCH_BASE}"/frontend/c/shadowprov/shadowprov.cmxs \
         -Wp,-fpass-shadowprov \
         -include "${LIBCRUNCH_BASE}"/frontend/c/shadowprov/shadowprov_helpers.h \
         -fno-builtin-memcpy
    ;;
    (*ldflags)
        /bin/echo -Wl,-plugin,"${LIBCRUNCH_BASE}"/frontend/c/shadowprov/gold-plugin.so \
       -Wl,-rpath,"${LIBCRUNCH_BASE}"/lib \
       -Wl,-rpath,"${LIBCRUNCH_BASE}"/frontend/c/shadowprov \
       -Wl,@"${LIBCRUNCH_BASE}"/frontend/c/shadowprov/wrap.ldopts \
       -Wl,--export-dynamic \
       -Wl,--emit-relocs
    ;;
    (*)    echo "Did not understand \$0: $0" 1>&2
    ;;
esac
