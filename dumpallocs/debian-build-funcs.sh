rebuild_debian () {
    find -name '*.cil.*' -o -name '*.i' -o -name '*.allocs' -type f | xargs rm -f
    export UNIQTYPES_BASE=${HOME}/work/devel/libcrunch.hg/dumpallocs/allocsites
    export LD_PRELOAD=${HOME}/work/devel/libpmirror.hg/lib/libheap_index_fast_hooks.so
    if [[ -z "$( echo "$PATH" | tr ':' '\n' | grep '/cil[^/]*/bin' )" ]]; then
       export PATH=/usr/local/src/cil/bin:${PATH}
    fi
    LIBCRUNCH_ALLOC_FNS="xcalloc(zZ) xmalloc(Z) xrealloc(pZ) xmallocz(Z)" \
    DEB_LDFLAGS_APPEND="-L${HOME}/work/devel/libcrunch.hg/lib -Wl,-R$( readlink -f ${HOME}/work/devel/libcrunch.hg/lib ) -Wl,--no-add-needed -lcrunch -Wl,--add-needed -ldl -Wl,--allow-shlib-undefined" \
        CC=${HOME}/work/devel/libcrunch.hg/bin/crunchcc \
        DEB_BUILD_OPTIONS="nostrip" dpkg-buildpackage 2>&1 | tee build.log
}

# DEB_LDFLAGS_APPEND="-L${HOME}/work/devel/typiklee.hg/lib \-Wl,-R$( readlink -f ${HOME}/work/devel/typiklee.hg/lib ) -Wl,-Bdynamic -Wl,--as-needed -lcrunch -ldl -Wl,--dynamic-list -Wl,dynamic-list -Wl,--allow-shlib-undefined -Wl,--no-as-needed" \
