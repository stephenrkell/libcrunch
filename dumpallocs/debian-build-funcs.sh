rebuild_debian () {
    find -name '*.cil.*' -o -name '*.i' -o -name '*.allocs' -type f | xargs rm -f
    export UNIQTYPES_BASE=${HOME}/work/devel/libcrunch.hg/dumpallocs/allocsites
    export LD_PRELOAD=${HOME}/work/devel/libpmirror.hg/lib/libheap_index_fast_hooks.so
    DEB_LDFLAGS_APPEND="-L${HOME}/work/devel/libcrunch.hg/lib -Wl,-R$( readlink -f ${HOME}/work/devel/libcrunch.hg/lib ) -Wl,--no-add-needed -lcrunch -Wl,--add-needed -ldl -Wl,--allow-shlib-undefined" \
        CC='/usr/local/src/cil/bin/cilly --save-temps --dotrumptr --dodumpallocs' \
        DEB_BUILD_OPTIONS="nostrip" dpkg-buildpackage
}

# DEB_LDFLAGS_APPEND="-L${HOME}/work/devel/typiklee.hg/lib \-Wl,-R$( readlink -f ${HOME}/work/devel/typiklee.hg/lib ) -Wl,-Bdynamic -Wl,--as-needed -lcrunch -ldl -Wl,--dynamic-list -Wl,dynamic-list -Wl,--allow-shlib-undefined -Wl,--no-as-needed" \
