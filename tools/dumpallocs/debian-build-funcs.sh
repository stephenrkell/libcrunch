rebuild_debian () {
    find -name '*.cil.*' -o -name '*.i' -o -name '*.allocs' -type f | xargs rm -f
    DEB_LDFLAGS_APPEND="-L${HOME}/work/devel/typiklee.hg/lib -Wl,-R$( readlink -f ${HOME}/work/devel/typiklee.hg/lib )" \
        LDLIBS="-lcrunch -ldl" CC='/usr/local/src/cil/bin/cilly --save-temps --dotrumptr --dodumpallocs' \
        DEB_BUILD_OPTIONS="nostrip" dpkg-buildpackage
}
