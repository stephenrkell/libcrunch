rebuild_debian () {
    find -name '*.cil.*' -o -name '*.i' -o -name '*.allocs' -type f | xargs rm -f
    CC='/usr/local/src/cil/bin/cilly --save-temps --dotrumptr --dodumpallocs' DEB_BUILD_OPTIONS=nostrip dpkg-buildpackage
}
