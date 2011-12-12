# NOTE: use /bin/echo
# because we get invoked from make, as sh
# hence invoking POSIX behaviour
# in which echo -e and echo -n aren't supported.
# Similarly, $'...' is not supported

# In short, in this file, stay POSIX-compatible!

obj_load_addrs () {
    LD_TRACE_LOADED_OBJECTS=1 "$1" 2>&1 | grep '=>' | grep -v 'linux-vdso' | \
    sed 's/.*=> //' | tr -d '()' | \
    tr -s '[:blank:]' '\t'
    /bin/echo -n "$( readlink -f "$1" )"
    /bin/echo -e '\t0x0'
}

mangle_objname () {
    #echo "asked to mangle: $1" 1>&2
    echo "$1" | tr '/ .-' '_'
}

obj_load_addrs_as_cpp_macros () {
    #echo "asked for: $1" 1>&2
    # We MUST output in sorted order, because allocsmt relies on this.
    obj_load_addrs "$1" | sort | while read obj base; do 
        #echo "obj is: $obj" 1>&2
        #echo "base is $base" 1>&2
        echo "-D__LOAD_ADDR_$( mangle_objname "${obj}" | tr '[a-z]' '[A-Z]' )"="${base}ULL"
        min_obj_load_addr=0x7eff00000000
        if [[ $base -gt $min_obj_load_addr ]] && ! [[ $base -eq 0 ]]; then
            echo "Warning: library $obj has a load address $base violating the assumed minimum $min_obj_load_addr" 1>&2
        fi
    done
}
