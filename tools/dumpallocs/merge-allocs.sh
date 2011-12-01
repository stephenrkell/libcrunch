#!/bin/bash

# We merge the binary-derived allocsite information from objdumpallocs
# with the precise source-level allocsite information from dumpallocs.ml.

while read obj func offset sourcefile sourceline alloctype rest; do
    # look for the source-level allocs file 
    for attempt in \
    $( echo "$sourcefile" | sed 's/\.c/.i.allocs/' ) \
    $( echo "$sourcefile".allocs ) \
    $( echo "$sourcefile" | sed 's/\.[^\.]*$/.allocs/' ) \
    $( echo "$sourcefile" | sed 's/\.[^\.]*$/.i.allocs/' ); do
        if [[ -r "$attempt" ]]; then
            echo "attempt $attempt exists" 1>&2
            # extract the line(s) that match our line,
            # first trying exact line, then trying one less
            (grep "${sourcefile}\t$sourceline" "$attempt" || \
                grep "${sourcefile}\t$(( $sourceline - 1 ))" "$attempt" ) | head -n1 | \
                read matched_sourcefile matched_sourceline matched_func typestring \
                && echo "$obj"$'\t'"$func"$'\t'"$offset"$'\t'"$sourcefile"$'\t'"$sourceline"$'\t'"$typestring"$'\t' \
                && continue || echo "attempt $attempt did not match sourcefile $sourcefile line $sourceline" 1>&2
        fi
    done
done
