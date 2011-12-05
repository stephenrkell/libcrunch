#!/bin/bash

# We merge the binary-derived allocsite information from objdumpallocs
# with the precise source-level allocsite information from dumpallocs.ml.

. ~/lib/bash/util

# We can't easily predict where the .allocs file will be for a given source file.
# (see dumpallocs.ml for the reason).

input="$(cat)"

lexicographic_compare_le () {
    sorted="$( echo "$1"$'\n'"$2" | LANG=C sort )"
    if [[ "$sorted" == "$1"$'\n'"$2" ]]; then
        echo "strings compare le" 1>&2
        return 0 # true
    elif [[ "$sorted" == "$2"$'\n'"$1" ]]; then
        echo "strings compare gt" 1>&2
        return 1 # false
    else
        echo "lexicographic_compare_le: internal error" 1>&2
        return 99
    fi
}

# first pass over the input: gather all the .allocs files we might want
all_allocs_file="$(mktemp)"
while read obj func offset sourcefile rest; do
    echo "$( dirname "$sourcefile" )"
done <<< "$input" | sort | uniq | while read dir; do find $dir -name '*.allocs'; done | \
( echo "Found allocs files:" >/dev/null; tee /dev/stderr) | xargs cat | sort -t$'\t' -k1 -k2 > "$all_allocs_file"

echo "all_allocs_file: $all_allocs_file" 1>&2

# second pass -- we read input grouped by source file then line number
keep_old_line=0
while read obj func offset sourcefile sourceline sourceline_end alloctype rest; do
    #echo "read line for obj $obj sourcefile $sourcefile" 1>&2
    
    # We have our source-level allocs data on fd 3
    while true; do 
        # read a line of allocs data, unless the last line is still good
        if [[ $keep_old_line -eq 0 ]]; then
            #echo "reading some more" 1>&2
            read alloc_sourcefile alloc_sourceline alloc_fun alloc_ciltype <&3 #|| break 2
            #echo "read returned $?, new sourceline is $alloc_sourceline" 1>&2
        else
            #echo "retained old line" 1>&2
            true
        fi
        
        # possibilities:
        # 1. this is the allocs data that matches our toplevel line
        # 2. this precedes the allocs data that matches our toplevel line
        # 3. our toplevel line will never be matched, and we need to advance past it
        # (There is no "this follows our toplevel line" case, because we sorted
        # both inputs.)
        # Detecting 1 is easy; if so, we advance both inputs.
        # Detecting 2: if it has source line < toplevel, we can safely skip it as it will never be needed.
        # Else if its source line is in our window, it's a match
        # Else we have case 3, so we need to advance toplevel.
        if [[ "$(readlink -f "$alloc_sourcefile")" == "$( readlink -f "$sourcefile" )" ]] && \
           [[ "$alloc_sourceline" -ge "$sourceline" ]] && \
           [[ "$alloc_sourceline" -lt "$(( $sourceline + $sourceline_end ))" ]]; then
           # matched -- output, and advance both inputs
           #echo "matched, so advancing both" 1>&2
           echo "$obj"$'\t'"$func"$'\t'"$offset"$'\t'"$sourcefile"$'\t'"$sourceline"$'\t'"$sourceline_end"$'\t'"$alloc_ciltype"$'\t' 
           keep_old_line=0
           continue 2
        # lexicographic compare...
        elif lexicographic_compare_le "$(readlink -f "$alloc_sourcefile")" "$( readlink -f "$sourcefile" )" ||
         ( [[ "$(readlink -f "$alloc_sourcefile")" == "$( readlink -f "$sourcefile" )" ]] && \
           [[ "$alloc_sourceline" -lt "$sourceline" ]] ); then
           # we will never use this line, so skip it
           echo "warning: skipping source allocs line: $alloc_sourcefile"$'\t'"$alloc_sourceline"$'\t'"$alloc_ciltype" 1>&2
           keep_old_line=0
           continue
        
        else 
            # Try advancing the outer loop and re-testing
            # We might have a match for the next iteration of the outer loop
            # PROBLEM: we might not!
            # Each time we advance the outer, we are giving up on matching that line. 
            echo "warning: skipping objdump allocs line: $obj $func $offset $sourcefile $sourceline $sourceline_end $alloctype $rest" 1>&2
            keep_old_line=1
            continue 2
        fi
    done
   
    
    
    # build a regexp matching any line in the range 
#    file_regexp="$( echo "$sourcefile" | escape_regexp )"
#    line_regexp="$( seq $sourceline $sourceline_end | tr '[:blank:]' '\n' | make_match_any_line_regexp )"
#    grep_output="$( grep "${file_regexp}\t${line_regexp}" "$attempt" )"
#    grep_status=$? 
#    if [[ "$( echo "$grep_output" | wc -l )" -gt 1 ]]; then
#        echo "Warning: multiple matching lines in $attempt:"$'\n'"$grep_output" 1>&2
#    fi
    
#    # look for the source-level allocs file 
#    for attempt in \
#    $( echo "$sourcefile" | sed 's/\.c/.i.allocs/' ) \
#    $( echo "$sourcefile".allocs ) \
#    $( echo "$sourcefile" | sed 's/\.[^\.]*$/.allocs/' ) \
#    $( echo "$sourcefile" | sed 's/\.[^\.]*$/.i.allocs/' ); do
#        if [[ -r "$attempt" ]]; then
#            echo "attempt $attempt exists" 1>&2
#            # build a regexp matching any line in the range 
#            file_regexp="$( echo "$sourcefile" | escape_regexp )"
#            line_regexp="$( seq $sourceline $sourceline_end | tr '[:blank:]' '\n' | make_match_any_line_regexp )"
#            grep_output="$( grep "${file_regexp}\t${line_regexp}" "$attempt" )"
#            grep_status=$? 
#            if [[ "$( echo "$grep_output" | wc -l )" -gt 1 ]]; then
#                echo "Warning: multiple matching lines in $attempt:"$'\n'"$grep_output" 1>&2
#            fi
#            echo "$grep_output" | head -n1 | \
#                read matched_sourcefile matched_sourceline matched_func typestring \
#                && echo "$obj"$'\t'"$func"$'\t'"$offset"$'\t'"$sourcefile"$'\t'"$sourceline"$'\t'"$sourceline_end"$'\t'"$typestring"$'\t' \
#                && continue || echo "attempt $attempt did not match sourcefile $sourcefile line $sourceline" 1>&2
#        fi
#    done
done <<<"$( echo "$input" | sort -t$'\t' -k4 -k5 )" 3<"$all_allocs_file"
#rm -f "$all_allocs_file"
