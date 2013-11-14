#!/bin/bash

# TODO: fix regexp to exclude function signatures like "void foo(bar *);"

. ~/lib/bash/util

declare -a line_numbers

POST=${POST:-3}
PRE=${PRE:-3}

find -name '*.c' | \
xargs grep -Hn '([[:blank:]]*[a-zA-Z_][a-zA-Z0-9_ ]*\([[:blank:]]*\*[[:blank:]]*\)\+)' | \
grep -v alloc | grep -vi char | less | grep -v '(void[[:blank:]]\+\*)' | cut -f1 | tr ':' '\t'  | \
while read file line; do
    if [[ -n "$oldfile" ]] && [[ "$file" != "$oldfile" ]]; then
        # flush results for the file just gone
        regexp="$( echo $line_numbers[@] | make_match_any_line_floating_eregexp )"
        cat -n "$oldfile" | grep -A${POST} -B${PRE} "\t$regexp" | sed "s^.*^${file}:&^"
        unset line_numbers
        ctr=0
    else
        # append this line number
        line_numbers[$ctr]="${line}"
        ctr=$(( ${ctr:-0} + 1 ))
    fi
    #sed -n "$(( ${line} - ${PRE} )),$(( ${line} + ${POST} )) p" "$file #"
    oldfile="$file"
done

# show lines from particular contexts in a file
# -- use grep for this 

