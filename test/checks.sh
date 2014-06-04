match () {
    line="$1"
    count_pattern="$2"
    
    input="$(cat)"
    matched="$( echo "$input" | grep "$line" )"
    nlines_matched="$( echo "$matched" | wc -l )"
    
    if ! [ $nlines_matched -eq 1 ]; then
        echo "Unexpectedly matched $nlines_matched lines" 1>&2
        exit 1
    fi
    
    echo "$matched" | sed 's/.*[^0-9]\([0-9]*\)$/\1/' | grep "$count_pattern" || \
        (echo "Count for \"$line\" did not match '$count_pattern'" 1>&2; false) || exit 1
    
    # only chain if we succeeded
    echo "$input"
    
}
