#!/usr/bin/env bash

n=100
for ((i=1; i<n; ++i)); do
    [[ $i -eq 3 || $i -eq 7 || $i -eq 93 ]] && continue
    # Intentionally slow (quadratic in n)
    found=false
    while read line; do
        [[ $line ==  $i ]] && found=true
    done < $1
    if ! ${found}; then
        exit -1;
    fi
done
