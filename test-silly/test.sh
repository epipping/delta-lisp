#!/usr/bin/env bash

for ((i=1; i<100; ++i)); do
    [[ $i -eq 3 || $i -eq 7 || $i -eq 93 ]] && continue
    # Intentionally slow
    grep -q '^'$i'$' "$1" || exit -1
done
