#!/bin/bash

echo CALLED!

grep -q 17 "$1" || exit -1
grep -q '^9$' "$1" || exit -1
grep -q 82 "$1" || exit -1

l=$(wc -l "$1" | cut -f 1 -d ' ')

[[ "$l" -ge 8 ]]
