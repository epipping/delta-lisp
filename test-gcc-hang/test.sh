#!/bin//bash

ulimit -t 2;

out=$(g++-4.6 -c -x c++ $1 -ftemplate-depth=200 -o /dev/null 2>&1 >/dev/null)

echo OUT: "$out"
[[ ${out} == *"internal compiler error"* ]] || exit -1
[[ ${out} == *" error: "* ]] || exit -1
