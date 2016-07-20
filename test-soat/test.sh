#!/usr/bin/env bash

# Fails at least with clang 3.6.2-3.8.1 (with assertions enabled!)
CLANGXX=${CLANGXX:-clang++}

out=`${CLANGXX} -x c++ -std=c++14 $1 2>&1 >/dev/null`
[[ "x$out" == x*"Pack expansion without unexpanded packs"* ]]
