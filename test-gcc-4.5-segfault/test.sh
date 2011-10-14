#!/usr/bin/env bash

g++-4.5 -S -x c++ $1 -o /dev/null |& grep -q 'Segmentation fault' || exit -1
g++-4.6 -fsyntax-only -x c++ $1 -o /dev/null || exit -1

