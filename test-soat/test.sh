#!/bin/sh

/Applications/Xcode-beta.app/Contents//Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang++ \
    -x c++ -std=c++14 $1 2>&1 >/dev/null \
    | grep -q 'Segmentation fault'
