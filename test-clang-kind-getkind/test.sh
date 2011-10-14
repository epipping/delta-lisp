#!/usr/bin/env bash

clang++ -c -std=c++0x -x c++ $1 -o /dev/null >/dev/null 2>&1

[[ $? -eq 254 ]]
