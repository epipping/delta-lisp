#!/usr/bin/env bash

clang++ -x c++ -c $1 -o /dev/null >/dev/null 2>&1;

[[ $? -eq 254 ]]
