#!/bin/sh

swiftc src/*.swift main.swift -o glr

if [ $# -eq 1 ]; then
    ./glr "$1"
elif [ $# -eq 2 ]; then
    ./glr "$1" "$2"
fi

rm -rf glr
