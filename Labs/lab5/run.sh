#!/bin/sh

swiftc src/*.swift main.swift -o glr

if [ $# -eq 0 ]; then
    ./glr ""
else
    # Передача аргумента программе
    ./glr "$1"
fi

rm -rf glr

