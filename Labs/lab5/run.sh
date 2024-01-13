#!/bin/sh

swiftc src/*.swift main.swift -o glr

./glr "$1" "$2"

rm -rf glr
