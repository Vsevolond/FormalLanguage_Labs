#!/bin/sh

swiftc src/*.swift main.swift -o glr
./glr
rm -rf glr

