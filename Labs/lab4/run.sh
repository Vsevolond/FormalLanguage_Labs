#!/bin/sh

swiftc src/lr0/*.swift main.swift -o lr0
./lr0
rm -rf lr0

