#!/bin/bash

echo "Building the compiler..."

raco exe -o sprtn --cs src/sprtn.rkt

echo "Creating a distributable program in build/"

mkdir -p build
raco distribute build/ sprtn
