#! /bin/sh

export FILE=/tmp/unimplemented.scm

guile --fresh-auto-compile -l $FILE -e '%main' 'foof.scm' 2>&1 | grep "unbound variable" | awk '{ print $7 }' | sort | uniq
