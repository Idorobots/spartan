#!/bin/bash

echo "Running the test suite..."

ARGS=""
for arg in "$@"; do
    ARGS="$ARGS $(echo "\"$arg\"")"
done

racket -e "(require \"test/main.rkt\") (run-all-tests '($ARGS))"
