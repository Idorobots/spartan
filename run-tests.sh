#!/bin/bash

echo "Running the test suite..."
racket -e '(require "test/main.rkt") (run-all-tests)'
