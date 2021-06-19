#!/bin/bash

mkdir -p generated

parser="generated/parser.rkt"

# Create parser bootstrap if it doesn't yet exist.
if [ ! -e "$parser" ] ; then

echo "Generating the parser first..."

cat <<EOF > $parser
#lang racket
(provide Program)
(define Program
  (lambda args
    '()))
EOF

# Ensure to generate actual parser.
racket -t src/main.rkt

echo "Generated!"

fi

echo "Running the test suite..."
# Run the test suite.
racket -e '(require "test/main.rkt") (run-all-tests)'
