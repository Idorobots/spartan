#!/bin/bash

mkdir -p generated

cd src/

parser="../generated/parser.rkt"

# Create parser bootstrap if it doesn't yet exist.
if [ ! -e "$parser" ] ; then

cat <<EOF > $parser
#lang racket
(provide Program)
(define Program
  (lambda args
    '()))
EOF

# Ensure to generate actual parser.
racket -t main.rkt

fi

# Run the test suite.
racket -e '(require "../test/main.rkt")' -e '(run-all-tests)'
