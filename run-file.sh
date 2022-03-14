#!/bin/bash

racket -e "(require \"src/main.rkt\") (time (run-file \"$1\"))"
