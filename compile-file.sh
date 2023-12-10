#!/bin/bash

racket -e "(require \"src/main.rkt\") (compile-file \"$1\")"
