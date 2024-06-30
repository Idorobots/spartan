#!/bin/bash

racket -e '(require "src/sprtn.rkt")' -- "${@:1}"
