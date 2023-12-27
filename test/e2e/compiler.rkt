#lang racket

;; Parse tree validation tests.

(require "../testing.rkt")
(require "../../src/compiler/ast.rkt")
(require "../../src/compiler/env.rkt")
(require "../../src/compiler/compiler.rkt")
(require "../../src/compiler/utils/gensym.rkt")
(require "../../src/compiler/passes/optimize.rkt")

(describe
 "compiler"
 (it "finds the expected errors"
     (map (lambda (filename)
            (test-file filename))
          (filter (lambda (filename)
                    (string-suffix? filename ".sprtn"))
                  (map (lambda (path)
                         (string-append "test/sprtn/errors/"
                                        (path->string path)))
                       (directory-list "test/sprtn/errors/"))))))
