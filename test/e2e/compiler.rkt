#lang racket

;; Parse tree validation tests.

(require "../testing.rkt")

(describe
 "compiler"
 (it "finds the expected errors"
     (map (lambda (filename)
            (test-file filename))
          (filter (lambda (filename)
                    (string-suffix? filename ".sprtn"))
                  (map (lambda (path)
                         (string-append "test/data/errors/"
                                        (path->string path)))
                       (directory-list "test/data/errors/"))))))
