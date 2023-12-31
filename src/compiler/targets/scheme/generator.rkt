#lang racket

(require "../../env.rkt")
(require "../../ast.rkt")
(require "../../errors.rkt")
(require "../../utils/utils.rkt")

(provide generate-scheme)

(define (generate-scheme env)
  ;; FIXME Actually implement a proper code-gen.
  (let ((data (map (lambda (v)
                     (list 'define
                           (car v)
                           (ast->plain (cdr v))))
                   (env-get env 'data)))
        (init (ast->plain (env-get env 'init))))
    (if (empty? data)
        init
        `(begin ,@data
                ,init))))
