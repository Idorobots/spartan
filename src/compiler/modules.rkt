#lang racket

;; Convenience functions for handling modules.

(require "utils/utils.rkt")
(require "ast.rkt")

(provide expand-structure-refs)

(define (expand-structure-refs loc head rest)
  (foldl (lambda (part acc)
           (make-ast-primop-app loc
                                '&structure-ref
                                (list acc
                                      (generated
                                       (make-ast-quote loc part)))))
         (make-ast-symbol loc head)
         (map (partial make-ast-symbol loc)
              rest)))
