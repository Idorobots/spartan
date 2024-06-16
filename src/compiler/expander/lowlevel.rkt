#lang racket

;; Low-level RT macro support.

(require "../utils/utils.rkt")
(require "../ast.rkt")
(require "../errors.rkt")

(provide (all-defined-out))

(define (ignore-expander expr use-env def-env)
  (let ((loc (ast-node-location expr)))
    (replace expr
             (make-ast-quote loc
                             (make-ast-list loc '())))))

(define (asm-expander expr use-env def-env)
  (define (expand-instruction instruction)
    (match-ast instruction
     ((list (symbol 'primop-app) (ast-quote (symbol op)) args ...)
      (replace instruction
               (make-ast-primop-app (ast-node-location instruction)
                                    op
                                    args)))
     (else
      (raise-compilation-error
       instruction
       "Bad `asm` instruction:"))))

  (match-ast expr
   ((list (symbol 'asm) first rest ...)
    (let ((loc (ast-node-location expr)))
      (replace expr
             (make-ast-body loc
                            (map expand-instruction (cons first rest))
                            "Bad `asm` syntax"))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `asm` syntax, expected a list of assembly instructions to follow:")))))
