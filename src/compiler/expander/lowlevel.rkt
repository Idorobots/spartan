#lang racket

;; Low-level RT macro support.

(require "../utils/utils.rkt")
(require "../ast.rkt")
(require "../errors.rkt")
(require "./syntax-forms.rkt")

(provide asm-expander declare-primitive-expander)

(define (declare-primitive-expander expr use-env def-env)
  (let ((loc (ast-node-location expr)))
    (match-ast expr
     ((list (symbol 'declare-primitive) (symbol name) meta ...)
      ;; TODO Register the primitive as a global definition in the compiler.
      (replace expr
               (generated
                (make-ast-quote loc
                                (make-ast-list loc '())))))
     ((list (symbol 'declare-primitive) (list op args ...) meta ...)
      (let ((name (valid-symbol op "Bad primitive operation name")))
        (replace expr
                 (make-ast-def loc
                               name
                               (generated
                                (make-ast-lambda loc
                                                 (valid-formals (generated
                                                                 (make-ast-list loc args))
                                                                "Bad primitive operation declaration")
                                                 (make-ast-body loc
                                                                (list (make-ast-primop-app loc
                                                                                           '&primitive-metadata
                                                                                           (cons name meta))
                                                                      (make-ast-primop-app loc
                                                                                           (ast-symbol-value name)
                                                                                           args))
                                                                "Bad primitive operation declaration")))))))
     (else
      (raise-compilation-error
       expr
       "Bad primitive operation declaration:")))))

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
