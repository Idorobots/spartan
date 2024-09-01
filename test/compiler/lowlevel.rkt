#lang racket

;; Low level macros

(require "../testing.rkt")
(require "../../src/compiler/ast.rkt")
(require "../../src/compiler/errors.rkt")
(require "../../src/compiler/expander/expander.rkt")
(require "../../src/compiler/expander/lowlevel.rkt")

(define se (make-static-environment))

(define (l n) (location n n))

(describe
 "low-level expanders"
 (it "asm embeds primop calls"
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (expand se
                       (make-ast-list (l 1)
                                      (list (make-ast-symbol (l 2) 'asm)))))
             "Bad `asm` syntax, expected a list of assembly instructions to follow:")
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (expand se
                       (make-ast-list (l 1)
                                      (list (make-ast-symbol (l 2) 'asm)
                                            (make-ast-symbol (l 3) 'foo)))))
             "Bad `asm` instruction:")
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (expand se
                       (make-ast-list (l 1)
                                      (list (make-ast-symbol (l 2) 'asm)
                                            (make-ast-list (l 3)
                                                           (list (make-ast-symbol (l 4) '&current-continuation)))))))
             "Bad `asm` instruction:")
     (assert (expand se
                     (make-ast-list (l 1)
                                      (list (make-ast-symbol (l 2) 'asm)
                                            (make-ast-list (l 3)
                                                           (list (make-ast-symbol (l 4) 'primop-app)
                                                                 (make-ast-quote (l 5)
                                                                                 (make-ast-symbol (l 6) '&current-continuation)))))))
             (make-ast-primop-app (l 3) '&current-continuation '()))
     (assert (expand se
                     (make-ast-list (l 1)
                                      (list (make-ast-symbol (l 2) 'asm)
                                            (make-ast-list (l 3)
                                                           (list (make-ast-symbol (l 4) 'primop-app)
                                                                 (make-ast-quote (l 5)
                                                                                 (make-ast-symbol (l 6) '&current-continuation))))
                                            (make-ast-list (l 7)
                                                           (list (make-ast-symbol (l 8) 'primop-app)
                                                                 (make-ast-quote (l 9)
                                                                                 (make-ast-symbol (l 10) '&yield-cont)))))))
             (set-ast-node-context
              (generated
               (make-ast-do (l 1)
                           (list (make-ast-primop-app (l 3) '&current-continuation '())
                                 (make-ast-primop-app (l 7) '&yield-cont '()))))
              "Bad `asm` syntax"))))
