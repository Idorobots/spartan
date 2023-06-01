#lang racket

;; Macro-expander tests.

(require "../testing.rkt")
(require "../../src/compiler/ast.rkt")
(require "../../src/compiler/errors.rkt")
(require "../../src/compiler/expander/expander.rkt")

(define se (make-static-environment))

(define (l n) (location n n))

(describe
 "built-in macros"
 (it "let* macro works"
     (assert (expand se
                     (make-ast-list (l 1)
                                    (list (make-ast-symbol (l 2) 'let*)
                                          (make-ast-list (l 3) '())
                                          (make-ast-symbol (l 10) 'c))))
             (make-ast-symbol (l 10) 'c))
     (assert (expand se
                     (make-ast-list (l 1)
                                    (list (make-ast-symbol (l 2) 'let*)
                                          (make-ast-list (l 3)
                                                         (list (make-ast-list (l 4)
                                                                              (list (make-ast-symbol (l 5) 'a)
                                                                                    (make-ast-number (l 6) 23)))))
                                          (make-ast-symbol (l 10) 'c))))
             (make-ast-let (l 1)
                           (list (make-ast-binding (l 4)
                                                   (make-ast-symbol (l 5) 'a)
                                                   (make-ast-number (l 6) 23)))
                           (make-ast-symbol (l 10) 'c)))
     (assert (expand se
                     (make-ast-list (l 1)
                                    (list (make-ast-symbol (l 2) 'let*)
                                          (make-ast-list (l 3)
                                                         (list (make-ast-list (l 4)
                                                                              (list (make-ast-symbol (l 5) 'a)
                                                                                    (make-ast-number (l 6) 23)))
                                                               (make-ast-list (l 7)
                                                                              (list (make-ast-symbol (l 8) 'b)
                                                                                    (make-ast-number (l 9) 5)))))
                                          (make-ast-symbol (l 10) 'c))))
             (make-ast-let (l 1)
                           (list (make-ast-binding (l 4)
                                                   (make-ast-symbol (l 5) 'a)
                                                   (make-ast-number (l 6) 23)))
                           (make-ast-let (l 1)
                                         (list (make-ast-binding (l 7)
                                                                 (make-ast-symbol (l 8) 'b)
                                                                 (make-ast-number (l 9) 5)))
                                         (make-ast-symbol (l 10) 'c))))
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (expand se
                       (make-ast-list (l 1)
                                      (list (make-ast-symbol (l 2) 'let*)
                                            (make-ast-list (l 3) '())))))
             "Bad `let*` syntax, expected a list of bindings and a body to follow:"))

 (it "handle macro works"
     (assert (expand se
                     (make-ast-list (l 1)
                                    (list (make-ast-symbol (l 2) 'handle)
                                          (make-ast-symbol (l 3) 'expr)
                                          (make-ast-symbol (l 4) 'handler))))
             (make-ast-app (l 1)
                           (make-ast-symbol (l 1) 'call/handler)
                           (list (make-ast-symbol (l 4) 'handler)
                                 (make-ast-lambda (l 1)
                                                  '()
                                                  (make-ast-symbol (l 3) 'expr))))))

 (it "shift macro works"
     (assert (expand se
                     (make-ast-list (l 1)
                                    (list (make-ast-symbol (l 2) 'shift)
                                          (make-ast-symbol (l 3) 'kont)
                                          (make-ast-symbol (l 4) 'expr))))
             (make-ast-app (l 1)
                           (make-ast-symbol (l 1) 'call/shift)
                           (list (make-ast-lambda (l 1)
                                                  (list (make-ast-symbol (l 3) 'kont))
                                                  (make-ast-symbol (l 4) 'expr)))))
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (expand se
                       (make-ast-list (l 1)
                                      (list (make-ast-symbol (l 2) 'shift)
                                            (make-ast-symbol (l 3) 'kont)))))
             "Bad `shift` syntax, expected an identifier and a body to follow:"))

 (it "reset macro works"
     (assert (expand se
                     (make-ast-list (l 1)
                                    (list (make-ast-symbol (l 2) 'reset)
                                          (make-ast-symbol (l 3) 'expr))))
             (make-ast-app (l 1)
                           (make-ast-symbol (l 1) 'call/reset)
                           (list (make-ast-lambda (l 1)
                                                  '()
                                                  (make-ast-symbol (l 3) 'expr)))))
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (expand se
                       (make-ast-list (l 1)
                                      (list (make-ast-symbol (l 2) 'reset)))))
             "Bad `reset` syntax, expected exactly one expression to follow:"))

 (it "letcc macro works"
     (assert (expand se
                     (make-ast-list (l 1)
                                    (list (make-ast-symbol (l 2) 'letcc)
                                          (make-ast-symbol (l 3) 'kont)
                                          (make-ast-symbol (l 4) 'expr))))
             (make-ast-app (l 1)
                           (make-ast-symbol (l 1) 'call/current-continuation)
                           (list (make-ast-lambda (l 1)
                                                  (list (make-ast-symbol (l 3) 'kont))
                                                  (make-ast-symbol (l 4) 'expr)))))
     (assert (expand se
                     (make-ast-list (l 1)
                                    (list (make-ast-symbol (l 2) 'letcc)
                                          (make-ast-symbol (l 3) 'kont)
                                          (make-ast-symbol (l 4) 'expr1)
                                          (make-ast-symbol (l 5) 'expr2))))
             (make-ast-app (l 1)
                           (make-ast-symbol (l 1) 'call/current-continuation)
                           (list (make-ast-lambda (l 1)
                                                  (list (make-ast-symbol (l 3) 'kont))
                                                  (generated
                                                   (set-ast-node-context
                                                    (make-ast-do (l 1)
                                                                 (list (make-ast-symbol (l 4) 'expr1)
                                                                       (make-ast-symbol (l 5) 'expr2)))
                                                    "Bad `letcc` body syntax"))))))
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (expand se
                       (make-ast-list (l 1)
                                      (list (make-ast-symbol (l 2) 'letcc)
                                            (make-ast-symbol (l 3) 'kont)))))
             "Bad `letcc` syntax, expected an identifier and a body to follow:")))
