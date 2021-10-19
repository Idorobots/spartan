#lang racket

;; Macro expander implementation.

;; The expander relies on Bawden & Rees syntactic closures and explicit renaming for hygiene and referential transparency.

(require "../utils/utils.rkt")
(require "../ast.rkt")
(require "../errors.rkt")
(require "macros.rkt")
(require "elaborators.rkt")

(provide expand make-static-environment)

(define (expand env expr)
  (match-ast expr

   ((string _)
    expr)

   ((number _)
    expr)

   ((ast-quote _)
    expr)

   ((ast-quasiquote _)
    (expand-quasiquote env expr))

   ;; Identifier macro expansion
   ((symbol key)
    #:when (and (environment-contains? env key)
                (expander? (environment-ref env key)))
    (expand env
            (apply-expander key env expr)))

   ;; Renamed symbol
   ((symbol key)
    #:when (environment-contains? env key)
    (environment-ref env key))

    ;; Just a free var in the current static env. FIXME This likely should be a compilation error.
   ((symbol _)
    expr)

   ;; Macro application
   ((list (symbol op) rest ...)
    #:when (and (environment-contains? env op)
                (expander? (environment-ref env op)))
    (expand env
            (apply-expander op env expr)))

   ;; Regular application
   ((list values ...)
    (expand env
            (apply-expander '&application
                             env
                             expr)))

   ;; Syntactic closures
   ((ast-syntactic-closure closure-env free expr)
    (expand (filter-environment free
                                env
                                closure-env)
            expr))

   ;; NOTE No need to descend into errors more than once.
   ((ast-error _)
    expr)

   (else
    (traverse-ast expand env expr))))

(define (expand-quasiquote env expr)
  (match-ast expr

   ;; NOTE Not to expand unquotes within hard-quoted expressions.
   ((ast-quote _)
     expr)

   ((ast-unquote _)
    (expand env expr))

   ((list (symbol 'unquote) rest ...)
    (expand env
            (apply-expander 'unquote env expr)))

   ((ast-unquote-splicing _)
    (expand env expr))

   ((list (symbol 'unquote-splicing) rest ...)
    (expand env
            (apply-expander 'unquote-splicing env expr)))

   ;; NOTE So that we don't loop forever in case of a bad splice.
   ((ast-error _)
    expr)

   (else
    (traverse-ast expand-quasiquote env expr))))

(define (apply-expander key env expr)
  (let ((expander (environment-ref env key)))
    ((expander-transformer expander)
     expr
     env
     (expander-environment expander))))

;; Static environment

(struct expander
  (environment
   transformer) ;; NOTE Should take 3 parameters - expr, use-env and a def-env.
  #:transparent
  #:constructor-name make-expander)

(define (make-builtin-expander transformer)
  (make-expander #f transformer))

(define (make-static-environment)
  (hasheq
   ;; Special expanders
   '&application (make-builtin-expander reconstruct-app) ;; FIXME Kinda hacky...

   ;; Syntax forms
   'if (make-builtin-expander reconstruct-if)
   'do (make-builtin-expander reconstruct-do)
   'lambda (make-builtin-expander reconstruct-lambda)
   'let (make-builtin-expander reconstruct-let)
   'letrec (make-builtin-expander reconstruct-letrec)
   'define (make-builtin-expander reconstruct-def)
   'quote (make-builtin-expander reconstruct-quote)
   'quasiquote (make-builtin-expander reconstruct-quasiquote)
   'unquote (make-builtin-expander reconstruct-unquote)
   'unquote-splicing (make-builtin-expander reconstruct-unquote-splicing)

   ;; Built-in macros
   'when (make-builtin-expander when-macro)
   'unless (make-builtin-expander unless-macro)
   'cond (make-builtin-expander cond-macro)
   'and (make-builtin-expander and-macro)
   'or (make-builtin-expander or-macro)
   'let* (make-builtin-expander let*-macro)
   'letcc (make-builtin-expander letcc-macro)
   'handle (make-builtin-expander handle-macro)
   'shift (make-builtin-expander shift-macro)
   'reset (make-builtin-expander reset-macro)
   'structure (make-builtin-expander structure-macro)
   'module (make-builtin-expander module-macro)))

(define (environment-contains? env key)
  (hash-has-key? env key))

(define (environment-ref env key)
  (hash-ref env key))

(define (extend-environment env key value)
  (hash-set env key value))

(define (filter-environment free dynamic-env static-env)
  (foldl (lambda (env free)
           (if (environment-contains? dynamic-env free)
               (extend-environment env
                                   free
                                   (environment-ref dynamic-env free))
               env))
         static-env
         free))
