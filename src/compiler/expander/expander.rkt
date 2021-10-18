#lang racket

;; Macro expander implementation.

;; The expander relies on Bawden & Rees syntactic closures and explicit renaming for hygiene and referential transparency.

(require "../utils/utils.rkt")
(require "../ast.rkt")
(require "../errors.rkt")
(require "macros.rkt")

(provide expand make-static-environment)

(define (expand env expr)
  (match-ast expr

   ((string _)
    expr)

   ((number _)
    expr)

   ((ast-quote _)
    expr)

   ((list (symbol 'quote) rest ...)
    expr)

   ;; FIXME This should be an expander producing plain lists.
   ((ast-quasiquote _)
    expr)

   ;; FIXME This should be an expander producing plain lists.
   ((list (symbol 'quasiquote) rest ...)
    expr)

   ;; Keyword macro expansion
   ((symbol key)
    #:when (and (environment-contains? env key)
                (expander? (environment-ref env key)))
    ;; FIXME This should expand the keyword macro instead.
    expr)

   ;; Renamed symbol
   ((symbol key)
    #:when (environment-contains? env key)
    (environment-ref env key))

    ;; Just a free var in the current static env. FIXME This likely should be a compilation error.
   ((symbol _)
    expr)

   ;; Regular macro expansion
   ((list (symbol op) rest ...)
    #:when (and (environment-contains? env op)
                (expander? (environment-ref env op)))
    (expand env
            (expand-expander op env expr)))

   ;; Syntactic closures
   ((ast-syntactic-closure closure-env free expr)
    (expand (filter-environment free
                                env
                                closure-env)
            expr))

   (else
    (traverse-ast expand env expr))))

(define (expand-expander key env expr)
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
  (hasheq 'when (make-builtin-expander when-macro)
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
