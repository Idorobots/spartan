#lang racket

(require "utils/utils.rkt")
(require "utils/refs.rkt")
(require "ast/nodes.rkt")

(provide (struct-out compilation-error) compilation-error-location
         raise-compilation-error collect-errors
         compiler-bug show-stacktrace)

;; Syntax error

(struct compilation-error
  (where
   what
   restart)
  #:transparent
  #:constructor-name make-compilation-error)

(define (compilation-error-location e)
  (ast-node-location (compilation-error-where e)))

(define (raise-compilation-error where what)
  (call/cc
   (lambda (cont)
     (raise (make-compilation-error where what cont)))))

;; Error gathering

(define (collect-errors initial-errors thunk)
  (let* ((errors (ref initial-errors))
         (result (with-handlers
                     ((compilation-error?
                       (lambda (error)
                         (push! errors error)
                         ;; NOTE Continue analysis with a special "error" object.
                         ((compilation-error-restart error)
                          (generated
                           (make-ast-error (compilation-error-location error)
                                           (compilation-error-where error)))))))
                   (thunk))))
    (list result (deref errors))))

;; Internal compiler errors

(define (show-stacktrace marks)
  (for ([s (continuation-mark-set->context marks)]
        [i (in-naturals)])
    ;; show just the names, not the full source information
    (when (car s) (printf "~s: ~s\n" i s))))

(define (compiler-bug what context)
  (show-stacktrace (current-continuation-marks))
  (error (format "Likely a compiler bug! ~a ~a" what context)))
