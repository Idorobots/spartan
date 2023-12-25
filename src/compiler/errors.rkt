#lang racket

(require syntax/srcloc)

(require "utils/utils.rkt")
(require "utils/refs.rkt")
(require "ast/nodes.rkt")

(provide (struct-out compilation-error) compilation-error-location
         raise-compilation-error collect-errors
         compiler-bug get-stacktrace)

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
(define (get-stacktrace marks)
  (map (lambda (s)
         (let ((loc (cdr s)))
           (format "~a(~a,~a): ~a"
                   (or (source-location-source loc) "?")
                   (or (source-location-line loc) "?")
                   (or (source-location-column loc) "?")
                   (car s))))
       (continuation-mark-set->context marks)))

(define (compiler-bug what context)
  (map displayln (get-stacktrace (current-continuation-marks)))
  (error (format "Likely a compiler bug! ~a ~a" what context)))
