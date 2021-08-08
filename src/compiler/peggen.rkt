#lang racket

;; PEG parser generator

(require (for-syntax "utils/peg.rkt"))

(provide match-result no-match matches matches? match-match match-start match-end generate-parser)

;; Matches
(define (no-match)
  '())

(define-struct match-result (result start end) #:transparent)

(define (matches m start end)
  (make-match-result m start end))

(define matches? match-result?)

(define (match-match m)
  (match-result-result m))

(define (match-start m)
  (match-result-start m))

(define (match-end m)
  (match-result-end m))

;; Parser generator
(define-syntax (generate-parser stx)
  (syntax-case stx ()
    ((generate-parser rules ...)
     (datum->syntax stx
                    (generate-grammar
                     (syntax->datum #'(rules ...)))))))
