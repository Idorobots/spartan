#lang racket

;; Various utils for Rete.

(require "../compiler/utils/utils.rkt")
(require "../compiler/utils/refs.rkt")

(provide (all-defined-out)
         (all-from-out "../compiler/utils/utils.rkt")
         (all-from-out "../compiler/utils/refs.rkt"))

(define (slice array start end)
  (reverse (let loop ((index start)
                      (acc '()))
             (if (= index end)
                 acc
                 (loop (+ 1 index) (cons (array-ref array index) acc))))))

(define (partition array size start mid end)
  (let ((pre (if (< start mid)
                 (slice array start mid)
                 (append (slice array start size)
                         (slice array 0 mid))))
        (past (if (< (+ 1 mid) end)
                  (slice array (+ 1 mid) end)
                  (append (slice array (+ mid 1) size)
                          (slice array 0 end)))))
    (list pre (array-ref array mid) past)))

(define (variable? pattern)
  (and (symbol? pattern) (starts-with? #\? pattern)))

(define (starts-with? character symbol)
  (equal? character (car (string->list (symbol->string symbol)))))

(define (complimentary f)
  (lambda args (not (apply f args))))

(define not-member (complimentary member))
(define not-equal? (complimentary equal?))
(define not-false? (complimentary false?))
(define not-void? (complimentary void?))

(define true? (partial equal? #t))

(define (any? pred lst)
  (if (empty? lst)
      #f
      (or (pred (car lst))
          (any? pred (cdr lst)))))

(define (wrap value max)
  (cond ((< value 0) (wrap (+ max value) max))
        ((>= value max) (wrap (- value max) max))
        ('else value)))

(define (constantly value)
  (lambda ignored
    value))
