#lang racket

;; Set of symbols

(require "utils.rkt")

(provide set
         set? set-empty? set-member?
         set-difference set-merge set-union set-sum set-intersection set-insert
         set->list)

;; NOTE This is actually performing better than the built-in set implementation.

(define (set? s)
  (and (list? s)
       (sorted? s symbol<?)))

(define (set . args)
  (sort args symbol<?))

(define (set-empty? set)
  (null? set))

(define (set-difference as bs)
  (filter (lambda (a)
            (not (set-member? bs a)))
          as))

(define (set-merge as bs)
  (cond ((empty? as)
         bs)
        ((empty? bs)
         as)
        ((symbol<? (car as)
                   (car bs))
         (cons (car as)
               (set-merge (cdr as)
                          bs)))
        (else
         (cons (car bs)
               (set-merge as
                          (cdr bs))))))

(define (set-union as bs)
  (set-merge as (set-difference bs as)))

(define (set-sum sets)
  (foldl set-union (set) sets))

(define (set-intersection as bs)
  (filter (partial set-member? as) bs))

(define (set-member? set value)
  (and (member value set) #t))

(define (set-insert s value)
  (set-union s (set value)))

(define (set->list s)
  s)
