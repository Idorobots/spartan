#lang racket

;; Pattern matching related stuff.

(require "utils.rkt")

(provide unify merge)

;; FIXME Needs better unification.
(define (unify pattern value)
  (cond ((variable? pattern) (list (cons pattern value)))
        ((and (pair? pattern) (pair? value))
         (let ((first (unify (car pattern) (car value)))
               (rest (unify (cdr pattern) (cdr value))))
           (cond ((null? first) null)
                 ((null? rest) null)
                 ((true? first) rest)
                 ((true? rest) first)
                 ('else (append first rest)))))
        ((equal? pattern value) #t) ;; NOTE Indicates that value matches pattern but doesn't bind anything.
        ('else null)))

(define (binding<? a b)
  (string<? (symbol->string (car a))
            (symbol->string (car b))))

(define (merge as bs)
  ;; NOTE O(max(len(as), len(bs)))
  (cond ((null? as) bs)
        ((null? bs) as)
        ('else (let loop ((as (sort as binding<?))
                          (bs (sort bs binding<?))
                          (acc null)
                          (intersect? #f))
                 (cond ((or (null? as) (null? bs)) (if intersect?
                                                       (append acc as bs)
                                                       #f))
                       ((binding<? (car as) (car bs)) (loop (cdr as)
                                                            bs
                                                            (cons (car as) acc)
                                                            intersect?))
                       ((binding<? (car bs) (car as)) (loop as
                                                            (cdr bs)
                                                            (cons (car bs) acc)
                                                            intersect?))
                       ((equal? (car as) (car bs)) (loop (cdr as)
                                                         (cdr bs)
                                                         (cons (car as) acc)
                                                         #t))
                       ('else #f))))))
