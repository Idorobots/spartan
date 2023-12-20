#lang racket

;; Various utilities

(provide empty? every? some? sorted? tagged-list? symbol<?
         last concat uniq zip offset iota
         id partial flip constantly
         array array-ref array-assign!
         matrix matrix-ref matrix-assign!
         -> levenshtein-distance)

;; Basic definitions making Scheme less-of-a-Scheme:
(define (empty? lst)
  (null? lst))

(define (every? p lst)
  (foldl (lambda (el acc)
           (and (p el) acc))
         #t
         lst))

(define (some? p lst)
  (foldl (lambda (el acc)
           (or (p el) acc))
         #f
         lst))

(define (sorted? lst pred)
  (or (null? lst)
      (null? (cdr lst))
      (and (pred (car lst)
                 (cadr lst))
           (sorted? (cdr lst) pred))))

(define (uniq lst)
  (let loop ((acc '())
             (lst lst))
    (cond ((empty? lst)
           (reverse acc))
          ((member (car lst) acc)
           (loop acc (cdr lst)))
          (else
           (loop (cons (car lst)
                       acc)
                 (cdr lst))))))

(define (zip a b)
  (if (or (empty? a)
          (empty? b))
      '()
      (cons (cons (car a)
                  (car b))
            (zip (cdr a)
                 (cdr b)))))

(define (last lst)
  (list-ref lst (- (length lst) 1)))

(define (concat a b)
  (if (empty? a)
      b
      (cons (car a) (concat (cdr a) b))))

;; Mutable arrays:
(define (array n x)
  (make-vector n x))

(define (array-ref array index)
  (vector-ref array index))

(define (array-assign! array index value)
  (vector-set! array index value))

(define (matrix rows cols value)
  (build-vector rows (lambda (_) (make-vector cols value))))

(define (matrix-ref matrix row col)
  (vector-ref (vector-ref matrix row) col))

(define (matrix-assign! matrix row col value)
  (vector-set! (vector-ref matrix row) col value))

;; Other stuff
(define-syntax ->
  (syntax-rules ()
    ((-> last)
     last)
    ((-> expr (op args ...) rest ...)
     (-> (op expr args ...) rest ...))
    ((-> expr op rest ...)
     (-> (op expr) rest ...))))

(define (tagged-list? tag lst)
  (and (pair? lst) (eq? (car lst) tag)))

(define (id x) x)

(define (partial f . args)
  (lambda rest
    (apply f (append args rest))))

(define (flip f x)
  (lambda (y)
    (f y x)))

(define (constantly v)
  (lambda _
    v))

(define (offset needle haystack)
  (- (length haystack)
     (length (member needle haystack))))

(define (symbol<? a b)
  (string<? (symbol->string a)
            (symbol->string b)))

(define (iota from to step)
  (if (> from to)
      '()
      (cons from (iota (+ from step) to step))))

(define (levenshtein-distance str1 str2)
  (let* ((len1 (string-length str1))
         (len2 (string-length str2))
         (distances (matrix (+ 1 len1) (+ 1 len2) 0)))
    ;; Initializing the matrix
    (for ((i (iota 0 len1 1)))
      (matrix-assign! distances i 0 i))
    (for ((j (iota 0 len2 1)))
      (matrix-assign! distances 0 j j))
    ;; Filling the matrix using dynamic programming
    (for* ((i (iota 1 len1 1))
           (j (iota 1 len2 1)))
      (let ((cost (if (char=? (string-ref str1 (- i 1))
                              (string-ref str2 (- j 1)))
                      0
                      1)))
        (matrix-assign! distances i j
                        (min (+ cost (matrix-ref distances (- i 1) (- j 1)))
                             (+ 1 (matrix-ref distances (- i 1) j))
                             (+ 1 (matrix-ref distances i (- j 1)))))))
    ;; Return the Levenshtein distance
    (matrix-ref distances len1 len2)))
