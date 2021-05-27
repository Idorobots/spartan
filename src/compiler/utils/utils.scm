;; Various utilities

;; Basic definitions making Scheme less-of-a-Scheme:
(define true #t)

(define false #f)

(define nil '())

(define (nil? x)
  (null? x))

(define (empty? lst)
  (nil? lst))

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

;; Other stuff
(define-syntax ->
  (syntax-rules ()
    ((-> last)
     last)
    ((-> expr (op args ...) rest ...)
     (-> (op expr args ...) rest ...))
    ((-> expr op rest ...)
     (-> (op expr) rest ...))))

(define-syntax do
  (syntax-rules ()
    ((do expression ...)
     (begin expression ...))))

(define (tagged-list? tag lst)
  (and (pair? lst) (eq? (car lst) tag)))

(define (not-nil? x)
  (not (nil? x)))

(define (id x) x)

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
