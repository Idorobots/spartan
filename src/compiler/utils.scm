;; Various utilities

(load "rename.scm")

;; Basic definitions making Scheme less-of-a-Scheme:
(define true #t)

(define false #f)

(define nil '())

(define (nil? x)
  (null? x))

(define (empty? lst)
  (nil? lst))

;; Mutable references:
(define (ref x)
  x)

(define (deref x)
  x)

;; Other stuff

(define-syntax do
  (syntax-rules ()
    ((do expression ...)
     (begin expression ...))))

(define (tagged-list? tag lst)
  (and (pair? lst) (eq? (car lst) tag)))

(define (not-nil? x)
  (not (nil? x)))

(define (id x) x)

(define &gensym-counter (ref 0))
(define (gensym root)
  (set! &gensym-counter (+ 1 (deref &gensym-counter)))
  (string->symbol (string-append (symbol->string (symbol->safe root))
                                 (number->string &gensym-counter))))
