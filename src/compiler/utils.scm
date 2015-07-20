;; Various utilities

(load "compiler/rename.scm")

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
  (make-vector 1 x))

(define (deref ref)
  (vector-ref ref 0))

(define (assign! ref x)
  (vector-set! ref 0 x))

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
  (assign! &gensym-counter (+ 1 (deref &gensym-counter)))
  (string->symbol (string-append (symbol->string (symbol->safe root))
                                 (number->string (deref &gensym-counter)))))

(define (gensym-reset!)
  (assign! &gensym-counter 0))