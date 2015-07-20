;; Various utilities

(load "rename.scm")

;; Basic definitions making Scheme less-of-a-Scheme:
(define true #t)

(define false #f)

(define nil '())

(define (nil? x)
  (null? x))

;; Mutable references:
(define (ref x)
  x)

(define (deref x)
  x)

;; IO:

(define (slurp file-name)
  (with-input-from-file file-name
    (lambda ()
      (list->string
       (reverse (let loop ((char (read-char))
                           (result '()))
                  (if (eof-object? char)
                      result
                      (loop (read-char) (cons char result)))))))))

(define (parse file)
  (with-input-from-string (string-append "(do" file ")")
    (lambda () (read))))

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
