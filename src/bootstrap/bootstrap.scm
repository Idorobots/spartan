;; Scheme bootstrap code
;; TODO Move some of this code to runtime, stdlib and compiler.

;; Basic definitions making Scheme not-Scheme:
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

(use-modules (ice-9 pretty-print))
(define old-pp pretty-print)
(define (pretty-print expr)
  (old-pp expr))

;; Other stuff

(define-syntax do
  (syntax-rules ()
    ((do expression ...)
     (begin expression ...))))
