;; Scheme bootstrap code

(define true #t)

(define false #f)

(define nil '())

(define (nil? x)
  (null? x))

(define (id x) x)

(define (empty? lst)
  (null? lst))

(define &gensym-counter 0)
(define (gensym root)
  (set! &gensym-counter (+ 1 &gensym-counter))
  (string->symbol (string-append (symbol->string (symbol->llvm root))
                                 (number->string &gensym-counter))))

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


(define-syntax do
  (syntax-rules ()
    ((do expression ...)
     (begin expression ...))))
