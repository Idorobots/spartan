;; Scheme bootstrap code

(define true #t)

(define false #f)

(define nil '())

(define (id x) x)

(define (empty? lst)
  (null? lst))

(define old-gensym gensym)
(define (gensym root)
  (old-gensym (symbol->string (symbol->llvm root))))

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

;; Required by other modules:
(define &stored-cont (list (lambda (x) x)))
