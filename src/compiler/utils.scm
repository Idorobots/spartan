;; Various utilities

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

;; Mutable arrays:
(define (array n x)
  (make-vector n x))

(define (array-ref array index)
  (vector-ref array index))

(define (array-assign! array index value)
  (vector-set! array index value))

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

(define *gensym-counter* (ref 0))
(define (gensym root)
  (assign! *gensym-counter* (+ 1 (deref *gensym-counter*)))
  (string->symbol (string-append (symbol->string root)
                                 (number->string (deref *gensym-counter*)))))

(define (gensym-reset!)
  (assign! *gensym-counter* 0))

(define (flip f x)
  (lambda (y)
    (f y x)))

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

;; File IO
(define (spit filename content)
  (with-output-to-file filename
    (lambda ()
      (write content))
      #:exists 'replace))

(define (slurp file-name)
  (with-input-from-file file-name
    (lambda ()
      (list->string
       (reverse (let loop ((char (read-char))
                           (result '()))
                  (if (eof-object? char)
                      result
                      (loop (read-char) (cons char result)))))))))

;; Internal errors

(define (compiler-bug)
  (error "Likely a compiler bug!"))
