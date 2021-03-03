;; Error handling within the compiler.

(load "compiler/tree-ast.scm")

(define (format-error input location what)
  (let* ((position (offset->line-and-col input (ast-get location 'start compiler-bug)))
         (line (+ 1 (car position)))
         (line-number (number->string line))
         (line-content (normalize-for-display (get-line input (car position))))
         (column (cadr position))
         (line-number-spacing (make-string (string-length line-number) #\space))
         (column-spacing (make-string column #\space)))
    (format (string-append "~a(~a,~a): ~a~n"
                           "~a |~a"
                           "~a  ~a^~~~~~~~~~~~n")
            (ast-get location 'filename "stdin")
            line
            column
            what
            line-number
            line-content
            line-number-spacing
            column-spacing)))

(define (offset->line-and-col input offset)
  (let loop ((line 0)
             (col 0)
             (i 0))
    (cond ((equal? i offset)
           (list line col))
          ((equal? (string-ref input i) #\newline)
           (loop (+ 1 line) 0 (+ 1 i)))
          (else
           (loop line (+ 1 col) (+ 1 i))))))

(define (get-line input line)
  (let loop ((offset 0)
             (i 0))
    (cond ((>= offset (string-length input))
           "")
          ((equal? i line)
           (car (regexp-match #rx"^[^\n]*\n?" input offset)))
          ((equal? (string-ref input offset) #\newline)
           (loop (+ 1 offset) (+ 1 i)))
          (else
           (loop (+ 1 offset) i)))))

(define (normalize-for-display line)
  (cond ((equal? (string-length line) 0)
         "\n")
        ((equal? (string-ref line
                             (- (string-length line) 1))
                 #\newline)
         line)
        (else
         (string-append line "\n"))))

(define (syntax-error? e)
  (tagged-list? 'syntax-error e))

(define (make-syntax-error location what restart)
  (list 'syntax-error location what restart))

(define (syntax-error-location e)
  (cadr e))

(define (syntax-error-what e)
  (caddr e))

(define (syntax-error-restart e)
  (cadddr e))

(define (raise-syntax-error location what)
  (call/cc
   (lambda (cont)
     (raise (make-syntax-error location what cont)))))
