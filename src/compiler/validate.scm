;; Parse tree linting.

(load "compiler/utils.scm")
(load "compiler/tree-ast.scm")

(define (validate expr input)
  (with-handlers
      ((syntax-error?
        (lambda (error)
          (let ((location (syntax-error-location error))
                (what (syntax-error-what error))
                (restart (syntax-error-restart error)))
            (display (format-error input location what))
            (newline)
            ;; NOTE Continue analysis with a special "error" object.
            (restart (at location
                         (generated
                          (make-error-node))))))))
    (map-ast id
             (lambda (expr)
               (case (ast-get expr 'type 'undefined)
                 ('unterminated-string (raise-syntax-error expr "Unterminated string literal - expected a closing `\"` to follow:"))
                 ('unterminated-list (raise-syntax-error expr "Unterminated list - expected a closing `)` to follow:"))
                 ('unterminated-quote (raise-syntax-error expr
                                                          (format "No expression following `~a`:"
                                                                  (ast-get expr 'value compiler-bug))))
                 (else expr)))
             expr)))

(define (compiler-bug)
  (error "Likely a compiler bug!"))

(define (format-error input location what)
  (let* ((position (offset->line-and-col input (ast-get location 'start compiler-bug)))
         (line (+ 1 (car position)))
         (line-number (number->string line))
         (line-content (get-line input line))
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
             (i 1))
    (cond ((equal? i line)
           (car (regexp-match #rx"^[^\n]*\n" input offset)))
          ((equal? (string-ref input offset) #\newline)
           (loop (+ 1 offset) (+ 1 i)))
          (else
           (loop (+ 1 offset) i)))))

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
