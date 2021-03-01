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
                 ('unterminated-string (raise-syntax-error expr "Unterminated string literal"))
                 ('unterminated-list (raise-syntax-error expr "Unterminated list"))
                 ('unterminated-quote (raise-syntax-error expr
                                                          (format "No expression following `~a`"
                                                                  (ast-get expr 'value (lambda ()
                                                                                         (error "There really isn't a default..."))))))
                 (else expr)))
             expr)))

(define (format-error input location what)
  (let* ((position (offset-to-line-and-col input (ast-get location 'start 0)))
         (line (car position))
         (column (cadr position)))
    (format "~a(~a,~a): ~a"
            (ast-get location 'filename "stdin")
            (+ line 1)
            column
            what)))

(define (offset-to-line-and-col input offset)
  (let loop ((line 0)
             (col 0)
             (i 0))
    (cond ((equal? i offset)
           (list line col))
          ((equal? (string-ref input i) #\newline)
           (loop (+ 1 line) 0 (+ 1 i)))
          (else
           (loop line (+ 1 col) (+ 1 i))))))

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
