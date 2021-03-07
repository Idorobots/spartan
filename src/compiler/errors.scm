;; Error handling within the compiler.

(define (report-errors env)
  (let ((errors (env-get env 'errors)))
    (unless (empty? errors)
        (map (partial report-error env)
             (sort errors
                   (lambda (a b)
                     (location<? (compilation-error-location a)
                                 (compilation-error-location b)))))
        (error "Compilation aborted due to errors."))
    env))

;; Syntax error

(define (make-compilation-error location what restart)
  (list 'compilation-error location what restart))

(define (compilation-error? e)
  (tagged-list? 'compilation-error e))

(define (compilation-error-location e)
  (cadr e))

(define (compilation-error-what e)
  (caddr e))

(define (compilation-error-restart e)
  (cadddr e))

(define (raise-compilation-error location what)
  (call/cc
   (lambda (cont)
     (raise (make-compilation-error location what cont)))))

;; Error gathering

(define (collect-errors initial-errors thunk)
  (let* ((errors (ref initial-errors))
         (result (with-handlers
                     ((compilation-error?
                       (lambda (error)
                         (push! errors error)
                         ;; NOTE Continue analysis with a special "error" object.
                         ((compilation-error-restart error)
                          (at (compilation-error-location error)
                              (generated
                               (make-error-node)))))))
                   (thunk))))
    (list result (deref errors))))

;; Error reporting

(define (report-error env error)
  (let* ((location (compilation-error-location error))
         (what (compilation-error-what error)))
    (display (format-error (env-get env 'module)
                           (env-get env 'input)
                           location
                           what))
    (newline)))

(define (format-error module input location what)
  (let* ((position (offset->line-and-col input (location-start location)))
         (line (+ 1 (car position)))
         (line-number (number->string line))
         (line-content (normalize-for-display (get-line input (car position))))
         (column (cadr position))
         (line-number-spacing (make-string (string-length line-number) #\space))
         (column-spacing (make-string column #\space)))
    (format (string-append "~a(~a,~a): ~a~n"
                           "~a |~a"
                           "~a  ~a^~~~~~~~~~~~n")
            module
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
