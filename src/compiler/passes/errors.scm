;; Error handling within the compiler.

(load "compiler/utils/refs.scm")
(load "compiler/utils/utils.scm")

(load "compiler/errors.scm")

(define (report-errors env)
  (let ((errors (env-get env 'errors)))
    (unless (empty? errors)
        (map (partial report-error env)
             (sort (reverse errors)
                   (lambda (a b)
                     (location<? (compilation-error-location a)
                                 (compilation-error-location b)))))
        (raise-compilation-error (location 0 0)
                                 (format "Compilation aborted due to ~s errors." (length errors))))
    env))

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
