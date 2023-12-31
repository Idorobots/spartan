#lang racket

(require syntax/srcloc)

(require "utils/utils.rkt")
(require "utils/refs.rkt")
(require "utils/io.rkt")
(require "ast/nodes.rkt")

(provide (struct-out compilation-error) compilation-error-location
         raise-compilation-error collect-errors
         compiler-bug get-stacktrace
         source-order format-error
         offset->line-and-col normalize-for-display get-line)

;; Syntax error

(struct compilation-error
  (where
   what
   restart)
  #:transparent
  #:constructor-name make-compilation-error)

(define (compilation-error-location e)
  (ast-node-location (compilation-error-where e)))

(define (raise-compilation-error where what)
  (call/cc
   (lambda (cont)
     (raise (make-compilation-error where what cont)))))

;; Error gathering

(define (collect-errors initial-errors thunk)
  (let* ((errors (ref initial-errors))
         (result (with-handlers
                     ((compilation-error?
                       (lambda (error)
                         (push! errors error)
                         ;; NOTE Continue analysis with a special "error" object.
                         ((compilation-error-restart error)
                          (generated
                           (make-ast-error (compilation-error-location error)
                                           (compilation-error-where error)))))))
                   (thunk))))
    (list result (deref errors))))

;; Compilation error formatting

(define (source-order errors)
  (sort (reverse errors)
        (lambda (a b)
          (location<? (compilation-error-location a)
                      (compilation-error-location b)))))

(define +min-margin+ 3)
(define +context+ 2)
(define +ellide-blocks-larger-than+ 5)

(define (format-error module input location what)
  (let* ((lines (count-lines input))
         (margin (inexact->exact (truncate (max +min-margin+ (ceiling (log (max lines 1) 10))))))
         (ellided-size (inexact->exact (truncate (floor (/ (- +ellide-blocks-larger-than+ 1) 2.0)))))
         (start (offset->line-and-col input (location-start location)))
         (start-line (car start))
         (start-col (cadr start))
         (end (offset->line-and-col input (location-end location)))
         (end-line (car end))
         (end-col (cadr end))
         (context-start (max 0 (- start-line +context+)))
         (context-end (min lines (+ end-line +context+))))
    (foldr string-append
           ""
           (cons (format "~a(~a,~a): ~a~n" module (+ 1 start-line) start-col what)
                 (append
                  (map (partial format-line input margin)
                       (line-range context-start start-line))
                  (cond ((= start-line end-line)
                         (list (format-line input margin start-line)
                               (format-underline margin start-col end-col)))
                        ((> (- end-line start-line) +ellide-blocks-larger-than+)
                         (append (format-lines input margin start-col -1 (line-range start-line (+ start-line ellided-size)))
                                 (list (format-ellide margin))
                                 (format-lines input margin -1 end-col (line-range (- end-line ellided-size) (+ end-line 1)))))
                        (else
                         (format-lines input margin start-col end-col (line-range start-line (+ end-line 1)))))
                  (map (partial format-line input margin)
                       (line-range (+ end-line 1) (+ context-end 1))))))))

(define (line-range start end)
  ;; NOTE Not inlusive of the end.
  (iota start (- end 1) 1))

(define (format-lines input margin first-start-col last-end-col lines)
  (map (lambda (line start-col end-col)
         (string-append
          (format-line input margin line)
          (format-underline margin start-col end-col)))
       lines
       (if (< first-start-col 0)
           (map (partial starting-col input) lines)
           (cons first-start-col
                 (cdr (map (partial starting-col input) lines))))
       (if (< last-end-col 0)
           (map (partial ending-col input) lines)
           (reverse
            (cons last-end-col
                  (cdr (reverse (map (partial ending-col input) lines))))))))

(define (starting-col input line)
  (string-length (car (regexp-match #rx"^[\n \t]*" (get-line input line)))))

(define (ending-col input line)
  (- (string-length (get-line input line)) 1))

(define (format-line input margin line)
  (let* ((number (number->string (+ 1 line)))
         (spacing (make-string (- margin (string-length number)) #\space))
         (content (normalize-for-display (get-line input line))))
    (format "~a~a ~a ~a" spacing number (yellow "|") content)))

(define (format-underline margin col-start col-end)
  (let ((margin (make-string margin #\space))
        (spacing (make-string col-start #\space))
        (underline (red (make-string (- col-end col-start) #\^))))
    (format "~a ~a ~a~a~n" margin (yellow "|") spacing underline)))

(define (format-ellide margin)
  (let ((margin (make-string (- margin 3) #\space)))
    (format "~a... ~a ...~n" margin (yellow "|"))))

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

(define (count-lines input)
  (let loop ((offset 0)
             (i 0))
    (cond ((>= offset (string-length input))
           i)
          ((equal? (string-ref input offset) #\newline)
           (loop (+ 1 offset) (+ 1 i)))
          (else
           (loop (+ 1 offset) i)))))

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
  (let* ((terminated (cond ((equal? (string-length line) 0)
                            "\n")
                           ((equal? (string-ref line
                                                (- (string-length line) 1))
                                    #\newline)
                            line)
                           (else
                            (string-append line "\n"))))
         ;; FIXME Effectively tab == 1 space. She'll be right...
         (no-tabs (regexp-replace #rx"\t" terminated " ")))
    no-tabs))

;; Internal compiler errors

(define (get-stacktrace marks)
  (map (lambda (s)
         (let ((loc (cdr s)))
           (format "~a(~a,~a): ~a"
                   (or (source-location-source loc) "?")
                   (or (source-location-line loc) "?")
                   (or (source-location-column loc) "?")
                   (car s))))
       (continuation-mark-set->context marks)))

(define (compiler-bug what context)
  (map displayln (get-stacktrace (current-continuation-marks)))
  (error (format "Likely a compiler bug! ~a ~a" what context)))
