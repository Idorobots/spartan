#lang racket

(require "./refs.rkt")

;; File IO

(provide println red green yellow gray spit slurp tmp-file set-color-output)

(define +colors-enabled+ (ref #t))

(define (set-color-output state)
  (assign! +colors-enabled+ state))

(define (println . args)
  (if (> (length args) 1)
      (print (apply format args))
      (print (car args)))
  (newline))

(define (ansi-wrap a b text)
  (if (deref +colors-enabled+)
      (format "\u001b[~am~a\u001b[~am" a text b)
      text))

(define (red text)
  (ansi-wrap 31 39 text))

(define (green text)
  (ansi-wrap 32 39 text))

(define (yellow text)
  (ansi-wrap 33 39 text))

(define (gray text)
  (ansi-wrap 90 39 text))

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

(define (tmp-file)
  (make-temporary-file "~a.rkt"))
