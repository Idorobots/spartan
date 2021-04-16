;; File IO

(define (println . args)
  (if (> (length args) 1)
      (print (apply format args))
      (print (car args)))
  (newline))

(define (ansi-wrap a b text)
  (format "\u001b[~am~a\u001b[~am" a text b))

(define (red text)
  (ansi-wrap 31 39 text))

(define (green text)
  (ansi-wrap 32 39 text))

(define (yellow text)
  (ansi-wrap 33 39 text))

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
