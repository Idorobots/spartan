;; File IO

(define (println . args)
  (if (> (length args) 1)
      (print (apply format args))
      (print (car args)))
  (newline))

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
