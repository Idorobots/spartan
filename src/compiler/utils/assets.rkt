#lang racket

(require (for-syntax "../utils/io.rkt"))

(provide embed-file-contents)

(define-syntax (embed-file-contents stx)
  (syntax-case stx ()
    ((embed-file-contents filename)
     (datum->syntax stx
                    (slurp (string-append (path->string (current-load-relative-directory))
                                          (syntax->datum #'filename)))))))
