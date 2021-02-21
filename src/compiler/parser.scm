;; A very simple parser.

(load "compiler/peg.scm")

(define (slurp file-name)
  (with-input-from-file file-name
    (lambda ()
      (list->string
       (reverse (let loop ((char (read-char))
                           (result '()))
                  (if (eof-object? char)
                      result
                      (loop (read-char) (cons char result)))))))))

(define foof
  (grammar
   '(Expression
     (/ List Atom String Quote))
   '(Quote
     (/ PlainQuote Quasiquote UnquoteSplicing Unquote))
   `(PlainQuote
     (Spacing "'" Expression)
     ,(lambda (input result)
        (map-match (lambda (matching spacing-start end)
                     (let ((start (car matching)))
                       (matches (list 'quote (caddr matching))
                                start
                                end)))
                   result)))
   `(Quasiquote
     (Spacing "`" Expression)
     ,(lambda (input result)
        (map-match (lambda (matching spacing-start end)
                     (let ((start (car matching)))
                       (matches (list 'quasiquote (caddr matching))
                                start
                                end)))
                   result)))
   `(Unquote
     (Spacing "," Expression)
     ,(lambda (input result)
        (map-match (lambda (matching spacing-start end)
                     (let ((start (car matching)))
                       (matches (list 'unquote (caddr matching))
                                start
                                end)))
                   result)))
   `(UnquoteSplicing
     (Spacing ",@" Expression)
     ,(lambda (input result)
        (map-match (lambda (matching spacing-start end)
                     (let ((start (car matching)))
                       (matches (list 'unquote-splicing (caddr matching))
                                start
                                end)))
                   result)))
   `(String
     (Spacing (& "\"") "\"[^\"]*\"")
     ,(lambda (input result)
        (map-match (lambda (matching spacing-start end)
                     (let ((start (car matching))
                           (content (caddr matching)))
                       (matches (substring content 1 (- (string-length content) 1))
                                start
                                end)))
                   result)))
   `(List
     (Spacing "(" (* Expression) Spacing ")")
     ,(lambda (input result)
        (map-match (lambda (matching spacing-start end)
                     (let ((start (car matching)))
                       (matches (caddr matching)
                                start
                                end)))
                   result)))
   '(Atom
     (/ Symbol Number))
   `(Number
     (Spacing "[+\\-]?[0-9]+(\\.[0-9]*)?")
     ,(lambda (input result)
        (map-match (lambda (matching spacing-start end)
                     (let ((start (car matching)))
                       (matches (string->number (cadr matching))
                                start
                                end)))
                   result)))
   `(Symbol
     (Spacing (! Number) "[^\\(\\)\"'`,@; \t\v\r\n]+")
     ,(lambda (input result)
        (map-match (lambda (matching spacing-start end)
                     (let ((start (car matching)))
                       (matches (string->symbol (caddr matching))
                                start
                                end)))
                   result)))
   `(Spacing
     (: (* (/ "[ \t\v\r\n]+" Comment)))
     ,(lambda (input result)
        (map-match (lambda (matching start end)
                     ;; NOTE So that we can skip the spacing later.
                     (matches end start end))
                   result)))
   '(Comment
     (: ";[^\n]*\n"))))

(define parse (compose car foof))
