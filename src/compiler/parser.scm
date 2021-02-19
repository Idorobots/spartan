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
   '((Expression <- (/ List Atom String Quote)))
   '((Quote      <- (/ PlainQuote Quasiquote Unquote UnquoteSplicing)))
   `((PlainQuote <- Spacing (: "'") Expression)
     ,(lambda (input result)
        (let-matches (matching spacing-start end) result
                     (let ((start (car matching)))
                       (matches (list 'quote (caddr matching))
                                start
                                end)))))
   `((Quasiquote <- Spacing (: "`") Expression)
     ,(lambda (input result)
        (let-matches (matching spacing-start end) result
                     (let ((start (car matching)))
                       (matches (list 'quasiquote (caddr matching))
                                start
                                end)))))
   `((Unquote    <- Spacing (: ",") Expression)
     ,(lambda (input result)
        (let-matches (matching spacing-start end) result
                     (let ((start (car matching)))
                       (matches (list 'unquote (caddr matching))
                                start
                                end)))))
   `((UnquoteSplicing <- Spacing (: ",@") Expression)
     ,(lambda (input result)
        (let-matches (matching spacing-start end) result
                     (let ((start (car matching)))
                       (matches (list 'unquote-splicing (caddr matching))
                                start
                                end)))))
   `((String     <- Spacing (: "\"") "[^\"]*" (: "\""))
     ,(lambda (input result)
        (trace 'String-transform input result)
        (let-matches (matching spacing-start end) result
                     (let ((start (car matching)))
                       (matches (caddr matching)
                                start
                                end)))))
   `((List       <- Spacing (: "\\(") (* Expression) Spacing (: "\\)"))
     ,(lambda (input result)
        (trace 'List-transform input result)
        (let-matches (matching spacing-start end) result
                     (let ((start (car matching)))
                       (matches (caddr matching)
                                start
                                end)))))
   '((Atom       <- (/ Symbol Number)))
   `((Number     <- Spacing "[+\\-]?[0-9]+(\\.[0-9]*)?")
     ,(lambda (input result)
        (trace 'Number-transform input result)
        (let-matches (matching spacing-start end) result
                     (let ((start (car matching)))
                       (matches (string->number (cadr matching))
                                start
                                end)))))
   `((Symbol     <- Spacing (! Number) "[^\\(\\)\"'`,; \t\v\r\n]+")
     ,(lambda (input result)
        (trace 'Symbol-transform input result)
        (let-matches (matching spacing-start end) result
                     (let ((start (car matching)))
                       (matches (string->symbol (caddr matching))
                                start
                                end)))))
   `((Spacing    <- (: (* (/ "[ \t\v\r\n]+" Comment))))
     ,(lambda (input result)
        (trace 'Spacing-tranform input result)
        (let-matches (matching start end) result
                     ;; NOTE So that we can skip the spacing later.
                     (matches end start end))))
   '((Comment    <- (: ";[^\n]*\n")))))

(define parse (compose car foof))
