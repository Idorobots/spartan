;; PEG parser generator tests.

(define (ast . properties)
  properties)

(define simple-lisp
  (grammar
   '((Expression < (/ List Atom String Quote)))
   `((Quote      < (: "'") Expression)
     ,(lambda (input result)
        (trace 'Quote-transform input result)
        (let-matches (matching start end) result
                     (matches (ast ':type 'quote
                                   ':value (cadr matching)
                                   ':original (substring input start end)
                                   ':start start
                                   ':end end)
                              start
                              end))))
   '((String     < (: "\"") "[^\"]*" (: "\"")))
   `((List       < (: "\\(") (* Expression) (: "\\)"))
     ,(lambda (input result)
        (trace 'List-transform input result)
        (let-matches (matching start end) result
                     (matches (ast ':type 'list
                                   ':value (cadr matching)
                                   ':original (substring input start end)
                                   ':start start
                                   ':end end)
                              start
                              end))))
   '((Atom       < (/ Symbol Number)))
   `((Number     < "[+\\-]?[0-9]+(\\.[0-9]*)?")
     ,(lambda (input result)
        (trace 'Number-transform input result)
        (let-matches (matching start end) result
                     (matches (ast ':type 'number
                                   ':value (string->number matching)
                                   ':original (substring input start end)
                                   ':start start
                                   ':end end)
                              start
                              end))))
   `((Symbol     < (! Number) "[^\\(\\)\"'`,; \t\v\r\n]+")
     ,(lambda (input result)
        (trace 'Symbol-transform input result)
        (let-matches (matching start end) result
                     (matches (ast ':type 'symbol
                                   ':value (string->symbol (cadr matching))
                                   ':original (substring input start end)
                                   ':start start
                                   ':end end)
                              start
                              end))))
   '((Spacing    <- (: (* (/ "[ \t\v\r\n]+" Comment)))))
   '((Comment    <- (: ";[^\n]*\n")))))

;; Some benchmark.

(time
 (simple-lisp
  (slurp "../test/foof/coroutines2.foo")))
