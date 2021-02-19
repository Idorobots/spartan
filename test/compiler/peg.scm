;; PEG parser generator tests.

;; Grammar.

(define (ast . properties)
  properties)

(define simple-lisp
  (grammar
   '((Expression <- (/ List Atom String Quote)))
   `((Quote      <- Spacing (: "'") Expression)
     ,(lambda (input result)
        (trace 'Quote-transform input result)
        (let-matches (matching start end) result
                     (matches (ast ':type 'quote
                                   ':value (caddr matching)
                                   ':original (substring input start end)
                                   ':start start
                                   ':end end)
                              start
                              end))))
   `((String     <- Spacing (: "\"") "[^\"]*" (: "\""))
     ,(lambda (input result)
        (trace 'String-transform input result)
        (let-matches (matching start end) result
                     (matches (ast ':type 'string
                                   ':value (caddr matching)
                                   ':original (substring input start end)
                                   ':start start
                                   ':end end)
                              start
                              end))))
   `((List       <- Spacing (: "\\(") (* Expression) (: "\\)"))
     ,(lambda (input result)
        (trace 'List-transform input result)
        (let-matches (matching start end) result
                     (matches (ast ':type 'list
                                   ':value (caddr matching)
                                   ':original (substring input start end)
                                   ':start start
                                   ':end end)
                              start
                              end))))
   '((Atom       <- (/ Symbol Number)))
   `((Number     <- Spacing "[+\\-]?[0-9]+(\\.[0-9]*)?")
     ,(lambda (input result)
        (trace 'Number-transform input result)
        (let-matches (matching start end) result
                     (matches (ast ':type 'number
                                   ':value (string->number (cadr matching))
                                   ':original (substring input start end)
                                   ':start start
                                   ':end end)
                              start
                              end))))
   `((Symbol     <- Spacing (! Number) "[^\\(\\)\"'`,; \t\v\r\n]+")
     ,(lambda (input result)
        (trace 'Symbol-transform input result)
        (let-matches (matching start end) result
                     (matches (ast ':type 'symbol
                                   ':value (string->symbol (caddr matching))
                                   ':original (substring input start end)
                                   ':start start
                                   ':end end)
                              start
                              end))))
   '((Spacing    <- (: (* (/ "[ \t\v\r\n]+" Comment)))))
   '((Comment    <- (: ";[^\n]*\n")))))

(assert (simple-lisp
         (with-output-to-string
           (lambda ()
             (pretty-print '(+ 1 2 3)))))
        '((:type quote
                 :value (:type list
                               :value ((:type symbol :value + :original "+" :start 2 :end 3)
                                       (:type number :value 1 :original " 1" :start 3 :end 5)
                                       (:type number :value 2 :original " 2" :start 5 :end 7)
                                       (:type number :value 3 :original " 3" :start 7 :end 9))
                               :original "(+ 1 2 3)"
                               :start 1
                               :end 10)
                 :original "'(+ 1 2 3)"
                 :start 0
                 :end 10)
          0
          10))

(assert (simple-lisp
         (with-output-to-string
           (lambda ()
             (pretty-print '(define (hello world)
                              (display "hello ")
                              (display wordl)
                              (newline))))))
        '((:type quote
                 :value (:type list
                               :value
                               ((:type symbol :value define :original "define" :start 2 :end 8)
                                (:type list
                                       :value ((:type symbol :value hello :original "hello" :start 10 :end 15)
                                               (:type symbol :value world :original " world" :start 15 :end 21))
                                       :original " (hello world)"
                                       :start 8
                                       :end 22)
                                (:type list
                                       :value ((:type symbol :value display :original "display" :start 24 :end 31)
                                               (:type string :value "hello " :original " \"hello \"" :start 31 :end 40))
                                       :original " (display \"hello \")"
                                       :start 22
                                       :end 41)
                                (:type list
                                       :value ((:type symbol :value display :original "display" :start 43 :end 50)
                                               (:type symbol :value wordl :original " wordl" :start 50 :end 56))
                                       :original " (display wordl)"
                                       :start 41
                                       :end 57)
                                (:type list
                                       :value ((:type symbol :value newline :original "newline" :start 59 :end 66))
                                       :original " (newline)"
                                       :start 57
                                       :end 67))
                               :original "(define (hello world) (display \"hello \") (display wordl) (newline))"
                               :start 1
                               :end 68)
                 :original "'(define (hello world) (display \"hello \") (display wordl) (newline))"
                 :start 0
                 :end 68)
          0
          68))


;; Some benchmarks

(time-execution
 (simple-lisp
  (let ((expr (slurp "../test/foof/coroutines2.foo")))
    (format "(begin ~a)"
            (foldl string-append
                   ""
                   (make-list 30 expr))))))

