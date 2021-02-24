;; PEG parser source generator tests

(define stripper
  (lambda (r)
    `(strip ,r)))

(describe
 "PEG generator"
 (it "Nonterminal forwards to the correct rule"
     (assert (generate-nonterminal 'Foo 'h 'in 'off id)
             '(Foo h in off))
     (assert (generate-nonterminal 'Foo 'h 'in 'off stripper)
             '(strip (Foo h in off))))

 (it "\"terminal\" handles single character patterns efficiently"
     (gensym-reset!)
     (assert (generate-matcher "f" 'h 'in 'off id)
             '(if (equal? (string-ref in off) #\f)
                  (matches "f" off (+ 1 off))
                  (no-match)))
     (gensym-reset!)
     (assert (generate-matcher "f" 'h 'in 'off stripper)
             '(strip
               (if (equal? (string-ref in off) #\f)
                   (matches "f" off (+ 1 off))
                   (no-match)))))
 (it "\"terminal\"uses compiled regexps for multi-char patterns"
     (gensym-reset!)
     (assert (generate-matcher "foo" 'h 'in 'off id)
             '(let ((result1 (regexp-match #rx"^foo" in off)))
                (if result1
                    (matches (car result1) off (+ off (string-length (car result1))))
                    (no-match))))
     (gensym-reset!)
     (assert (generate-matcher "foo" 'h 'in 'off stripper)
             '(strip
               (let ((result1 (regexp-match #rx"^foo" in off)))
                 (if result1
                     (matches (car result1) off (+ off (string-length (car result1))))
                     (no-match))))))
 (it "\"terminal\"handles synatx characters fine"
     (gensym-reset!)
     (assert (generate-matcher "'" 'h 'in 'off id)
             '(if (equal? (string-ref in off) #\')
                  (matches "'" off (+ 1 off))
                  (no-match))))

 (it "(...) sequences matchers"
     (gensym-reset!)
     (assert (generate-sequence '(Foo Bar Baz) 'h 'in 'off id)
             '(let ((result1 (Foo h in off)))
                (if (matches? result1)
                    (let ((match2 (match-match result1))
                          (end3 (match-end result1)))
                      (let ((result4 (Bar h in end3)))
                        (if (matches? result4)
                            (let ((match5 (match-match result4))
                                  (end6 (match-end result4)))
                              (let ((result7 (Baz h in end6)))
                                (if (matches? result7)
                                    (let ((match8 (match-match result7))
                                          (end9 (match-end result7)))
                                      (matches (list match2 match5 match8) off end9))
                                    (no-match))))
                            (no-match))))
                    (no-match))))
     (gensym-reset!)
     (assert (generate-sequence '(Foo Bar) 'h 'in 'off stripper)
             '(strip
               (let ((result1 (Foo h in off)))
                 (if (matches? result1)
                     (let ((match2 (match-match result1))
                           (end3 (match-end result1)))
                       (let ((result4 (Bar h in end3)))
                         (if (matches? result4)
                             (let ((match5 (match-match result4))
                                   (end6 (match-end result4)))
                               (matches (list match2 match5) off end6))
                             (no-match))))
                     (no-match))))))

 (it "(/ ...) returns the first match"
     (gensym-reset!)
     (assert (generate-or '(/ Foo Bar Baz) 'h 'in 'off id)
             '(let ((result3 (Foo h in off)))
                (if (matches? result3)
                    result3
                    (let ((result2 (Bar h in off)))
                      (if (matches? result2)
                          result2
                          (let ((result1 (Baz h in off)))
                            (if (matches? result1) result1 (no-match))))))))
     (gensym-reset!)
     (assert (generate-or '(/ Foo Bar) 'h 'in 'off stripper)
             '(strip
               (let ((result2 (Foo h in off)))
                 (if (matches? result2)
                     result2
                     (let ((result1 (Bar h in off)))
                       (if (matches? result1) result1 (no-match))))))))

 (it "(* ...) matches multiple values"
     (gensym-reset!)
     (assert (generate-zero-or-more '(* Foo) 'h 'in 'off id)
             '(let loop4 ((matches3 '())
                          (end2 off))
                (let ((result1 (Foo h in end2)))
                  (if (matches? result1)
                      (loop4 (cons (match-match result1)
                                   matches3)
                             (match-end result1))
                      (matches (reverse matches3) off end2)))))
     (gensym-reset!)
     (assert (generate-zero-or-more '(* Foo) 'h 'in 'off stripper)
             '(strip
               (let loop4 ((matches3 '())
                           (end2 off))
                 (let ((result1 (Foo h in end2)))
                   (if (matches? result1)
                       (loop4 (cons (match-match result1)
                                    matches3)
                              (match-end result1))
                       (matches (reverse matches3) off end2)))))))

 (it "(+ ...) matches at least one value"
     (gensym-reset!)
     (assert (generate-one-or-more '(+ Foo) 'h 'in 'off id)
             '(if (matches? (Foo h in off))
                  (let loop4 ((matches3 '())
                              (end2 off))
                    (let ((result1 (Foo h in end2)))
                      (if (matches? result1)
                          (loop4 (cons (match-match result1)
                                       matches3)
                                 (match-end result1))
                          (matches (reverse matches3) off end2))))
                  (no-match)))
     (gensym-reset!)
     (assert (generate-one-or-more '(+ Foo) 'h 'in 'off stripper)
             '(strip
               (if (matches? (Foo h in off))
                   (let loop4 ((matches3 '())
                               (end2 off))
                     (let ((result1 (Foo h in end2)))
                       (if (matches? result1)
                           (loop4 (cons (match-match result1)
                                        matches3)
                                  (match-end result1))
                           (matches (reverse matches3) off end2))))
                   (no-match)))))

 (it "(? ...) always matches"
     (gensym-reset!)
     (assert (generate-optional '(? Foo) 'h 'in 'off id)
             '(let ((result1 (Foo h in off)))
                (if (matches? result1)
                    result1
                    (matches '() off off))))
     (gensym-reset!)
     (assert (generate-optional '(? Foo) 'h 'in 'off stripper)
             '(strip
               (let ((result1 (Foo h in off)))
                 (if (matches? result1)
                     result1
                     (matches '() off off))))))

 (it "(! ...) fails if a match is found"
     (gensym-reset!)
     (assert (generate-not '(! Foo) 'h 'in 'off id)
             '(if (matches? (Foo h in off))
                  (no-match)
                  (matches '() off off)))
     (gensym-reset!)
     (assert (generate-not '(! Foo) 'h 'in 'off stripper)
             '(strip
               (if (matches? (Foo h in off))
                   (no-match)
                   (matches '() off off)))))

 (it "(& ...) doesn't advance the scan"
     (gensym-reset!)
     (assert (generate-and '(& Foo) 'h 'in 'off id)
             '(let ((result1 (Foo h in off)))
                (if (matches? result1)
                    (matches (match-match result1) off off)
                    (no-match))))
     (gensym-reset!)
     (assert (generate-and '(& Foo) 'h 'in 'off stripper)
             '(strip
               (let ((result1 (Foo h in off)))
                 (if (matches? result1)
                     (matches (match-match result1) off off)
                     (no-match))))))

 (it "(: ...) drops match from the input, but advances the scan"
     (gensym-reset!)
     (assert (generate-drop '(: Foo) 'h 'in 'off id)
             '(let ((result1 (Foo h in off)))
                (if (matches? result1)
                    (let ((end2 (match-end result1)))
                      (matches '() end2 end2))
                    (no-match))))
     (gensym-reset!)
     (assert (generate-drop '(: Foo) 'h 'in 'off stripper)
             '(strip
               (let ((result1 (Foo h in off)))
                 (if (matches? result1)
                     (let ((end2 (match-end result1)))
                       (matches '() end2 end2))
                     (no-match))))))

 (it "(~ ...) concatenates all submatches"
     (gensym-reset!)
     (assert (generate-concat '(~ Foo) 'h 'in 'off id)
             '(let ((result1 (Foo h in off)))
                (if (matches? result1)
                    (matches
                     (foldl string-append-immutable "" (match-match result1))
                     (match-start result1)
                     (match-end result1))
                    (no-match))))
     (gensym-reset!)
     (assert (generate-concat '(~ Foo) 'h 'in 'off stripper)
             '(strip
               (let ((result1 (Foo h in off)))
                 (if (matches? result1)
                     (matches
                      (foldl string-append-immutable "" (match-match result1))
                      (match-start result1)
                      (match-end result1))
                     (no-match)))))))

(define (ast . properties)
  properties)

(define (map-match f m)
  (if (matches? m)
      (f (match-match m)
         (match-start m)
         (match-end m))
      m))

(generate-parser
 '(SimpleLisp
   (/ List Atom String Quote))
 '(Quote
   (Spacing (: "'") SimpleLisp)
   (lambda (input result)
     (map-match (lambda (matching spacing-start end)
                  (let ((start (car matching)))
                    (matches (ast ':type 'quote
                                  ':value (caddr matching)
                                  ':start start
                                  ':end end)
                             start
                             end)))
                result)))
 '(String
   (Spacing (: "\"") "[^\"]*" (: "\""))
   (lambda (input result)
     (map-match (lambda (matching spacing-start end)
                  (let ((start (car matching)))
                    (matches (ast ':type 'string
                                  ':value (caddr matching)
                                  ':original (substring input start end)
                                  ':start start
                                  ':end end)
                             start
                             end)))
                result)))
 '(List
   (Spacing (: "\\(") (* SimpleLisp) Spacing (: "\\)"))
   (lambda (input result)
     (map-match (lambda (matching spacing-start end)
                  (let ((start (car matching)))
                    (matches (ast ':type 'list
                                  ':value (caddr matching)
                                  ':start start
                                  ':end end)
                             start
                             end)))
                result)))
 '(Atom
   (/ Symbol Number))
 '(Number
   (Spacing "[+\\-]?[0-9]+(\\.[0-9]*)?")
   (lambda (input result)
     (map-match (lambda (matching spacing-start end)
                  (let ((start (car matching)))
                    (matches (ast ':type 'number
                                  ':value (string->number (cadr matching))
                                  ':original (substring input start end)
                                  ':start start
                                  ':end end)
                             start
                             end)))
                result)))
 '(Symbol
   (Spacing (! Number) "[^\\(\\)\"'`,; \t\v\r\n]+")
   (lambda (input result)
     (map-match (lambda (matching spacing-start end)
                  (let ((start (car matching)))
                    (matches (ast ':type 'symbol
                                  ':value (string->symbol (caddr matching))
                                  ':original (substring input start end)
                                  ':start start
                                  ':end end)
                             start
                             end)))
                result)))
 '(Spacing
   (: (* (/ "[ \t\v\r\n]+" Comment)))
   (lambda (input result)
     (map-match (lambda (matching start end)
                  ;; NOTE So that we can skip the spacing later.
                  (matches end start end))
                result)))
 '(Comment
   (: ";[^\n]*\n")))

(describe
 "Generated grammar"
 (it "parses simple expressions"
     (assert (SimpleLisp
              (with-output-to-string
                (lambda ()
                  (pretty-print '(+ 1 2 3)))))
             (matches '(:type quote
                              :value (:type list
                                            :value ((:type symbol :value + :original "+" :start 2 :end 3)
                                                    (:type number :value 1 :original "1" :start 4 :end 5)
                                                    (:type number :value 2 :original "2" :start 6 :end 7)
                                                    (:type number :value 3 :original "3" :start 8 :end 9))
                                            :start 1
                                            :end 10)
                              :start 0
                              :end 10)
                      0
                      10)))

 (it "parses more complex expressions"
     (assert (SimpleLisp
              (with-output-to-string
                (lambda ()
                  (pretty-print '(define (hello world)
                                   ;; Display hello world!
                                   (display "hello ")
                                   (display wordl)
                                   (newline))))))
             (matches '(:type quote
                              :value (:type list
                                            :value
                                            ((:type symbol :value define :original "define" :start 2 :end 8)
                                             (:type list
                                                    :value ((:type symbol :value hello :original "hello" :start 10 :end 15)
                                                            (:type symbol :value world :original "world" :start 16 :end 21))
                                                    :start 9
                                                    :end 22)
                                             (:type list
                                                    :value ((:type symbol :value display :original "display" :start 24 :end 31)
                                                            (:type string :value "hello " :original "\"hello \"" :start 32 :end 40))
                                                    :start 23
                                                    :end 41)
                                             (:type list
                                                    :value ((:type symbol :value display :original "display" :start 43 :end 50)
                                                            (:type symbol :value wordl :original "wordl" :start 51 :end 56))
                                                    :start 42
                                                    :end 57)
                                             (:type list
                                                    :value ((:type symbol :value newline :original "newline" :start 59 :end 66))
                                                    :start 58
                                                    :end 67))
                                            :start 1
                                            :end 68)
                              :start 0
                              :end 68)
                      0
                      68)))

 (it "handles spacing correctly"
     (assert (SimpleLisp "(foo   )")
             (matches '(:type list
                              :value ((:type symbol :value foo :original "foo" :start 1 :end 4))
                              :start 0
                              :end 8)
                      0
                      8))))
