#lang racket

;; PEG parser source generator tests

(require "../testing.rkt")
(require "../../src/compiler/peggen.rkt")
(require "../../src/compiler/utils/peg.rkt")
(require "../../src/compiler/utils/gensym.rkt")
(require "../../src/compiler/utils/utils.rkt")
(require "../../src/compiler/utils/io.rkt")

;; NOTE These are needed for parser generator tests.
(provide map-match ast)

(define stripper
  (lambda (r)
    `(strip ,r)))

(describe
 "PEG generator"
 (it "EOF matches just the end of file"
     (assert (generate-eof '() 'in 'off id)
             '(if (equal? off (string-length in))
                  (matches '() off off)
                  (no-match)))
     (assert (generate-eof '() 'in 'off stripper)
             '(strip
               (if (equal? off (string-length in))
                   (matches '() off off)
                   (no-match)))))

 (it "Nonterminal forwards to the correct rule"
     (assert (generate-nonterminal 'Foo 'in 'off id)
             '(Foo in off))
     (assert (generate-nonterminal 'Foo 'in 'off stripper)
             '(strip (Foo in off))))

 (it "\"terminal\" handles single character patterns efficiently"
     (gensym-reset!)
     (assert (generate-matcher "f" 'in 'off id)
             '(if (and (< off (string-length in))
                       (equal? (string-ref in off) #\f))
                  (matches "f" off (+ 1 off))
                  (no-match)))
     (gensym-reset!)
     (assert (generate-matcher "f" 'in 'off stripper)
             '(strip
               (if (and (< off (string-length in))
                        (equal? (string-ref in off) #\f))
                   (matches "f" off (+ 1 off))
                   (no-match)))))
 (it "\"terminal\" uses compiled regexps for multi-char patterns"
     (gensym-reset!)
     (assert (generate-matcher "foo" 'in 'off id)
             '(let ((result1 (regexp-match #rx"^foo" in off)))
                (if result1
                    (matches (car result1) off (+ off (string-length (car result1))))
                    (no-match))))
     (gensym-reset!)
     (assert (generate-matcher "foo" 'in 'off stripper)
             '(strip
               (let ((result1 (regexp-match #rx"^foo" in off)))
                 (if result1
                     (matches (car result1) off (+ off (string-length (car result1))))
                     (no-match))))))
 (it "\"terminal\" handles synatx characters fine"
     (gensym-reset!)
     (assert (generate-matcher "'" 'in 'off id)
             '(if (and (< off (string-length in))
                       (equal? (string-ref in off) #\'))
                  (matches "'" off (+ 1 off))
                  (no-match))))

 (it "(...) sequences matchers"
     (gensym-reset!)
     (assert (generate-sequence '(Foo Bar Baz) 'in 'off id)
             '(let ((result1 (Foo in off)))
                (if (matches? result1)
                    (let ((match2 (match-match result1))
                          (end3 (match-end result1)))
                      (let ((result4 (Bar in end3)))
                        (if (matches? result4)
                            (let ((match5 (match-match result4))
                                  (end6 (match-end result4)))
                              (let ((result7 (Baz in end6)))
                                (if (matches? result7)
                                    (let ((match8 (match-match result7))
                                          (end9 (match-end result7)))
                                      (matches (list match2 match5 match8) off end9))
                                    (no-match))))
                            (no-match))))
                    (no-match))))
     (gensym-reset!)
     (assert (generate-sequence '(Foo Bar) 'in 'off stripper)
             '(strip
               (let ((result1 (Foo in off)))
                 (if (matches? result1)
                     (let ((match2 (match-match result1))
                           (end3 (match-end result1)))
                       (let ((result4 (Bar in end3)))
                         (if (matches? result4)
                             (let ((match5 (match-match result4))
                                   (end6 (match-end result4)))
                               (matches (list match2 match5) off end6))
                             (no-match))))
                     (no-match))))))

 (it "(/ ...) returns the first match"
     (gensym-reset!)
     (assert (generate-or '(/ Foo Bar Baz) 'in 'off id)
             '(let ((result3 (Foo in off)))
                (if (matches? result3)
                    result3
                    (let ((result2 (Bar in off)))
                      (if (matches? result2)
                          result2
                          (let ((result1 (Baz in off)))
                            (if (matches? result1) result1 (no-match))))))))
     (gensym-reset!)
     (assert (generate-or '(/ Foo Bar) 'in 'off stripper)
             '(strip
               (let ((result2 (Foo in off)))
                 (if (matches? result2)
                     result2
                     (let ((result1 (Bar in off)))
                       (if (matches? result1) result1 (no-match))))))))

 (it "(* ...) matches multiple values"
     (gensym-reset!)
     (assert (generate-zero-or-more '(* Foo) 'in 'off id)
             '(let loop4 ((matches3 '())
                          (end2 off))
                (let ((result1 (Foo in end2)))
                  (if (matches? result1)
                      (loop4 (cons (match-match result1)
                                   matches3)
                             (match-end result1))
                      (matches (reverse matches3) off end2)))))
     (gensym-reset!)
     (assert (generate-zero-or-more '(* Foo) 'in 'off stripper)
             '(strip
               (let loop4 ((matches3 '())
                           (end2 off))
                 (let ((result1 (Foo in end2)))
                   (if (matches? result1)
                       (loop4 (cons (match-match result1)
                                    matches3)
                              (match-end result1))
                       (matches (reverse matches3) off end2)))))))

 (it "(+ ...) matches at least one value"
     (gensym-reset!)
     (assert (generate-one-or-more '(+ Foo) 'in 'off id)
             '(if (matches? (Foo in off))
                  (let loop4 ((matches3 '())
                              (end2 off))
                    (let ((result1 (Foo in end2)))
                      (if (matches? result1)
                          (loop4 (cons (match-match result1)
                                       matches3)
                                 (match-end result1))
                          (matches (reverse matches3) off end2))))
                  (no-match)))
     (gensym-reset!)
     (assert (generate-one-or-more '(+ Foo) 'in 'off stripper)
             '(strip
               (if (matches? (Foo in off))
                   (let loop4 ((matches3 '())
                               (end2 off))
                     (let ((result1 (Foo in end2)))
                       (if (matches? result1)
                           (loop4 (cons (match-match result1)
                                        matches3)
                                  (match-end result1))
                           (matches (reverse matches3) off end2))))
                   (no-match)))))

 (it "(? ...) always matches"
     (gensym-reset!)
     (assert (generate-optional '(? Foo) 'in 'off id)
             '(let ((result1 (Foo in off)))
                (if (matches? result1)
                    result1
                    (matches '() off off))))
     (gensym-reset!)
     (assert (generate-optional '(? Foo) 'in 'off stripper)
             '(strip
               (let ((result1 (Foo in off)))
                 (if (matches? result1)
                     result1
                     (matches '() off off))))))

 (it "(! ...) fails if a match is found"
     (gensym-reset!)
     (assert (generate-not '(! Foo) 'in 'off id)
             '(if (matches? (Foo in off))
                  (no-match)
                  (matches '() off off)))
     (gensym-reset!)
     (assert (generate-not '(! Foo) 'in 'off stripper)
             '(strip
               (if (matches? (Foo in off))
                   (no-match)
                   (matches '() off off)))))

 (it "(& ...) doesn't advance the scan"
     (gensym-reset!)
     (assert (generate-and '(& Foo) 'in 'off id)
             '(let ((result1 (Foo in off)))
                (if (matches? result1)
                    (matches (match-match result1) off off)
                    (no-match))))
     (gensym-reset!)
     (assert (generate-and '(& Foo) 'in 'off stripper)
             '(strip
               (let ((result1 (Foo in off)))
                 (if (matches? result1)
                     (matches (match-match result1) off off)
                     (no-match))))))

 (it "(: ...) drops match from the input, but advances the scan"
     (gensym-reset!)
     (assert (generate-drop '(: Foo) 'in 'off id)
             '(let ((result1 (Foo in off)))
                (if (matches? result1)
                    (let ((end2 (match-end result1)))
                      (matches '() end2 end2))
                    (no-match))))
     (gensym-reset!)
     (assert (generate-drop '(: Foo) 'in 'off stripper)
             '(strip
               (let ((result1 (Foo in off)))
                 (if (matches? result1)
                     (let ((end2 (match-end result1)))
                       (matches '() end2 end2))
                     (no-match))))))

 (it "(~ ...) concatenates all submatches"
     (gensym-reset!)
     (assert (generate-concat '(~ Foo) 'in 'off id)
             '(let ((result1 (Foo in off)))
                (if (matches? result1)
                    (matches
                     (foldr string-append-immutable "" (match-match result1))
                     (match-start result1)
                     (match-end result1))
                    (no-match))))
     (gensym-reset!)
     (assert (generate-concat '(~ Foo) 'in 'off stripper)
             '(strip
               (let ((result1 (Foo in off)))
                 (if (matches? result1)
                     (matches
                      (foldr string-append-immutable "" (match-match result1))
                      (match-start result1)
                      (match-end result1))
                     (no-match))))))

 (it "inlines non-transforming rules"
     (assert (inline-rules
              '((Foo (/ Bar Baz))
                (Bar Baz)
                (Baz (Foo Bar)
                     (lambda (input result)
                       result))))
             '((Foo (/ Baz Baz))
               (Bar Baz)
               (Baz ((/ Baz Baz) Baz)
                    (lambda (input result)
                      result)))))

 (it "optimizes rules"
     (let ((rules
            '((R1 (/ A A))
              (R2 (/ B B) (lambda (i r) r))
              (R3 (/ A B A))
              (R4 (A))
              (R5 (/ A (/ B C) (/ D E)))
              (R6 (+ (/ A (/ B C))))
              (R7 (/ A (/ B (/ C D))))
              (R8 (~ "foo" "bar" "baz"))))
           (expected
            '((R1 A)
              (R2 B (lambda (i r) r))
              (R3 (/ A B))
              (R4 A)
              (R5 (/ A B C D E))
              (R6 (+ (/ A B C)))
              (R7 (/ A B C D))
              (R8 "foobarbaz"))))
       (map (lambda (rule expected)
              (assert (optimize-rules (list rule))
                      (list expected)))
            rules
            expected))))

(define (ast . properties)
  properties)

(define (map-match f m)
  (if (matches? m)
      (f (match-match m)
         (match-start m)
         (match-end m))
      m))

(generate-parser
 (SimpleLisp
  (/ List Atom String Quote))
 (Quote
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
 (String
  (/ UnterminatedString ProperString))
 (UnterminatedString
  (Spacing (: "\"") "[^\"]*" ())
  (lambda (input result)
    (map-match (lambda (matching spacing-start end)
                 (raise (format "Unterminated string at location: ~a" (car matching))))
               result)))
 (ProperString
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
 (List
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
 (Atom
  (/ Symbol Number))
 (Number
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
 (Symbol
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
 (Spacing
  (: (* (/ "[ \t\v\r\n]+" Comment)))
  (lambda (input result)
    (map-match (lambda (matching start end)
                 ;; NOTE So that we can skip the spacing later.
                 (matches end start end))
               result)))
 (Comment
  (: ";[^\n]*\n")))

(generate-parser
 (Weird
  (/ List Foo))
 (Foo
  (Spacing "foo")
  (lambda (input result)
    (map-match (lambda (matching start end)
                 (matches (ast ':type 'symbol
                               ':value (cadr matching)
                               ':start start
                               ':end end)
                          start
                          end))
               result)))
 (List
  (/ ProperList UnterminatedList))
 (ProperList
  (Spacing "(" ListContents Spacing ")")
  (lambda (input result)
    (map-match (lambda (matching start end)
                 (matches (ast ':type 'list
                               ':value (caddr matching)
                               ':start start
                               ':end end)
                          start
                          end))
               result)))
 (UnterminatedList
  (Spacing "(" ListContents Spacing EOF)
  (lambda (input result)
    (map-match (lambda (matching start end)
                 (matches (ast ':type 'invalid-list
                               ':value (caddr matching)
                               ':start start
                               ':end end)
                          start
                          end))
               result)))
 (ListContents
  (* Weird))
 (Spacing
  (* " "))
 (EOF
  ()))

(generate-parser
 (Concat
  (~ "foo" "bar" "baz")))

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
                      8)))

 (it "handles EOF correctly"
     (assert (Weird "(foo")
             (matches '(:type invalid-list
                              :value ((:type symbol :value "foo" :start 1 :end 4))
                              :start 0
                              :end 4)
                      0
                      4))
     (assert (Weird "(foo (foo foo)")
             (matches '(:type invalid-list
                              :value ((:type symbol :value "foo" :start 1 :end 4)
                                      (:type list
                                             :value ((:type symbol :value "foo" :start 6 :end 9)
                                                     (:type symbol :value "foo" :start 9 :end 13))
                                             :start 4
                                             :end 14))
                              :start 0
                              :end 14)
                      0
                      14))
     (assert (with-handlers ((string?
                              (lambda (e)
                                e)))
               (SimpleLisp "\"This string will fail to parse, but in a controlled way"))
             "Unterminated string at location: 0")
     (assert (with-handlers ((string?
                              (lambda (e)
                                e)))
               (SimpleLisp "(do (display \"This string will fail to parse, but in a controlled way) (newline))"))
             "Unterminated string at location: 13"))

 (it "handles concatenation correctly"
     (assert (Concat "foobarbaz")
             (matches "foobarbaz"
                      0
                      9)))

 (it "doesn't have cache collisions between calls"
     ;; NOTE Simulate hash collision that could happen when the cache is reused between calls to parse.
     (assert (list (SimpleLisp "foo")
                   (SimpleLisp "oof"))
             (list (matches '(:type symbol :value foo :original "foo" :start 0 :end 3) 0 3)
                   (matches '(:type symbol :value oof :original "oof" :start 0 :end 3) 0 3)))))
