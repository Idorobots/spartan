;; PEG parser generator tests.

;; Nonterminal

(define id-rule (lambda args
                    args))

(define all-rules (ref `((Rule ,id-rule))))

(assert (equal? (compile-rule-pattern 'Rule all-rules)
                (compile-rule-pattern 'Rule all-rules)))

(assert ((compile-rule-pattern 'Rule all-rules) ;; Should delegate correctly.
         23 "input" 23)
        '(23 "input" 23))

;; "terminal"
(define input-hash
  (let ((i (ref 3245346)))
    (lambda ()
      (assign! i (+ (deref i) 123456))
      (deref i))))

(assert ((compile-rule-pattern "foo" '())
         (input-hash) "" 0)
        (no-match))

(assert ((compile-rule-pattern "foo" '())
         (input-hash) "barfoo" 0)
        (no-match))

(assert ((compile-rule-pattern "foo" '())
         (input-hash) "foobar" 3)
        (no-match))

(assert ((compile-rule-pattern "foo" '())
         (input-hash) "foobar" 0)
        (matches "foo" 0 3))

(assert ((compile-rule-pattern "(foo|bar)+" '())
         (input-hash) "foobar" 0)
        (matches "foobar" 0 6))

;; (...)

(assert ((compile-rule-pattern '("foo" "bar") '())
         (input-hash) "foofoo" 0)
        (no-match))

(assert ((compile-rule-pattern '("foo" "bar") '())
         (input-hash) "barfoo" 0)
        (no-match))

(assert ((compile-rule-pattern '("foo" "bar") '())
         (input-hash) "foobar" 0)
        (matches '("foo" "bar") 0 6))

(assert ((compile-rule-pattern '("foo" "bar") '())
         (input-hash) "foobarbaz" 0)
        (matches '("foo" "bar") 0 6))

;; (/ ...)

(assert ((compile-rule-pattern '(/ "foo" "bar") '())
         (input-hash) "bazbarfoo" 0)
        (no-match))

(assert ((compile-rule-pattern '(/ "foo" "bar") '())
         (input-hash) "barfoo" 0)
        (matches "bar" 0 3))

(assert ((compile-rule-pattern '(/ "foo" "bar") '())
         (input-hash) "foobarbaz" 0)
        (matches "foo" 0 3))

;; (* ...)

(assert ((compile-rule-pattern '(* "foo") '())
         (input-hash) "barfoo" 0)
        (matches '() 0 0))

(assert ((compile-rule-pattern '(* "foo") '())
         (input-hash) "foobarbaz" 0)
        (matches '("foo") 0 3))

(assert ((compile-rule-pattern '(* "foo") '())
         (input-hash) "foofoofoo" 0)
        (matches '("foo" "foo" "foo") 0 9))

(assert ((compile-rule-pattern '(* (/ "foo" "bar")) '())
         (input-hash) "foobarbarfoo" 0)
        (matches '("foo" "bar" "bar" "foo") 0 12))

;; (+ ...)

(assert ((compile-rule-pattern '(+ "foo") '())
         (input-hash) "barfoo" 0)
        (no-match))

(assert ((compile-rule-pattern '(+ "foo") '())
         (input-hash) "foobarbaz" 0)
        (matches '("foo") 0 3))

(assert ((compile-rule-pattern '(+ "foo") '())
         (input-hash) "foofoofoo" 0)
        (matches '("foo" "foo" "foo") 0 9))

(assert ((compile-rule-pattern '(+ (/ "foo" "bar")) '())
         (input-hash) "foobarbarfoo" 0)
        (matches '("foo" "bar" "bar" "foo") 0 12))

;; (? ...)

(assert ((compile-rule-pattern '(? "foo") '())
         (input-hash) "barfoo" 0)
        (matches '() 0 0))

(assert ((compile-rule-pattern '(? "foo") '())
         (input-hash) "foobarbaz" 0)
        (matches "foo" 0 3))

(assert ((compile-rule-pattern '(? (/ "foo" "bar")) '())
         (input-hash) "foobarbarfoo" 0)
        (matches "foo" 0 3))

(assert ((compile-rule-pattern '(? (/ "foo" "bar")) '())
         (input-hash) "foobarbarfoo" 3)
        (matches "bar" 3 6))

;; (! ...)

(assert ((compile-rule-pattern '(! "foo") '())
         (input-hash) "barfoo" 0)
        (matches '() 0 0))

(assert ((compile-rule-pattern '(! "foo") '())
         (input-hash) "foobarbaz" 0)
        (no-match))

(assert ((compile-rule-pattern '(! (/ "foo" "bar")) '())
         (input-hash) "foobarbarfoo" 0)
        (no-match))

(assert ((compile-rule-pattern '(! (/ "foo" "bar")) '())
         (input-hash) "foobarbarfoo" 3)
        (no-match))

;; (& ...)

(assert ((compile-rule-pattern '(& "foo") '())
         (input-hash) "foaobarbarfoo" 0)
        (no-match))

(assert ((compile-rule-pattern '(& "foo") '())
         (input-hash) "foobarbarfoo" 0)
        (matches "foo" 0 0))

(define *expensive-rule-ran* #f)

(define expensive-rule
  (lambda (_ input offset)
    (set! *expensive-rule-ran* #t)
    (matches "very expensive" 5 23)))

(define all-rules (ref `((Expensive ,expensive-rule))))

(assert ((compile-rule-pattern '((& "exp") Expensive)
                               all-rules)
         (input-hash) "extravaganza!" 0)
        (no-match))

(assert (not *expensive-rule-ran*))

(assert ((compile-rule-pattern '((& "exp") Expensive)
                               all-rules)
         (input-hash) "experience the amazig parser generators!" 0)
        (matches '("exp" "very expensive") 0 23))

(assert *expensive-rule-ran*)

;; (: ...)

(assert ((compile-rule-pattern '(: "foo") '())
         (input-hash) "barfoo" 0)
        (no-match))

(assert ((compile-rule-pattern '(: "foo") '())
         (input-hash) "foobarbarfoo" 0)
        (matches '() 3 3))

(assert ((compile-rule-pattern '((: "foo") "bar" (: "foo")) '())
         (input-hash) "foobarfoo" 0)
        (matches '(() "bar" ()) 0 9))

;; (~ ...)

(assert ((compile-rule-pattern '(~ "bar" "foo") '())
         (input-hash) "foobarfoo" 0)
        (no-match))

(assert ((compile-rule-pattern '(~ "foo" "bar") '())
         (input-hash) "foobarbaz" 0)
        (matches "foobar" 0 6))

(assert ((compile-rule-pattern '(~ (+ "foo")) '())
         (input-hash) "foofoofoofoofoobar" 0)
        (matches "foofoofoofoofoo" 0 15))

;; Grammar.

(define (ast . properties)
  properties)

(define simple-lisp
  (grammar
   '(Expression
     (/ List Atom String Quote))
   `(Quote
     (Spacing (: "'") Expression)
     ,(lambda (input result)
        (map-match (lambda (matching spacing-start end)
                     (let ((start (car matching)))
                       (matches (ast ':type 'quote
                                     ':value (caddr matching)
                                     ':start start
                                     ':end end)
                                start
                                end)))
                   result)))
   `(String
     (Spacing (: "\"") "[^\"]*" (: "\""))
     ,(lambda (input result)
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
   `(List
     (Spacing (: "\\(") (* Expression) Spacing (: "\\)"))
     ,(lambda (input result)
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
   `(Number
     (Spacing "[+\\-]?[0-9]+(\\.[0-9]*)?")
     ,(lambda (input result)
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
   `(Symbol
     (Spacing (! Number) "[^\\(\\)\"'`,; \t\v\r\n]+")
     ,(lambda (input result)
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
   `(Spacing
     (: (* (/ "[ \t\v\r\n]+" Comment)))
     ,(lambda (input result)
        (map-match (lambda (matching start end)
                     ;; NOTE So that we can skip the spacing later.
                     (matches end start end))
                   result)))
   '(Comment
     (: ";[^\n]*\n"))))

(assert (simple-lisp "(foo   )")
        (matches '(:type list
                         :value ((:type symbol :value foo :original "foo" :start 1 :end 4))
                         :start 0
                         :end 8)
                 0
                 8))

(assert (simple-lisp
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
                 10))

(assert (simple-lisp
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
                 68))
