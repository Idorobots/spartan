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
