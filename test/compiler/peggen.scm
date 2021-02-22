;; PEG parser source generator tests

(define stripper
  (lambda (r)
    `(strip ,r)))

;; Nonterminal

(assert (generate-nonterminal 'Foo 'h 'input 'off id)
        '(Foo h input off))

(assert (generate-nonterminal 'Foo 'h 'input 'off stripper)
        '(strip (Foo h input off)))

;; "terminal"

(gensym-reset!)
(assert (generate-matcher "f" 'h 'input 'off id)
        '(if (equal? (string-ref input off) #\f)
             (matches "f" off (+ 1 off))
             (no-match)))

(gensym-reset!)
(assert (generate-matcher "f" 'h 'input 'off stripper)
        '(strip
          (if (equal? (string-ref input off) #\f)
             (matches "f" off (+ 1 off))
             (no-match))))

(gensym-reset!)
(assert (generate-matcher "foo" 'h 'input 'off id)
        '(let ((result1 (regexp-match #rx"^foo" input off)))
           (if result1
               (matches (car result1) off (+ off (string-length (car result1))))
               (no-match))))

(gensym-reset!)
(assert (generate-matcher "foo" 'h 'input 'off stripper)
        '(strip
          (let ((result1 (regexp-match #rx"^foo" input off)))
            (if result1
                (matches (car result1) off (+ off (string-length (car result1))))
                (no-match)))))

;; (...)

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
                (no-match)))))

;; (/ ...)

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
                  (if (matches? result1) result1 (no-match)))))))

;; (* ...)

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
                  (matches (reverse matches3) off end2))))))

;; (+ ...)

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
              (no-match))))

;; (? ...)

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
                (matches '() off off)))))

;; (! ...)

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
              (matches '() off off))))

;; (& ...)

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
               (no-match)))))

;; (: ...)

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
                (no-match)))))

;; (~ ...)

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
                (no-match)))))
