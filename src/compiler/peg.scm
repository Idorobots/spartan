;; PEG parser generator

(load "compiler/utils.scm")

;; Some optimizations

(define (memoize f)
  (let* ((previous-runs (make-hash)))
    (lambda args
      (hash-ref! previous-runs
                 args
                 (lambda ()
                   (apply f args))))))

(define (memoize-input f)
  (let* ((previous-runs (make-hasheq)))
    (lambda (hash input offset)
      (hash-ref! previous-runs
                 (+ hash offset)
                 (lambda ()
                   (f hash input offset))))))

;; Matches
(define (no-match)
  '())

(define (matches m start end)
  (vector m start end))

(define matches? vector?)

(define (match-match m)
  (vector-ref m 0))

(define (match-start m)
  (vector-ref m 1))

(define (match-end m)
  (vector-ref m 2))

;; Rules
(define terminal? string?)
(define nonterminal? symbol?)

;; Grammar compiler
(define (grammar . rules)
  (let* ((all-rules (ref '()))
         (compiled (map (lambda (r)
                          (let* ((name (car r))
                                 (body (cadr r))
                                 (transform (cddr r))
                                 (compiled (compile-rule-pattern body all-rules))
                                 (transform (if (empty? transform)
                                                (lambda (input result)
                                                  result)
                                                (car transform))))
                            (list name
                                  (memoize-input
                                   (lambda (hash input offset)
                                     (let ((result (compiled hash input offset)))
                                       (if (matches? result)
                                           (transform input result)
                                           result)))))))
                        rules)))
    (assign! all-rules compiled)
    (lambda (input)
      ((cadar compiled)
       (* (eq-hash-code input)
          (string-length input))
       input
       0))))

(define compile-rule-pattern
  (memoize
   (lambda (rule all-rules)
     (memoize-input
      (cond ((nonterminal? rule)      (compile-nonterminal rule all-rules))
            ((terminal? rule)         (compile-matcher rule all-rules))
            ((equal? (length rule) 1) (compile-rule-pattern (car rule) all-rules))
            ((tagged-list? '/ rule)   (compile-or rule all-rules))
            ((tagged-list? '* rule)   (compile-zero-or-more rule all-rules))
            ((tagged-list? '+ rule)   (compile-one-or-more rule all-rules))
            ((tagged-list? ': rule)   (compile-drop rule all-rules))
            ((tagged-list? '? rule)   (compile-optional rule all-rules))
            ((tagged-list? '! rule)   (compile-not rule all-rules))
            ((tagged-list? '& rule)   (compile-and rule all-rules))
            ((tagged-list? '~ rule)   (compile-concat rule all-rules))
            (else                     (compile-sequence rule all-rules)))))))

;; Nonterminal
(define find-rule
  (memoize
   (lambda (name all-rules)
     (let ((r (assoc name all-rules)))
       (if r
           (cadr r)
           (error (format "Invalid rule name used: ~a" name)))))))

(define (compile-nonterminal rule-name all-rules)
  (lambda (hash input offset)
    ((find-rule rule-name (deref all-rules)) hash input offset)))

;; "terminal"
(define (compile-matcher regex _)
  (if (equal? 1 (string-length regex)) ;; FIXME "." won't work.
      (let ((char (string-ref regex 0)))
        (lambda (_ input offset)
          (if (equal? (string-ref input offset) char)
              (matches regex offset (+ 1 offset))
              (no-match))))
      (let ((compiled-regex (regexp (string-append-immutable "^" regex))))
        (lambda (_ input offset)
        (let ((result (regexp-match compiled-regex input offset)))
          (if result
              (matches (car result)
                       offset
                       (+ offset (string-length (car result))))
              (no-match)))))))

;; (...)
(define (compile-sequence subrules all-rules)
  (let ((compiled (map (lambda (r)
                         (compile-rule-pattern r all-rules))
                       subrules)))
    (lambda (hash input offset)
      (let ((result (foldl (lambda (r acc)
                             (if (matches? acc)
                                 (let ((result (r hash input (match-end acc))))
                                   (if (matches? result)
                                       (matches (cons (match-match result)
                                                      (match-match acc))
                                                (match-start acc)
                                                (match-end result))
                                       (no-match)))
                                 (no-match)))
                           (matches '() offset offset)
                           compiled)))
        (if (matches? result)
            (matches (reverse (match-match result))
                     (match-start result)
                     (match-end result))
            result)))))

;; (/ ...)
(define (compile-or subrules all-rules)
  (let ((compiled (map (lambda (r)
                         (compile-rule-pattern r all-rules))
                       (cdr subrules))))
    (lambda (hash input offset)
      (foldl (lambda (r acc)
               (if (matches? acc)
                   acc
                   (r hash input offset)))
             (no-match)
             compiled))))

;; (* ...)
(define (compile-zero-or-more subrules all-rules)
  (let ((compiled (compile-rule-pattern (cdr subrules) all-rules)))
    (lambda (hash input offset)
      (let loop ((s offset)
                 (e offset)
                 (ms '()))
        (let ((result (compiled hash input e)))
          (if (matches? result)
              (loop s
                    (match-end result)
                    (cons (match-match result)
                          ms))
              (matches (reverse ms) s e)))))))

;; (+ ...)
(define (compile-one-or-more subrules all-rules)
  (let ((compiled (compile-rule-pattern (cdr subrules) all-rules)))
    (lambda (hash input offset)
      (let loop ((s offset)
                 (e offset)
                 (ms '()))
        (let ((result (compiled hash input e)))
          (if (matches? result)
              (loop s
                    (match-end result)
                    (cons (match-match result)
                          ms))
              (if (> (length ms) 0)
                  (matches (reverse ms) s e)
                  (no-match))))))))

;; (? ...)
(define (compile-optional subrules all-rules)
  (let ((compiled (compile-rule-pattern (cdr subrules) all-rules)))
    (lambda (hash input offset)
      (let ((result (compiled hash input offset)))
        (if (matches? result)
            result
            (matches '() offset offset))))))

;; (! ...)
(define (compile-not subrules all-rules)
  (let ((compiled (compile-rule-pattern (cdr subrules) all-rules)))
    (lambda (hash input offset)
      (let ((result (compiled hash input offset)))
        (if (matches? result)
            (no-match)
            (matches '() offset offset))))))

;; (& ...)
(define (compile-and subrules all-rules)
  (let ((compiled (compile-rule-pattern (cdr subrules) all-rules)))
    (lambda (hash input offset)
      (let ((result (compiled hash input offset)))
        (if (matches? result)
            (matches (match-match result) offset offset)
            (no-match))))))

;; (: ...)
(define (compile-drop subrules all-rules)
  (let ((compiled (compile-rule-pattern (cdr subrules) all-rules)))
    (lambda (hash input offset)
      (let ((result (compiled hash input offset)))
        (if (matches? result)
            (let ((end (match-end result)))
              ;; NOTE Skips the scan.
              (matches '() end end))
            (no-match))))))

;; (~ ...)
(define (compile-concat subrules all-rules)
  (let ((compiled (compile-rule-pattern (cdr subrules) all-rules)))
    (lambda (hash input offset)
      (let ((result (compiled hash input offset)))
        (if (matches? result)
            (matches (foldr string-append-immutable "" (match-match result))
                     (match-start result)
                     (match-end result))
            (no-match))))))
