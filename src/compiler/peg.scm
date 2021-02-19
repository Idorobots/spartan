;; PEG parser generator

(load "compiler/utils.scm")

;; Some debugging & optimizations

(define (memoize f)
  (let* ((previous-runs (make-hash)))
    (lambda args
      (hash-ref! previous-runs args
                 (lambda ()
                   (apply f args))))))

;; Matches
(define (no-match)
  '())

(define (matches matching start end)
  (list matching start end))

(define matches? (compose not empty?))

(define (map-match f m)
  (if (matches? m)
      (apply f m)
      m))

;; Grammar
(define (grammar . rules)
  (let* ((compiled (map (lambda (r)
                          (let* ((name (car r))
                                 (body (cadr r))
                                 (transform (cddr r))
                                 (compiled (compile-rule-pattern body))
                                 (transform (if (empty? transform)
                                                (lambda (input result)
                                                  result)
                                                (car transform))))
                            (list name
                                  (memoize
                                   (lambda (rules)
                                     (let ((linked (compiled rules)))
                                       (lambda (input offset)
                                         (transform input (linked input offset)))))))))
                        rules))
         (linked ((cadar compiled) compiled)))
    (lambda (input)
      (linked input 0))))

;; Rules
(define terminal? string?)
(define nonterminal? symbol?)

(define compile-rule-pattern
  (memoize
   (lambda (rule)
     (memoize
      (cond ((nonterminal? rule)      (compile-nonterminal rule))
            ((terminal? rule)         (compile-matcher rule))
            ((equal? (length rule) 1) (compile-rule-pattern (car rule)))
            ((tagged-list? '/ rule)   (compile-or rule))
            ((tagged-list? '* rule)   (compile-zero-or-more rule))
            ((tagged-list? '+ rule)   (compile-one-or-more rule))
            ((tagged-list? ': rule)   (compile-drop rule))
            ((tagged-list? '? rule)   (compile-optional rule))
            ((tagged-list? '! rule)   (compile-not rule))
            ((tagged-list? '& rule)   (compile-and rule))
            ((tagged-list? '~ rule)   (compile-concat rule))
            (else                     (compile-sequence rule)))))))

(define (find-rule name rules)
  (let ((r (assoc name rules)))
    (if r
        (cadr r)
        (error (format "Invalid rule name used: ~a" name)))))

;; Nonterminal
(define (compile-nonterminal rule-name)
  (lambda (rules)
    (let ((linked '()))
      (lambda (input offset)
        (when (empty? linked)
          ;; NOTE Needs to be embedded within the inner function to break recursivity of the rules.
          (set! linked ((find-rule rule-name rules) rules)))
        (linked input offset)))))

;; "terminal"
(define (compile-matcher regex)
  (lambda (_)
    (lambda (input offset)
      (let* ((actual-input (substring input offset))
             (result (regexp-match (string-append "^" regex) actual-input)))
        (if result
            (matches (car result)
                     offset
                     (+ offset (string-length (car result))))
            (no-match))))))

;; (...)
(define (compile-sequence subrules)
  (let ((compiled (map (lambda (r)
                         (compile-rule-pattern r))
                       subrules)))
    (lambda (rules)
      (let ((linked (map (lambda (r)
                           (r rules))
                         compiled)))
        (lambda (input offset)
          (foldl (lambda (r acc)
                   (map-match (lambda (ms s e)
                                (map-match (lambda (m new-s new-e)
                                             (matches (append ms (list m)) s new-e))
                                           (r input e)))
                              acc))
                 (matches '() offset offset)
                 linked))))))

;; (/ ...)
(define (compile-or subrules)
  (let ((compiled (map (lambda (r)
                         (compile-rule-pattern r))
                       (cdr subrules))))
    (lambda (rules)
      (let ((linked (map (lambda (r)
                           (r rules))
                         compiled)))
        (lambda (input offset)
          (foldl (lambda (r acc)
                   (if (matches? acc)
                       acc
                       (r input offset)))
                 (no-match)
                 linked))))))

;; (* ...)
(define (compile-zero-or-more subrules)
  (let ((compiled (compile-rule-pattern (cdr subrules))))
    (lambda (rules)
      (let ((linked (compiled rules)))
        (lambda (input offset)
          (let loop ((s offset)
                     (e offset)
                     (ms '()))
            (let ((result (linked input e)))
              (if (matches? result)
                  (map-match (lambda (m _ new-e)
                               (loop s new-e (cons m ms)))
                             result)
                  (matches (reverse ms) s e)))))))))

;; (+ ...)
(define (compile-one-or-more subrules)
  (let ((compiled (compile-rule-pattern (cdr subrules))))
    (lambda (rules)
      (let ((linked (compiled rules)))
        (lambda (input offset)
          (let loop ((s offset)
                     (e offset)
                     (ms '()))
            (let ((result (linked input e)))
              (if (matches? result)
                  (map-match (lambda (m new-s new-e)
                               (loop s new-e (cons m ms)))
                             result)
                  (if (> (length ms) 0)
                      (matches (reverse ms) s e)
                      (no-match))))))))))

;; (? ...)
(define (compile-optional subrules)
  (let ((compiled (compile-rule-pattern (cdr subrules))))
    (lambda (rules)
      (let ((linked (compiled rules)))
        (lambda (input offset)
          (let ((result (linked input offset)))
            (if (matches? result)
                result
                (matches '() offset offset))))))))

;; (! ...)
(define (compile-not subrules)
  (let ((compiled (compile-rule-pattern (cdr subrules))))
    (lambda (rules)
      (let ((linked (compiled rules)))
        (lambda (input offset)
          (let ((result (linked input offset)))
            (if (matches? result)
                (no-match)
                (matches '() offset offset))))))))

;; (& ...)
(define (compile-and subrules)
  (let ((compiled (compile-rule-pattern (cdr subrules))))
    (lambda (rules)
      (let ((linked (compiled rules)))
        (lambda (input offset)
          (let ((result (linked input offset)))
            (map-match (lambda (m s e)
                         ;; NOTE Doesn't advance the scan.
                         (matches m offset offset))
                       result)))))))

;; (: ...)
(define (compile-drop subrules)
  (let ((compiled (compile-rule-pattern (cdr subrules))))
    (lambda (rules)
      (let ((linked (compiled rules)))
        (lambda (input offset)
          (let ((result (linked input offset)))
            (map-match (lambda (m s e)
                         ;; NOTE Skips the scan.
                         (matches '() e e))
                       result)))))))

;; (~ ...)
(define (compile-concat subrules)
  (let ((compiled (compile-rule-pattern (cdr subrules))))
    (lambda (rules)
      (let ((linked (compiled rules)))
        (lambda (input offset)
          (let ((result (linked input offset)))
            (map-match (lambda (ms s e)
                         (matches (foldr string-append "" ms) s e))
                       result)))))))
