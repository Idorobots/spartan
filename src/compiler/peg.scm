;; PEG parser generator

(load "compiler/utils.scm")

;; Some debugging & optimizations

(define (trace . args)
  '())

;; (define *trace-count* 0)
;; (define (trace where input offset)
;;   (when where
;;     (set! *trace-count* (+ 1 *trace-count*)))
;;   (display (format "~a - ~a at ~a\n" where input offset)))

;; (define (memoize f)
;;   f)

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

(define-syntax let-matches
  (syntax-rules ()
    ((_ (m s e) expr body ...)
     (let ((result expr))
       (if (matches? result)
           (let ((m (car result))
                 (s (cadr result))
                 (e (caddr result)))
             body ...)
           (no-match))))))

;; Grammar
(define (grammar . rules)
  (let* ((compiled (map (lambda (r)
                          (let* ((name (caar r))
                                 (body (cddar r))
                                 (transform (cdr r))
                                 (compiled (compile-rule-pattern body))
                                 (transform (if (empty? transform)
                                                (lambda (input result)
                                                  result)
                                                (car transform))))
                            (list name
                                  (memoize
                                   (lambda (rules)
                                     (let ((linked (compiled rules)))
                                       (memoize
                                        (lambda (input offset)
                                          (trace name input offset)
                                          (transform input (linked input offset))))))))))
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
     (trace 'compile-rule-pattern rule '())
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
  (trace 'compile-nonterminal rule-name '())
  (lambda (rules)
    (trace 'link-nonterminal rule-name '())
    (let ((linked '()))
      (memoize
       (lambda (input offset)
         (when (empty? linked)
           ;; NOTE Needs to be embedded within the inner function to break recursivity of the rules.
           (set! linked ((find-rule rule-name rules) rules)))
         (trace 'nonterminal input offset)
         (linked input offset))))))

;; "terminal"
(define (compile-matcher regex)
  (trace 'compile-matcher regex '())
  (lambda (_)
    (trace 'link-matcher regex '())
    (memoize
     (lambda (input offset)
       (let* ((actual-input (substring input offset))
              (result (regexp-match (string-append "^" regex) actual-input)))
         (trace 'matcher result regex)
         (if result
             (matches (car result)
                      offset
                      (+ offset (string-length (car result))))
             (no-match)))))))

;; (...)
(define (compile-sequence subrules)
  (trace 'compile-sequence subrules '())
  (let ((compiled (map (lambda (r)
                         (compile-rule-pattern r))
                       subrules)))
    (lambda (rules)
      (trace 'link-sequence subrules '())
      (let ((linked (map (lambda (r)
                           (r rules))
                         compiled)))
        (memoize
         (lambda (input offset)
           (trace 'sequence input offset)
           (foldl (lambda (r acc)
                    (let-matches (ms s e) acc
                                 (let-matches (m new-s new-e) (r input e)
                                              (matches (append ms (list m)) s new-e))))
                  (matches '() offset offset)
                  linked)))))))

;; (/ ...)
(define (compile-or subrules)
  (trace 'compile-or subrules '())
  (let ((compiled (map (lambda (r)
                         (compile-rule-pattern r))
                       (cdr subrules))))
    (lambda (rules)
      (trace 'link-or subrules '())
      (let ((linked (map (lambda (r)
                           (r rules))
                         compiled)))
        (memoize
         (lambda (input offset)
           (trace 'or input offset)
           (foldl (lambda (r acc)
                    (if (matches? acc)
                        acc
                        (r input offset)))
                  (no-match)
                  linked)))))))

;; (* ...)
(define (compile-zero-or-more subrules)
  (trace 'compile-zero-or-more subrules '())
  (let ((compiled (compile-rule-pattern (cdr subrules))))
    (lambda (rules)
      (trace 'link-zero-or-more subrules '())
      (let ((linked (compiled rules)))
        (memoize
         (lambda (input offset)
           (trace 'zero-or-more input offset)
           (let loop ((s offset)
                      (e offset)
                      (ms '()))
             (let ((result (linked input e)))
               (if (matches? result)
                   (let-matches (m _ new-e) result
                                (loop s new-e (cons m ms)))
                   (matches (reverse ms) s e))))))))))

;; (+ ...)
(define (compile-one-or-more subrules)
  (trace 'compile-one-or-more subrules '())
  (let ((compiled (compile-rule-pattern (cdr subrules))))
    (lambda (rules)
      (trace 'link-one-or-more subrules '())
      (let ((linked (compiled rules)))
        (memoize
         (lambda (input offset)
           (trace 'one-or-more input offset)
           (let loop ((s offset)
                      (e offset)
                      (ms '()))
             (let ((result (linked input e)))
               (if (matches? result)
                   (let-matches (m new-s new-e) result
                                (loop s new-e (cons m ms)))
                   (if (> (length ms) 0)
                       (matches (reverse ms) s e)
                       (no-match)))))))))))

;; (? ...)
(define (compile-optional subrules)
  (trace 'compile-optional subrules '())
  (let ((compiled (compile-rule-pattern (cdr subrules))))
    (lambda (rules)
      (trace 'link-optional subrules '())
      (let ((linked (compiled rules)))
        (memoize
         (lambda (input offset)
           (trace 'optional input offset)
           (let ((result (linked input offset)))
             (if (matches? result)
                 result
                 (matches '() offset offset)))))))))

;; (! ...)
(define (compile-not subrules)
  (trace 'compile-not subrules '())
  (let ((compiled (compile-rule-pattern (cdr subrules))))
    (lambda (rules)
      (trace 'link-not subrules '())
      (let ((linked (compiled rules)))
        (memoize
         (lambda (input offset)
           (trace 'not input offset)
           (let ((result (linked input offset)))
             (if (matches? result)
                 (no-match)
                 (matches '() offset offset)))))))))

;; (& ...)
(define (compile-and subrules)
  (trace 'compile-and subrules '())
  (let ((compiled (compile-rule-pattern (cdr subrules))))
    (lambda (rules)
      (trace 'link-and subrules '())
      (let ((linked (compiled rules)))
        (memoize
         (lambda (input offset)
           (trace 'and input offset)
           (let ((result (linked input offset)))
             (let-matches (m s e) result
                          ;; NOTE Doesn't advance the scan.
                          (matches m offset offset)))))))))

;; (: ...)
(define (compile-drop subrules)
  (trace 'compile-drop subrules '())
  (let ((compiled (compile-rule-pattern (cdr subrules))))
    (lambda (rules)
      (trace 'link-drop subrules '())
      (let ((linked (compiled rules)))
        (memoize
         (lambda (input offset)
           (trace 'drop input offset)
           (let ((result (linked input offset)))
             (let-matches (m s e) result
                          ;; NOTE Skips the scan.
                          (matches '() e e)))))))))

;; (~ ...)
(define (compile-concat subrules)
  (trace 'compile-drop subrules '())
  (let ((compiled (compile-rule-pattern (cdr subrules))))
    (lambda (rules)
      (trace 'link-concat subrules '())
      (let ((linked (compiled rules)))
        (memoize
         (lambda (input offset)
           (trace 'drop input offset)
           (let ((result (linked input offset)))
             (let-matches (ms s e) result
                          (matches (foldr string-append "" ms) s e)))))))))
