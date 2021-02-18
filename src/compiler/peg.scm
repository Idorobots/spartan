;; PEG parser generator

(load "compiler/utils.scm")

;; Some debugging & optimizations

;; (define *trace-count* 0)
;; (define (trace where input offset)
;;   (when where
;;     (set! *trace-count* (+ 1 *trace-count*)))
;;   (display (format "~a - ~a at ~a\n" where input offset)))

(define (trace . args)
  '())

(define (memoize f)
  (let* ((previous-runs '()))
    (lambda args
      (let ((prev (assoc args previous-runs)))
        (if prev
            (begin
              (trace 'memoize f prev)
              (cadr prev))
            (let ((result (apply f args)))
              (set! previous-runs (cons (list args result)
                                        previous-runs))
              result))))))

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
(define left-arrow '<-)
(define strip-arrow '<)
(define concat-arrow '<~)

(define (grammar . rules)
  (let* ((compiled (map (lambda (r)
                          (let* ((name (caar r))
                                 (type (cadar r))
                                 (body (cddar r))
                                 (transform (cdr r))
                                 (compiled (compile-rule-pattern (if (equal? type concat-arrow)
                                                             (list '~ body)
                                                             body)
                                                         (equal? type strip-arrow)))
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
   (lambda (rule stripping?)
     (trace 'compile-rule-pattern rule stripping?)
     (memoize
      (cond ((nonterminal? rule)      (compile-nonterminal rule stripping?))
            ((terminal? rule)         (compile-matcher rule stripping?))
            ((equal? (length rule) 1) (compile-rule-pattern (car rule) stripping?))
            ((tagged-list? '/ rule)   (compile-or rule stripping?))
            ((tagged-list? '* rule)   (compile-zero-or-more rule stripping?))
            ((tagged-list? '+ rule)   (compile-one-or-more rule stripping?))
            ((tagged-list? ': rule)   (compile-drop rule stripping?))
            ((tagged-list? '? rule)   (compile-optional rule stripping?))
            ((tagged-list? '! rule)   (compile-not rule stripping?))
            ((tagged-list? '& rule)   (compile-and rule stripping?))
            ((tagged-list? '~ rule)   (compile-concat rule stripping?))
            (else                     (compile-sequence rule stripping?)))))))

(define (find-rule name rules)
  (let ((r (assoc name rules)))
    (if r
        (cadr r)
        (error (format "Invalid rule name used: ~a" name)))))

;; Terminal
(define (compile-nonterminal rule-name stripping?)
  (trace 'compile-nonterminal rule-name stripping?)
  (lambda (rules)
    (trace 'link-nonterminal rule-name stripping?)
    (let ((linked '()))
      (memoize
       (lambda (input offset)
         (when (empty? linked)
           ;; NOTE Needs to be embedded within the inner function to break recursivity of the rules.
           (set! linked ((find-rule rule-name rules) rules)))
         (trace 'nonterminal input offset)
         (linked input offset))))))

;; "..."
(define (compile-matcher regex stripping?)
  (trace 'compile-matcher regex stripping?)
  (lambda (rules)
    (trace 'link-matcher regex stripping?)
    (let* ((strip (if stripping?
                      ((find-rule 'Spacing rules) rules)
                      (lambda (input offset)
                        (matches '() offset offset)))))
      (memoize
       (lambda (input offset)
         (let* ((mat (strip input offset))
                (off (if (matches? mat)
                         (let-matches (m s e) mat
                                      e)
                         offset))
                (actual-input (substring input off))
                (result (regexp-match (string-append "^" regex) actual-input)))
           (trace 'matcher result regex)
           (if result
               (matches (car result)
                        off
                        (+ off (string-length (car result))))
               (no-match))))))))

;; (...)
(define (compile-sequence subrules stripping?)
  (trace 'compile-sequence subrules stripping?)
  (let ((compiled (map (lambda (r)
                         (compile-rule-pattern r stripping?))
                       subrules)))
    (lambda (rules)
      (trace 'link-sequence subrules stripping?)
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
(define (compile-or subrules stripping?)
  (trace 'compile-or subrules stripping?)
  (let ((compiled (map (lambda (r)
                         (compile-rule-pattern r stripping?))
                       (cdr subrules))))
    (lambda (rules)
      (trace 'link-or subrules stripping?)
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
(define (compile-zero-or-more subrules stripping?)
  (trace 'compile-zero-or-more subrules stripping?)
  (let ((compiled (compile-rule-pattern (cdr subrules) stripping?)))
    (lambda (rules)
      (trace 'link-zero-or-more subrules stripping?)
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
(define (compile-one-or-more subrules stripping?)
  (trace 'compile-one-or-more subrules stripping?)
  (let ((compiled (compile-rule-pattern (cdr subrules) stripping?)))
    (lambda (rules)
      (trace 'link-one-or-more subrules stripping?)
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
(define (compile-optional subrules stripping?)
  (trace 'compile-optional subrules stripping?)
  (let ((compiled (compile-rule-pattern (cdr subrules) stripping?)))
    (lambda (rules)
      (trace 'link-optional subrules stripping?)
      (let ((linked (compiled rules)))
        (memoize
         (lambda (input offset)
           (trace 'optional input offset)
           (let ((result (linked input offset)))
             (if (matches? result)
                 result
                 (matches '() offset offset)))))))))

;; (! ...)
(define (compile-not subrules stripping?)
  (trace 'compile-not subrules stripping?)
  (let ((compiled (compile-rule-pattern (cdr subrules) stripping?)))
    (lambda (rules)
      (trace 'link-not subrules stripping?)
      (let ((linked (compiled rules)))
        (memoize
         (lambda (input offset)
           (trace 'not input offset)
           (let ((result (linked input offset)))
             (if (matches? result)
                 (no-match)
                 (matches '() offset offset)))))))))

;; (& ...)
(define (compile-and subrules stripping?)
  (trace 'compile-and subrules stripping?)
  (let ((compiled (compile-rule-pattern (cdr subrules) stripping?)))
    (lambda (rules)
      (trace 'link-and subrules stripping?)
      (let ((linked (compiled rules)))
        (memoize
         (lambda (input offset)
           (trace 'and input offset)
           (let ((result (linked input offset)))
             (let-matches (m s e) result
                          (matches m offset e)))))))))

;; (: ...)
(define (compile-drop subrules stripping?)
  (trace 'compile-drop subrules stripping?)
  (let ((compiled (compile-rule-pattern (cdr subrules) stripping?)))
    (lambda (rules)
      (trace 'link-drop subrules stripping?)
      (let ((linked (compiled rules)))
        (memoize
         (lambda (input offset)
           (trace 'drop input offset)
           (let ((result (linked input offset)))
             (let-matches (m s e) result
                          (matches '() offset e)))))))))

;; (~ ...)
(define (compile-concat subrules stripping?)
  (trace 'compile-drop subrules stripping?)
  (let ((compiled (compile-rule-pattern (cdr subrules) stripping?)))
    (lambda (rules)
      (trace 'link-concat subrules stripping?)
      (let ((linked (compiled rules)))
        (memoize
         (lambda (input offset)
           (trace 'drop input offset)
           (let ((result (linked input offset)))
             (let-matches (ms s e) result
                          (matches (foldl string-append "" ms) s e)))))))))
