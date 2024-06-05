#lang racket

;; PEG parser generator utils

(require "gensym.rkt")
(require "utils.rkt")

(provide generate-grammar
         generate-eof generate-nonterminal generate-matcher generate-sequence generate-or generate-zero-or-more
         generate-one-or-more generate-optional generate-not generate-and generate-drop generate-concat
         ;; FIXME For test access
         inline-rules prune-rules optimize-rules)

;; Parser generator
(define (generate-grammar rules)
  (let* ((top-name (caar rules))
         (hash (gensym 'hash))
         (input (gensym 'input))
         (inlined (inline-rules rules))
         (pruned (prune-rules top-name inlined))
         (optimized (optimize-rules pruned))
         (caches (map (lambda (_) (gensym 'cache)) optimized)))
    `(define ,top-name
       (let ,(map generate-cache caches)
         (letrec ,(map generate-rule optimized caches)
           (lambda (,input)
             ,@(map clear-cache caches)
             (,top-name ,input 0)))))))

(define +peg-inline-loops+ 5)

(define (non-transforming? rule)
  (equal? 2 (length rule)))

(define (inline-rules rules)
  (define (inline-rules-once rules)
    (let* ((inlineable (filter non-transforming? rules)))
      (map (lambda (r)
             (inline-rule inlineable r))
           rules)))
  (let loop ((i +peg-inline-loops+)
             (rs rules)
             (prev '()))
    (if (or (= i 0)
            (equal? rs prev))
        rs
        (loop (- i 1)
              (inline-rules-once rs)
              rs))))

(define (inline-rule inlineable rule)
  (let ((name (car rule))
        (pattern (cadr rule))
        (transform (cddr rule)))
    (list* name
           (update-pattern inlineable pattern)
           transform)))

(define (update-pattern rules pattern)
  (cond ((null? pattern)
         pattern)
        ((pair? pattern)
         (cons (update-pattern rules (car pattern))
               (update-pattern rules (cdr pattern))))
        ((symbol? pattern)
         (let ((r (assoc pattern rules)))
           (if r
               (cadr r)
               pattern)))
        (else
         pattern)))

(define +peg-prune-loops+ 5)

(define (prune-rules top-name rules)
  (define (prune-rules-once rules)
    (let ((used (foldl append (list top-name) (map collect-nonterminals rules))))
      (filter (lambda (r)
                (member (car r) used))
              rules)))
  (let loop ((i +peg-prune-loops+)
             (rs rules)
             (prev '()))
    (if (or (= i 0)
            (equal? rs prev))
        rs
        (loop (- i 1)
              (prune-rules-once rs)
              rs))))

(define (collect-nonterminals rule)
  (define (collect-in-pattern pattern)
    (cond ((symbol? pattern)
           (list pattern))
          ((pair? pattern)
           (append (collect-in-pattern (car pattern))
                   (collect-in-pattern (cdr pattern))))
          (else
           '())))
  (uniq (collect-in-pattern (cadr rule))))

(define +peg-optimize-loops+ 5)

(define (optimize-rules rules)
  (define (optimize-rules-once rules)
    (map optimize-rule rules))
  (let loop ((i +peg-optimize-loops+)
             (rs rules)
             (prev '()))
    (if (or (= i 0)
            (equal? rs prev))
        rs
        (loop (- i 1)
              (optimize-rules-once rs)
              rs))))

(define (optimize-rule rule)
  (let ((name (car rule))
        (pattern (cadr rule))
        (transform (cddr rule)))
    (list* name
           (optimize-pattern pattern)
           transform)))

(define (optimize-pattern pattern)
  (match pattern
    ;; No low-hanging optimization fruit.
    ((list '* subpatterns ...)
     (cons '* (map optimize-pattern subpatterns)))
    ((list '+ subpatterns ...)
     (cons '+ (map optimize-pattern subpatterns)))
    ((list '? subpatterns ...)
     (cons '? (map optimize-pattern subpatterns)))
    ((list '! subpatterns ...)
     (cons '! (map optimize-pattern subpatterns)))
    ((list '& subpatterns ...)
     (cons '& (map optimize-pattern subpatterns)))

    ;; Concat
    ((list '~ subpatterns ...)
     #:when (every? string-or-eof? subpatterns)
     (string-join (map regexp-escape subpatterns) ""))
    ((list '~ (list '* subpatterns ...))
     #:when (every? string-or-eof? subpatterns)
     (string-append-immutable "(" (string-join (map regexp-escape subpatterns) "") ")*"))
    ((list '~ (list '+ subpatterns ...))
     #:when (every? string-or-eof? subpatterns)
     (string-append-immutable "(" (string-join (map regexp-escape subpatterns) "") ")+"))
    ((list '~ (list '? subpatterns ...))
     #:when (every? string-or-eof? subpatterns)
     (string-append-immutable "(" (string-join (map regexp-escape subpatterns) "") ")?"))
    ((list '~ subpatterns ...)
     (cons '~ (map optimize-pattern subpatterns)))

    ;; Drop
    ((list ': subpatterns ...)
     #:when (every? string-or-eof? subpatterns)
     (list ': (string-join (map regexp-escape subpatterns) "")))
    ((list ': (list '* subpatterns ...))
     #:when (every? string-or-eof? subpatterns)
     (list ': (string-append-immutable "(" (string-join (map regexp-escape subpatterns) "") ")*")))
    ((list ': (list '+ subpatterns ...))
     #:when (every? string-or-eof? subpatterns)
     (list ': (string-append-immutable "(" (string-join (map regexp-escape subpatterns) "") ")+")))
    ((list ': (list '? subpatterns ...))
     #:when (every? string-or-eof? subpatterns)
     (list ': (string-append-immutable "(" (string-join (map regexp-escape subpatterns) "") ")?")))
    ((list ': subpatterns ...)
     (cons ': (map optimize-pattern subpatterns)))

    ;; Selection
    ((list '/ subpattern)
     (optimize-pattern subpattern))
    ((list '/ subpatterns ...)
     #:when (every? string-or-eof? subpatterns)
     (string-append-immutable "(" (string-join (map regexp-escape subpatterns) "|") ")"))
    ((list '/ subpatterns ...)
     (list* '/ (splice-by (lambda (p)
                            (if (tagged-list? '/ p)
                                (cdr p)
                                (list p)))
                          (map optimize-pattern
                               (uniq subpatterns)))))

    ;; Sequences
    ((list subpattern)
     (optimize-pattern subpattern))
    ((list subpatterns ...)
     (splice-by (lambda (p)
                  (if (and (list? p)
                           (not (null? p))
                           (not (member (car p) '(/ * + ? ! & : ~))))
                      p
                      (list p)))
                (map optimize-pattern
                     subpatterns)))

    ;; Terminals, etc
    (else
     pattern)))

(define (string-or-eof? p)
  (or (string? p)
      (empty? p)))

(define (regexp-escape p)
  (match p
    ("." "\\.")
    ("(" "\\(")
    (")" "\\)")
    ("[" "\\[")
    ("]" "\\]")
    ("{" "\\{")
    ("}" "\\}")
    ("\\" "\\\\")
    ((list) "$")
    (else p)))

(define (splice-by transform patterns)
  (let loop ((acc '())
             (ps patterns))
    (if (empty? ps)
        acc
        (loop (append acc (transform (car ps)))
              (cdr ps)))))

(define (generate-cache cache)
  `(,cache (make-hasheq)))

(define (clear-cache cache)
  `(hash-clear! ,cache))

(define (generate-cache-access cache key value)
  (let ((result (gensym 'result)))
    `(if (hash-has-key? ,cache ,key)
         (hash-ref! ,cache ,key '())
         (let ((,result ,value))
           (hash-set! ,cache ,key ,result)
           ,result))))

(define (generate-rule rule cache)
  (let ((name (car rule))
        (pattern (cadr rule))
        (transform (cddr rule))
        (input (gensym 'input))
        (offset (gensym 'offset)))
    `(,name
      (lambda (,input ,offset)
        ,(generate-cache-access cache
                                offset
                                (generate-rule-pattern pattern
                                                       input
                                                       offset
                                                       (lambda (r)
                                                         (if (empty? transform)
                                                             r
                                                             (let ((result (gensym 'result)))
                                                               `(let ((,result ,r))
                                                                  (if (matches? ,result)
                                                                      (,(car transform) ,input ,result)
                                                                      ,result)))))))))))

(define terminal? string?)
(define nonterminal? symbol?)

(define (generate-rule-pattern rule input offset cont)
  (cond ((empty? rule)            (generate-eof rule input offset cont))
        ((nonterminal? rule)      (generate-nonterminal rule input offset cont))
        ((terminal? rule)         (generate-matcher rule input offset cont))
        ((equal? (length rule) 1) (generate-rule-pattern (car rule) input offset cont))
        ((tagged-list? '/ rule)   (generate-or rule input offset cont))
        ((tagged-list? '* rule)   (generate-zero-or-more rule input offset cont))
        ((tagged-list? '+ rule)   (generate-one-or-more rule input offset cont))
        ((tagged-list? ': rule)   (generate-drop rule input offset cont))
        ((tagged-list? '? rule)   (generate-optional rule input offset cont))
        ((tagged-list? '! rule)   (generate-not rule input offset cont))
        ((tagged-list? '& rule)   (generate-and rule input offset cont))
        ((tagged-list? '~ rule)   (generate-concat rule input offset cont))
        (else                     (generate-sequence rule input offset cont))))

;; EOF
(define (generate-eof rule input offset cont)
  (cont `(if (equal? ,offset (string-length ,input))
             (matches '() ,offset ,offset)
             (no-match))))

;; Nonterminal
(define (generate-nonterminal rule-name input offset cont)
  (cont `(,rule-name ,input ,offset)))

;; "terminal"
(define (generate-matcher regex input offset cont)
  (cont (if (equal? 1 (string-length regex)) ;; FIXME Won't work for "." and other single char regexps.
            (let ((char (string-ref regex 0)))
              `(if (and (< ,offset (string-length ,input))
                        (equal? (string-ref ,input ,offset) ,char))
                   (matches ,regex ,offset (+ 1 ,offset))
                   (no-match)))
            (let ((r (pregexp (string-append-immutable "^" regex)))
                  (result (gensym 'result)))
              `(let ((,result (regexp-match ,r ,input ,offset)))
                 (if ,result
                     (matches (car ,result)
                              ,offset
                              (+ ,offset (string-length (car ,result))))
                     (no-match)))))))

;; (...)
(define (generate-sequence subrules input offset cont)
  (cont (let loop ((subrules subrules)
                   (results '())
                   (last-end offset))
          (if (empty? subrules)
              `(matches ,(cons 'list
                               (map (lambda (r)
                                      `(match-match ,r))
                                    (reverse results)))
                        (match-start ,(last results))
                        ,last-end)
              (let ((subrule (car subrules))
                    (result (gensym 'result))
                    (end (gensym 'end)))
                (generate-rule-pattern subrule
                                       input
                                       last-end
                                       (lambda (r)
                                         `(let ((,result ,r))
                                            (if (matches? ,result)
                                                (let ((,end (match-end ,result)))
                                                  ,(loop (cdr subrules)
                                                         (cons result results)
                                                         end))
                                                (no-match))))))))))

;; (/ ...)
(define (generate-or subrules input offset cont)
  (cont (foldr (lambda (subrule acc)
                 (let ((result (gensym 'result)))
                   (generate-rule-pattern subrule
                                          input
                                          offset
                                          (lambda (r)
                                            `(let ((,result ,r))
                                               (if (matches? ,result)
                                                   ,result
                                                   ,acc))))))
               `(no-match)
               (cdr subrules))))

;; (* ...)
(define (generate-zero-or-more subrules input offset cont)
  (let ((subrule (cdr subrules))
        (result (gensym 'result))
        (end (gensym 'end))
        (results (gensym 'results))
        (loop (gensym 'loop))
        (final (gensym 'final-result)))
    (cont `(let ,loop ((,results '())
                       (,end ,offset))
                ,(generate-rule-pattern subrule
                                        input
                                        end
                                        (lambda (r)
                                          `(let ((,result ,r))
                                             (if (matches? ,result)
                                                 (,loop (cons ,result ,results)
                                                        (match-end ,result))
                                                 (let ((,final (reverse ,results)))
                                                   (matches (map match-match ,final)
                                                            (if (empty? ,final)
                                                                ,offset
                                                                (match-start (car ,final)))
                                                            ,end))))))))))

;; (+ ...)
(define (generate-one-or-more subrules input offset cont)
  (let ((subrule (cdr subrules)))
    (generate-zero-or-more subrules
                           input
                           offset
                           (lambda (rest)
                             (let ((result (gensym 'result)))
                               (cont `(let ((,result ,rest))
                                        (if (and (matches? ,result)
                                                 (= 0 (length (match-match ,result))))
                                            (no-match)
                                            ,result))))))))

;; (? ...)
(define (generate-optional subrules input offset cont)
  (generate-rule-pattern (cdr subrules)
                         input
                         offset
                         (lambda (r)
                           (let ((result (gensym 'result)))
                             (cont `(let ((,result ,r))
                                      (if (matches? ,result)
                                          ,result
                                          (matches '() ,offset ,offset))))))))

;; (! ...)
(define (generate-not subrules input offset cont)
  (generate-rule-pattern (cdr subrules)
                         input
                         offset
                         (lambda (result)
                           (cont `(if (matches? ,result)
                                      (no-match)
                                      (matches '() ,offset ,offset))))))

;; (& ...)
(define (generate-and subrules input offset cont)
  (generate-rule-pattern (cdr subrules)
                         input
                         offset
                         (lambda (r)
                           (let ((result (gensym 'result)))
                             (cont `(let ((,result ,r))
                                      (if (matches? ,result)
                                          ;; NOTE Doesn't advance the scan.
                                          (matches (match-match ,result) ,offset ,offset)
                                          (no-match))))))))

;; (: ...)
(define (generate-drop subrules input offset cont)
  (generate-rule-pattern (cdr subrules)
                         input
                         offset
                         (lambda (r)
                           (let ((result (gensym 'result))
                                 (end (gensym 'end)))
                             (cont `(let ((,result ,r))
                                      (if (matches? ,result)
                                          ;; NOTE Skips the scan.
                                          (let ((,end (match-end ,result)))
                                            (matches '() ,end ,end))
                                          (no-match))))))))

;; (~ ...)
(define (generate-concat subrules input offset cont)
  (generate-rule-pattern (cdr subrules)
                         input
                         offset
                         (lambda (r)
                           (let ((result (gensym 'result)))
                             (cont `(let ((,result ,r))
                                      (if (matches? ,result)
                                          ;; FIXME This produces a hard-to debug error when a match is not a list of strings.
                                          (matches (foldr string-append-immutable "" (match-match ,result))
                                                   (match-start ,result)
                                                   (match-end ,result))
                                          (no-match))))))))
