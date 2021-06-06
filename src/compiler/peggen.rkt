#lang racket

;; PEG parser source generator

(require "utils/gensym.rkt")
(require "utils/io.rkt")
(require "utils/utils.rkt")

(provide match-result no-match matches matches? match-match match-start match-end
         memoize-input eq-len-hash-input
         generate-rkt-parser
         ;; FIXME These are exported for test access only.
         generate-scm-parser
         generate-eof generate-nonterminal generate-matcher generate-sequence generate-or generate-zero-or-more
         generate-one-or-more generate-optional generate-not generate-and generate-drop generate-concat)

;; Some optimization
(define (memoize-input previous-runs f)
  (lambda (hash input offset)
    (hash-ref! previous-runs
               (+ hash offset)
               (lambda ()
                 (f hash input offset)))))

;; Matches
(define (no-match)
  '())

(define-struct match-result (result start end) #:transparent)

(define (matches m start end)
  (make-match-result m start end))

(define matches? match-result?)

(define (match-match m)
  (match-result-result m))

(define (match-start m)
  (match-result-start m))

(define (match-end m)
  (match-result-end m))

;; Parser generator
(define (generate-rkt-parser filename imports . rules)
  (let ((top-name (caar rules)))
    (with-output-to-file filename
      (lambda ()
        (display "#lang racket")
        (newline)
        (display ";; THIS CODE IS GENERATED, MODIFYING IT IS FUTILE.")
        (newline)
        (for-each (lambda (i)
                    (pretty-write `(require ,i)))
                  imports)
        (pretty-write `(provide ,top-name))
        (pretty-write (generate-grammar rules)))
      #:exists 'replace)))

(define (generate-scm-parser filename . rules)
  (spit filename (generate-grammar rules)))

(define (generate-grammar rules)
  (let* ((top-name (caar rules))
         (hash (gensym 'hash))
         (input (gensym 'input))
         (inlined (inline-rules rules))
         (caches (map (lambda (_) (gensym 'cache)) inlined)))
    `(define ,top-name
       (let ,(map generate-cache caches)
         (letrec ,(map generate-rule inlined caches)
           (lambda (,input hash-input)
             ,@(map clear-cache caches)
             ;; FIXME No need to hash the input and pass it on to the rules.
             (let ((,hash (hash-input ,input)))
               (,top-name ,hash ,input 0))))))))

(define (eq-len-hash-input input)
  (* (eq-hash-code input)
     (string-length input)))

(define +peg-inline-loops+ 5)

(define (inline-rules rules)
  (define (inline-rules-once rules)
    (let* ((inlineable (filter (lambda (rule)
                                 (equal? 2 (length rule)))
                               rules)))
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
               (cdr r)
               pattern)))
        (else
         pattern)))

(define (generate-cache cache)
  `(,cache (make-hasheq)))

(define (clear-cache cache)
  `(hash-clear! ,cache))

(define (generate-rule rule cache)
  (let ((name (car rule))
        (pattern (cadr rule))
        (transform (cddr rule))
        (hash (gensym 'hash))
        (input (gensym 'input))
        (offset (gensym 'offset)))
    `(,name
      (memoize-input ,cache
       (lambda (,hash ,input ,offset)
         ,(generate-rule-pattern pattern
                                 hash
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

(define (generate-rule-pattern rule hash input offset cont)
  (cond ((empty? rule)            (generate-eof rule hash input offset cont))
        ((nonterminal? rule)      (generate-nonterminal rule hash input offset cont))
        ((terminal? rule)         (generate-matcher rule hash input offset cont))
        ((equal? (length rule) 1) (generate-rule-pattern (car rule) hash input offset cont))
        ((tagged-list? '/ rule)   (generate-or rule hash input offset cont))
        ((tagged-list? '* rule)   (generate-zero-or-more rule hash input offset cont))
        ((tagged-list? '+ rule)   (generate-one-or-more rule hash input offset cont))
        ((tagged-list? ': rule)   (generate-drop rule hash input offset cont))
        ((tagged-list? '? rule)   (generate-optional rule hash input offset cont))
        ((tagged-list? '! rule)   (generate-not rule hash input offset cont))
        ((tagged-list? '& rule)   (generate-and rule hash input offset cont))
        ((tagged-list? '~ rule)   (generate-concat rule hash input offset cont))
        (else                     (generate-sequence rule hash input offset cont))))

;; EOF
(define (generate-eof rule hash input offset cont)
  (cont `(if (equal? ,offset (string-length ,input))
             (matches '() ,offset ,offset)
             (no-match))))

;; Nonterminal
(define (generate-nonterminal rule-name hash input offset cont)
  (cont `(,rule-name ,hash ,input ,offset)))

;; "terminal"
(define (generate-matcher regex hash input offset cont)
  (cont (if (equal? 1 (string-length regex)) ;; FIXME Won't work for "." and other single char regexps.
            (let ((char (string-ref regex 0)))
              `(if (and (< ,offset (string-length ,input))
                        (equal? (string-ref ,input ,offset) ,char))
                   (matches ,regex ,offset (+ 1 ,offset))
                   (no-match)))
            (let ((r (regexp (string-append-immutable "^" regex)))
                  (result (gensym 'result)))
              `(let ((,result (regexp-match ,r ,input ,offset)))
                 (if ,result
                     (matches (car ,result)
                              ,offset
                              (+ ,offset (string-length (car ,result))))
                     (no-match)))))))

;; (...)
(define (generate-sequence subrules hash input offset cont)
  (cont (let loop ((subrules subrules)
                   (matches '())
                   (last-end offset))
          (if (empty? subrules)
              `(matches ,(cons 'list (reverse matches))
                        ,offset
                        ,last-end)
              (let ((subrule (car subrules))
                    (result (gensym 'result))
                    (mat (gensym 'match))
                    (end (gensym 'end)))
                (generate-rule-pattern subrule
                                       hash
                                       input
                                       last-end
                                       (lambda (r)
                                         `(let ((,result ,r))
                                            (if (matches? ,result)
                                                (let ((,mat (match-match ,result))
                                                      (,end (match-end ,result)))
                                                  ,(loop (cdr subrules)
                                                         (cons mat matches)
                                                         end))
                                                (no-match))))))))))

;; (/ ...)
(define (generate-or subrules hash input offset cont)
  (cont (foldr (lambda (subrule acc)
                 (let ((result (gensym 'result)))
                   (generate-rule-pattern subrule
                                          hash
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
(define (generate-zero-or-more subrules hash input offset cont)
  (let ((subrule (cdr subrules))
        (result (gensym 'result))
        (end (gensym 'end))
        (matches (gensym 'matches))
        (loop (gensym 'loop)))
    (cont `(let ,loop ((,matches '())
                       (,end ,offset))
                ,(generate-rule-pattern subrule
                                        hash
                                        input
                                        end
                                        (lambda (r)
                                          `(let ((,result ,r))
                                             (if (matches? ,result)
                                                 (,loop (cons (match-match ,result)
                                                              ,matches)
                                                        (match-end ,result))
                                                 (matches (reverse ,matches) ,offset ,end)))))))))

;; (+ ...)
(define (generate-one-or-more subrules hash input offset cont)
  (let ((subrule (cdr subrules)))
    (generate-zero-or-more subrules
                           hash
                           input
                           offset
                           (lambda (rest)
                             (generate-rule-pattern subrule
                                                    hash
                                                    input
                                                    offset
                                                    (lambda (first)
                                                      ;; NOTE Ignores the result and relies on (* ...) to match it again.
                                                      ;; FIXME Might be a bit slow at times.
                                                      (cont `(if (matches? ,first)
                                                                 ,rest
                                                                 (no-match)))))))))

;; (? ...)
(define (generate-optional subrules hash input offset cont)
  (generate-rule-pattern (cdr subrules)
                         hash
                         input
                         offset
                         (lambda (r)
                           (let ((result (gensym 'result)))
                             (cont `(let ((,result ,r))
                                      (if (matches? ,result)
                                          ,result
                                          (matches '() ,offset ,offset))))))))

;; (! ...)
(define (generate-not subrules hash input offset cont)
  (generate-rule-pattern (cdr subrules)
                         hash
                         input
                         offset
                         (lambda (result)
                           (cont `(if (matches? ,result)
                                      (no-match)
                                      (matches '() ,offset ,offset))))))

;; (& ...)
(define (generate-and subrules hash input offset cont)
  (generate-rule-pattern (cdr subrules)
                         hash
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
(define (generate-drop subrules hash input offset cont)
  (generate-rule-pattern (cdr subrules)
                         hash
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
(define (generate-concat subrules hash input offset cont)
  (generate-rule-pattern (cdr subrules)
                         hash
                         input
                         offset
                         (lambda (r)
                           (let ((result (gensym 'result)))
                             (cont `(let ((,result ,r))
                                      (if (matches? ,result)
                                          (matches (foldr string-append-immutable "" (match-match ,result))
                                                   (match-start ,result)
                                                   (match-end ,result))
                                          (no-match))))))))
