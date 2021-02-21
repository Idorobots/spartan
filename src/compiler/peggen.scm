;; PEG parser source generator

(load "compiler/utils.scm")
(load "compiler/peg.scm")

;; Parser generator
(define (generate-parser . rules)
  (let ((top-name (caar rules))
        (hash (gensym 'hash))
        (input (gensym 'input)))
    `(define ,top-name
       (letrec ,(map generate-rule rules)
         (lambda (,input)
           (let ((,hash (* (eq-hash-code ,input)
                           (string-length ,input))))
             (,top-name ,hash ,input 0)))))))

(define (generate-rule rule)
  (let ((name (car rule))
        (pattern (cadr rule))
        (transform (cddr rule))
        (hash (gensym 'hash))
        (input (gensym 'input))
        (offset (gensym 'offset)))
    `(,name
      (memoize-input
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

(define (generate-rule-pattern rule hash input offset cont)
  (cond ((nonterminal? rule)      (generate-nonterminal rule hash input offset cont))
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

;; Nonterminal
(define (generate-nonterminal rule-name hash input offset cont)
  (cont `(,rule-name ,hash ,input ,offset)))

;; "terminal"
(define (generate-matcher regex hash input offset cont)
  (cont (if (equal? 1 (string-length regex))
            (let ((char (string-ref regex 0)))
              `(if (equal? (string-ref ,input ,offset) ,char)
                   (matches ,regex ,offset (+ 1 ,offset))
                   (no-match)))
            (let ((r (string-append-immutable "^" regex))
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
                                          (matches (foldl string-append-immutable "" (match-match ,result))
                                                   (match-start ,result)
                                                   (match-end ,result))
                                          (no-match))))))))
