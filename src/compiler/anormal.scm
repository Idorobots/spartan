;; A-normal form conversion.
;; Assumes syntax & macro-expanded code.

(load "compiler/ast.scm")
(load "compiler/utils.scm")

(define (normalize expr kont)
  (let ((n (flip normalize id)))
    (cond ((symbol? expr) (kont expr))
          ((number? expr) (kont expr))
          ((string? expr) (kont expr))
          ((vector? expr) (kont expr))
          ((nil? expr) (kont expr))
          ((char? expr) (kont expr))
          ((quote? expr) (kont expr))
          ((lambda? expr) (kont (make-lambda (lambda-args expr)
                                             (n (lambda-body expr)))))
          ((application? expr) (normalize-named (app-op expr)
                                                (lambda (op)
                                                  (normalize-sequence (app-args expr)
                                                                      (lambda (args)
                                                                        (kont (make-app op args)))))))
          ((if? expr) (normalize-named (if-predicate expr)
                                       (lambda (p)
                                         (make-if p
                                                  (n (if-then expr))
                                                  (n (if-else expr))))))
          ((letrec? expr) (kont (make-letrec (map (lambda (b)
                                                    (list (car b) (n (cadr b))))
                                                  (letrec-bindings expr))
                                             (n (letrec-body expr)))))
          ((fix? expr) (kont (make-fix (map (lambda (b)
                                                    (list (car b) (n (cadr b))))
                                                  (fix-bindings expr))
                                             (n (fix-body expr)))))
          ;; These should disappear in this phase.
          ((do? expr) (normalize-sequence (do-statements expr)
                                          (lambda (sts)
                                            (kont (last sts)))))
          ;; These shouldn't be here.
          ((define? expr) (kont (make-val-define (define-name expr)
                                                 (n (define-value expr)))))
          ((let? expr) (kont (make-let (map (lambda (b)
                                              (list (car b) (n (cadr b))))
                                            (let-bindings expr))
                                       (n (let-body expr)))))
          ;; --
          ('else (error "Unexpected expression: " expr)))))

(define (normalize-named expr kont)
  (normalize expr (lambda (normalized)
                    (if (atomic? normalized)
                        (kont normalized)
                        (let ((temp (gensym 'temp)))
                          (make-let-1 temp normalized
                                      (kont temp)))))))

(define (normalize-sequence exprs kont)
  (if (empty? exprs)
      (kont '())
      (normalize-named (car exprs)
                       (lambda (temp)
                         (normalize-sequence (cdr exprs)
                                             (lambda (rest)
                                               (kont (cons temp rest))))))))

(define (atomic? expr)
  (foldl (lambda (predicate is-atomic)
           (or is-atomic (predicate expr)))
         #f
         (list symbol?
               number?
               string?
               vector?
               nil?
               char?
               quote?)))
