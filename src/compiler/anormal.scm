;; A-normal form conversion.
;; Assumes syntax & macro-expanded code.

(load "compiler/ast.scm")
(load "compiler/utils/utils.scm")
(load "compiler/utils/gensym.scm")

(define (normalize expr kont)
  (let ((n (flip normalize id)))
    (cond ((atomic? expr) (kont expr))
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
          ((let? expr) (kont (make-let (map (lambda (b)
                                              (list (car b) (n (cadr b))))
                                            (let-bindings expr))
                                       (n (let-body expr)))))
          ((fix? expr) (kont (make-fix (map (lambda (b)
                                                    (list (car b) (n (cadr b))))
                                                  (fix-bindings expr))
                                             (n (fix-body expr)))))
          ;; These should disappear in this phase.
          ((do? expr) (normalize-sequence (do-statements expr)
                                          (lambda (sts)
                                            (kont (last sts)))))
          ;; --
          ('else (error "Unexpected expression: " expr)))))

(define (normalize-named expr kont)
  (normalize expr
             (lambda (normalized)
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
  (or (simple? expr)
      (symbol? expr)))
