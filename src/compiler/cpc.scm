;; Continuation Passing Converter
;; Assumes macro- & letrec-expanded code. This phase contorts the code so much that it invalidates free-vars & bindings annotations.


(load "compiler/utils/gensym.scm")
(load "compiler/utils/utils.scm")

(load "compiler/env.scm")

(define (continuation-passing-convert env)
  (env-update env 'ast (flip cpc (make-identity-continuation))))

(define (make-identity-continuation)
  id)

(define (cpc expr kont)
  (case (get-type expr)
    ((symbol number string quote) (kont expr))
    ((if) (cpc-if expr kont))
    ((do) (cpc-do expr kont))
    ((let) (cpc-let expr kont))
    ((fix) (cpc-fix expr kont))
    ((lambda) (cpc-lambda expr kont))
    ((primop-app) (cpc-primop-app expr kont))
    ((app) (cpc-app expr kont))
    (else (compiler-bug "Unexpected expression passed to cpc:" expr))))

(define (cpc-if expr kont)
  (let* ((loc (get-location expr))
         (ct (at loc (make-gensym-node 'cont)))
         (value (at loc (make-gensym-node 'value)))
         (rest (lambda (v)
                 (at loc
                     (make-yield-node ct v)))))
    (cpc (ast-if-condition expr)
         (lambda (condition)
           (at loc
               (make-let-1-node ct
                                (at loc
                                    (make-cont-node value (kont value)))
                                (replace expr
                                         (make-if-node condition
                                                       (cpc (ast-if-then expr) rest)
                                                       (cpc (ast-if-else expr) rest)))))))))

(define (make-let-1-node var val body)
  (generated
   (make-let-node (list (at (get-location var)
                            (generated
                             (make-binding-node var val))))
                  body)))

(define (make-cont-node arg body)
  (generated
   (make-lambda-node (list arg) body)))

(define (make-gensym-node root)
  (generated
   (make-symbol-node (gensym root))))

(define (make-yield-node cont hole)
  (generated
   (make-primop-app-node
    (at (get-location cont)
        (generated
         (make-symbol-node '&yield-cont)))
    (list cont hole))))

(define (cpc-do expr kont)
  (cpc-sequence (ast-do-exprs expr)
                (lambda (exprs)
                  (kont (ast-update expr 'exprs (constantly exprs))))))

(define (cpc-sequence exprs kont)
  (if (> (length exprs) 0)
      (cpc (car exprs)
           (lambda (first)
             (cpc-sequence (cdr exprs)
                           (lambda (rest)
                             (kont (cons first rest))))))
      (kont '())))

(define (cpc-lambda expr kont)
  (let* ((loc (get-location expr))
         (ct (at loc (make-gensym-node 'cont))))
    (kont (replace expr
                   (make-lambda-node (append (ast-lambda-formals expr)
                                             (list ct))
                                     (cpc (ast-lambda-body expr)
                                          (lambda (s)
                                            (at loc
                                                (make-yield-node ct s)))))))))

(define (cpc-primop-app expr kont)
  (let ((args (ast-primop-app-args expr)))
    (cpc-sequence args
                  (compose kont (partial ast-update expr 'args) constantly))))

(define (cpc-app expr kont)
  (let* ((loc (get-location expr))
         (value (at loc (make-gensym-node 'value)))
         (cont (at loc
                   (make-cont-node value (kont value))))
         (args (ast-app-args expr)))
    (cpc (ast-app-op expr)
         (lambda (op)
           (cpc-sequence args
                         (lambda (args)
                           (replace expr
                                    (make-app-node op
                                                   (append args
                                                           (list cont))))))))))

(define (cpc-fix expr kont)
  (replace expr
           (make-fix-node (map (lambda (b)
                                 ;; NOTE These are guaranteed to be pure lambda expressions,
                                 ;; NOTE so no need to propagate the current continuation there.
                                 (ast-update b 'val (flip cpc (make-identity-continuation))))
                               (ast-fix-bindings expr))
                          (cpc (ast-fix-body expr)
                               kont))))

(define (cpc-let expr kont)
  (let ((bindings (ast-let-bindings expr)))
    (cpc-sequence (map ast-binding-val (ast-let-bindings expr))
                  (lambda (vals)
                    (replace expr
                             (make-let-node (map (lambda (var val)
                                                   (make-binding-node var val))
                                                 (map ast-binding-var bindings)
                                                 vals)
                                            (cpc (ast-let-body expr)
                                                 kont)))))))

(load "compiler/ast.scm")

(define (cp-convert expr kont)
  (cond ((symbol? expr) (kont expr))
        ((number? expr) (kont expr))
        ((string? expr) (kont expr))
        ((vector? expr) (kont expr))
        ((nil? expr) (kont expr))
        ((char? expr) (kont expr))
        ((quote? expr) (kont expr))
        ((if? expr) (cp-convert-if expr kont))
        ((do? expr) (cp-convert-do expr kont))
        ((let? expr) (cp-convert-let expr kont))
        ((lambda? expr) (cp-convert-lambda expr kont))
        ((fix? expr) (cp-convert-fix expr kont))
        ((application? expr) (cp-convert-app expr kont))
        ;; --
        ('else (error "Unexpected expression: " expr))))

(define (make-yield expr)
  (cons '&yield-cont expr))

(define (cp-convert-sequence exprs kont)
  (if (empty? exprs)
      (kont nil)
      (cp-convert (first-statement exprs)
                  (lambda (fst)
                    (cp-convert-sequence (rest-statements exprs)
                                         (lambda (rst)
                                           (kont (cons fst rst))))))))

(define (returning-last statements kont)
  (make-do (returning-last* statements kont)))

(define (returning-last* statements kont)
  (if (empty? (rest-statements statements))
      (list (kont (first-statement statements)))
      (cons (first-statement statements)
            (returning-last* (rest-statements statements)
                             kont))))

(define (cp-convert-lambda expr kont)
  (let ((ct (gensym 'cont)))
    (kont (make-lambda
           (append (lambda-args expr) (list ct))
           (cp-convert (lambda-body expr)
                       (lambda (s)
                         (make-yield (make-app-1 ct s))))))))

(define (cp-convert-do expr kont)
  (cp-convert-sequence (do-statements expr)
                       (lambda (sts)
                         (returning-last sts kont))))

(define (cp-convert-if expr kont)
  (let* ((ct (gensym 'cont))
         (rest (lambda (v) (make-yield (make-app-1 ct v))))
         (value (gensym 'value)))
    (cp-convert (if-predicate expr)
                (lambda (condition)
                  (make-let-1 ct (make-lambda-1 value (kont value))
                              (make-if condition
                                       (cp-convert (if-then expr) rest)
                                       (cp-convert (if-else expr) rest)))))))

(define (cp-convert-fix expr kont)
  (make-fix (map (lambda (b)
                   (list (car b)
                         (cp-convert (cadr b)
                                     (make-identity-continuation))))
                 (let-bindings expr))
            (cp-convert (fix-body expr)
                        kont)))

(define (binding-steps names values finally)
  (if (empty? names)
      finally
      (cp-convert (car values)
                  (lambda (v)
                    (make-let-1 (car names) v
                                (binding-steps (cdr names)
                                               (cdr values)
                                               finally))))))

(define (cp-convert-let expr kont)
  (let ((bindings (let-bindings expr)))
    (binding-steps (map car bindings)
                   (map cadr bindings)
                   (cp-convert (let-body expr)
                               kont))))

(define (cp-convert-app expr kont)
  (if (primop-application? expr)
      (kont (make-app (app-op expr)
                      (map (flip cp-convert (make-identity-continuation))
                           (app-args expr))))
      (let ((value (gensym 'value)))
        (cp-convert (app-op expr)
                    (lambda (op)
                      (cp-convert-sequence (app-args expr)
                                           (lambda (args)
                                             (make-app op
                                                       (append args (list (make-lambda-1 value
                                                                                         (kont value))))))))))))
