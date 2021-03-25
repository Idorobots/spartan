;; Free vars computation

(load "compiler/utils/set.scm")
(load "compiler/utils/utils.scm")

(load "compiler/tree-ast.scm")

(define (annotate-free-vars env)
  (env-update env 'ast compute-free-vars))

(define (compute-free-vars expr)
  (map-ast id
           (lambda (expr)
             (case (get-type expr)
               ((do)
                (free-vars
                 (set-sum (map get-fv (ast-do-exprs expr)))
                 expr))
               ((if)
                (free-vars
                 (set-sum
                  (list (get-fv (ast-if-condition expr))
                        (get-fv (ast-if-then expr))
                        (get-fv (ast-if-else expr))))
                 expr))
               ((lambda)
                (let ((bound (set-sum (map get-fv (ast-lambda-formals expr)))))
                  (free-vars
                   (set-difference (get-fv (ast-lambda-body expr))
                                   bound)
                   (bound-vars bound
                               expr))))
               ((let)
                (compute-let-fv expr))
               ((letrec)
                (compute-letrec-fv expr))
               ((app)
                (free-vars
                 (set-union (get-fv (ast-app-op expr))
                            (set-sum (map get-fv (ast-app-args expr))))expr))
               ((primop-app)
                (free-vars
                 (set-sum (map get-fv (ast-primop-app-args expr)))
                 expr))
               (else
                expr)))
           expr))

(define (compute-let-fv expr)
  (let* ((bindings (ast-let-bindings expr))
         (bound (set-sum (map (compose get-fv ast-binding-var) bindings)))
         (free-in-bindings (set-sum (map (compose get-fv ast-binding-val) bindings)))
         (free-in-body (get-fv (ast-let-body expr))))
    (free-vars
     (set-union free-in-bindings
                (set-difference free-in-body
                                bound))
     (bound-vars bound
                 expr))))

(define (compute-letrec-fv expr)
  (let* ((bindings (ast-letrec-bindings expr))
         (bound (set-sum (map (compose get-fv ast-binding-var) bindings)))
         (free-in-bindings (set-sum (map (compose get-fv ast-binding-val) bindings)))
         (free-in-body (get-fv (ast-letrec-body expr))))
    (free-vars
     (set-difference (set-union free-in-bindings free-in-body)
                     bound)
     (bound-vars bound
                 expr))))

(define (get-fv expr)
  ;; NOTE Symbols always are their own free var, no need to store that in the AST.
  (if (symbol-node? expr)
      (set (ast-symbol-value expr))
      (get-free-vars expr)))

(load "compiler/ast.scm")

;; FIXME This ought to be a separate AST annotation phase.
;; FIXME Rewrite in terms of ast/walk.
(define (free-vars-old expr)
  (cond ((symbol? expr) (set expr))
        ((simple? expr) (set))
        ((lambda? expr) (set-difference (free-vars-old (lambda-body expr))
                                        (apply set (lambda-args expr))))
        ((do? expr) (set-sum (map free-vars-old
                                  (do-statements expr))))
        ((if? expr) (set-sum (list (free-vars-old (if-predicate expr))
                                   (free-vars-old (if-then expr))
                                   (free-vars-old (if-else expr)))))
        ((let? expr) (set-union (set-sum (map (compose free-vars-old cadr)
                                              (let-bindings expr)))
                                (set-difference (free-vars-old (let-body expr))
                                                (set-sum (map (compose free-vars-old car)
                                                              (let-bindings expr))))))
        ((or (letrec? expr)
             (fix? expr)) (set-difference (set-union (set-sum (map (compose free-vars-old cadr)
                                                                   (let-bindings expr)))
                                                     (free-vars-old (let-body expr)))
                                          (set-sum (map (compose free-vars-old car)
                                                        (let-bindings expr)))))
        ((application? expr) (set-union (free-vars-old (app-op expr))
                                        (foldl set-union
                                               (set)
                                               (map free-vars-old
                                                    (app-args expr)))))
        (else (error "Unexpected expression:" expr))))
