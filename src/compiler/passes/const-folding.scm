;; Constant propagation.

(load-once "compiler/utils/utils.scm")

(load-once "compiler/env.scm")
(load-once "compiler/pass.scm")
(load-once "compiler/ast.scm")

(define fold-constants
  (pass (schema "fold-constants"
                'ast (ast-subset? '(const symbol if do let letrec fix binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast constant-folding))))

(define (constant-folding expr)
  (map-ast (lambda (expr)
             (match-ast expr
              ((primop-app 'car (const (list first rest ...)))
               (replace expr
                        (make-ast-const (ast-node-location expr)
                                        first)))
              ((primop-app 'cdr (const (list _ rest ...)))
               (replace expr
                        (make-ast-const (ast-node-location expr)
                                        (generated
                                         (make-ast-list (ast-node-location expr) rest)))))
              ((primop-app 'cadr (const (list _ second rest ...)))
               (replace expr
                        (make-ast-const (ast-node-location expr)
                                        second)))
              ((primop-app 'cddr (const (list _ _ rest ...)))
               (replace expr
                        (make-ast-const (ast-node-location expr)
                                        (generated
                                         (make-ast-list (ast-node-location expr) rest)))))
              ((primop-app 'list (const (list values ...)))
               (replace expr
                        (make-ast-const (ast-node-location expr)
                                        (generated
                                         (make-ast-list (ast-node-location expr) values)))))
              ((primop-app 'cons (const first) (const (list rest ...)))
               (replace expr
                        (make-ast-const (ast-node-location expr)
                                        (generated
                                         (make-ast-list (ast-node-location expr) (cons first rest))))))
              ((primop-app '* (const (number a)) (const (number b)))
               (replace expr
                        (make-ast-const (ast-node-location expr)
                                        (generated
                                         (make-ast-number (ast-node-location expr)
                                                          (* a b))))))
              ((primop-app '+ (const (number a)) (const (number b)))
               (replace expr
                        (make-ast-const (ast-node-location expr)
                                        (generated
                                         (make-ast-number (ast-node-location expr)
                                                          (+ a b))))))
              ((primop-app '/ (const (number a)) (const (number b)))
               (replace expr
                        (make-ast-const (ast-node-location expr)
                                        (generated
                                         (make-ast-number (ast-node-location expr)
                                                          (/ a b))))))
              ((primop-app '- (const (number a)) (const (number b)))
               (replace expr
                        (make-ast-const (ast-node-location expr)
                                        (generated
                                         (make-ast-number (ast-node-location expr)
                                                          (- a b))))))
              ;; TODO
              ;; append, concat & boolean returning values.
              (else
               ;; NOTE If anything seems funny about this app, we leave it up for the runtime to break.
               expr)))
           expr))
