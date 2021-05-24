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
             (ast-case expr
              ((primop-app 'car (const (list ,first . ,rest)))
               (replace expr
                        (make-ast-const first)))
              ((primop-app 'cdr (const (list _ . ,rest)))
               (replace expr
                        (make-ast-const
                         (at (ast-node-location expr)
                             (generated
                              (make-ast-list rest))))))
              ((primop-app 'cadr (const (list _ ,second . ,rest)))
               (replace expr
                        (make-ast-const second)))
              ((primop-app 'cddr (const (list _ _ . ,rest)))
               (replace expr
                        (make-ast-const
                         (at (ast-node-location expr)
                             (generated
                              (make-ast-list rest))))))
              ((primop-app 'list (const (list . ,values)))
               (replace expr
                        (make-ast-const
                         (at (ast-node-location expr)
                             (generated
                              (make-ast-list values))))))
              ((primop-app 'cons (const ,first) (const (list . ,rest)))
               (replace expr
                        (make-ast-const
                         (at (ast-node-location expr)
                             (generated
                              (make-ast-list (cons first rest)))))))
              ((primop-app '* (const (number ,a)) (const (number ,b)))
               (replace expr
                        (make-ast-const
                         (at (ast-node-location expr)
                             (generated
                              (make-ast-number (* (ast-number-value a)
                                                   (ast-number-value b))))))))
              ((primop-app '+ (const (number ,a)) (const (number ,b)))
               (replace expr
                        (make-ast-const
                         (at (ast-node-location expr)
                             (generated
                              (make-ast-number (+ (ast-number-value a)
                                                   (ast-number-value b))))))))
              ((primop-app '/ (const (number ,a)) (const (number ,b)))
               (replace expr
                        (make-ast-const
                         (at (ast-node-location expr)
                             (generated
                              (make-ast-number (/ (ast-number-value a)
                                                   (ast-number-value b))))))))
              ((primop-app '- (const (number ,a)) (const (number ,b)))
               (replace expr
                        (make-ast-const
                         (at (ast-node-location expr)
                             (generated
                              (make-ast-number (- (ast-number-value a)
                                                   (ast-number-value b))))))))
              ;; TODO
              ;; append, concat & boolean returning values.
              (else
               ;; NOTE If anything seems funny about this app, we leave it up for the runtime to break.
               expr)))
           expr))
