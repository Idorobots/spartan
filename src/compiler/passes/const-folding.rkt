#lang racket

;; Constant propagation.

(require "../utils/utils.rkt")

(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")

(provide fold-constants
         ;; FIXME For test access.
         constant-folding)

(define fold-constants
  (pass (schema "fold-constants"
                'ast (ast-subset? '(const symbol if do let letrec fix binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast constant-folding))))

(define (constant-folding expr)
  (map-ast (lambda (expr)
             (match-ast expr
              ;; List operations
              ((primop-app 'car (const (list first rest ...)))
               (emit-const expr first))
              ((primop-app 'cdr (const (list _ rest ...)))
               (emit-const expr
                           (generated
                            (make-ast-list (ast-node-location expr) rest))))
              ((primop-app 'cadr (const (list _ second rest ...)))
               (emit-const expr second))
              ((primop-app 'cddr (const (list _ _ rest ...)))
               (emit-const expr
                           (generated
                            (make-ast-list (ast-node-location expr) rest))))
              ((primop-app 'list (const (list values ...)))
               (emit-const expr
                           (generated
                            (make-ast-list (ast-node-location expr) values))))
              ((primop-app 'cons (const first) (const (list rest ...)))
               (emit-const expr
                           (generated
                            (make-ast-list (ast-node-location expr) (cons first rest)))))
              ((primop-app 'cons (primop-app 'car (symbol x)) (primop-app 'cdr (symbol x)))
               ;; NOTE Making it an artificial node as it replaces a whole expression.
               (generated
                (make-ast-symbol (ast-node-location expr)
                                 x)))
              ;; List predicates
              ((primop-app 'nil? (const (list args ...)))
               (emit-bool expr
                          (if (null? args)
                              'true
                              'false)))
              ((primop-app 'empty? (const (list args ...)))
               (emit-bool expr
                          (if (empty? args)
                              'true
                              'false)))
              ;; Basic math operations
              ((primop-app '* (const (number a)) (const (number b)))
               (emit-const expr
                           (generated
                            (make-ast-number (ast-node-location expr)
                                             (* a b)))))
              ((primop-app '+ (const (number a)) (const (number b)))
               (emit-const expr
                           (generated
                            (make-ast-number (ast-node-location expr)
                                             (+ a b)))))
              ((primop-app '/ (const (number a)) (const (number b)))
               (emit-const expr
                           (generated
                            (make-ast-number (ast-node-location expr)
                                             (/ a b)))))
              ((primop-app '- (const (number a)) (const (number b)))
               (emit-const expr
                           (generated
                            (make-ast-number (ast-node-location expr)
                                             (- a b)))))
              ((primop-app 'quotient (const (number a)) (const (number b)))
               (emit-const expr
                           (generated
                            (make-ast-number (ast-node-location expr)
                                             (quotient a b)))))
              ((primop-app 'remainder (const (number a)) (const (number b)))
               (emit-const expr
                           (generated
                            (make-ast-number (ast-node-location expr)
                                             (remainder a b)))))
              ((primop-app 'modulo (const (number a)) (const (number b)))
               (emit-const expr
                           (generated
                            (make-ast-number (ast-node-location expr)
                                             (modulo a b)))))
              ;; Math predicates
              ((primop-app 'zero? (const (number a)))
               (emit-bool expr
                          (if (zero? a)
                              'true
                              'false)))
              ((primop-app '= (const (number a)) (const (number b)))
               (emit-bool expr
                          (if (= a b)
                              'true
                              'false)))
              ((primop-app '> (const (number a)) (const (number b)))
               (emit-bool expr
                          (if (> a b)
                              'true
                              'false)))
              ((primop-app '>= (const (number a)) (const (number b)))
               (emit-bool expr
                          (if (>= a b)
                              'true
                              'false)))
              ((primop-app '< (const (number a)) (const (number b)))
               (emit-bool expr
                          (if (< a b)
                              'true
                              'false)))
              ((primop-app '<= (const (number a)) (const (number b)))
               (emit-bool expr
                          (if (<= a b)
                              'true
                              'false)))
              ;; Other functions
              ((primop-app 'not (symbol x))
               (case x
                 ((true false)
                  (emit-bool expr
                             (if (equal? x 'false)
                                 'true
                                 'false)))
                 (else
                  expr)))
              ;; Other predicates
              ((primop-app 'equal? (const a) (const b))
               (emit-bool expr
                          (if (ast-eqv? a b)
                              'true
                              'false)))
              ;; append, concat & boolean returning values.
              (else
               ;; NOTE If anything seems funny about this app, we leave it up for the runtime to break.
               expr)))
           expr))

(define (emit-const expr new-value)
  (make-ast-const (ast-node-location expr)
                  new-value))

(define (emit-bool expr value)
  (generated
   (make-ast-symbol (ast-node-location expr)
                    value)))
