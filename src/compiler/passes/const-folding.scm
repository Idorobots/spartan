;; Constant propagation.

(load "compiler/utils/utils.scm")

(load "compiler/env.scm")
(load "compiler/pass.scm")
(load "compiler/ast.scm")

(define fold-constants
  (pass (schema "fold-constants"
                'ast (ast-subset? '(const symbol
                                    if do let letrec binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast constant-folding))))

(define (constant-folding expr)
  (map-ast id
           (lambda (expr)
             (ast-case
              expr
              ((primop-app ,op . ,args)
               (if (every? const-node? args)
                   (fold-builtin expr)
                   expr))
              (else
               expr)))
           expr))

(define (foldable arg-types extractor)
  (cons arg-types extractor))

(define +foldable-builtins+
  (list (cons 'car (foldable (list list-node?)
                             (compose car ast-list-values)))
        (cons 'cdr (foldable (list list-node?)
                             (compose cdr ast-list-values)))
        (cons 'cadr (foldable (list list-node?)
                              (compose cadr ast-list-values)))
        (cons 'cddr (foldable (list list-node?)
                              (compose cddr ast-list-values)))
        ;; TODO
        ;; (cons 'list list)
        ;; (cons 'cons cons)
        ;; (cons 'append append)
        ;; (cons 'concat concat)
        ;; (cons 'nil? nil?)
        ;; (cons 'equal? equal?)
        ;; (cons 'not not)
        ;; (cons '* *)
        ;; (cons '+ +)
        ;; (cons '- -)
        ;; (cons '/ /)
        ;; (cons '= =)
        ;; (cons '< <)
        ;; (cons 'zero? zero?)
  ))

(define (fold-builtin expr)
  (let* ((op (ast-primop-app-op expr))
         (args (map ast-const-value (ast-primop-app-args expr)))
         (f (assoc op +foldable-builtins+)))
    (if (and f
             (equal? (length (cadr f))
                     (length args))
             (every? true?
                     (map (lambda (pred arg)
                            (pred arg))
                          (cadr f)
                          args)))
        (replace expr
                 (generated
                  (make-const-node
                   (apply (cddr f) args))))
        ;; NOTE If anything seems funny about this app, we leave it up for the runtime to break.
        expr)))
