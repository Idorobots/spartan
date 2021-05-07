;; Lambda inlining.

(load-once "compiler/utils/utils.scm")
(load-once "compiler/substitute.scm")

(load-once "compiler/env.scm")
(load-once "compiler/pass.scm")
(load-once "compiler/ast.scm")

(define inline-lambdas
  (pass (schema "inline-lambdas"
                'globals a-list?
                'ast (ast-subset? '(const symbol if do let letrec fix binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast (partial lambda-inlining '())))))

(define (lambda-inlining lambdas expr)
  (let ((loop (partial walk-ast (partial lambda-inlining lambdas))))
    (ast-case
     expr
     ;; Beta reduction
     ((app (lambda ,formals ,body) . ,args)
      (if (equal? (length formals)
                  (length args))
          (replace expr
                   (beta-reduce formals
                                (map loop args)
                                (loop body)))
          (loop expr)))
     ;; Actual inlining
     ((app ,op . ,args)
      (if (symbol-node? op)
          (let ((l (assoc (ast-symbol-value op) lambdas)))
            (if l
                (replace expr
                         (beta-reduce (ast-lambda-formals (cdr l))
                             (map loop args)
                             ;; NOTE Can't immediately expand the lambda, since it would never terminate on self-recursive functions.
                             (ast-lambda-body (cdr l))))
                (loop expr)))
          (loop expr)))
     ;; Collect lambdas
     ((let ,bindings ,body)
      (let ((ls (map (lambda (b)
                       (cons (ast-symbol-value (ast-binding-var b))
                             (ast-binding-val b)))
                     (filter (compose lambda-node? ast-binding-val)
                        bindings))))
        (ast-update (ast-update expr 'bindings (partial map loop))
                    'body
                    (partial lambda-inlining (append ls lambdas)))))
     ((letrec ,bindings ,body)
      (let* ((ls (map (lambda (b)
                       (cons (ast-symbol-value (ast-binding-var b))
                             (ast-binding-val b)))
                     (filter (compose lambda-node? ast-binding-val)
                             bindings)))
             (loop (partial lambda-inlining (append ls lambdas))))
        (ast-update (ast-update expr 'body loop)
                    'bindings
                    (partial map loop))))
     ((fix ,bindings ,body)
      (let* ((ls (map (lambda (b)
                       (cons (ast-symbol-value (ast-binding-var b))
                             (ast-binding-val b)))
                     bindings))
             (loop (partial lambda-inlining (append ls lambdas))))
        (ast-update (ast-update expr 'body loop)
                    'bindings
                    (partial map loop))))
     (else
      (loop expr)))))

(define (beta-reduce formals args body)
  (generated
   (make-let-node (map (lambda (var val)
                         (at (get-location val)
                             (generated
                              (make-binding-node var val))))
                       formals
                       args)
                  body)))
