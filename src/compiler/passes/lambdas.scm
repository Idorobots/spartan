;; Lambda inlining.

(load-once "compiler/utils/utils.scm")
(load-once "compiler/utils/gensym.scm")

(load-once "compiler/substitute.scm")
(load-once "compiler/propagate.scm") ;; FIXME For reconstruct-*-node
(load-once "compiler/env.scm")
(load-once "compiler/pass.scm")
(load-once "compiler/ast.scm")

(define inline-lambdas
  (pass (schema "inline-lambdas"
                'ast (ast-subset? '(const symbol if do let letrec fix binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast (partial lambda-inlining '())))))

(define (lambda-inlining lambdas expr)
  (let ((loop (partial walk-ast (partial lambda-inlining lambdas))))
    (ast-case expr
     ;; Beta reduction
     ((app (lambda ,formals ,body) . ,args)
      (if (equal? (length formals)
                  (length args))
          (beta-reduce expr
                       formals
                       (map loop args)
                       (loop body))
          (loop expr)))
     ;; Actual inlining
     ((app (symbol ,op) . ,args)
      (let ((l (assoc (ast-symbol-value op) lambdas)))
        (if l
            (let ((formals (ast-lambda-formals (cdr l))))
              (if (equal? (length formals)
                          (length args))
                  (beta-reduce expr
                               formals
                               (map loop args)
                               ;; NOTE Can't immediately expand the lambda, since it would never terminate on self-recursive functions.
                               (ast-lambda-body (cdr l)))
                  (loop expr)))
            (loop expr))))
     ;; Collect lambdas
     ((let ,bindings ,body)
      (let ((ls (map (lambda (b)
                       (cons (ast-symbol-value (ast-binding-var b))
                             (ast-binding-val b)))
                     (filter suitable-lambda?
                             (filter (compose lambda-node? ast-binding-val)
                                     bindings))))
            (lambdas (filter-lambdas lambdas (ast-node-bound-vars expr))))
        (ast-update (ast-update expr 'bindings (partial map loop))
                    'body
                    (partial lambda-inlining (append ls lambdas)))))
     ((letrec ,bindings ,body)
      (let* ((ls (map (lambda (b)
                        (cons (ast-symbol-value (ast-binding-var b))
                              (ast-binding-val b)))
                      (filter suitable-lambda?
                              (filter (compose lambda-node? ast-binding-val)
                                      bindings))))
             (lambdas (filter-lambdas lambdas (ast-node-bound-vars expr)))
             (loop (partial lambda-inlining (append ls lambdas))))
        (ast-update (ast-update expr 'body loop)
                    'bindings
                    (partial map loop))))
     ((fix ,bindings ,body)
      (let* ((ls (map (lambda (b)
                        (cons (ast-symbol-value (ast-binding-var b))
                              (ast-binding-val b)))
                      (filter suitable-lambda?
                              bindings)))
             (lambdas (filter-lambdas lambdas (ast-node-bound-vars expr)))
             (loop (partial lambda-inlining (append ls lambdas))))
        (ast-update (ast-update expr 'body loop)
                    'bindings
                    (partial map loop))))
     (else
      (loop expr)))))

(define (filter-lambdas lambdas bound-vars)
  (filter (lambda (l)
            (set-empty? (set-intersection (ast-node-free-vars (cdr l))
                                          bound-vars)))
          lambdas))

(define (beta-reduce original formals args body)
  (let* ((renamed (map temporary-name formals))
         (subs (make-subs
                (map (lambda (formal tmp)
                       (cons (ast-symbol-value formal)
                             (lambda (orig)
                               (at (ast-node-location orig)
                                   tmp))))
                     formals
                     renamed))))
    (reconstruct-let-node original
                          (map (lambda (var val)
                                 (at (ast-node-location val)
                                     (generated
                                      (make-binding-node var val))))
                               renamed
                               args)
                          (substitute-symbols subs body))))

(define (temporary-name original)
  (at (ast-node-location original)
      (generated
       (make-symbol-node
        (gensym (ast-symbol-value original))))))

(define +max-inlineable-size+ 10)

(define (suitable-lambda? b)
  (< (ast-size b) +max-inlineable-size+))
