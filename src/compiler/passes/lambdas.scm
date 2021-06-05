;; Lambda inlining.

(require "../utils/utils.rkt")
(require "../utils/gensym.rkt")

(require "../substitute.rkt")
(load-once "compiler/propagate.scm") ;; FIXME For reconstruct-*-node
(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")

(define inline-lambdas
  (pass (schema "inline-lambdas"
                'ast (ast-subset? '(const symbol if do let letrec fix binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast (partial lambda-inlining '())))))

(define (lambda-inlining lambdas expr)
  (let ((loop (partial traverse-ast lambda-inlining lambdas)))
    (match-ast expr
     ;; Beta reduction
     ((app (lambda formals body) args ...)
      (if (equal? (length formals)
                  (length args))
          (beta-reduce expr
                       formals
                       (map loop args)
                       (loop body))
          (loop expr)))
     ;; Actual inlining
     ((app (symbol op) args ...)
      (let ((l (assoc op lambdas)))
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
     ((let bindings body)
      (let ((ls (map (lambda (b)
                       (cons (ast-symbol-value (ast-binding-var b))
                             (ast-binding-val b)))
                     (filter suitable-lambda?
                             (filter (compose ast-lambda? ast-binding-val)
                                     bindings))))
            (lambdas (filter-lambdas lambdas (ast-node-bound-vars expr))))
        (-> expr
            (set-ast-let-bindings (map loop bindings))
            (set-ast-let-body (lambda-inlining (append ls lambdas) body)))))
     ((letrec bindings body)
      (let* ((ls (map (lambda (b)
                        (cons (ast-symbol-value (ast-binding-var b))
                              (ast-binding-val b)))
                      (filter suitable-lambda?
                              (filter (compose ast-lambda? ast-binding-val)
                                      bindings))))
             (lambdas (filter-lambdas lambdas (ast-node-bound-vars expr)))
             (loop (partial lambda-inlining (append ls lambdas))))
        (-> expr
            (set-ast-letrec-body (loop body))
            (set-ast-letrec-bindings (map loop bindings)))))
     ((fix bindings body)
      (let* ((ls (map (lambda (b)
                        (cons (ast-symbol-value (ast-binding-var b))
                              (ast-binding-val b)))
                      (filter suitable-lambda?
                              bindings)))
             (lambdas (filter-lambdas lambdas (ast-node-bound-vars expr)))
             (loop (partial lambda-inlining (append ls lambdas))))
        (-> expr
            (set-ast-fix-body (loop body))
            (set-ast-fix-bindings (map loop bindings)))))
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
                             (constantly tmp)))
                     formals
                     renamed))))
    (reconstruct-let-node original
                          (map (lambda (var val)
                                 (generated
                                  (make-ast-binding (ast-node-location val) var val)))
                               renamed
                               args)
                          (substitute-symbols subs body))))

(define (temporary-name original)
  (make-ast-gensym (ast-node-location original)
                   (ast-symbol-value original)))

(define +max-inlineable-size+ 15)

(define (suitable-lambda? b)
  (< (ast-size b) +max-inlineable-size+))
