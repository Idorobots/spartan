#lang racket

;; Lambda inlining.

(require "../utils/utils.rkt")
(require "../utils/gensym.rkt")
(require "../utils/set.rkt")

(require "../substitute.rkt")
(require (only-in "../propagate.rkt"
                  reconstruct-let-node))
(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")

(provide inline-lambdas
         ;; FIXME For test access.
         lambda-inlining +max-inlineable-size+ temporary-name)

(define inline-lambdas
  (pass (schema "inline-lambdas"
                'ast (ast-subset? '(const symbol if do let letrec fix binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast (partial lambda-inlining '())))
        (schema "inline-lambdas output"
                'ast (ast-subset? '(const symbol if do let letrec fix binding lambda app primop-app)))))

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

;; NOTE Max "depth" of a lambda expression to consider during inlining.
(define +max-inlineable-size+ 23)
;; NOTE Recursive functions smaller than this much won't be inlined,
;; NOTE as the introduced continuations are more expensive than the self call.
(define +max-inlineable-size-for-recursive+ 5)

;; NOTE Currently only &current-continuation is lexically dependant.
;; NOTE It needs to be compiled in the scope of the original function that introduced the primop.
(define +lexically-dependant-primops+ '(&current-continuation))

(define (suitable-lambda? b)
  (let ((size (ast-size b)))
    (and (<= size +max-inlineable-size+)
         (not (and (<= size +max-inlineable-size-for-recursive+)
                   (ast-binding-self-recursive b)))
         (not (ast-contains? (lambda (e)
                               (match-ast e
                                          ((primop-app op args ...)
                                           #:when (member op +lexically-dependant-primops+)
                                           #t)
                                          (else
                                           #f)))
                             b)))))
