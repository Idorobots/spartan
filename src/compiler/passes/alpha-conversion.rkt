#lang racket

;; Alpha renaming phase

(require "../utils/utils.rkt")
(require "../utils/gensym.rkt")

(require "../substitute.rkt")
(require (only-in "../propagate.rkt"
                  reconstruct-fix-node reconstruct-letrec-node reconstruct-let-node))
(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")

(provide alpha-convert
         ;; FIXME For test access.
         alpha-rename)

(define alpha-convert
  (pass (schema "alpha-convert"
                'ast (ast-subset? '(const symbol if do let letrec fix binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast (partial alpha-rename (make-subs '()))))
        (schema "alpha-convert output"
                'ast (ast-subset? '(const symbol if do let letrec fix binding lambda app primop-app)))))

(define (alpha-rename subs expr)
  (define (loop subs expr)
    (match-ast expr
     ((const _)
      expr)

     ((symbol s)
      (rename-symbol subs expr))

     ((lambda formals body)
      (let* ((filtered-subs (filter-subs subs (ast-node-bound-vars expr)))
             (updated-subs (make-alpha-subs formals
                                            filtered-subs)))
        (make-ast-lambda (ast-node-location expr)
                         (map (partial rename-symbol updated-subs) formals)
                         (loop updated-subs body))))

     ((binding _ val)
      (set-ast-binding-val expr (loop subs val)))

     ((let bindings body)
      (let* ((filtered-subs (filter-subs subs (ast-node-bound-vars expr)))
             (updated-subs (make-alpha-subs (map ast-binding-var bindings) filtered-subs)))
        (reconstruct-let-node expr
                              (map (lambda (b)
                                     (set-ast-binding-var b
                                                          (rename-symbol updated-subs
                                                                         (ast-binding-var b))))
                                   (map (partial loop subs)
                                        (map (partial rename-binding updated-subs)
                                             bindings)))
                              (loop updated-subs body))))

     ((letrec bindings body)
      (let* ((filtered-subs (filter-subs subs (ast-node-bound-vars expr)))
             (updated-subs (make-alpha-subs (map ast-binding-var bindings) filtered-subs)))
        (reconstruct-letrec-node expr
                                 (map (partial loop updated-subs)
                                      (map (partial rename-binding updated-subs)
                                           bindings))
                                 (loop updated-subs body))))

     ((fix bindings body)
      (let* ((filtered-subs (filter-subs subs (ast-node-bound-vars expr)))
             (updated-subs (make-alpha-subs (map ast-binding-var bindings) filtered-subs)))
        (reconstruct-fix-node expr
                              (map (partial loop updated-subs)
                                   (map (partial rename-binding updated-subs)
                                        bindings))
                              (loop updated-subs body))))

     (else
      (walk-ast (partial loop subs) expr))))
  (loop subs expr))

(define (make-alpha-subs vars subs)
  (extend-subs (map (lambda (var)
                      (let ((s (ast-symbol-value var)))
                        (cons s (gensym s))))
                    (filter (lambda (f)
                              ;; FIXME Wildcards are handled in a later phase.
                              (not (equal? '_ (ast-symbol-value f))))
                            vars))
               subs))

(define (rename-symbol subs expr)
  (let ((s (ast-symbol-value expr)))
    (set-ast-symbol-value expr
                          (replace-sub subs s (constantly s)))))

(define (rename-binding subs b)
  (set-ast-binding-var b
                       (rename-symbol subs (ast-binding-var b))))
