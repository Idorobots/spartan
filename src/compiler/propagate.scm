;; Generic binding propagation

(require "utils/utils.rkt")

(load-once "compiler/ast.scm")
(load-once "compiler/substitute.scm") ;; FIXME For subs-related functions.

(load-once "compiler/passes/freevars.scm") ;; FIXME Just for compute-*-fv

(define (propagate partition-by make-subs replace-with subs expr)
  (define (loop subs expr)
    (replace-with subs
                  expr
                  (lambda (expr)
                    (match-ast expr
                     ((const _)
                      expr)
                     ((lambda _ body)
                      (set-ast-lambda-body expr (loop (filter-subs subs
                                                                   (ast-node-bound-vars expr))
                                                      body)))
                     ((let bindings body)
                      (let* ((bs (partition-bindings partition-by bindings))
                             (filtered-subs (filter-subs subs (ast-node-bound-vars expr)))
                             (updated-subs (make-subs (car bs) filtered-subs))
                             (updated-bindings (map (partial loop subs)
                                                    (cdr bs))))
                        (reconstruct-let-node expr
                                              updated-bindings
                                              (loop updated-subs body))))
                     ((letrec bindings body)
                      (let* ((bs (select-first (partition-bindings partition-by bindings)
                                               bindings));; NOTE Can't use all propagatable bindings as they might interfere.
                             (filtered-subs (filter-subs subs (ast-node-bound-vars expr)))
                             (updated-subs (make-subs (car bs) filtered-subs))
                             (updated-bindings (map (partial loop updated-subs)
                                                    (cdr bs))))
                        (reconstruct-letrec-node expr
                                                 updated-bindings
                                                 (loop updated-subs body))))
                     ((fix bindings body)
                      (let* ((bs (select-first (partition-bindings partition-by bindings)
                                               bindings)) ;; NOTE Can't use all propagatable bindings as they might interfere.
                             (filtered-subs (filter-subs subs (ast-node-bound-vars expr)))
                             (updated-subs (make-subs (car bs) filtered-subs))
                             (updated-bindings (map (partial loop updated-subs)
                                                    (cdr bs))))
                        (reconstruct-fix-node expr
                                              updated-bindings
                                              (loop updated-subs body))))
                     ((binding _ val)
                      (set-ast-binding-val expr (loop subs val)))
                     (else
                      (traverse-ast loop subs expr))))))
  (loop subs expr))

(define (partition-bindings pred bindings)
  (foldr (lambda (b acc)
           (if (pred b)
               (cons (cons b (car acc))
                     (cdr acc))
               (cons (car acc)
                     (cons b (cdr acc)))))
         (cons '() '())
         bindings))

(define (select-first partitioned original)
  (let ((left (car partitioned)))
    (if (empty? left)
        partitioned
        (let ((selected (car left)))
          (cons (list selected)
                (filter (lambda (b)
                          (not (equal? b selected)))
                        original))))))

(define (reconstruct-let-node parent bindings body)
  (if (empty? bindings)
      body
      (compute-let-fv
       (make-ast-let (ast-node-location parent)
                     bindings
                     body))))

(define (reconstruct-letrec-node parent bindings body)
  (if (empty? bindings)
      body
      (compute-letrec-fv
       (make-ast-letrec (ast-node-location parent)
                        bindings
                        body))))

(define (reconstruct-fix-node parent bindings body)
  (if (empty? bindings)
      body
      (compute-fix-fv
       (make-ast-fix (ast-node-location parent)
                     bindings
                     body))))
