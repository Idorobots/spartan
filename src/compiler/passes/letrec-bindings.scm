;; Letrec expansion phase.

(load "compiler/utils/scc.scm")
(load "compiler/utils/utils.scm")

(load "compiler/env.scm")
(load "compiler/pass.scm")
(load "compiler/ast.scm")

(load "compiler/passes/freevars.scm") ;; FIXME Just for compute-let-fv & compute-letrec-fv

;; This expansion phase is facilitated by first running SCC algorithm that splits the letrec bindings into smaller, managable chunks and then performs a fixpoint conversion on the resulting lambdas and assignment conversion on the complex values esentially elliminating recursion and letrec.

;; This conversion uses topological sort on a strongly-connected components DAG of the input letrec in order to "untangle" the recursive definitions as much as possible.

;; This method levredges the fact that binding sets usually are sparsly connected by recursion, meaning they can be reordered to construct much smaller sets of bindings that can be handled separately.
;; This is achieved by using a strongly connected components (SCC) algorithm, such as the Kosaraju's or Tarjan's algorithm, to first partition the graph into smaller chunks and then topologically sort them to preserve variable dependencies.
;; For instance, the following letrec:

;; (letrec ((a (lambda () (b)))
;;          (b (lambda () (c) (d)))
;;          (c (lambda () (a)))
;;          (d (lambda () (e)))
;;          (e (lambda () 23))
;;   (d))

;; ...results in the following graph:

;;   a
;;  / \
;; b---c
;; |
;; d---e

;; ...and the sorted strongly connected components:

;; '((e) (d) (a b c))

;; This in turn allows us to rewrite the original letrec as:

;; (let ((e (lambda () 23)))
;;   (let (d (lambda () (e)))
;;     (letrec ((a (lambda () (b)))
;;              (b (lambda () (c) (d)))
;;              (c (lambda () (a))))
;;       (d))))

;; ...which is considerably simpler to compile and optimize.

(define reorder-letrec-bindings
  (pass (schema "reorder-letrec-bindings"
                'ast (ast-subset? '(const symbol
                                    if do let letrec binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast reorder-letrec))))

(define (reorder-letrec expr)
  (map-ast id
           (lambda (expr)
             (if (letrec-node? expr)
                 (replace expr
                          (scc-reorder (derive-graph expr) expr))
                 expr))
           expr))

;; Dependency derivation:

(define +ordered-bindings+ false)

(define (derive-graph expr)
  (let* ((deps (derive-dependencies expr)))
    (if +ordered-bindings+
        ;; NOTE Explicitly includes variable ordering.
        (append deps
                (filter (lambda (e)
                          (not (member e deps)))
                        (derive-ordering expr)))
        deps)))

(define (derive-dependencies expr)
  (let ((vars (apply set (get-bound-vars expr))))
    (foldl append
           '()
           (map (lambda (b)
                  (map (lambda (e)
                         ;; NOTE So that we can process somewhat malformed expressions.
                         (list (safe-symbol-value (ast-binding-var b)) e))
                       (set-intersection vars
                                         (get-free-vars (ast-binding-val b)))))
                (ast-letrec-bindings expr)))))

(define (derive-ordering expr)
  (let ((vars (map (compose safe-symbol-value ast-binding-var)
                   ;; NOTE Straight up values cannot side-effect, so we don't need to preserve their ordering.
                   (filter (compose (partial equal? 'complex) get-complexity)
                           (ast-letrec-bindings expr)))))
    (if (empty? vars)
        '()
        (map list
             (cdr vars)
             (reverse (cdr (reverse vars)))))))

;; Reordering:

(define (scc-reorder dep-graph expr)
  (let ((bindings (ast-letrec-bindings expr))
        (body (ast-letrec-body expr))
        (scc (scc (get-bound-vars expr)
                  dep-graph)))
    (if (empty? scc)
        (reconstruct-let-node expr bindings body)
        (reorder-bindings scc expr bindings body))))

(define (reorder-bindings scc parent bindings body)
  (foldr (lambda (component acc)
           (let ((bs (filter (compose (flip member component) safe-symbol-value ast-binding-var)
                             bindings)))
             ;; NOTE Nodes resulting from reordering are artificially created, hence they are marked as such.
             (generated
              (if (recoursive? bs)
                  (reconstruct-letrec-node parent bs acc)
                  (reconstruct-let-node parent bs acc)))))
         body
         scc))

(define (reconstruct-letrec-node parent bindings body)
  (if (empty? bindings)
      body
      (compute-letrec-fv
       (at (get-location parent)
           (make-letrec-node bindings
                             body)))))

(define (reconstruct-let-node parent bindings body)
  (if (empty? bindings)
      body
      (compute-let-fv
       (at (get-location parent)
           (make-let-node bindings
                          body)))))