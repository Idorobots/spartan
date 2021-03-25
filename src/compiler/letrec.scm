;; Letrec expansion phase.

(load "compiler/utils/scc.scm")
(load "compiler/utils/utils.scm")

(load "compiler/env.scm")
(load "compiler/freevars.scm") ;; FIXME Just for get-fv & compute-let-fv

(load "compiler/ast.scm")
(load "compiler/substitute.scm")

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

(define (letrec-expand env)
  (env-update env 'ast expand-letrec))

(define (expand-letrec expr)
  (map-ast (lambda (expr)
             (if (letrec-node? expr)
                 (ref-conversion expr)
                 expr))
           id
           expr))

;; This conversion combines the best of three(?) worlds by using SCC reordering and mutation.

;; It begins much like the SCC-based conversion by reordering bindings and concentrating them into strongly connected components.
;; Afterwards, each SCC is converted according to the following logic:
;; First, partition the bindings into three sets - simple, lambdas and complex.

;; (letrec ((simple 23)
;;          (complex (func simple))
;;          (func (lambda (x) (+ x simple))))
;;   complex)

;; Simple bindings can be pulled out into a wrapping let expression.

;; (let ((simple 23))
;;   (letrec ((complex (func simple))
;;            (func (lambda (x) (+ x simple))))
;;     complex))

;; Lambdas are converted into a fix expression that can be efficiently handled later.

;; (let ((simple 23))
;;   (letrec ((complex (func simple)))
;;     (fix ((func (lambda (x) (+ x simple))))
;;          complex)))

;; Complex bindings are converted using the let-ref-assign method.

;; (let ((simple 23))
;;   (let ((complex (ref '())))
;;     (fix ((func (lambda (x) (+ x simple))))
;;       (assign complex (func simple))
;;       (deref complex)))

;; The fix expression can be either handled much like in the fixpoint conversion, or directly during the closure conversion phase by allocating a fat closure for all of these functions.

(define (ref-conversion expr)
  (scc-reorder (derive-graph expr)
               (partial waddell fix let-ref-assign)
               expr))

;; Dependency derivation:

(define +ordered-bindings+ false)

(define (derive-graph expr)
  (let*((bindings (ast-letrec-bindings expr))
        (deps (derive-dependencies bindings)))
    (if +ordered-bindings+
        ;; NOTE Explicitly includes variable ordering.
        (append deps
                (filter (lambda (e)
                          (not (member e deps)))
                        (derive-ordering bindings)))
        deps)))

(define (derive-dependencies bindings)
  (let ((vars (map (compose ast-symbol-value ast-binding-var) bindings)))
    (foldl append
           '()
           (map (lambda (b)
                  (map (lambda (e)
                         (list (ast-symbol-value (ast-binding-var b)) e))
                       (set-intersection (apply set vars)
                                         (get-fv (ast-binding-val b)))))
                bindings))))

(define (derive-ordering bindings)
  (let ((vars (map (compose ast-symbol-value ast-binding-var)
                   ;; NOTE Straight up values cannot side-effect, so we don't need to preserve their ordering.
                   (filter (compose not value-node? ast-binding-val)
                           bindings))))
    (if (not (empty? vars))
        (map list
             (cdr vars)
             (reverse (cdr (reverse vars))))
        '())))

;; Reordering:

(define (scc-reorder dep-graph fixer expr)
  (let ((bindings (ast-letrec-bindings expr))
        (body (ast-letrec-body expr))
        (scc (scc (get-bound-vars expr)
                  dep-graph)))
    (if (empty? scc)
        (reconstruct-let-node expr bindings body)
        (reorder-bindings scc fixer expr bindings body))))

(define (reorder-bindings scc fixer parent bindings body)
  (foldr (lambda (component acc)
           (let ((bs (filter (compose (flip member component) ast-symbol-value ast-binding-var)
                             bindings)))
             (if (recoursive? bs)
                 (fixer parent bs acc)
                 (reconstruct-let-node parent bs acc))))
         body
         scc))

(define (recoursive? bindings)
  (or (> (length bindings) 1)
      (set-member? (get-fv (ast-binding-val (car bindings)))
                   (ast-symbol-value (ast-binding-var (car bindings))))))

;; This conversion distributes the bindings into three groups - simple, lambdas & complex, and converts them accordingly.

(define (waddell fix let-void-set parent bindings body)
  (let* ((simple (filter (compose simple-node? ast-binding-val)
                         bindings))
         (lambdas (filter (compose lambda-node? ast-binding-val)
                          bindings))
         (complex (filter (lambda (b)
                            (not (or (member b simple)
                                     (member b lambdas))))
                          bindings))
         (lambdas-builder (if (recoursive? lambdas)
                              (partial fix parent lambdas)
                              (partial reconstruct-let-node parent lambdas)))
         (complex-builder (if (empty? complex)
                              lambdas-builder
                              (lambda (body)
                                (ast-update (let-void-set parent complex body)
                                            'body
                                            lambda-builder)))))
    (reconstruct-let-node parent
                          simple
                          (complex-builder
                           body))))

;; This conversion relies on boxing & assignments to implement assignment conversion on the variables that require it.

(define (let-ref-assign parent bindings body)
  (if (empty? bindings)
      body
      (let* ((vars (map (compose ast-symbol-value ast-binding-var) bindings))
             (refs (map (lambda (b)
                          (make-binding
                           (ast-binding-var b)
                           (replace (ast-binding-val b)
                                    (generated
                                     (make-app-node (make-symbol-node 'ref)
                                                    (list (make-quote-node
                                                           (make-list-node '()))))))))
                        bindings))
             (setters (map (lambda (b)
                             (let ((val (ast-binding-val b))
                                   (var (ast-binding-var b)))
                               (replace val
                                        (free-vars (set-union (get-fv val) (set 'assign! (ast-symbol-value var)))
                                                   (generated
                                                    (make-app-node (at (get-location val)
                                                                       (generated (make-symbol-node 'assign!)))
                                                                   (list var
                                                                         (derefy vars val))))))))
                           bindings))
             (body (derefy vars body)))
        (reconstruct-let-node parent
                              refs
                              (if (empty? setters)
                                  body
                                  (replace body
                                           (free-vars (set-union (get-fv body)
                                                                 (set-sum (map get-fv setters)))
                                                      (generated
                                                       (make-do-node (append setters (list body)))))))))))

(define (derefy refs expr)
  (map-ast id
           (lambda (expr)
             (if (and (symbol-node? expr)
                      (member (ast-symbol-value expr) refs))
                 (replace expr
                          (free-vars (set-insert (get-fv expr) 'deref)
                                     (generated
                                      (make-app-node (at (get-location expr)
                                                         (generated
                                                          (make-symbol-node 'deref)))
                                                     (list expr)))))
                 expr))
           expr))

;; Delegates implementation of the actual fixpoint conversion to the closure conversion phase.

(define (fix parent bindings body)
  (if (empty? bindings)
      body
      (replace parent
               (generated
                ;; FIXME This isn't actually a letrec...
                (compute-letrec-fv
                 (make-fix-node bindings
                                body))))))

(define (reconstruct-let-node parent bindings body)
  (if (empty? bindings)
      body
      (replace parent
               (generated
                (compute-let-fv
                 (make-let-node bindings
                                body))))))
