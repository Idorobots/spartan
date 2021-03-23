;; Letrec expansion phase.

(load "compiler/utils/utils.scm")

(load "compiler/ast.scm")
(load "compiler/freevars.scm")
(load "compiler/substitute.scm")

;; This expansion phase is facilitated by first running SCC algorithm that splits the letrec bindings into smaller, managable chunks and then performs a fixpoint conversion on the resulting lambdas and assignment conversion on the complex values esentially elliminating recursion and letrec.

(define (letrec-expand expr)
  (walk identity
        (lambda (expr)
          (if (letrec? expr)
              (ref-conversion expr)
              expr))
        expr))

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

;; ((a b) (a c) (b c) (c a)) -> ((b a) (c a) (c b) (a c))
(define (transpose g)
  (map reverse (filter (lambda (e)
                         (> (length e) 1))
                       g)))

(define (edges v g)
  (map cadr
       (filter (lambda (e)
                 (and (> (length e) 1)
                      (equal? v (car e))))
               g)))

(define (visit v visited l graph)
  (let ((edges (edges v graph)))
    (if (member v visited) ;; FIXME Could be quicker.
        (list visited l)
        (let* ((result (foldl (lambda (e acc)
                                (visit e (car acc) (cadr acc) graph))
                              (list (cons v visited) l)
                              edges))
               (new-visited (car result))
               (new-l (cadr result)))
          (list new-visited
                (cons v new-l))))))

(define (assign v root assigned scc t-graph)
  (let ((edges (edges v t-graph)))
    (if (member v assigned) ;; FIXME Could be quicker.
        (list assigned scc)
        (foldl (lambda (e acc)
                 (assign e
                         root
                         (car acc)
                         (cadr acc)
                         t-graph))
               (list (cons v assigned)
                     (cons (list root v) scc))
               edges))))

(define (group scc)
  (map cdr
       (foldr (lambda (e acc)
                (cond ((empty? acc) (list e))
                      ((equal? (car e)
                               (caar acc))
                       (cons (append (car acc) (cdr e))
                             (cdr acc)))
                      (else
                       (cons e acc))))
              '()
              scc)))

;; SCC according to the Kosaraju's algorithm:

(define (scc verts graph)
  (let* ((visited (foldl (lambda (v acc)
                           (visit v (car acc) (cadr acc) graph))
                         (list '() '())
                         verts))
         (postorder (cadr visited))
         (t-graph (transpose graph))
         (assigned (foldl (lambda (v acc)
                            (assign v v (car acc) (cadr acc) t-graph))
                          (list '() '())
                          postorder))
         (scc (cadr assigned)))
    (group scc)))

;; Dependency derivation::

(define (derive-dependencies bindings)
  (let ((vars (bindings-vars bindings)))
    (foldl append
           '()
           (map (lambda (b)
                  (map (lambda (e)
                         (list (binding-var b) e))
                       (filter (lambda (v)
                                 (member v vars))
                               (free-vars (binding-val b)))))
                bindings))))

(define (derive-ordering bindings)
  (let ((vars (bindings-vars
               ;; NOTE Straight up values cannot side-effect, so we don't need to preserve their ordering.
               (filter (compose not value? binding-val)
                       bindings))))
    (if (not (empty? vars))
        (map list
             (cdr vars)
             (reverse (cdr (reverse vars))))
        '())))

(define +ordered-bindings+ false)

(define (derive-graph expr)
  ;; NOTE Explicitly includes variable ordering.
  (if +ordered-bindings+
      (lambda (bindings)
        (let ((deps (derive-dependencies bindings)))
          (append deps
                  (filter (lambda (e)
                            (not (member e deps)))
                          (derive-ordering bindings)))))
      derive-dependencies))

;; Reordering:

(define (recoursive? bindings)
  (or (> (length bindings) 1)
      (member (binding-var (car bindings))
              (free-vars (binding-val (car bindings))))))

(define (reorder-bindings fixer bindings body scc)
  (foldr (lambda (component acc)
           (let ((bs (filter (lambda (b)
                               (member (binding-var b)
                                       component))
                             bindings)))
             (if (recoursive? bs)
                 (fixer
                  (make-letrec bs acc))
                 (make-let bs acc))))
         body
         scc))

(define (scc-reorder deriver fixer expr)
  (let* ((bindings (letrec-bindings expr))
         (body (letrec-body expr))
         (dep-graph (deriver bindings))
         (scc (scc (bindings-vars bindings) dep-graph)))
    (if (empty? scc)
        (make-let bindings body)
        (reorder-bindings fixer bindings body scc))))

;; This conversion relies on boxing & assignments to implement assignment conversion on the variables that require it.

(define (derefy refs expr)
  (substitute (map (lambda (r)
                     (cons r (list 'deref r)))
                   refs)
              expr))

(define (let-ref-assign expr)
  (let ((bindings (letrec-bindings expr))
        (body (letrec-body expr)))
    (if (empty? bindings)
        body
        (let* ((vars (bindings-vars bindings))
               (body-free-vars (append (free-vars body)
                                       (free-vars (bindings-vals bindings))))
               (let-builder (lambda (bindings body)
                              (if (empty? bindings)
                                  body
                                  (make-let bindings
                                            body))))
               (refs (map (lambda (v)
                            (list v '(ref '())))
                          vars))
               (setters (map (lambda (b)
                               (make-app 'assign! (list (car b)
                                                        (derefy vars (cadr b)))))
                             bindings))
               (body (derefy vars body)))
          (let-builder refs
                       (if (empty? setters)
                           body
                           (make-do (append setters
                                            (list body)))))))))

;; Delegates implementation of the actual fixpoint conversion to the closure conversion phase.

(define (fix expr)
  (make-fix (letrec-bindings expr)
            (letrec-body expr)))

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

(define (waddell fix let-void-set expr)
  (let* ((bindings (letrec-bindings expr))
         (simple (filter (lambda (b)
                           (simple? (binding-val b)))
                         bindings))
         (lambdas (filter (lambda (b)
                            (lambda? (binding-val b)))
                          bindings))
         (complex (filter (lambda (b)
                            (not (or (member b simple)
                                     (member b lambdas))))
                          bindings))
         (lambdas-builder (cond ((empty? lambdas)
                                 identity)
                                ((not (recoursive? lambdas))
                                 (lambda (body)
                                   (make-let lambdas
                                             body)))
                                (else
                                 (lambda (body)
                                   (fix
                                    (make-letrec lambdas
                                                 body))))))
         (complex-builder (if (empty? complex)
                              lambdas-builder
                              (lambda (body)
                                (let ((conv (let-void-set (make-letrec complex body))))
                                  (list (car conv) (letrec-bindings conv)
                                        (lambdas-builder
                                         (letrec-body conv)))))))
         (simple-builder (if (empty? simple)
                             identity
                             (lambda (body)
                               (make-let simple
                                         body)))))
    (simple-builder
     (complex-builder
      (letrec-body expr)))))

(define (ref-conversion expr)
  (scc-reorder (derive-graph expr)
               (lambda (expr)
                 (waddell fix
                          let-ref-assign
                          expr))
               expr))
