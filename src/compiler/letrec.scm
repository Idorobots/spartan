;; Letrec expansion phase.

(load "compiler/ast.scm")
(load "compiler/utils.scm")

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

;; FIXME Rewrite in terms of ast/walk.
(define (free-vars expr)
  ;; FIXME This ought to be a separate AST annotation phase.
  (cond ((symbol? expr)
         (list expr))
        ((simple? expr)
         '())
        ((or (let? expr)
             (letrec? expr))
         (let ((bindings (let-bindings expr)))
           (filter (lambda (v)
                     (not (member v (bindings-vars bindings))))
                   (foldl append
                          (free-vars (let-body expr))
                          (map free-vars (bindings-vals bindings))))))
        ((letcc? expr)
         (let ((binding (let-bindings expr)))
           (filter (lambda (v)
                     (not (equal? v binding)))
                   (free-vars (let-body expr)))))
        ((lambda? expr)
         (filter (lambda (v)
                   (not (member v (lambda-args expr))))
                 (free-vars (lambda-body expr))))
        (else
         (append (free-vars (car expr))
                 (free-vars (cdr expr))))))

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

;; This conversion uses the generalized Y combinator in order to implement recursion without circularity or mutation.

;; The conversion is performed in multiple steps, starting with the input code:

;; (letrec ((even? (lambda (x)
;;                   (or (zero? x)
;;                       (odd? (- x 1)))))
;;          (odd? (lambda (x)
;;                  (not (even? x)))))
;;   (list (even? 1)
;;         (odd? 2)
;;         (even? 2)
;;         (odd? 3)))

;; First add all the letrec binding vars as extra parameters to the lambdas:

;; (let ((even?+ (lambda (x even? odd?)
;;                 (or (zero? x)
;;                     (odd? (- x 1)))))
;;       (odd?+ (lambda (x even? odd?)
;;                (not (even? x)))))
;;   (list (even? 1)
;;         (odd? 2)
;;         (even? 2)
;;         (odd? 3)))

;; Abstract these into a separate lambda that returns the original function:

;; (let ((even?+ (lambda (even? odd?)
;;                 (lambda (x)
;;                   (or (zero? x)
;;                       (odd? (- x 1))))))
;;       (odd?+ (lambda (even? odd?)
;;                (lambda (x)
;;                  (not (even? x))))))
;;   (list (even? 1)
;;         (odd? 2)
;;         (even? 2)
;;         (odd? 3)))

;; Redefine the original functions within the body of the outer lambda:

;; (let ((even?+ (lambda (even?* odd?*)
;;                 (let ((even? (even?* even?* odd?*))
;;                       (odd? (odd?* even?* odd?*)))
;;                   (lambda (x)
;;                     (or (zero? x)
;;                         (odd? (- x 1)))))))
;;       (odd?+ (lambda (even?* odd?*)
;;                (let ((even? (even?* even?* odd?*))
;;                      (odd? (odd?* even?* odd?*)))
;;                  (lambda (x)
;;                    (not (even? x)))))))
;;   (list (even? 1)
;;         (odd? 2)
;;         (even? 2)
;;         (odd? 3)))

;; Redefine the original functions within the letrec body:

;; (let ((even?+ (lambda (even?* odd?*)
;;                 (lambda (x)
;;                   (let ((even? (even?* even?* odd?*))
;;                         (odd? (odd?* even?* odd?*)))
;;                     (or (zero? x)
;;                         (odd? (- x 1)))))))
;;       (odd?+ (lambda (even?* odd?*)
;;                (lambda (x)
;;                  (let ((even? (even?* even?* odd?*))
;;                        (odd? (odd?* even?* odd?*)))
;;                    (not (even? x)))))))
;;   (let ((even? (even?+ even?+ odd?+))
;;         (odd? (odd?+ even?+ odd?+)))
;;     (list (even? 1)
;;           (odd? 2)
;;           (even? 2)
;;           (odd? 3))))

;; Prune the unused values:

;; (let ((even? (lambda (even?* odd?*)
;;                (lambda (x)
;;                  (let ((odd? (odd?* even?* odd?*)))
;;                    (or (zero? x)                       ;; Same as original.
;;                        (odd? (- x 1)))))))
;;       (odd? (lambda (even?* odd?*)
;;               (lambda (x)
;;                 (let ((even? (even?* even?* odd?*)))
;;                   (not (even? x)))))))                 ;; Same as original.
;;   (let ((even? (even?+ even?+ odd?+))
;;         (odd? (odd?+ even?+ odd?+)))
;;     (list (even? 1)                                    ;; Same as original.
;;           (odd? 2)
;;           (even? 2)
;;           (odd? 3))))

(define (rebound-vars vars all-vars)
  (map (lambda (v)
         (list v (cons v all-vars)))
       vars))

(define (filter-unused bindings free-vars)
  (filter (lambda (b)
            (member (binding-var b) free-vars))
          bindings))

(define (rewrapped-lambdas vars lambdas rebound)
  (map (lambda (v l)
         (list v
               (let* ((body (lambda-body l))
                      (free-vars (free-vars body))
                      (reconstructed (filter-unused rebound free-vars)))
                 (make-lambda vars
                              (make-lambda (lambda-args l)
                                           (if (empty? reconstructed)
                                               body
                                               (make-let reconstructed
                                                         body)))))))
       vars
       lambdas))

(define (filter-bindings pred? bindings)
  (cond ((empty? bindings)
         '())
        ((pred? (cadar bindings))
         (cons (car bindings)
               (filter-bindings pred? (cdr bindings))))
        (else
         (filter-bindings pred? (cdr bindings)))))

;; FIXME Rewrite in terms of ast/walk.
(define (derefy expr refs)
  (cond ((empty? refs)
         expr)
        ((simple? expr)
         expr)
        ((and (symbol? expr)
              (member expr refs))
         (make-app-1 'deref expr))
        ((or (let? expr)
             (letrec? expr))
         (let* ((bindings (let-bindings expr))
                (body (let-body expr))
                (vars (bindings-vars bindings))
                (unbound-refs (filter (lambda (r)
                                        (not (member r vars)))
                                      refs))
                (derefied-bindings (map (lambda (b)
                                          (list (car b)
                                                (derefy (cadr b) unbound-refs)))
                                        bindings)))
           ((if (let? expr)
                make-let
                make-letrec)
            derefied-bindings
            (derefy body unbound-refs))))
        ((lambda? expr)
         (let ((vars (lambda-args expr)))
           (make-lambda vars
                        (derefy (lambda-body expr)
                                (filter (lambda (r)
                                          (not (member r vars)))
                                        refs)))))
        ((pair? expr)
         (cons (derefy (car expr) refs)
               (derefy (cdr expr) refs)))
        (else expr)))

(define (let-ref-fix expr)
  (let ((bindings (letrec-bindings expr))
        (body (letrec-body expr)))
    (if (empty? bindings)
        body
        (let* ((lambda-bindings (filter-bindings lambda? bindings))
               (lambda-vars (bindings-vars lambda-bindings))
               (rest-bindings (filter-bindings (compose not lambda?) bindings))
               (rest-vars (bindings-vars rest-bindings))
               (vars (bindings-vars bindings))
               (rebound (rebound-vars lambda-vars lambda-vars))
               (body-free-vars (append (free-vars body)
                                       (free-vars (bindings-vals rest-bindings))))
               (let-builder (lambda (bindings body)
                              (if (empty? bindings)
                                  body
                                  (make-let bindings
                                            body))))
               (refs (map (lambda (v)
                            (list v '(ref '())))
                          rest-vars))
               (setters (map (lambda (b)
                               (make-app 'assign! (list (car b)
                                                        (derefy (cadr b) rest-vars))))
                             rest-bindings))
               (body (derefy body rest-vars)))
          (let-builder refs
                       (let-builder (rewrapped-lambdas lambda-vars
                                                       (map (lambda (v)
                                                              (derefy v rest-vars))
                                                            (bindings-vals lambda-bindings))
                                                       rebound)
                                    (let-builder (filter-unused rebound body-free-vars)
                                                 (if (empty? setters)
                                                     body
                                                     (make-do (append setters
                                                                      (list body)))))))))))

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

;; Complex bindings are converted using the let-void-set method.

;; (let ((simple 23))
;;   (let ((complex (void)))
;;     (fix ((func (lambda (x) (+ x simple))))
;;          (set! complex (func simple))
;;          complex)))

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
                 (waddell let-ref-fix ;; FIXME This ideally should just replace lambda-bindings with a (fix ...) operator.
                          let-ref-fix
                          expr))
               expr))
