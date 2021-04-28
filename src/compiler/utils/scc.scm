;; Strongly-connected components

(load-once "compiler/utils/set.scm")
(load-once "compiler/utils/utils.scm")

;; Kosaraju's algorithm:

(define (scc verts graph)
  (let* ((visited (foldl (lambda (v acc)
                           (visit v (car acc) (cdr acc) graph))
                         (cons (set) '())
                         verts))
         (postorder (cdr visited))
         (t-graph (transpose graph))
         (assigned (foldl (lambda (v acc)
                            (assign v v (car acc) (cdr acc) t-graph))
                          (cons (set) '())
                          postorder))
         (scc (cdr assigned)))
    (group scc)))

(define (visit v visited postorder graph)
  (let ((edges (edges v graph)))
    (if (set-member? visited v)
        (cons visited postorder)
        (let* ((result (foldl (lambda (e acc)
                                (visit e (car acc) (cdr acc) graph))
                              (cons (set-insert visited v) postorder)
                              edges))
               (new-visited (car result))
               (new-postorder (cdr result)))
          (cons new-visited
                (cons v new-postorder))))))

;; ((a b) (a c) (b c) (c a)) -> ((b a) (c a) (c b) (a c))
(define (transpose g)
  (map reverse (filter (lambda (e)
                         (> (length e) 1))
                       g)))

(define (assign v root assigned scc t-graph)
  (let ((edges (edges v t-graph)))
    (if (set-member? assigned v)
        (cons assigned scc)
        (foldl (lambda (e acc)
                 (assign e root (car acc) (cdr acc) t-graph))
               (cons (set-insert assigned v)
                     (cons (list root v) scc))
               edges))))

(define (edges v g)
  (map cadr
       (filter (lambda (e)
                 (and (> (length e) 1)
                      (equal? v (car e))))
               g)))

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
