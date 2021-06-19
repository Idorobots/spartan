#lang racket

;; Rete nodes live here.

(require "utils.rkt")

(provide node type next-nodes data
         root-node node-a node-g node-1 node-2 node-2l node-r node-p node-t)

(define (node type next . data)
  (list* type next data))

(define (type node)
  (list-ref node 0))

(define (next-nodes node)
  (list-ref node 1))

(define (data node n)
  (list-ref node (+ 2 n)))

;; Nodes

(define (root-node nodes)
  (node 'root-node (ref nodes)))

(define (node-a action)
  (node 'node-a
        action))

(define (node-g var generator next-node)
  (node 'node-g
        (ref (list next-node))
        (lambda ()
          (list (cons var (generator))))))

(define (node-1 pattern next-node)
  (node 'node-1
        (ref (list next-node))
        pattern
        (ref null)))

(define (node-2 next-node)
  (node 'node-2
        (ref (list next-node))
        (ref null)
        (ref null)))

(define (node-2l node-2)
  (node 'node-2l
        node-2))

(define (node-r fun var acc vars next-node)
  (node 'node-r
        (ref (list next-node))
        (lambda (fact acc)
          (let ((vals (map (lambda (v) (assoc v fact))
                           vars)))
            (unless (any? false? vals)
              (apply fun acc (map cdr vals)))))
        var
        (ref acc)))

(define (var-apply fun vars alist)
  (apply fun
         (map (lambda (v)
                (if (variable? v)
                    (let ((b (assoc v alist)))
                      (if (pair? b)
                          (cdr b)
                          null))
                    v))
              vars)))

(define (node-p fun vars next-node)
  (node 'node-p
        (ref (list next-node))
        (lambda (fact)
          (var-apply fun vars fact))))

(define (node-t var buffer-size fun vars next-node)
  (let ((buffer (ref (array buffer-size '())))
        (curr-index (ref -1))
        (beginning (ref 0))
        (end (ref 0))
        (trigger (ref 0))
        (triggered? (ref #f)))
    (node 'node-t
          (ref (list next-node))
          (lambda (fact cont)
            (assign! curr-index
                     (wrap (+ 1 (deref curr-index))
                           buffer-size))
            (array-assign! (deref buffer) (deref curr-index) fact)
            (if (deref triggered?)
                (when (= (deref curr-index) (wrap (- (deref beginning) 1)
                                                  buffer-size))
                  (let ((values (partition (deref buffer)
                                           buffer-size
                                           (deref beginning)
                                           (deref trigger)
                                           (deref beginning))))
                    (assign! triggered? #f)
                    (assign! trigger 0)
                    (assign! beginning 0)
                    (assign! curr-index -1)
                    (assign! buffer (array buffer-size '()))
                    (cont (list (cons var values)))))
                (when (var-apply fun vars fact)
                  (assign! triggered? #t)
                  (assign! trigger (deref curr-index))
                  (assign! beginning
                           (wrap (- (deref trigger) (floor (/ buffer-size 2)))
                                 buffer-size))))))))
