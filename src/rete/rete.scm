;; Rete implementation.

(load "rete/utils.scm")
(load "rete/nodes.scm")
(load "rete/patternmatch.scm")
(load "rete/compiler.scm")
(load "rete/factstore.scm")

;; State:

(define *rete* (ref (root-node null)))
(define *rules* (ref null))

;; Utils:

(define (reset-rete!)
  (reset-facts!)
  (assign! *rete* (root-node null))
  (assign! *rules* null))

(define (call-next f nodes value)
  (for-each (lambda (n) (f n value)) nodes))

(define (unify-call binding memory f)
  (for-each f
            (filter not-false?
                    (map (partial merge binding)
                         memory))))

(define (apply-rule id . args)
  (if (procedure? id)
      (apply id args)
      (let ((rule (assoc id (deref *rules*))))
        (when rule
          (apply (cdr rule) args)))))

;; Rete actions:

(define (assert-fact! node fact)
  (match node
    (`(root-node ,next)
     (call-next assert-fact! (deref next) fact))

    (`(node-a ,action)
     (apply-rule action fact))

    (`(node-1 ,next ,pattern ,memory)
     (unless (member fact (deref memory))
       (let ((bindings (unify pattern fact)))
         (unless (null? bindings)
           (assign! memory (cons fact (deref memory)))
           (call-next assert-fact! (deref next) bindings)))))

    (`(node-r ,next ,fun ,var ,acc)
     (let ((r (fun fact (deref acc))))
       (unless (equal? r (deref acc))
         (assign! acc r)
         (call-next assert-fact! (deref next) (list (cons var r))))))

    (`(node-p ,next ,fun)
     (when (fun fact)
       (call-next assert-fact! (deref next) fact)))

    (`(node-t ,next ,fun)
     (fun fact
          (lambda (result)
            (call-next assert-fact! (deref next) result))))

    (`(node-g ,next ,generator)
     (call-next assert-fact! (deref next) (generator)))

    (`(node-2 ,next ,l-mem ,r-mem)
     (assert-fact-node2! next fact r-mem l-mem))

    (`(node-2l (node-2 ,next ,l-mem ,r-mem))
     (assert-fact-node2! next fact l-mem r-mem))))

(define (assert-fact-node2! nodes fact this-mem other-mem)
  (unless (member fact (deref this-mem))
    (assign! this-mem (cons fact (deref this-mem)))
    (unify-call fact
                (deref other-mem)
                (partial call-next
                         assert-fact!
                         (deref nodes)))))

(define (retract-fact! node fact)
  (match node
    (`(root-node ,next)
     (call-next retract-fact! (deref next) fact))

    (`(node-1 ,next ,pattern ,memory)
     (when (member fact (deref memory))
       (let ((bindings (unify pattern fact)))
         (unless (null? bindings)
           (call-next retract-fact! (deref next) bindings)))
       (assign! memory
                (filter (partial not-equal? fact)
                        (deref memory)))))

    (`(node-r ,next ,fun ,var ,acc)
     (let ((r (fun fact (deref acc))))
       (unless (equal? r (deref acc))
         ;; NOTE No need to retract anything but we still need to check the next node.
         (call-next retract-fact! (deref next) (list (cons var r))))))

    (`(node-p ,next ,fun)
     (when (fun fact)
       (call-next retract-fact! (deref next) fact)))

    (`(node-t ,next ,fun)
     (fun fact
          (lambda (result)
            (call-next retract-fact! (deref next) result))))

    (`(node-g ,next ,generator)
     (call-next retract-fact! (deref next) (generator)))

    (`(node-2 ,next ,l-mem ,r-mem)
     (retract-fact-node2! next fact r-mem l-mem))

    (`(node-2l (node-2 ,next ,l-mem ,r-mem))
     (retract-fact-node2! next fact l-mem r-mem))

    (_ null)))

(define (retract-fact-node2! nodes fact this-mem other-mem)
  (when (member fact (deref this-mem))
    (unify-call fact
                (deref other-mem)
                (partial call-next
                         retract-fact!
                         (deref nodes)))
    ;; FIXME Since two different rules can introduce the same bindings
    ;; FIXME a global renaming scheme should be used or original facts
    ;; FIXME should be stored.
    (assign! this-mem
             (filter (partial not-equal? fact)
                     (deref this-mem)))))

(define (signal-fact! node fact)
  (match node
    (`(root-node ,next)
     (call-next signal-fact! (deref next) fact))

    (`(node-a ,action)
     (apply-rule action fact))

    (`(node-1 ,next ,pattern ,memory)
     (unless (member fact (deref memory))
       (let ((bindings (unify pattern fact)))
         (unless (null? bindings)
           (call-next signal-fact! (deref next) bindings)))))

    (`(node-r ,next ,fun ,var ,acc)
     (let ((r (fun fact (deref acc))))
       (unless (equal? r (deref acc))
         ;; NOTE We need to store the new acc anyway.
         (assign! acc r)
         (call-next signal-fact! (deref next) (list (cons var r))))))

    (`(node-p ,next ,fun)
     (when (fun fact)
       (call-next signal-fact! (deref next) fact)))

    (`(node-t ,next ,fun)
     (fun fact
          (lambda (result)
            (call-next signal-fact! (deref next) result))))

    (`(node-g ,next ,generator)
     (call-next signal-fact! (deref next) (generator)))

    (`(node-2 ,next ,l-mem ,r-mem)
     (signal-fact-node2! next fact r-mem l-mem))

    (`(node-2l (node-2 ,next ,l-mem ,r-mem))
     (signal-fact-node2! next fact l-mem r-mem))))

(define (signal-fact-node2! nodes fact this-mem other-mem)
  (unless (member fact (deref this-mem))
    (unify-call fact
                (deref other-mem)
                (partial call-next
                         signal-fact!
                         (deref nodes)))))

;; Network merging & optimization:

(define (merge-networks node-a node-b)
  ;; FIXME Actually implement network merging...
  (root-node (append (deref (next-nodes node-a))
                     (deref (next-nodes node-b)))))

(define (add-rule! id node action)
  (assign! *rules*
           (cons (cons id action) (deref *rules*)))
  (assign! *rete*
           (merge-networks (deref *rete*) node)))

(define (remove-rule! id)
  (assign! *rules*
           (filter (lambda (r)
                     (not-equal? (car r) id))
                   (deref *rules*))))

;; Convenience functions:
(define (assert! fact)
  (add-fact! fact)
  (assert-fact! (deref *rete*) fact))

(define (signal! fact)
  (signal-fact! (deref *rete*) fact))

(define (retract! fact)
  (remove-fact! fact)
  (retract-fact! (deref *rete*) fact))

(define (whenever pattern action)
  (let ((id (gensym 'rule)))
    (add-rule! id
               (compile-rule pattern id)
               action)
    id))

(define (select pattern)
  (let* ((store (ref nil))
         (rule (compile-rule pattern
                             (lambda (bindings)
                               (assign! store
                                        (cons bindings
                                              (deref store)))))))
    (map-facts (lambda (fact)
                 (assert-fact! rule fact)))
    (deref store)))
