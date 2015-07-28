;; An implementation of backward chaining on top of Rete networks.

(load "rete/utils.scm")
(load "rete/rete.scm")
(load "rete/compiler.scm")

(define-syntax select
  (syntax-rules ()
    ((select variables pattern)
     (let* ((store (ref null))
            (rule (compile-rule 'pattern
                                (lambda (bindings)
                                  (assign! store
                                           (cons (map (lambda (v)
                                                        (let ((b (assoc v bindings)))
                                                          (when b (cdr b))))
                                                      'variables)
                                                 (deref store)))))))
       (map-facts (lambda (fact)
                    (assert-fact! rule fact)))
       (deref store)))))
